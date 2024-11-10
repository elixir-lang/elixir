defmodule Module.Types do
  @moduledoc false

  alias Module.Types.{Descr, Expr, Pattern, Helpers}

  # TODO: Consider passing inferred types from infer into warnings
  # TODO: Consider removing code from locals tracker

  # These functions are not inferred because they are added/managed by the compiler
  @no_infer [__protocol__: 1, behaviour_info: 1]

  @doc false
  def infer(module, file, defs, env) do
    finder = &List.keyfind(defs, &1, 0)
    handler = &local_handler(&1, &2, &3, finder)
    stack = stack(:infer, file, module, {:__info__, 1}, :all, env, handler)
    context = context(%{})

    {types, _context} =
      for {fun_arity, kind, _meta, _clauses} = def <- defs,
          kind == :def and fun_arity not in @no_infer,
          reduce: {[], context} do
        {types, context} ->
          {_kind, inferred, context} = local_handler(fun_arity, stack, context, fn _ -> def end)
          {[{fun_arity, inferred} | types], context}
      end

    Map.new(types)
  end

  @doc false
  def warnings(module, file, defs, no_warn_undefined, cache) do
    finder = &List.keyfind(defs, &1, 0)
    handler = &local_handler(&1, &2, &3, finder)
    stack = stack(:dynamic, file, module, {:__info__, 1}, no_warn_undefined, cache, handler)
    context = context(%{})

    context =
      Enum.reduce(defs, context, fn {fun_arity, _kind, _meta, _clauses} = def, context ->
        {_kind, _inferred, context} = local_handler(fun_arity, stack, context, fn _ -> def end)
        context
      end)

    context =
      for {fun_arity, pending} <- context.local_used, pending != [], reduce: context do
        context ->
          {_fun_arity, kind, _meta, clauses} = List.keyfind(defs, fun_arity, 0)
          {_kind, _inferred, mapping} = Map.fetch!(context.local_sigs, fun_arity)

          clauses_indexes =
            for type_index <- pending, {clause_index, ^type_index} <- mapping, do: clause_index

          Enum.reduce(clauses_indexes, context, fn clause_index, context ->
            {meta, _args, _guards, _body} = Enum.fetch!(clauses, clause_index)
            stack = %{stack | function: fun_arity}
            Helpers.warn(__MODULE__, {:unused_clause, kind, fun_arity}, meta, stack, context)
          end)
      end

    context.warnings
  end

  defp local_handler(fun_arity, stack, context, finder) do
    case context.local_sigs do
      %{^fun_arity => {kind, inferred, _mapping}} ->
        {kind, inferred, context}

      %{^fun_arity => kind} when is_atom(kind) ->
        {kind, :none, context}

      local_sigs ->
        {{fun, arity}, kind, meta, clauses} =
          finder.(fun_arity) || raise "could not find #{inspect(fun_arity)}"

        expected = List.duplicate(Descr.dynamic(), arity)
        stack = stack |> fresh_stack(fun_arity) |> with_file_meta(meta)
        context = put_in(context.local_sigs, Map.put(local_sigs, fun_arity, kind))

        {_, _, mapping, clauses_types, clauses_context} =
          Enum.reduce(clauses, {0, 0, [], [], context}, fn
            {meta, args, guards, body}, {index, total, mapping, inferred, context} ->
              context = fresh_context(context)

              try do
                {args_types, context} =
                  Pattern.of_head(args, guards, expected, :default, meta, stack, context)

                {return_type, context} =
                  Expr.of_expr(body, stack, context)

                {type_index, inferred} =
                  add_inferred(inferred, args_types, return_type, total - 1, [])

                if type_index == -1 do
                  {index + 1, total + 1, [{index, total} | mapping], inferred, context}
                else
                  {index + 1, total, [{index, type_index} | mapping], inferred, context}
                end
              rescue
                e ->
                  internal_error!(e, __STACKTRACE__, kind, meta, fun, args, guards, body, stack)
              end
          end)

        inferred = {:infer, Enum.reverse(clauses_types)}
        triplet = {kind, inferred, mapping}
        context = restore_context(context, clauses_context)
        context = update_in(context.local_sigs, &Map.put(&1, fun_arity, triplet))
        {kind, inferred, context}
    end
  end

  # We check for term equality of types as an optimization
  # to reduce the amount of check we do at runtime.
  defp add_inferred([{args, existing_return} | tail], args, return, index, acc),
    do: {index, Enum.reverse(acc, [{args, Descr.union(existing_return, return)} | tail])}

  defp add_inferred([head | tail], args, return, index, acc),
    do: add_inferred(tail, args, return, index - 1, [head | acc])

  defp add_inferred([], args, return, -1, acc),
    do: {-1, [{args, return} | Enum.reverse(acc)]}

  defp with_file_meta(stack, meta) do
    case Keyword.fetch(meta, :file) do
      {:ok, {meta_file, _}} -> %{stack | file: meta_file}
      :error -> stack
    end
  end

  defp internal_error!(e, trace, kind, meta, fun, args, guards, body, stack) do
    def_expr = {kind, meta, [guards_to_expr(guards, {fun, [], args}), [do: body]]}

    exception =
      RuntimeError.exception("""
      found error while checking types for #{Exception.format_mfa(stack.module, fun, length(args))}:

      #{Exception.format_banner(:error, e, stack)}\

      The exception happened while checking this code:

      #{Macro.to_string(def_expr)}

      Please report this bug at: https://github.com/elixir-lang/elixir/issues
      """)

    reraise exception, trace
  end

  defp guards_to_expr([], left) do
    left
  end

  defp guards_to_expr([guard | guards], left) do
    guards_to_expr(guards, {:when, [], [left, guard]})
  end

  @doc false
  def stack(mode, file, module, function, no_warn_undefined, cache, handler)
      when mode in [:static, :dynamic, :infer] do
    %{
      # The fallback meta used for literals in patterns and guards
      meta: [],
      # File of module
      file: file,
      # Module of definitions
      module: module,
      # Current function
      function: function,
      # List of calls to not warn on as undefined or :all
      no_warn_undefined: no_warn_undefined,
      # A tuple with cache information or a Macro.Env struct indicating no remote traversals
      cache: cache,
      # The mode controls what happens on function application when
      # there are gradual arguments. Non-gradual arguments always
      # perform subtyping and return its output (OUT).
      #
      #   * :strict - Requires types signatures (not implemented).
      #     * Strong arrows with gradual performs subtyping and returns OUT
      #     * Weak arrows with gradual performs subtyping and returns OUT
      #
      #   * :static - Type signatures have been given.
      #     * Strong arrows with gradual performs compatibility and returns OUT
      #     * Weak arrows with gradual performs compatibility and returns dynamic()
      #
      #   * :dynamic - Type signatures have not been given.
      #     * Strong arrows with gradual performs compatibility and returns dynamic(OUT)
      #     * Weak arrows with gradual performs compatibility and returns dynamic()
      #
      #   * :infer - Same as :dynamic but skips remote calls.
      #
      # The mode may also control exhaustiveness checks in the future (to be decided).
      # We may also want for applications with subtyping in dynamic mode to always
      # intersect with dynamic, but this mode may be too lax (to be decided based on
      # feedback).
      mode: mode,
      # The function for handling local calls
      local_handler: handler
    }
  end

  @doc false
  def context(local_sigs) do
    %{
      # A list of all warnings found so far
      warnings: [],
      # All vars and their types
      vars: %{},
      # Variables and arguments from patterns
      pattern_info: nil,
      # If type checking has found an error/failure
      failed: false,
      # Local signatures
      local_sigs: local_sigs,
      # Local clauses
      local_used: %{}
    }
  end

  defp fresh_stack(stack, function) do
    %{stack | function: function}
  end

  defp fresh_context(context) do
    %{context | vars: %{}, failed: false}
  end

  defp restore_context(%{vars: vars, failed: failed}, later_context) do
    %{later_context | vars: vars, failed: failed}
  end

  ## Diagnostics

  def format_diagnostic({:unused_clause, kind, {fun, arity}}) do
    %{
      message: "this clause of #{kind} #{fun}/#{arity} is never used"
    }
  end
end
