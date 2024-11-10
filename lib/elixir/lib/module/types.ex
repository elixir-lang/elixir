defmodule Module.Types do
  @moduledoc false

  alias Module.Types.{Descr, Expr, Pattern}

  # TODO: Inference of tail recursion
  # TODO: Checking of unused private functions/clauses

  # These functions are not inferred because they are added/managed by the compiler
  @no_infer [__protocol__: 1, behaviour_info: 1]

  @doc false
  def infer(module, file, defs, env) do
    finder = &List.keyfind(defs, &1, 0)
    handler = &infer_signature_for(&1, &2, module, file, finder, env)
    context = context({handler, %{}})

    {types, _context} =
      for {fun_arity, kind, _meta, _clauses} = def <- defs,
          kind == :def and fun_arity not in @no_infer,
          reduce: {[], context} do
        {types, context} ->
          {_kind, inferred, context} =
            infer_signature_for(fun_arity, context, module, file, fn _ -> def end, env)

          {[{fun_arity, inferred} | types], context}
      end

    Map.new(types)
  end

  defp infer_signature_for(fun_arity, context, module, file, finder, env) do
    case context.local_handler do
      {_, %{^fun_arity => {kind, inferred}}} ->
        {kind, inferred, context}

      {_, _} ->
        {{fun, arity}, kind, _meta, clauses} = finder.(fun_arity)
        expected = List.duplicate(Descr.dynamic(), arity)

        stack = stack(:infer, file, module, fun_arity, :all, env)
        context = update_local_state(context, &Map.put(&1, fun_arity, {kind, :none}))

        {pair_types, context} =
          Enum.reduce(clauses, {[], context}, fn
            {meta, args, guards, body}, {inferred, context} ->
              context = context(context.local_handler)

              try do
                {args_types, context} =
                  Pattern.of_head(args, guards, expected, :default, meta, stack, context)

                {return_type, context} = Expr.of_expr(body, stack, context)
                {add_inferred(inferred, args_types, return_type, []), context}
              rescue
                e ->
                  internal_error!(e, __STACKTRACE__, kind, meta, module, fun, args, guards, body)
              end
          end)

        inferred = {:infer, Enum.reverse(pair_types)}
        {kind, inferred, update_local_state(context, &Map.put(&1, fun_arity, {kind, inferred}))}
    end
  end

  @doc false
  def warnings(module, file, defs, no_warn_undefined, cache) do
    finder = &List.keyfind(defs, &1, 0)
    handler = &warnings_for(&1, &2, module, file, finder, no_warn_undefined, cache)
    context = context({handler, %{}})

    context =
      Enum.reduce(defs, context, fn {fun_arity, _kind, _meta, _clauses} = def, context ->
        finder = fn _ -> def end

        {_kind, _inferred, context} =
          warnings_for(fun_arity, context, module, file, finder, no_warn_undefined, cache)

        context
      end)

    context.warnings
  end

  defp warnings_for(fun_arity, context, module, file, finder, no_warn_undefined, cache) do
    case context.local_handler do
      {_, %{^fun_arity => {kind, inferred}}} ->
        {kind, inferred, context}

      {_, _} ->
        {{fun, arity}, kind, meta, clauses} = finder.(fun_arity)
        expected = List.duplicate(Descr.dynamic(), arity)

        file = with_file_meta(meta, file)
        stack = stack(:dynamic, file, module, fun_arity, no_warn_undefined, cache)
        context = update_local_state(context, &Map.put(&1, fun_arity, {kind, :none}))

        {pair_types, context} =
          Enum.reduce(clauses, {[], context}, fn
            {meta, args, guards, body}, {inferred, context} ->
              context = fresh_context(context)

              try do
                {args_types, context} =
                  Pattern.of_head(args, guards, expected, :default, meta, stack, context)

                {return_type, context} = Expr.of_expr(body, stack, context)
                {add_inferred(inferred, args_types, return_type, []), context}
              rescue
                e ->
                  internal_error!(e, __STACKTRACE__, kind, meta, module, fun, args, guards, body)
              end
          end)

        inferred = {:infer, Enum.reverse(pair_types)}
        {kind, inferred, update_local_state(context, &Map.put(&1, fun_arity, {kind, inferred}))}
    end
  end

  # We check for term equality of types as an optimization
  # to reduce the amount of check we do at runtime.
  defp add_inferred([{args, existing_return} | tail], args, return, acc),
    do: Enum.reverse(acc, [{args, Descr.union(existing_return, return)} | tail])

  defp add_inferred([head | tail], args, return, acc),
    do: add_inferred(tail, args, return, [head | acc])

  defp add_inferred([], args, return, acc),
    do: [{args, return} | Enum.reverse(acc)]

  defp with_file_meta(meta, file) do
    case Keyword.fetch(meta, :file) do
      {:ok, {meta_file, _}} -> meta_file
      :error -> file
    end
  end

  defp internal_error!(e, stack, kind, meta, module, fun, args, guards, body) do
    def_expr = {kind, meta, [guards_to_expr(guards, {fun, [], args}), [do: body]]}

    exception =
      RuntimeError.exception("""
      found error while checking types for #{Exception.format_mfa(module, fun, length(args))}:

      #{Exception.format_banner(:error, e, stack)}\

      The exception happened while checking this code:

      #{Macro.to_string(def_expr)}

      Please report this bug at: https://github.com/elixir-lang/elixir/issues
      """)

    reraise exception, stack
  end

  defp guards_to_expr([], left) do
    left
  end

  defp guards_to_expr([guard | guards], left) do
    guards_to_expr(guards, {:when, [], [left, guard]})
  end

  @doc false
  def stack(mode, file, module, function, no_warn_undefined, cache)
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
      mode: mode
    }
  end

  @doc false
  def context(local_handler, warnings \\ []) do
    %{
      # A list of all warnings found so far
      warnings: warnings,
      # All vars and their types
      vars: %{},
      # Variables and arguments from patterns
      pattern_info: nil,
      # If type checking has found an error/failure
      failed: false,
      # Local handler
      local_handler: local_handler
    }
  end

  defp fresh_context(%{local_handler: local_handler, warnings: warnings}) do
    context(local_handler, warnings)
  end

  defp update_local_state(%{local_handler: {handler, state}} = context, fun) do
    %{context | local_handler: {handler, fun.(state)}}
  end
end
