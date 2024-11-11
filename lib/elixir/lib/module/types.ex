defmodule Module.Types do
  @moduledoc false
  alias Module.Types.{Descr, Expr, Pattern, Helpers}

  # These functions are not inferred because they are added/managed by the compiler
  @no_infer [__protocol__: 1, behaviour_info: 1]

  @doc false
  def infer(module, file, defs, private, used, env) do
    finder = &:lists.keyfind(&1, 1, defs)

    handler = fn meta, fun_arity, stack, context ->
      case local_handler(meta, fun_arity, stack, context, finder) do
        false ->
          undefined_function!(:undefined_function, meta, fun_arity, stack, env)
          false

        {kind, _, _} = triplet ->
          if (kind == :defmacro or kind == :defmacrop) and not Keyword.has_key?(meta, :super) do
            undefined_function!(:incorrect_dispatch, meta, fun_arity, stack, env)
            false
          else
            triplet
          end
      end
    end

    stack = stack(:infer, file, module, {:__info__, 1}, :all, env, handler)
    context = context(%{})

    {types, %{local_sigs: local_sigs}} =
      for {fun_arity, kind, meta, _clauses} = def <- defs,
          kind == :def or kind == :defmacro,
          reduce: {[], context} do
        {types, context} ->
          {_kind, inferred, context} =
            local_handler(meta, fun_arity, stack, context, fn _ -> def end)

          if kind == :def and fun_arity not in @no_infer do
            {[{fun_arity, inferred} | types], context}
          else
            {types, context}
          end
      end

    unreachable =
      for {fun_arity, _kind, _meta, _defaults} = info <- private,
          warn_unused_def(info, local_sigs, used, env),
          not is_map_key(local_sigs, fun_arity),
          do: fun_arity

    {Map.new(types), unreachable}
  end

  defp undefined_function!(reason, meta, {fun, arity}, stack, env) do
    env = %{env | function: stack.function, file: stack.file}
    tuple = {reason, {fun, arity}, stack.module}
    :elixir_errors.module_error(Helpers.with_span(meta, fun), env, __MODULE__, tuple)
  end

  defp warn_unused_def({_fun_arity, _kind, false, _}, _reachable, _used, _env) do
    :ok
  end

  defp warn_unused_def({fun_arity, kind, meta, 0}, reachable, used, env) do
    case meta == false or Map.has_key?(reachable, fun_arity) or fun_arity in used do
      true -> :ok
      false -> :elixir_errors.file_warn(meta, env, __MODULE__, {:unused_def, fun_arity, kind})
    end

    :ok
  end

  defp warn_unused_def({tuple, kind, meta, default}, reachable, used, env) when default > 0 do
    {name, arity} = tuple
    min = arity - default
    max = arity

    case min_reachable_default(max, min, :none, name, reachable, used) do
      :none -> :elixir_errors.file_warn(meta, env, __MODULE__, {:unused_def, tuple, kind})
      ^min -> :ok
      ^max -> :elixir_errors.file_warn(meta, env, __MODULE__, {:unused_args, tuple})
      diff -> :elixir_errors.file_warn(meta, env, __MODULE__, {:unused_args, tuple, diff})
    end

    :ok
  end

  defp min_reachable_default(max, min, last, name, reachable, used) when max >= min do
    fun_arity = {name, max}

    case Map.has_key?(reachable, fun_arity) or fun_arity in used do
      true -> min_reachable_default(max - 1, min, max, name, reachable, used)
      false -> min_reachable_default(max - 1, min, last, name, reachable, used)
    end
  end

  defp min_reachable_default(_max, _min, last, _name, _reachable, _used) do
    last
  end

  @doc false
  def warnings(module, file, defs, no_warn_undefined, cache) do
    finder = &:lists.keyfind(&1, 1, defs)
    handler = &local_handler(&1, &2, &3, &4, finder)
    stack = stack(:dynamic, file, module, {:__info__, 1}, no_warn_undefined, cache, handler)
    context = context(%{})

    context =
      Enum.reduce(defs, context, fn {fun_arity, _kind, meta, _clauses} = def, context ->
        {_kind, _inferred, context} =
          local_handler(meta, fun_arity, stack, context, fn _ -> def end)

        context
      end)

    context = warn_unused_clauses(defs, stack, context)
    context.warnings
  end

  defp warn_unused_clauses(defs, stack, context) do
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
  end

  defp local_handler(_meta, fun_arity, stack, context, finder) do
    case context.local_sigs do
      %{^fun_arity => {kind, inferred, _mapping}} ->
        {kind, inferred, context}

      %{^fun_arity => kind} when is_atom(kind) ->
        {kind, :none, context}

      local_sigs ->
        case finder.(fun_arity) do
          {fun_arity, kind, meta, clauses} ->
            context = put_in(context.local_sigs, Map.put(local_sigs, fun_arity, kind))

            {inferred, mapping, context} =
              local_handler(fun_arity, kind, meta, clauses, stack, context)

            context =
              update_in(context.local_sigs, &Map.put(&1, fun_arity, {kind, inferred, mapping}))

            {kind, inferred, context}

          false ->
            false
        end
    end
  end

  defp local_handler({fun, arity} = fun_arity, kind, meta, clauses, stack, context) do
    expected = List.duplicate(Descr.dynamic(), arity)
    stack = stack |> fresh_stack(fun_arity) |> with_file_meta(meta)

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
    {inferred, mapping, restore_context(context, clauses_context)}
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

  ## Module errors

  def format_error({:unused_args, {name, arity}}),
    do: "default values for the optional arguments in #{name}/#{arity} are never used"

  def format_error({:unused_args, {name, arity}, count}) when arity - count == 1,
    do: "the default value for the last optional argument in #{name}/#{arity} is never used"

  def format_error({:unused_args, {name, arity}, count}),
    do:
      "the default values for the last #{arity - count} optional arguments in #{name}/#{arity} are never used"

  def format_error({:unused_def, {name, arity}, :defp}),
    do: "function #{name}/#{arity} is unused"

  def format_error({:unused_def, {name, arity}, :defmacrop}),
    do: "macro #{name}/#{arity} is unused"

  def format_error({:undefined_function, {f, a}, _})
      when {f, a} in [__info__: 1, behaviour_info: 1, module_info: 1, module_info: 0],
      do:
        "undefined function #{f}/#{a} (this function is auto-generated by the compiler and must always be called as a remote, as in __MODULE__.#{f}/#{a})"

  def format_error({:undefined_function, {f, a}, module}),
    do:
      "undefined function #{f}/#{a} (expected #{inspect(module)} to define such a function or for it to be imported, but none are available)"

  def format_error({:incorrect_dispatch, {f, a}, _module}),
    do: "cannot invoke macro #{f}/#{a} before its definition"
end
