defmodule Module.Types do
  @moduledoc false

  alias Module.Types.{Descr, Expr, Pattern}

  # These functions are not inferred because they are added/managed by the compiler
  @no_infer [__protocol__: 1, behaviour_info: 1]

  @doc false
  def infer(module, file, defs, env) do
    context = context()

    for {{fun, arity}, :def, _meta, clauses} <- defs,
        {fun, arity} not in @no_infer,
        into: %{} do
      stack = stack(:infer, file, module, {fun, arity}, :all, env)
      expected = List.duplicate(Descr.dynamic(), arity)

      pair_types =
        Enum.reduce(clauses, [], fn {meta, args, guards, body}, inferred ->
          try do
            {args, context} =
              Pattern.of_head(args, guards, expected, :default, meta, stack, context)

            {return, _context} = Expr.of_expr(body, stack, context)
            add_inferred(inferred, args, return, [])
          rescue
            e -> internal_error!(e, __STACKTRACE__, :def, meta, module, fun, args, guards, body)
          end
        end)

      {{fun, arity}, {:infer, Enum.reverse(pair_types)}}
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

  @doc false
  def warnings(module, file, defs, no_warn_undefined, cache) do
    context = context()

    Enum.flat_map(defs, fn {{fun, arity}, kind, meta, clauses} ->
      file = with_file_meta(meta, file)
      stack = stack(:dynamic, file, module, {fun, arity}, no_warn_undefined, cache)
      expected = List.duplicate(Descr.dynamic(), arity)

      Enum.flat_map(clauses, fn {meta, args, guards, body} ->
        try do
          {_types, context} =
            Pattern.of_head(args, guards, expected, :default, meta, stack, context)

          {_type, context} = Expr.of_expr(body, stack, context)
          context.warnings
        rescue
          e ->
            internal_error!(e, __STACKTRACE__, kind, meta, module, fun, args, guards, body)
        end
      end)
    end)
  end

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
  def context() do
    %{
      # A list of all warnings found so far
      warnings: [],
      # All vars and their types
      vars: %{},
      # Variables and arguments from patterns
      pattern_info: nil,
      # If type checking has found an error/failure
      failed: false
    }
  end
end
