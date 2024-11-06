defmodule Module.Types do
  @moduledoc false

  alias Module.Types.{Expr, Pattern}

  @doc false
  def warnings(module, file, defs, no_warn_undefined, cache) do
    context = context()

    Enum.flat_map(defs, fn {{fun, arity} = function, kind, meta, clauses} ->
      stack =
        stack(:dynamic, with_file_meta(meta, file), module, function, no_warn_undefined, cache)

      Enum.flat_map(clauses, fn {meta, args, guards, body} ->
        try do
          warnings_from_clause(meta, args, guards, body, stack, context)
        rescue
          e ->
            def_expr = {kind, meta, [guards_to_expr(guards, {fun, [], args}), [do: body]]}

            error =
              RuntimeError.exception("""
              found error while checking types for #{Exception.format_mfa(module, fun, arity)}:

              #{Exception.format_banner(:error, e, __STACKTRACE__)}\

              The exception happened while checking this code:

              #{Macro.to_string(def_expr)}

              Please report this bug at: https://github.com/elixir-lang/elixir/issues
              """)

            reraise error, __STACKTRACE__
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

  defp guards_to_expr([], left) do
    left
  end

  defp guards_to_expr([guard | guards], left) do
    guards_to_expr(guards, {:when, [], [left, guard]})
  end

  defp warnings_from_clause(meta, args, guards, body, stack, context) do
    dynamic = Module.Types.Descr.dynamic()
    expected = Enum.map(args, fn _ -> dynamic end)
    {_types, context} = Pattern.of_head(args, guards, expected, :default, meta, stack, context)
    {_type, context} = Expr.of_expr(body, stack, context)
    context.warnings
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
      # List of calls to not warn on as undefined
      no_warn_undefined: no_warn_undefined,
      # A list of cached modules received from the parallel compiler
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
