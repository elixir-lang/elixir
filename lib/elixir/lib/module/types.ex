defmodule Module.Types do
  @moduledoc false

  alias Module.Types.{Expr, Pattern}

  @doc false
  def warnings(module, file, defs, no_warn_undefined, cache) do
    context = context()

    Enum.flat_map(defs, fn {{fun, arity} = function, kind, meta, clauses} ->
      stack = stack(with_file_meta(meta, file), module, function, no_warn_undefined, cache)

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
    {_types, context} = Pattern.of_head(args, guards, meta, stack, context)
    {_type, context} = Expr.of_expr(body, stack, context)
    context.warnings
  end

  @doc false
  def stack(file, module, function, no_warn_undefined, cache) do
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
      cache: cache
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
