defmodule Module.Types do
  @moduledoc false

  defmodule Error do
    defexception [:message]
  end

  alias Module.Types.{Expr, Pattern}

  @doc false
  def warnings(module, file, defs, no_warn_undefined, cache) do
    stack = stack()

    Enum.flat_map(defs, fn {{fun, arity} = function, kind, meta, clauses} ->
      context = context(with_file_meta(meta, file), module, function, no_warn_undefined, cache)

      Enum.flat_map(clauses, fn {_meta, args, guards, body} ->
        try do
          warnings_from_clause(args, guards, body, stack, context)
        rescue
          e ->
            def_expr = {kind, meta, [guards_to_expr(guards, {fun, [], args}), [do: body]]}

            error =
              Error.exception("""
              found error while checking types for #{Exception.format_mfa(module, fun, arity)}:

              #{Exception.format_banner(:error, e, __STACKTRACE__)}\

              The exception happened while checking this code:

              #{Macro.to_string(def_expr)}

              In case it is a bug, please report it at: https://github.com/elixir-lang/elixir/issues
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

  defp warnings_from_clause(args, guards, body, stack, context) do
    with {:ok, _types, context} <- Pattern.of_head(args, guards, stack, context),
         {:ok, _type, context} <- Expr.of_expr(body, stack, context) do
      context.warnings
    else
      {:error, context} -> context.warnings
    end
  end

  @doc false
  def context(file, module, function, no_warn_undefined, cache) do
    %{
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
      # Expression variable to type variable
      vars: %{},
      # Type variable to expression variable
      types_to_vars: %{},
      # Type variable to type
      types: %{},
      # Trace of all variables that have been refined to a type,
      # including the type they were refined to, why, and where
      traces: %{},
      # Counter to give type variables unique names
      counter: 0,
      # Track if a variable was inferred from a type guard function such is_tuple/1
      # or a guard function that fails such as elem/2, possible values are:
      # `:guarded` when `is_tuple(x)`
      # `:guarded` when `is_tuple and elem(x, 0)`
      # `:fail` when `elem(x, 0)`
      guard_sources: %{},
      # A list with all warnings from the running the code
      warnings: []
    }
  end

  @doc false
  def stack() do
    %{
      # Stack of variables we have refined during unification,
      # used for creating relevant traces
      unify_stack: [],
      # Last expression we have recursed through during inference,
      # used for tracing
      last_expr: nil,
      # When false do not add a trace when a type variable is refined,
      # useful when merging contexts where the variables already have traces
      trace: true,
      # There are two factors that control how we track guards.
      #
      # * consider_type_guards?: if type guards should be considered.
      #   This applies only at the root and root-based "and" and "or" nodes.
      #
      # * keep_guarded? - if a guarded clause should remain as guarded
      #   even on failure. Used on the right side of and.
      #
      type_guards: {_consider_type_guards? = true, _keep_guarded? = false},
      # Context used to determine if unification is bi-directional, :expr
      # is directional, :pattern is bi-directional
      context: nil
    }
  end

  @doc false
  def expr_to_string(expr) do
    expr
    |> reverse_rewrite()
    |> Macro.to_string()
  end

  defp reverse_rewrite(guard) do
    Macro.prewalk(guard, fn
      {{:., _, [mod, fun]}, meta, args} -> erl_to_ex(mod, fun, args, meta)
      other -> other
    end)
  end

  defp erl_to_ex(mod, fun, args, meta) do
    case :elixir_rewrite.erl_to_ex(mod, fun, args) do
      {Kernel, fun, args} -> {fun, meta, args}
      {mod, fun, args} -> {{:., [], [mod, fun]}, meta, args}
    end
  end
end
