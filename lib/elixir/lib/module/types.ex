defmodule Module.Types do
  @moduledoc false

  import Module.Types.Helpers
  alias Module.Types.{Expr, Pattern}

  @doc """
  Infer function definitions' types.
  """
  def infer_definitions(file, module, defs) do
    clauses = infer_signatures(file, module, defs)
    infer_bodies(clauses)
  end

  defp infer_signatures(file, module, defs) do
    Enum.map(defs, fn {{fun, _arity} = function, kind, meta, clauses} ->
      stack = head_stack()
      context = head_context(file, module, function)

      clauses =
        Enum.map(clauses, fn {_meta, params, guards, body} ->
          def_expr = {kind, meta, [guards_to_expr(guards, {fun, [], params})]}
          stack = push_expr_stack(def_expr, stack)

          case of_head(params, guards, stack, context) do
            {:ok, _signature, context} -> {:ok, {context, body}}
            {:error, reason} -> {:error, reason}
          end
        end)

      {function, clauses}
    end)
  end

  defp infer_bodies(clauses) do
    Enum.map(clauses, fn {function, clauses} ->
      errors =
        Enum.flat_map(clauses, fn
          {:ok, {head_context, body}} ->
            stack = body_stack()
            context = body_context(head_context)

            case Expr.of_expr(body, stack, context) do
              {:ok, _type, _context} -> []
              {:error, reason} -> [reason]
            end

          {:error, reason} ->
            [reason]
        end)

      {function, errors}
    end)
  end

  @doc false
  def of_head(params, guards, stack, context) do
    with {:ok, types, context} <-
           map_reduce_ok(params, context, &Pattern.of_pattern(&1, stack, &2)),
         # TODO: Check that of_guard/3 returns a boolean
         {:ok, _, context} <- Pattern.of_guard(guards_to_or(guards), stack, context),
         do: {:ok, lift_types(types, context), context}
  end

  @doc false
  def head_context(file, module, function) do
    %{
      # File of module
      file: file,
      # Module of definitions
      module: module,
      # Current function
      function: function,
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
      # Track if a variable was infered from a type guard function such is_tuple/1
      # or a guard function that fails such as elem/2, possible values are:
      # `:guarded` when `is_tuple(x)`
      # `:fail` when `elem(x, 0)`
      # `:guarded_fail` when `is_tuple and elem(x, 0)`
      guard_sources: %{}
    }
  end

  @doc false
  def head_stack() do
    %{
      # Stack of expression we have recursed through during inference,
      # used for tracing
      expr_stack: [],
      # When false do not add a trace when a type variable is refined,
      # useful when merging contexts where the variables already have traces
      trace: true,
      # Track if we are in a context where type guard functions should
      # affect inference
      type_guards_enabled?: true,
      # Context used to determine if unification is bi-directional, :expr
      # is directional, :pattern is bi-directional
      context: :pattern
    }
  end

  @doc false
  def body_context(head_context) do
    %{
      # File of module
      file: head_context.file,
      # Module of definitions
      module: head_context.module,
      # Current function
      function: head_context.function,
      # Expression variable to type variable
      vars: head_context.vars,
      # Type variable to expression variable
      types_to_vars: head_context.types_to_vars,
      # Type variable to type
      types: head_context.types,
      # Trace of all variables that have been refined to a type,
      # including the type they were refined to, why, and where
      traces: head_context.traces,
      # Counter to give type variables unique names
      counter: head_context.counter
    }
  end

  @doc false
  def body_stack() do
    %{
      # Stack of expression we have recursed through during inference,
      # used for tracing
      expr_stack: [],
      # When false do not add a trace when a type variable is refined,
      # useful when merging contexts where the variables already have traces
      trace: true,
      # Context used to determine if unification is bi-directional, :expr
      # is directional, :pattern is bi-directional
      context: :expr
    }
  end

  @doc """
  Lifts type variables to their infered types from the context.
  """
  def lift_types(types, context) do
    context = %{
      types: context.types,
      lifted_types: %{},
      lifted_counter: 0
    }

    {types, _context} = Enum.map_reduce(types, context, &do_lift_type/2)
    types
  end

  @doc false
  def lift_type(type, context) do
    context = %{
      types: context.types,
      lifted_types: %{},
      lifted_counter: 0
    }

    {type, _context} = do_lift_type(type, context)
    type
  end

  ## GUARDS

  # TODO: Remove this and let multiple when be treated as multiple clauses,
  #       meaning they will be intersection types
  defp guards_to_or([]) do
    []
  end

  defp guards_to_or(guards) do
    Enum.reduce(guards, fn guard, acc -> {{:., [], [:erlang, :orelse]}, [], [guard, acc]} end)
  end

  defp guards_to_expr([], left) do
    left
  end

  defp guards_to_expr([guard | guards], left) do
    guards_to_expr(guards, {:when, [], [left, guard]})
  end

  ## VARIABLE LIFTING

  # Lift type variable to its infered (hopefully concrete) types from the context
  defp do_lift_type({:var, var}, context) do
    case Map.fetch(context.lifted_types, var) do
      {:ok, lifted_var} ->
        {{:var, lifted_var}, context}

      :error ->
        case Map.fetch(context.types, var) do
          {:ok, :unbound} ->
            new_lifted_var(var, context)

          {:ok, type} ->
            # Remove visited types to avoid infinite loops
            # then restore after we are done recursing on vars
            types = context.types
            context = %{context | types: Map.delete(context.types, var)}
            {type, context} = do_lift_type(type, context)
            {type, %{context | types: types}}

          :error ->
            new_lifted_var(var, context)
        end
    end
  end

  defp do_lift_type({:tuple, types}, context) do
    {types, context} = Enum.map_reduce(types, context, &do_lift_type/2)
    {{:tuple, types}, context}
  end

  defp do_lift_type({:map, pairs}, context) do
    {pairs, context} =
      Enum.map_reduce(pairs, context, fn {key, value}, context ->
        {key, context} = do_lift_type(key, context)
        {value, context} = do_lift_type(value, context)
        {{key, value}, context}
      end)

    {{:map, pairs}, context}
  end

  defp do_lift_type({:list, type}, context) do
    {type, context} = do_lift_type(type, context)
    {{:list, type}, context}
  end

  defp do_lift_type(other, context) do
    {other, context}
  end

  defp new_lifted_var(original_var, context) do
    types = Map.put(context.lifted_types, original_var, context.lifted_counter)
    counter = context.lifted_counter + 1

    type = {:var, context.lifted_counter}
    context = %{context | lifted_types: types, lifted_counter: counter}
    {type, context}
  end

  ## ERROR FORMATTING

  def format_warning({:unable_unify, left, right, expr, traces}) do
    [
      "function clause will never match, found incompatibility:\n\n    ",
      format_type(left),
      " !~ ",
      format_type(right),
      "\n\n",
      format_expr(expr),
      format_traces(traces),
      "Conflict found at"
    ]
  end

  defp format_expr(nil) do
    []
  end

  defp format_expr(expr) do
    [
      "in expression:\n\n    ",
      expr_to_string(expr),
      "\n\n"
    ]
  end

  defp format_traces([]) do
    []
  end

  defp format_traces(traces) do
    Enum.map(traces, fn
      {var, {:type, type, expr, location}} ->
        [
          "where \"",
          Macro.to_string(var),
          "\" was given the type ",
          Module.Types.format_type(type),
          " in:\n\n    # ",
          format_location(location),
          "    ",
          expr_to_string(expr),
          "\n\n"
        ]

      {var1, {:var, var2, expr, location}} ->
        [
          "where \"",
          Macro.to_string(var1),
          "\" was given the same type as \"",
          Macro.to_string(var2),
          "\" in:\n\n    # ",
          format_location(location),
          "    ",
          expr_to_string(expr),
          "\n\n"
        ]
    end)
  end

  defp format_location({file, line}) do
    file = Path.relative_to_cwd(file)
    line = if line, do: [Integer.to_string(line)], else: []
    [file, ?:, line, ?\n]
  end

  @doc false
  def format_type({:union, types}) do
    "#{Enum.map_join(types, " | ", &format_type/1)}"
  end

  def format_type({:tuple, types}) do
    "{#{Enum.map_join(types, ", ", &format_type/1)}}"
  end

  def format_type({:list, type}) do
    "[#{format_type(type)}]"
  end

  def format_type({:map, pairs}) do
    case List.keytake(pairs, :__struct__, 0) do
      {{:__struct__, struct}, pairs} ->
        "%#{inspect(struct)}{#{format_map_pairs(pairs)}}"

      nil ->
        "%{#{format_map_pairs(pairs)}}"
    end
  end

  def format_type({:atom, literal}) do
    inspect(literal)
  end

  def format_type(atom) when is_atom(atom) do
    "#{atom}()"
  end

  def format_type({:var, index}) do
    "var#{index}"
  end

  defp format_map_pairs(pairs) do
    Enum.map_join(pairs, ", ", fn {left, right} ->
      "#{format_type(left)} => #{format_type(right)}"
    end)
  end

  @doc false
  def expr_to_string(expr) do
    expr
    |> reverse_rewrite()
    |> Macro.to_string()
  end

  defp reverse_rewrite(guard) do
    Macro.prewalk(guard, fn
      {:., _, [:erlang, :orelse]} -> :or
      {:., _, [:erlang, :andalso]} -> :and
      {{:., _, [mod, fun]}, _, args} -> erl_to_ex(mod, fun, args)
      other -> other
    end)
  end

  defp erl_to_ex(mod, fun, args) do
    case :elixir_rewrite.erl_to_ex(mod, fun, args) do
      {Kernel, fun, args} -> {fun, [], args}
      {mod, fun, args} -> {{:., [], [mod, fun]}, [], args}
    end
  end
end
