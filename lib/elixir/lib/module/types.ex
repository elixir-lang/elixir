defmodule Module.Types do
  @moduledoc false

  import Module.Types.Helpers
  alias Module.Types.Infer

  @doc """
  Infer function definitions' types.
  """
  def infer_definitions(file, module, defs) do
    Enum.map(defs, fn {{fun, _arity} = function, kind, meta, clauses} ->
      context = context(file, module, function)
      stack = stack()

      types =
        map_ok(clauses, fn {_meta, params, guards, _body} ->
          def_expr = {kind, meta, [guards_to_expr(guards, {fun, [], params})]}

          stack = push_expr_stack(def_expr, stack)
          of_clause(params, guards, stack, context)
        end)

      {function, types}
    end)
  end

  @doc false
  def of_clause(params, guards, stack, context) do
    with {:ok, types, context} <-
           map_reduce_ok(params, context, &Infer.of_pattern(&1, stack, &2)),
         context = %{context | current_types: %{}, current_traces: %{}},
         # TODO: Check that of_guard/3 returns a boolean
         {:ok, _, context} <- Infer.of_guard(guards_to_or(guards), stack, context),
         do: {:ok, types, context}
  end

  @doc false
  def context(file, module, function) do
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
      # DEV
      current_types: %{},
      current_traces: %{},
      current_type_guards: %{},
      trace: true
    }
  end

  @doc false
  def stack() do
    %{
      # Stack of variables we have refined during unification,
      # used for creating relevant traces
      unify_stack: [],
      # Stack of expression we have recursed through during inference,
      # used for tracing
      expr_stack: []
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
    case :maps.find(var, context.lifted_types) do
      {:ok, lifted_var} ->
        {{:var, lifted_var}, context}

      :error ->
        case :maps.find(var, context.types) do
          {:ok, :unbound} ->
            new_lifted_var(var, context)

          {:ok, type} ->
            # Remove visited types to avoid infinite loops
            # then restore after we are done recursing on vars
            types = context.types
            context = %{context | types: :maps.remove(var, context.types)}
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
    types = :maps.put(original_var, context.lifted_counter, context.lifted_types)
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
    "{#{Enum.map_join(types, " | ", &format_type/1)}}"
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

  defp expr_to_string(expr) do
    expr
    |> rewrite_guard()
    |> Macro.to_string()
  end

  defp rewrite_guard(guard) do
    Macro.prewalk(guard, fn
      {{:., _, [:erlang, :element]}, _, [{{:., _, [:erlang, :+]}, _, [int, 1]}, arg]} ->
        {:elem, [], [arg, int]}

      {{:., _, [:erlang, :element]}, _, [int, arg]} when is_integer(int) ->
        {:elem, [], [arg, int - 1]}

      {:., _, [:erlang, call]} ->
        rewrite_guard_call(call)

      other ->
        other
    end)
  end

  defp rewrite_guard_call(:orelse), do: :or
  defp rewrite_guard_call(:andalso), do: :and
  defp rewrite_guard_call(:"=<"), do: :<=
  defp rewrite_guard_call(:"/="), do: :!=
  defp rewrite_guard_call(:"=:="), do: :===
  defp rewrite_guard_call(:"=/="), do: :!==

  defp rewrite_guard_call(op) when op in [:band, :bor, :bnot, :bsl, :bsr, :bxor],
    do: {:., [], [Bitwise, op]}

  defp rewrite_guard_call(op) when op in [:xor, :element, :size], do: {:., [], [:erlang, op]}
  defp rewrite_guard_call(op), do: op
end
