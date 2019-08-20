defmodule Module.Types do
  @moduledoc false

  import Module.Types.Helpers
  import Module.Types.Infer, only: [of_guard: 2, of_pattern: 2]

  @doc """
  Infer function definitions' types.
  """
  def infer_definitions(file, module, defs) do
    Enum.map(defs, fn {{fun, _arity} = function, kind, meta, clauses} ->
      context = context(file, module, function)

      types =
        map_ok(clauses, fn {_meta, params, guards, _body} ->
          def_expr = {kind, meta, [guards_to_expr(guards, {fun, [], params})]}

          expr_stack(def_expr, context, fn context ->
            of_clause(params, guards, context)
          end)
        end)

      {function, types}
    end)
  end

  @doc false
  def of_clause(params, guards, context) do
    with {:ok, types, context} <- map_reduce_ok(params, context, &of_pattern/2),
         {:ok, context} <- of_guard(guards_to_or(guards), context),
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
      # Stack of variables we have refined during unification,
      # used for creating relevant traces
      unify_stack: [],
      # Stack of expression we have recursed through during inference,
      # used for tracing
      expr_stack: [],
      # Counter to give type variables unique names
      counter: 0
    }
  end

  @doc """
  Lifts type variables to their infered types from the context.
  """
  def lift_types(types, context) do
    context = %{
      types: context.types,
      quantified_types: %{},
      quantified_counter: 0
    }

    {types, _context} = Enum.map_reduce(types, context, &do_lift_type/2)
    types
  end

  @doc false
  def lift_type(type, context) do
    context = %{
      types: context.types,
      quantified_types: %{},
      quantified_counter: 0
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

  ## VARIABLE QUANTIFICATION

  # Lift type variable to its infered types from the context
  defp do_lift_type({:var, var}, context) do
    case :maps.find(var, context.quantified_types) do
      {:ok, quantified_var} ->
        {{:var, quantified_var}, context}

      :error ->
        case :maps.find(var, context.types) do
          {:ok, :unbound} ->
            new_quantified_var(var, context)

          {:ok, type} ->
            # Remove visited types to avoid infinite loops
            # then restore after we are done recursing on vars
            types = context.types
            context = %{context | types: :maps.remove(var, context.types)}
            {type, context} = do_lift_type(type, context)
            {type, %{context | types: types}}

          :error ->
            new_quantified_var(var, context)
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

  defp do_lift_type({:cons, left, right}, context) do
    {left, context} = do_lift_type(left, context)
    {right, context} = do_lift_type(right, context)
    {{:cons, left, right}, context}
  end

  defp do_lift_type(other, context) do
    {other, context}
  end

  defp new_quantified_var(original_var, context) do
    types = :maps.put(original_var, context.quantified_counter, context.quantified_types)
    counter = context.quantified_counter + 1

    type = {:var, context.quantified_counter}
    context = %{context | quantified_types: types, quantified_counter: counter}
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
  def format_type({:tuple, types}) do
    "{#{Enum.map_join(types, ", ", &format_type/1)}}"
  end

  def format_type({:cons, left, :null}) do
    "[#{format_type(left)}]"
  end

  def format_type({:cons, left, right}) do
    "[#{format_type(left)} | #{format_type(right)}]"
  end

  def format_type({:map, pairs}) do
    case List.keytake(pairs, :__struct__, 0) do
      {{:__struct__, struct}, pairs} ->
        "%#{inspect(struct)}{#{format_map_pairs(pairs)}}"

      nil ->
        "%{#{format_map_pairs(pairs)}}"
    end
  end

  def format_type({:literal, literal}) do
    inspect(literal)
  end

  def format_type(:null) do
    "[]"
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
