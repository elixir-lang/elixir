defmodule Module.Types do
  @moduledoc false

  import Module.Types.Helpers
  alias Module.Types.{Expr, Pattern}

  @doc """
  Infer function definitions' types.
  """
  def infer_definitions(file, module, defs) do
    Enum.flat_map(defs, fn {{fun, _arity} = function, kind, meta, clauses} ->
      context = context(file, module, function)

      Enum.flat_map(clauses, fn {_meta, params, guards, body} ->
        def_expr = {kind, meta, [guards_to_expr(guards, {fun, [], params})]}
        infer_clause(params, guards, body, def_expr, context)
      end)
    end)
  end

  @doc false
  def infer_clause(params, guards, body, def_expr, context) do
    with {:ok, _types, context} <- of_head(params, guards, def_expr, context),
         {:ok, _type, _context} <- of_body(body, context) do
      []
    else
      {:error, reason} ->
        [reason]
    end
  end

  @doc false
  def of_head(params, guards, def_expr, context) do
    stack = push_expr_stack(def_expr, stack())

    with {:ok, types, context} <-
           map_reduce_ok(params, context, &Pattern.of_pattern(&1, stack, &2)),
         # TODO: Check that of_guard/3 returns boolean() | :fail
         {:ok, _, context} <- Pattern.of_guard(guards_to_or(guards), stack, context),
         do: {:ok, types, context}
  end

  @doc false
  def of_body(body, context) do
    Expr.of_expr(body, stack(), context)
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
      # Track if a variable was infered from a type guard function such is_tuple/1
      # or a guard function that fails such as elem/2, possible values are:
      # `:guarded` when `is_tuple(x)`
      # `:guarded` when `is_tuple and elem(x, 0)`
      # `:fail` when `elem(x, 0)`
      guard_sources: %{}
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
      # Track if we are in a context where type guard functions should
      # affect inference
      type_guards_enabled?: true,
      # Context used to determine if unification is bi-directional, :expr
      # is directional, :pattern is bi-directional
      context: nil
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
      Enum.map_reduce(pairs, context, fn {kind, key, value}, context ->
        {key, context} = do_lift_type(key, context)
        {value, context} = do_lift_type(value, context)
        {{kind, key, value}, context}
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
    cond do
      (match?({:ok, _}, map_dot(expr)) and (map_type?(left) and atom_type?(right))) or
          (atom_type?(left) and map_type?(right)) ->
        {:ok, {map, field}} = map_dot(expr)

        """
        parentheses are required when dynamically invoking zero-arity functions in \
        expression:

            #{String.replace(expr_to_string(expr), "\n", "\n    ")}

        "#{expr_to_string(map)}" is an atom and you attempted to fetch the field \
        #{field}. Make sure that "#{expr_to_string(map)}" is a map or add parenthesis \
        to invoke a function instead:

            #{String.replace(expr_to_string(invert_parens(expr)), "\n", "\n    ")}

        Conflict found at\
        """

      (match?({:ok, _}, remote_call(expr)) and (map_type?(left) and atom_type?(right))) or
          (atom_type?(left) and map_type?(right)) ->
        {:ok, {module, fun}} = remote_call(expr)

        """
        parentheses are not allowed when fetching fields on a map in expression:

            #{String.replace(expr_to_string(expr), "\n", "\n    ")}

        "#{expr_to_string(module)}" is a map and you attempted to invoke the function \
        #{fun}/0. Make sure that "#{expr_to_string(module)}" is an atom or remove \
        parentheses to fetch a field:

            #{String.replace(expr_to_string(invert_parens(expr)), "\n", "\n    ")}

        Conflict found at\
        """

      map_type?(left) and map_type?(right) and match?({:ok, _}, missing_field(left, right)) ->
        {:ok, atom} = missing_field(left, right)

        [
          "undefined field \"#{atom}\" in expression:",
          "\n\n    ",
          String.replace(expr_to_string(expr), "\n", "\n    "),
          "\n\n",
          format_traces(traces),
          "Conflict found at"
        ]

      true ->
        [
          "incompatible types:\n\n    ",
          format_types(left, right),
          "\n\n",
          format_expr(expr),
          format_traces(traces),
          "Conflict found at"
        ]
    end
  end

  defp map_dot(expr) do
    with {{:., _meta1, [map, field]}, meta2, []} <- expr,
         true <- Keyword.get(meta2, :no_parens, false) do
      {:ok, {map, field}}
    else
      _ -> :error
    end
  end

  defp remote_call(expr) do
    with {{:., _meta1, [module, field]}, meta2, []} <- expr,
         false <- Keyword.get(meta2, :no_parens, false) do
      {:ok, {module, field}}
    else
      _ -> :error
    end
  end

  defp invert_parens({{:., meta1, [expr1, expr2]}, meta2, []}) do
    {{:., meta1, [expr1, expr2]}, Keyword.update(meta2, :no_parens, true, &not/1), []}
  end

  defp missing_field(
         {:map, [{:required, {:atom, atom} = type, _}, {:optional, :dynamic, :dynamic}]},
         {:map, fields}
       ) do
    if List.keymember?(fields, type, 1) do
      :error
    else
      {:ok, atom}
    end
  end

  defp missing_field(
         {:map, fields},
         {:map, [{:required, {:atom, atom} = type, _}, {:optional, :dynamic, :dynamic}]}
       ) do
    if List.keymember?(fields, type, 1) do
      :error
    else
      {:ok, atom}
    end
  end

  defp format_types(left, right) do
    cond do
      map_type?(left) and not map_type?(right) ->
        [format_simplified_map(left), " !~ ", format_type(right)]

      not map_type?(left) and map_type?(right) ->
        [format_type(left), " !~ ", format_simplified_map(right)]

      true ->
        [format_type(left), " !~ ", format_type(right)]
    end
  end

  defp map_type?({:map, _}), do: true
  defp map_type?(_other), do: false

  defp atom_type?(:atom), do: true
  defp atom_type?(:boolean), do: true
  defp atom_type?({:atom, _}), do: false
  defp atom_type?(_other), do: false

  defp format_simplified_map({:map, pairs}) do
    case List.keyfind(pairs, {:atom, :__struct__}, 1) do
      {:required, {:atom, :__struct__}, {:atom, struct}} ->
        "%#{inspect(struct)}{}"

      {:required, {:atom, :__struct__}, {:var, _} = var} ->
        "%#{format_type(var)}{}"

      _ ->
        "map()"
    end
  end

  defp format_expr(nil) do
    []
  end

  defp format_expr(expr) do
    [
      "in expression:\n\n    ",
      String.replace(expr_to_string(expr), "\n", "\n    "),
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
          String.replace(expr_to_string(expr), "\n", "\n    "),
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
          String.replace(expr_to_string(expr), "\n", "\n    "),
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
    case List.keytake(pairs, {:atom, :__struct__}, 1) do
      {{:required, {:atom, :__struct__}, {:atom, struct}}, pairs} ->
        "%#{inspect(struct)}{#{format_map_pairs(pairs)}}"

      _ ->
        "%{#{format_map_pairs(pairs)}}"
    end
  end

  def format_type({:atom, literal}) do
    inspect(literal)
  end

  def format_type({:var, index}) do
    "var#{index}"
  end

  def format_type(atom) when is_atom(atom) do
    "#{atom}()"
  end

  defp format_map_pairs(pairs) do
    {atoms, others} = Enum.split_with(pairs, &match?({:required, {:atom, _}, _}, &1))
    {required, optional} = Enum.split_with(others, &match?({:required, _, _}, &1))

    Enum.map_join(atoms ++ required ++ optional, ", ", fn
      {:required, {:atom, atom}, right} ->
        "#{atom}: #{format_type(right)}"

      {:required, left, right} ->
        "#{format_type(left)} => #{format_type(right)}"

      {:optional, left, right} ->
        "optional(#{format_type(left)}) => #{format_type(right)}"
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
