defmodule Module.Types do
  @moduledoc false

  defmacrop is_var(expr) do
    quote do
      is_tuple(unquote(expr)) and
        tuple_size(unquote(expr)) == 3 and
        is_atom(elem(unquote(expr), 0)) and
        is_atom(elem(unquote(expr), 2))
    end
  end

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

  ## INFERENCE

  @doc false
  # :atom
  def of_pattern(atom, context) when is_atom(atom) do
    {:ok, {:literal, atom}, context}
  end

  # 12
  def of_pattern(literal, context) when is_integer(literal) do
    {:ok, :integer, context}
  end

  # 1.2
  def of_pattern(literal, context) when is_float(literal) do
    {:ok, :float, context}
  end

  # "..."
  def of_pattern(literal, context) when is_binary(literal) do
    {:ok, :binary, context}
  end

  # <<...>>>
  def of_pattern({:<<>>, _meta, args} = expr, context) do
    expr_stack(expr, context, fn context ->
      case reduce_ok(args, context, &of_binary/2) do
        {:ok, context} -> {:ok, :binary, context}
        {:error, reason} -> {:error, reason}
      end
    end)
  end

  # [left | right]
  def of_pattern([{:|, _meta, [left_expr, right_expr]}] = expr, context) do
    expr_stack(expr, context, fn context ->
      with {:ok, left, context} <- of_pattern(left_expr, context),
           {:ok, right, context} <- of_pattern(right_expr, context),
           do: {:ok, {:cons, left, right}, context}
    end)
  end

  # [left, right]
  def of_pattern([left_expr | right_expr] = expr, context) do
    expr_stack(expr, context, fn context ->
      with {:ok, left, context} <- of_pattern(left_expr, context),
           {:ok, right, context} <- of_pattern(right_expr, context),
           do: {:ok, {:cons, left, right}, context}
    end)
  end

  # []
  def of_pattern([], context) do
    {:ok, :null, context}
  end

  # _
  def of_pattern({:_, _meta, atom}, context) when is_atom(atom) do
    {:ok, :dynamic, context}
  end

  # var
  def of_pattern(var, context) when is_var(var) do
    {type, context} = new_var(var, context)
    {:ok, type, context}
  end

  # {left, right}
  def of_pattern({left, right}, context) do
    of_pattern({:{}, [], [left, right]}, context)
  end

  # {...}
  def of_pattern({:{}, _meta, exprs} = expr, context) do
    expr_stack(expr, context, fn context ->
      case map_reduce_ok(exprs, context, &of_pattern/2) do
        {:ok, types, context} -> {:ok, {:tuple, types}, context}
        {:error, reason} -> {:error, reason}
      end
    end)
  end

  # left = right
  def of_pattern({:=, _meta, [left_expr, right_expr]} = expr, context) do
    expr_stack(expr, context, fn context ->
      with {:ok, left_type, context} <- of_pattern(left_expr, context),
           {:ok, right_type, context} <- of_pattern(right_expr, context),
           do: start_unify(left_type, right_type, context)
    end)
  end

  # %{...}
  def of_pattern({:%{}, _meta, args} = expr, context) do
    result =
      expr_stack(expr, context, fn context ->
        map_reduce_ok(args, context, fn {key, value}, context ->
          with {:ok, key_type, context} <- of_pattern(key, context),
               {:ok, value_type, context} <- of_pattern(value, context),
               do: {:ok, {key_type, value_type}, context}
        end)
      end)

    case result do
      {:ok, pairs, context} -> {:ok, {:map, pairs}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  def of_pattern({:%, _meta, _args}, context) do
    {:ok, {:map, []}, context}
  end

  defp of_guard(guard, context) do
    expr_stack(guard, context, fn context ->
      # Flatten `foo and bar` to list of type constraints
      reduce_ok(flatten_binary_op(:and, guard), context, fn guard, context ->
        # Flatten `foo or bar` to later build a union
        union = Enum.map(flatten_binary_op(:or, guard), &type_guard/1)

        # Only use group of guards using a single variable
        with {:ok, args, guard_types} <- unzip_ok(union),
             [_arg] <- Enum.uniq(Enum.map(args, &remove_meta/1)) do
          expr_stack(guard, context, fn context ->
            union = to_union(guard_types, context)
            unify_guard(hd(args), union, context)
          end)
        else
          _ -> {:ok, context}
        end
      end)
    end)
  end

  # TODO: Remove this and let multiple when be treated as multiple clauses,
  #       meaning they will be intersection types
  defp guards_to_or([]) do
    []
  end

  defp guards_to_or(guards) do
    Enum.reduce(guards, fn guard, acc -> {{:., [], [:erlang, :orelse]}, [], [guard, acc]} end)
  end

  defp flatten_binary_op(:and, {{:., _, [:erlang, :andalso]}, _, [left, right]}) do
    flatten_binary_op(:and, left) ++ flatten_binary_op(:and, right)
  end

  defp flatten_binary_op(:or, {{:., _, [:erlang, :orelse]}, _, [left, right]}) do
    flatten_binary_op(:or, left) ++ flatten_binary_op(:or, right)
  end

  defp flatten_binary_op(_op, other) do
    [other]
  end

  defp unify_guard(arg, guard_type, context) when is_var(arg) do
    # TODO: It is incorrect to use of_pattern/2 but it helps for simplicity now
    with {:ok, arg_type, context} <- of_pattern(arg, context),
         {:ok, _, context} <- start_unify(arg_type, guard_type, context),
         do: {:ok, context}
  end

  defp unify_guard(_arg, _guard_type, context) do
    {:ok, context}
  end

  # Unimplemented guards:
  # !=/2 !==/2 </2 <=/2 >/2 >=/2 ==/2 ===/2 not/1
  # */2 +/1 +/2 -/1 -/2 //2
  # abs/2 ceil/1 floor/1 round/1 trunc/1
  # elem/2 hd/1 in/2 length/1 map_size/1 tl/1 tuple_size/1
  # is_function/1 is_function/2 is_list/1 is_number/1 is_pid/1 is_port/1 is_reference/1 is_tuple/1
  # node/1 self/1
  # binary_part/3 bit_size/1 byte_size/1 div/2 rem/2 node/0

  @type_guards %{
    is_atom: :atom,
    is_binary: :binary,
    is_bitstring: :binary,
    is_boolean: :boolean,
    is_float: :float,
    is_integer: :integer,
    is_map: {:map, []}
  }

  defp type_guard({{:., _, [:erlang, guard]}, _, [arg]}) do
    case :maps.find(guard, @type_guards) do
      {:ok, type} -> {:ok, arg, type}
      :error -> {:error, :unknown_guard}
    end
  end

  defp type_guard(_other), do: {:error, :unknown_guard}

  # binary-pattern :: specifier
  defp of_binary({:"::", _meta, [expr, specifiers]} = full_expr, context) do
    specifiers = List.flatten(collect_specifiers(specifiers))

    expected_type =
      cond do
        :integer in specifiers -> :integer
        :float in specifiers -> :float
        :bits in specifiers -> :binary
        :bitstring in specifiers -> :binary
        :bytes in specifiers -> :binary
        :binary in specifiers -> :binary
        :utf8 in specifiers -> :integer
        :utf16 in specifiers -> :integer
        :utf32 in specifiers -> :integer
        true -> :integer
      end

    expr_stack(full_expr, context, fn context ->
      with {:ok, type, context} <- of_pattern(expr, context),
           {:ok, _type, context} <- start_unify(type, expected_type, context),
           do: {:ok, context}
    end)
  end

  # binary-pattern
  defp of_binary(expr, context) do
    case of_pattern(expr, context) do
      {:ok, type, context} when type in [:integer, :float, :binary] ->
        {:ok, context}

      {:ok, type, _context} ->
        {:error, {:invalid_binary_type, type}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Push expr to stack inside fun and pop after fun
  # The expression stack is used for
  defp expr_stack(expr, context, fun) do
    expr_stack = context.expr_stack

    case fun.(%{context | expr_stack: [expr | context.expr_stack]}) do
      {:ok, context} -> {:ok, %{context | expr_stack: expr_stack}}
      {:ok, type, context} -> {:ok, type, %{context | expr_stack: expr_stack}}
      {:error, reason} -> {:error, reason}
    end
  end

  ## UNIFICATION

  defp start_unify(left, right, context) do
    case unify(left, right, context) do
      {:ok, type, context} -> {:ok, type, %{context | unify_stack: []}}
      {:error, reason} -> {:error, reason}
    end
  end

  defp unify(type, {:var, var}, context) do
    case :maps.get(var, context.types) do
      :unbound ->
        context = refine_var(var, type, context)
        context = push_unify_stack(var, context)

        if recursive_type?(type, [], context) do
          error({:unable_unify, type, {:var, var}}, context)
        else
          {:ok, {:var, var}, context}
        end

      var_type ->
        # Only add trace if the variable wasn't already "expanded"
        context =
          if variable_expanded?(var, context) do
            context
          else
            trace_var(var, type, context)
          end

        context = push_unify_stack(var, context)

        case unify(type, var_type, context) do
          {:ok, var_type, context} ->
            context = refine_var(var, var_type, context)
            {:ok, {:var, var}, context}

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp unify({:var, var}, type, context) do
    unify(type, {:var, var}, context)
  end

  defp unify({:tuple, lefts}, {:tuple, rights}, context)
       when length(lefts) == length(rights) do
    result =
      map_reduce_ok(Enum.zip(lefts, rights), context, fn {left, right}, context ->
        unify(left, right, context)
      end)

    case result do
      {:ok, types, context} -> {:ok, {:tuple, types}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  defp unify({:cons, left_left, left_right}, {:cons, right_left, right_right}, context) do
    with {:ok, left, context} <- unify(left_left, right_left, context),
         {:ok, right, context} <- unify(left_right, right_right, context),
         do: {:ok, {:cons, left, right}, context}
  end

  defp unify({:map, left_pairs}, {:map, right_pairs}, context) do
    # Since maps in patterns only support literal keys (excluding maps)
    # we can do exact type match without subtype checking

    unique_right_pairs =
      Enum.reject(right_pairs, fn {key, _value} ->
        :lists.keyfind(key, 1, left_pairs)
      end)

    unique_pairs = left_pairs ++ unique_right_pairs

    # Build union of all unique key-value pairs between the maps
    result =
      map_reduce_ok(unique_pairs, context, fn {left_key, left_value}, context ->
        case :lists.keyfind(left_key, 1, right_pairs) do
          {^left_key, right_value} ->
            case unify(left_value, right_value, context) do
              {:ok, value, context} -> {:ok, {left_key, value}, context}
              {:error, reason} -> {:error, reason}
            end

          false ->
            {:ok, {left_key, left_value}, context}
        end
      end)

    case result do
      {:ok, pairs, context} -> {:ok, {:map, pairs}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  defp unify(:dynamic, right, context) do
    {:ok, right, context}
  end

  defp unify(left, :dynamic, context) do
    {:ok, left, context}
  end

  defp unify(left, right, context) do
    cond do
      subtype?(left, right, context) -> {:ok, left, context}
      subtype?(right, left, context) -> {:ok, right, context}
      true -> error({:unable_unify, left, right}, context)
    end
  end

  # Check unify stack to see if variable was already expanded
  defp variable_expanded?(var, context) do
    Enum.any?(context.unify_stack, &variable_same?(var, &1, context))
  end

  # Find if two variables are the same or point to the other
  defp variable_same?(same, same, _context) do
    true
  end

  defp variable_same?(left, right, context) do
    case :maps.find(left, context.types) do
      {:ok, {:var, new_left}} ->
        variable_same?(new_left, right, context)

      _ ->
        case :maps.find(right, context.types) do
          {:ok, {:var, new_right}} -> variable_same?(left, new_right, context)
          :error -> false
        end
    end
  end

  defp push_unify_stack(var, context) do
    %{context | unify_stack: [var | context.unify_stack]}
  end

  defp new_var(var, context) do
    case :maps.find(var_name(var), context.vars) do
      {:ok, type} ->
        {type, context}

      :error ->
        type = {:var, context.counter}
        vars = :maps.put(var_name(var), type, context.vars)
        types_to_vars = :maps.put(context.counter, var, context.types_to_vars)
        types = :maps.put(context.counter, :unbound, context.types)
        traces = :maps.put(context.counter, [], context.traces)
        counter = context.counter + 1

        context = %{
          context
          | vars: vars,
            types_to_vars: types_to_vars,
            types: types,
            traces: traces,
            counter: counter
        }

        {type, context}
    end
  end

  defp refine_var(var, type, context) do
    types = :maps.put(var, type, context.types)
    trace_var(var, type, %{context | types: types})
  end

  defp trace_var(var, type, context) do
    line = get_meta(hd(context.expr_stack))[:line]
    trace = {type, context.expr_stack, {context.file, line}}
    traces = :maps.update_with(var, &[trace | &1], context.traces)
    %{context | traces: traces}
  end

  defp var_name({name, _meta, context}), do: {name, context}

  # Check if a variable is recursive and incompatible with itself
  # Bad: `{var} = var`
  # Good: `x = y; y = z; z = x`
  defp recursive_type?({:var, var} = parent, parents, context) do
    case :maps.get(var, context.types) do
      :unbound ->
        false

      type ->
        if type in parents do
          not Enum.all?(parents, &match?({:var, _}, &1))
        else
          recursive_type?(type, [parent | parents], context)
        end
    end
  end

  defp recursive_type?({:cons, left, right} = parent, parents, context) do
    recursive_type?(left, [parent | parents], context) or
      recursive_type?(right, [parent | parents], context)
  end

  defp recursive_type?({:tuple, types} = parent, parents, context) do
    Enum.any?(types, &recursive_type?(&1, [parent | parents], context))
  end

  defp recursive_type?({:map, pairs} = parent, parents, context) do
    Enum.any?(pairs, fn {key, value} ->
      recursive_type?(key, [parent | parents], context) or
        recursive_type?(value, [parent | parents], context)
    end)
  end

  defp recursive_type?(_other, _parents, _context) do
    false
  end

  # Collect binary type specifiers,
  # from `<<pattern::integer-size(10)>>` collect `integer`
  defp collect_specifiers({:-, _meta, [left, right]}) do
    [collect_specifiers(left), collect_specifiers(right)]
  end

  defp collect_specifiers({specifier, _meta, []}) do
    [specifier]
  end

  defp collect_specifiers({name, _meta, context}) when is_atom(name) and is_atom(context) do
    [name]
  end

  defp collect_specifiers(_other) do
    []
  end

  defp subtype?({:literal, boolean}, :boolean, _context) when is_boolean(boolean), do: true
  defp subtype?({:literal, atom}, :atom, _context) when is_atom(atom), do: true
  defp subtype?(:boolean, :atom, _context), do: true
  defp subtype?({:tuple, _}, :tuple, _context), do: true

  # TODO: Lift unions to unify/3?
  defp subtype?({:union, left_types}, {:union, _} = right_union, context) do
    Enum.all?(left_types, &subtype?(&1, right_union, context))
  end

  defp subtype?(left, {:union, right_types}, context) do
    Enum.any?(right_types, &subtype?(left, &1, context))
  end

  defp subtype?(left, right, _context), do: left == right

  defp to_union(types, context) when types != [] do
    if :dynamic in types do
      :dynamic
    else
      # Filter subtypes
      # `boolean() | atom()` => `atom()`
      # `:foo | atom()` => `atom()`
      # Does not unify `true | false` => `boolean()`
      case unique_super_types(types, context) do
        [type] -> type
        types -> {:union, types}
      end
    end
  end

  defp unique_super_types([type | types], context) do
    types = Enum.reject(types, &subtype?(&1, type, context))

    if Enum.any?(types, &subtype?(type, &1, context)) do
      unique_super_types(types, context)
    else
      [type | unique_super_types(types, context)]
    end
  end

  defp unique_super_types([], _context) do
    []
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

  ## ERROR GENERATION

  # Collect relevant information from context and traces to report error
  defp error({:unable_unify, left, right}, context) do
    {fun, arity} = context.function
    line = get_meta(hd(context.expr_stack))[:line]
    location = {context.file, line, {context.module, fun, arity}}

    traces = type_traces(context)
    common_expr = common_super_expr(traces)
    traces = simplify_traces(traces, context)

    {:error, {__MODULE__, {:unable_unify, left, right, common_expr, traces}, [location]}}
  end

  # Collect relevant traces from context.traces using context.unify_stack
  defp type_traces(context) do
    stack = Enum.uniq(context.unify_stack)

    Enum.flat_map(stack, fn var_index ->
      case :maps.find(var_index, context.traces) do
        {:ok, traces} ->
          expr_var = :maps.get(var_index, context.types_to_vars)
          Enum.map(traces, &{expr_var, &1})

        _other ->
          []
      end
    end)
  end

  # Only use last expr from trace and tag if trace is for
  # a concrete type or type variable
  defp simplify_traces(traces, context) do
    Enum.flat_map(traces, fn {var, {type, [expr | _], location}} ->
      case type do
        {:var, var_index} ->
          var2 = :maps.get(var_index, context.types_to_vars)
          [{var, {:var, var2, expr, location}}]

        _ ->
          [{var, {:type, type, expr, location}}]
      end
    end)
  end

  # Find first common super expression among all traces
  defp common_super_expr([]) do
    nil
  end

  defp common_super_expr([{_var, {_type, expr_stack, _location}} | traces]) do
    Enum.find_value(expr_stack, fn expr ->
      common? =
        Enum.all?(traces, fn {_var, {_type, expr_stack, _location}} -> expr in expr_stack end)

      if common? do
        expr
      end
    end)
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

  ## HELPERS

  defp remove_meta({fun, meta, args}) when is_list(meta), do: {fun, [], args}
  defp remove_meta(other), do: other

  defp get_meta({_fun, meta, _args}) when is_list(meta), do: meta
  defp get_meta(_other), do: []

  defp unzip_ok(list) do
    do_unzip_ok(list, [], [])
  end

  defp do_unzip_ok([{:ok, head1, head2} | tail], acc1, acc2) do
    do_unzip_ok(tail, [head1 | acc1], [head2 | acc2])
  end

  defp do_unzip_ok([{:error, reason} | _tail], _acc1, _acc2), do: {:error, reason}

  defp do_unzip_ok([], acc1, acc2), do: {:ok, Enum.reverse(acc1), Enum.reverse(acc2)}

  defp map_ok(list, fun) do
    do_map_ok(list, [], fun)
  end

  defp do_map_ok([head | tail], acc, fun) do
    case fun.(head) do
      {:ok, elem} ->
        do_map_ok(tail, [elem | acc], fun)

      result when elem(result, 0) == :ok ->
        result = Tuple.delete_at(result, 0)
        do_map_ok(tail, [result | acc], fun)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp do_map_ok([], acc, _fun), do: {:ok, Enum.reverse(acc)}

  defp reduce_ok(list, acc, fun) do
    do_reduce_ok(list, acc, fun)
  end

  defp do_reduce_ok([head | tail], acc, fun) do
    case fun.(head, acc) do
      {:ok, acc} ->
        do_reduce_ok(tail, acc, fun)

      result when elem(result, 0) == :ok ->
        result = Tuple.delete_at(result, 0)
        do_reduce_ok(tail, result, fun)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp do_reduce_ok([], acc, _fun), do: {:ok, acc}

  defp map_reduce_ok(list, acc, fun) do
    do_map_reduce_ok(list, {[], acc}, fun)
  end

  defp do_map_reduce_ok([head | tail], {list, acc}, fun) do
    case fun.(head, acc) do
      {:ok, elem, acc} ->
        do_map_reduce_ok(tail, {[elem | list], acc}, fun)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp do_map_reduce_ok([], {list, acc}, _fun), do: {:ok, Enum.reverse(list), acc}
end
