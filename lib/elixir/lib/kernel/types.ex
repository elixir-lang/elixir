defmodule Kernel.Types do
  @moduledoc false

  defguardp is_var(expr)
            when is_tuple(expr) and
                   tuple_size(expr) == 3 and
                   is_atom(elem(expr, 0)) and
                   is_atom(elem(expr, 2))

  def infer_defs(_module, defs) do
    Enum.map(defs, fn {name, kind, _meta, clauses} ->
      types =
        Enum.map(clauses, fn {_meta, params, _guards, _body} ->
          of_clause(params, context())
        end)

      {name, kind, types}
    end)
  rescue
    e ->
      case :code.ensure_loaded(Access) do
        {:module, _} -> reraise(e, __STACKTRACE__)
        {:error, _} -> {:error, :bootstrapping}
      end
  end

  def context(enum \\ []) do
    context = %{vars: %{}, types: %{}, counter: 0}
    Map.merge(context, Map.new(enum))
  end

  def of_clause({:when, _meta, [params, guard]}, context) do
    with {:ok, types, context} <- of_pattern_multi(params, context),
         {:ok, context} <- of_guard(when_to_or(guard), context),
         do: {:ok, types, context}
  end

  def of_clause(params, context) do
    of_pattern_multi(params, context)
  end

  def of_pattern_multi(patterns, pattern_types \\ [], context)

  def of_pattern_multi([pattern | patterns], pattern_types, context) do
    case of_pattern(pattern, context) do
      {:ok, type, context} -> of_pattern_multi(patterns, [type | pattern_types], context)
      {:error, reason} -> {:error, reason}
    end
  end

  def of_pattern_multi([], pattern_types, context) do
    {:ok, Enum.reverse(pattern_types), context}
  end

  def of_pattern(atom, context) when is_atom(atom) do
    {:ok, {:literal, atom}, context}
  end

  def of_pattern(literal, context) when is_integer(literal) do
    {:ok, :integer, context}
  end

  def of_pattern(literal, context) when is_float(literal) do
    {:ok, :float, context}
  end

  def of_pattern(literal, context) when is_binary(literal) do
    {:ok, :binary, context}
  end

  def of_pattern({:<<>>, _meta, args}, context) do
    # TODO: Add bitstring type
    case of_binary(args, context) do
      {:ok, context} -> {:ok, :binary, context}
      {:error, reason} -> {:error, reason}
    end
  end

  def of_pattern([{:|, _meta, [left_expr, right_expr]}], context) do
    with {:ok, left, context} <- of_pattern(left_expr, context),
         {:ok, right, context} <- of_pattern(right_expr, context),
         do: {:ok, {:cons, left, right}, context}
  end

  def of_pattern([left_expr | right_expr], context) do
    with {:ok, left, context} <- of_pattern(left_expr, context),
         {:ok, right, context} <- of_pattern(right_expr, context),
         do: {:ok, {:cons, left, right}, context}
  end

  def of_pattern([], context) do
    {:ok, :null, context}
  end

  def of_pattern(var, context) when is_var(var) do
    {type, context} = new_var(var_name(var), context)
    {:ok, type, context}
  end

  def of_pattern({left, right}, context) do
    of_pattern({:{}, [], [left, right]}, context)
  end

  def of_pattern({:{}, _meta, exprs}, context) do
    case of_pattern_multi(exprs, context) do
      {:ok, types, context} -> {:ok, {:tuple, types}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  def of_pattern({:=, _meta, [left_expr, right_expr]}, context) do
    with {:ok, left_type, context} <- of_pattern(left_expr, context),
         {:ok, right_type, context} <- of_pattern(right_expr, context),
         do: unify(left_type, right_type, context)
  end

  def of_pattern(expr, _context) do
    {:error, {:unsupported_pattern, expr}}
  end

  # TODO: Remove this and let multiple when be treated as multiple clauses,
  #       meaning they will be intersection types
  defp when_to_or({:when, meta, [guards, more_guards]}) do
    {:or, meta, [when_to_or(guards), when_to_or(more_guards)]}
  end

  defp when_to_or(other) do
    other
  end

  defp flatten_binary_op(op, {op, _meta, [left, right]}) do
    flatten_binary_op(op, left) ++ flatten_binary_op(op, right)
  end

  defp flatten_binary_op(_op, other) do
    [other]
  end

  def of_guard(guard, context) do
    reduce_ok(flatten_binary_op(:and, guard), context, fn guard, context ->
      # TODO: Union types Enum.map(flatten_binary_op(:or, guard))
      with {:ok, arg, guard_type} <- type_guard(guard),
           # TODO: It is incorrect to use of_pattern/2 but it helps for simplicity now
           {:ok, arg_type, context} <- of_pattern(arg, context),
           {:ok, _, context} <- unify(arg_type, guard_type, context),
           do: {:ok, context}
    end)
  end

  # Unimplemented guards:
  # !=/2 !==/2 </2 <=/2 >/2 >=/2 ==/2 ===/2 not/1
  # */2 +/1 +/2 -/1 -/2 //2
  # abs/2 ceil/1 floor/1 round/1 trunc/1
  # elem/2 hd/1 in/2 length/1 map_size/1 tl/1 tuple_size/1
  # is_function/1 is_function/2 is_list/1 is_map/1 is_number/1 is_pid/1 is_port/1 is_reference/1 is_tuple/1
  # node/1 self/1
  # binary_part/3 bit_size/1 byte_size/1 div/2 rem/2 node/0

  @type_guards %{
    is_atom: :atom,
    is_binary: :binary,
    # TODO: Add bitstring type
    is_bitstring: :binary,
    is_float: :float,
    is_integer: :integer,
    is_nil: {:literal, nil}
  }

  defp type_guard({guard, _meta, [arg]}) do
    case Map.fetch(@type_guards, guard) do
      {:ok, type} -> {:ok, arg, type}
      :error -> :error
    end
  end

  defp type_guard(_other), do: :error

  defp of_binary([], context) do
    {:ok, context}
  end

  defp of_binary([{:"::", _meta, [expr, specifiers]} | args], context) do
    specifiers = List.flatten(collect_specifiers(specifiers))

    expected_type =
      cond do
        :integer in specifiers -> :integer
        :float in specifiers -> :float
        # TODO: Add bitstring type
        :bits in specifiers -> :binary
        :bitstring in specifiers -> :binary
        :bytes in specifiers -> :binary
        :binary in specifiers -> :binary
        :utf8 in specifiers -> :integer
        :utf16 in specifiers -> :integer
        :utf32 in specifiers -> :integer
        true -> :integer
      end

    with {:ok, type, context} <- of_pattern(expr, context),
         {:ok, _type, context} <- unify(type, expected_type, context),
         do: of_binary(args, context)
  end

  defp of_binary([expr | args], context) do
    case of_pattern(expr, context) do
      {:ok, type, context} when type in [:integer, :float, :binary] ->
        of_binary(args, context)

      {:ok, type, _context} ->
        {:error, {:invalid_binary_type, type}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp unify(same, same, context) do
    {:ok, same, context}
  end

  defp unify({:var, left}, {:var, right}, context) do
    case {Map.fetch(context.types, left), Map.fetch(context.types, right)} do
      {{:ok, type}, :error} -> {:ok, type, put_in(context.types[right], type)}
      {:error, {:ok, type}} -> {:ok, type, put_in(context.types[left], type)}
      {_, {:ok, :unbound}} -> {:ok, {:var, left}, put_in(context.types[right], {:var, left})}
      {{:ok, :unbound}, _} -> {:ok, {:var, right}, put_in(context.types[left], {:var, right})}
      {{:ok, same}, {:ok, same}} -> {:ok, same}
      {{:ok, left_type}, {:ok, right_type}} -> unify(left_type, right_type, context)
    end
  end

  defp unify(type, {:var, var}, context) do
    case Map.fetch(context.types, var) do
      :error -> add_var(var, type, context)
      {:ok, :unbound} -> add_var(var, type, context)
      {:ok, var_type} -> unify(type, var_type, context)
    end
  end

  defp unify({:var, var}, type, context) do
    unify(type, {:var, var}, context)
  end

  defp unify(left, right, _context) do
    {:error, {:unable_unify, left, right}}
  end

  defp add_var(var, type, context) do
    context = put_in(context.types[var], type)

    if recursive_type?(type, [], context) do
      {:error, {:recursive_type, type}}
    else
      {:ok, type, context}
    end
  end

  defp recursive_type?({:var, var} = parent, parents, context) do
    case Map.fetch(context.types, var) do
      :error -> false
      {:ok, :unbound} -> false
      {:ok, type} -> type in parents or recursive_type?(type, [parent | parents], context)
    end
  end

  defp recursive_type?({:cons, left, right} = parent, parents, context) do
    recursive_type?(left, [parent | parents], context) or
      recursive_type?(right, [parent | parents], context)
  end

  defp recursive_type?({:tuple, types} = parent, parents, context) do
    Enum.any?(types, &recursive_type?(&1, [parent | parents], context))
  end

  defp recursive_type?(_other, _parents, _context) do
    false
  end

  defp collect_specifiers({:-, _meta, [left, right]}) do
    [collect_specifiers(left), collect_specifiers(right)]
  end

  defp collect_specifiers({specifier, _meta, []}) do
    [specifier]
  end

  defp collect_specifiers(var) when is_var(var) do
    [var_name(var)]
  end

  defp collect_specifiers(_other) do
    []
  end

  defp new_var(var_name, context) do
    case Map.fetch(context.vars, var_name) do
      {:ok, type} ->
        {type, context}

      :error ->
        type = {:var, context.counter}
        types = Map.put(context.types, context.counter, :unbound)
        vars = Map.put(context.vars, var_name, type)
        context = %{context | vars: vars, types: types, counter: context.counter + 1}
        {type, context}
    end
  end

  # NOTE: Ignores the variable's context
  defp var_name({name, _meta, _context}), do: name

  def lift_types(type, context) do
    context =
      context
      |> Map.put(:quantified_types, %{})
      |> Map.put(:quantified_counter, 0)

    do_lift_types(type, context)
  end

  defp do_lift_types({:var, var}, context) do
    case Map.fetch(context.quantified_types, var) do
      {:ok, quantified_var} ->
        {{:var, quantified_var}, context}

      :error ->
        case Map.fetch(context.types, var) do
          {:ok, :unbound} ->
            new_quantified_var(var, context)

          {:ok, type} ->
            # Remove visited types to avoid infinite loops
            # then restore after we are done recursing on vars
            types = context.types
            context = update_in(context.types, &Map.delete(&1, var))
            {type, context} = do_lift_types(type, context)
            {type, put_in(context.types, types)}

          :error ->
            new_quantified_var(var, context)
        end
    end
  end

  defp do_lift_types({:tuple, types}, context) do
    {types, context} = Enum.map_reduce(types, context, &do_lift_types/2)
    {{:tuple, types}, context}
  end

  defp do_lift_types({:cons, left, right}, context) do
    {left, context} = do_lift_types(left, context)
    {right, context} = do_lift_types(right, context)
    {{:cons, left, right}, context}
  end

  defp do_lift_types({:fn, params, return}, context) do
    {params, context} = Enum.map_reduce(params, context, &do_lift_types/2)
    {return, context} = do_lift_types(return, context)
    {{:fn, params, return}, context}
  end

  defp do_lift_types(other, context) do
    {other, context}
  end

  defp new_quantified_var(original_var, context) do
    types = Map.put(context.quantified_types, original_var, context.quantified_counter)
    counter = context.quantified_counter + 1
    type = {:var, context.quantified_counter}
    context = %{context | quantified_types: types, quantified_counter: counter}
    {type, context}
  end

  defp reduce_ok(list, acc, fun) do
    do_reduce_ok(list, acc, fun)
  end

  defp do_reduce_ok([head | tail], acc, fun) do
    case fun.(head, acc) do
      {:ok, acc} -> do_reduce_ok(tail, acc, fun)
      {:error, reason} -> {:error, reason}
    end
  end

  defp do_reduce_ok([], acc, _fun), do: {:ok, acc}
end
