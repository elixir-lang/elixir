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

  def infer_definitions(file, module, defs) do
    Enum.map(defs, fn {function, _kind, _meta, clauses} ->
      context = context(file, module, function)

      types =
        map_ok(clauses, fn {_meta, params, guards, _body} ->
          of_clause(params, guards, context)
        end)

      {function, types}
    end)
  end

  def of_clause(params, guards, context) do
    with {:ok, types, context} <- of_pattern_multi(params, context),
         {:ok, context} <- of_guard(guards_to_or(guards), context),
         do: {:ok, types, context}
  end

  def context(file, module, function) do
    %{
      file: file,
      line: nil,
      module: module,
      function: function,
      expr: nil,
      vars: %{},
      types: %{},
      counter: 0
    }
  end

  def lift_types(types, context) do
    context = %{
      types: context.types,
      quantified_types: %{},
      quantified_counter: 0
    }

    {types, _context} = Enum.map_reduce(types, context, &do_lift_type/2)
    types
  end

  def lift_type(type, context) do
    context = %{
      types: context.types,
      quantified_types: %{},
      quantified_counter: 0
    }

    {type, _context} = do_lift_type(type, context)
    type
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

  def of_pattern({:_, meta, atom}, context) when is_atom(atom) do
    # Ensure wildcard pattern is given a unique variable name
    var = {:_, meta, context.counter}
    {type, context} = new_var(var, context)
    {:ok, type, context}
  end

  def of_pattern(var, context) when is_var(var) do
    {type, context} = new_var(var, context)
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

  def of_pattern({:=, _meta, [left_expr, right_expr]} = expr, context) do
    with {:ok, left_type, context} <- of_pattern(left_expr, context),
         {:ok, right_type, context} <- of_pattern(right_expr, context),
         do: unify(left_type, right_type, with_expr(expr, context))
  end

  def of_pattern(_other, context) do
    {:ok, :dynamic, context}
  end

  def format_type({:tuple, types}) do
    "{#{Enum.map_join(types, ", ", &format_type/1)}}"
  end

  def format_type({:cons, left, :null}) do
    "[#{format_type(left)}]"
  end

  def format_type({:cons, left, right}) do
    "[#{format_type(left)} | #{format_type(right)}]"
  end

  def format_type({:fn, [], return}) do
    "fn(-> #{format_type(return)})"
  end

  def format_type({:fn, params, return}) do
    "fn(#{Enum.map_join(params, ", ", &format_type/1)} -> #{format_type(return)})"
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

  defp of_pattern_multi(patterns, pattern_types \\ [], context)

  defp of_pattern_multi([pattern | patterns], pattern_types, context) do
    case of_pattern(pattern, context) do
      {:ok, type, context} -> of_pattern_multi(patterns, [type | pattern_types], context)
      {:error, reason} -> {:error, reason}
    end
  end

  defp of_pattern_multi([], pattern_types, context) do
    {:ok, Enum.reverse(pattern_types), context}
  end

  # TODO: Remove this and let multiple when be treated as multiple clauses,
  #       meaning they will be intersection types
  defp guards_to_or([]) do
    []
  end

  defp guards_to_or(guards) do
    Enum.reduce(guards, fn guard, acc -> {:or, [], [guard, acc]} end)
  end

  defp flatten_binary_op(op, {op, _meta, [left, right]}) do
    flatten_binary_op(op, left) ++ flatten_binary_op(op, right)
  end

  defp flatten_binary_op(_op, other) do
    [other]
  end

  def of_guard(guard, context) do
    reduce_ok(flatten_binary_op(:and, guard), context, fn guard, context ->
      union = Enum.map(flatten_binary_op(:or, guard), &type_guard/1)

      with {:ok, args, guard_types} <- unzip_ok(union),
           [arg] <- Enum.uniq(args) do
        union = to_union(guard_types, context)
        unify_guard(arg, union, with_expr(guard, context))
      else
        _ -> {:ok, context}
      end
    end)
  end

  defp unify_guard(arg, guard_type, context) do
    # TODO: It is incorrect to use of_pattern/2 but it helps for simplicity now
    with {:ok, arg_type, context} <- of_pattern(arg, context),
         {:ok, _, context} <- unify(arg_type, guard_type, context),
         do: {:ok, context}
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
    case :maps.find(guard, @type_guards) do
      {:ok, type} -> {:ok, arg, type}
      :error -> {:error, :unknown_guard}
    end
  end

  defp type_guard(_other), do: {:error, :unknown_guard}

  defp of_binary([], context) do
    {:ok, context}
  end

  defp of_binary([{:"::", _meta, [expr, specifiers]} = full_expr | args], context) do
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
         {:ok, _type, context} <- unify(type, expected_type, with_expr(full_expr, context)),
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

  defp unify({:var, left}, {:var, right}, context) do
    case {:maps.find(left, context.types), :maps.find(right, context.types)} do
      {{:ok, type}, :error} ->
        {:ok, type, %{context | types: :maps.put(right, type, context.types)}}

      {:error, {:ok, type}} ->
        {:ok, type, %{context | types: :maps.put(left, type, context.types)}}

      {_, {:ok, :unbound}} ->
        {:ok, {:var, left}, %{context | types: :maps.put(right, {:var, left}, context.types)}}

      {{:ok, :unbound}, _} ->
        {:ok, {:var, right}, %{context | types: :maps.put(left, {:var, right}, context.types)}}

      {{:ok, left_type}, {:ok, right_type}} ->
        unify(left_type, right_type, context)
    end
  end

  defp unify(type, {:var, var}, context) do
    case :maps.find(var, context.types) do
      {:ok, :unbound} -> add_var(var, type, context)
      {:ok, var_type} -> unify(type, var_type, context)
      :error -> add_var(var, type, context)
    end
  end

  defp unify({:var, var}, type, context) do
    unify(type, {:var, var}, context)
  end

  defp unify(left, right, context) do
    cond do
      subtype?(left, right, context) -> {:ok, right, context}
      subtype?(right, left, context) -> {:ok, left, context}
      true -> error({:unable_unify, left, right}, context)
    end
  end

  defp add_var(var, type, context) do
    context = %{context | types: :maps.put(var, type, context.types)}

    if recursive_type?(type, [], context) do
      error({:recursive_type, type}, context)
    else
      {:ok, type, context}
    end
  end

  defp recursive_type?({:var, var} = parent, parents, context) do
    case :maps.find(var, context.types) do
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

  defp collect_specifiers({name, _meta, context}) when is_atom(name) and is_atom(context) do
    [name]
  end

  defp collect_specifiers(_other) do
    []
  end

  defp new_var(var, context) do
    case :maps.find(var_name(var), context.vars) do
      {:ok, type} ->
        {type, context}

      :error ->
        type = {:var, context.counter}
        vars = :maps.put(var_name(var), type, context.vars)
        types = :maps.put(context.counter, :unbound, context.types)
        context = %{context | vars: vars, types: types, counter: context.counter + 1}
        {type, context}
    end
  end

  defp var_name({name, _meta, context}), do: {name, context}

  defp subtype?({:var, var}, type, context),
    do: subtype?(:maps.get(var, context.types), type, context)

  defp subtype?(type, {:var, var}, context),
    do: subtype?(type, :maps.get(var, context.types), context)

  defp subtype?({:literal, boolean}, :boolean, _context) when is_boolean(boolean), do: true
  defp subtype?({:literal, atom}, :atom, _context) when is_atom(atom), do: true
  defp subtype?(:boolean, :atom, _context), do: true
  defp subtype?({:tuple, _}, :tuple, _context), do: true

  defp subtype?({:tuple, lefts}, {:tuple, rights}, context)
       when length(lefts) == length(rights) do
    Enum.all?(Enum.zip(lefts, rights), fn {left, right} -> subtype?(left, right, context) end)
  end

  defp subtype?({:tuple, _lefts}, {:tuple, _rights}, _context) do
    false
  end

  defp subtype?({:union, left_types}, {:union, _} = right_union, context) do
    Enum.all?(left_types, &subtype?(&1, right_union, context))
  end

  defp subtype?(left, {:union, right_types}, context) do
    Enum.any?(right_types, &subtype?(left, &1, context))
  end

  defp subtype?(:dynamic, _right, _context), do: true

  defp subtype?(_left, :dynamic, _context), do: true

  defp subtype?(left, right, _context), do: left == right

  defp to_union(types, context) when types != [] do
    case unique_super_types(types, context) do
      [type] -> type
      types -> {:union, types}
    end
  end

  defp unique_super_types([type | types], context) do
    types =
      types
      |> Enum.reject(&(subtype?(type, &1, context) or subtype?(&1, type, context)))
      |> unique_super_types(context)

    [type | types]
  end

  defp unique_super_types([], _context) do
    []
  end

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

  defp do_lift_type({:cons, left, right}, context) do
    {left, context} = do_lift_type(left, context)
    {right, context} = do_lift_type(right, context)
    {{:cons, left, right}, context}
  end

  defp do_lift_type({:fn, params, return}, context) do
    {params, context} = Enum.map_reduce(params, context, &do_lift_type/2)
    {return, context} = do_lift_type(return, context)
    {{:fn, params, return}, context}
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

  defp get_meta({_fun, meta, _args}) when is_list(meta), do: meta
  defp get_meta(_other), do: []

  defp with_expr(expr, context) do
    %{context | expr: expr, line: get_meta(expr)[:line]}
  end

  defp error(reason, context) do
    {fun, arity} = context.function
    location = {context.file, context.line, {context.module, fun, arity}}
    reason = Tuple.insert_at(reason, 1, context.expr)
    {:error, {reason, [location]}}
  end

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
end
