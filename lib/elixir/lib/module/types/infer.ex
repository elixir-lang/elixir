defmodule Module.Types.Infer do
  @moduledoc false

  import Module.Types.Helpers

  defmacrop is_var(expr) do
    quote do
      is_tuple(unquote(expr)) and
        tuple_size(unquote(expr)) == 3 and
        is_atom(elem(unquote(expr), 0)) and
        is_atom(elem(unquote(expr), 2))
    end
  end

  ## PATTERNS

  @doc """
  Return the type and typing context of a pattern expression or an error
  in case of a typing conflict.
  """
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
           do: unify(left_type, right_type, context)
    end)
  end

  # %{...}
  def of_pattern({:%{}, _meta, args} = expr, context) do
    case expr_stack(expr, context, &of_pairs(args, &1)) do
      {:ok, pairs, context} -> {:ok, {:map, pairs_to_unions(pairs, context)}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # %var{...}
  def of_pattern({:%, _meta1, [var, {:%{}, _meta2, args}]} = expr, context) when is_var(var) do
    expr_stack(expr, context, fn context ->
      with {:ok, pairs, context} <- of_pairs(args, context),
           {var_type, context} = new_var(var, context),
           {:ok, _, context} <- unify(var_type, :atom, context) do
        pairs = [{{:literal, :__struct__}, var_type} | pairs]
        {:ok, {:map, pairs}, context}
      end
    end)
  end

  # %Struct{...}
  def of_pattern({:%, _meta1, [module, {:%{}, _meta2, args}]} = expr, context)
      when is_atom(module) do
    case expr_stack(expr, context, &of_pairs(args, &1)) do
      {:ok, pairs, context} ->
        pairs = [{{:literal, :__struct__}, module} | pairs]
        {:ok, {:map, pairs}, context}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp of_pairs(pairs, context) do
    map_reduce_ok(pairs, context, fn {key, value}, context ->
      with {:ok, key_type, context} <- of_pattern(key, context),
           {:ok, value_type, context} <- of_pattern(value, context),
           do: {:ok, {key_type, value_type}, context}
    end)
  end

  defp pairs_to_unions(pairs, context) do
    # Maps only allow simple literal keys in patterns so
    # we do not have to do subtype checking

    Enum.reduce(pairs, [], fn {key, value}, pairs ->
      case :lists.keyfind(key, 1, pairs) do
        {^key, {:union, union}} ->
          :lists.keystore(key, 1, pairs, {key, to_union([value | union], context)})

        {^key, original_value} ->
          :lists.keystore(key, 1, pairs, {key, to_union([value, original_value], context)})

        false ->
          [{key, value} | pairs]
      end
    end)
  end

  ## GUARDS

  @doc """
  Refines the type variables in the typing context using type check guards
  such as `is_integer/1`.
  """
  def of_guard(guard, context) do
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
         {:ok, _, context} <- unify(arg_type, guard_type, context),
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

  ## BINARY PATTERNS

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
           {:ok, _type, context} <- unify(type, expected_type, context),
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

  ## UNIFICATION

  @doc """
  Unifies two types and returns the unified type and an updated typing context
  or an error in case of a typing conflict.
  """
  def unify(left, right, context) do
    case do_unify(left, right, context) do
      {:ok, type, context} -> {:ok, type, %{context | unify_stack: []}}
      {:error, reason} -> {:error, reason}
    end
  end

  defp do_unify(type, {:var, var}, context) do
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

        case do_unify(type, var_type, context) do
          {:ok, var_type, context} ->
            context = refine_var(var, var_type, context)
            {:ok, {:var, var}, context}

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp do_unify({:var, var}, type, context) do
    do_unify(type, {:var, var}, context)
  end

  defp do_unify({:tuple, lefts}, {:tuple, rights}, context)
       when length(lefts) == length(rights) do
    result =
      map_reduce_ok(Enum.zip(lefts, rights), context, fn {left, right}, context ->
        do_unify(left, right, context)
      end)

    case result do
      {:ok, types, context} -> {:ok, {:tuple, types}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  defp do_unify({:cons, left_left, left_right}, {:cons, right_left, right_right}, context) do
    with {:ok, left, context} <- do_unify(left_left, right_left, context),
         {:ok, right, context} <- do_unify(left_right, right_right, context),
         do: {:ok, {:cons, left, right}, context}
  end

  defp do_unify({:map, left_pairs}, {:map, right_pairs}, context) do
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
            case do_unify(left_value, right_value, context) do
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

  defp do_unify(:dynamic, right, context) do
    {:ok, right, context}
  end

  defp do_unify(left, :dynamic, context) do
    {:ok, left, context}
  end

  defp do_unify(left, right, context) do
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
          _ -> false
        end
    end
  end

  defp push_unify_stack(var, context) do
    %{context | unify_stack: [var | context.unify_stack]}
  end

  @doc """
  Adds a variable to the typing context and returns its type variables.
  If the variable has already been added, return the existing type variable.
  """
  def new_var(var, context) do
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

  @doc """
  Checks if the first argument is a subtype of the second argument.
  Only checks for simple and concrete types.
  """
  def subtype?({:literal, boolean}, :boolean, _context) when is_boolean(boolean), do: true
  def subtype?({:literal, atom}, :atom, _context) when is_atom(atom), do: true
  def subtype?(:boolean, :atom, _context), do: true
  def subtype?({:tuple, _}, :tuple, _context), do: true

  # TODO: Lift unions to unify/3?
  def subtype?({:union, left_types}, {:union, _} = right_union, context) do
    Enum.all?(left_types, &subtype?(&1, right_union, context))
  end

  def subtype?(left, {:union, right_types}, context) do
    Enum.any?(right_types, &subtype?(left, &1, context))
  end

  def subtype?(left, right, _context), do: left == right

  @doc """
  Returns a "simplified" union using `subtype?/3` to remove redundant
  types. Due to limitations in `subtype?/3` some overlapping types
  may still be included.
  """
  def to_union(types, context) when types != [] do
    if :dynamic in types do
      :dynamic
    else
      case unique_super_types(types, context) do
        [type] -> type
        types -> {:union, types}
      end
    end
  end

  # Filter subtypes
  # `boolean() | atom()` => `atom()`
  # `:foo | atom()` => `atom()`
  # Does not unify `true | false` => `boolean()`
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

  # Collect relevant information from context and traces to report error
  defp error({:unable_unify, left, right}, context) do
    {fun, arity} = context.function
    line = get_meta(hd(context.expr_stack))[:line]
    location = {context.file, line, {context.module, fun, arity}}

    traces = type_traces(context)
    common_expr = common_super_expr(traces)
    traces = simplify_traces(traces, context)

    {:error, {Module.Types, {:unable_unify, left, right, common_expr, traces}, [location]}}
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

  defp remove_meta({fun, meta, args}) when is_list(meta), do: {fun, [], args}
  defp remove_meta(other), do: other

  defp get_meta({_fun, meta, _args}) when is_list(meta), do: meta
  defp get_meta(_other), do: []
end
