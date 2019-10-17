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
  def of_pattern(atom, _stack, context) when is_atom(atom) do
    {:ok, {:atom, atom}, context}
  end

  # 12
  def of_pattern(literal, _stack, context) when is_integer(literal) do
    {:ok, :integer, context}
  end

  # 1.2
  def of_pattern(literal, _stack, context) when is_float(literal) do
    {:ok, :float, context}
  end

  # "..."
  def of_pattern(literal, _stack, context) when is_binary(literal) do
    {:ok, :binary, context}
  end

  # <<...>>>
  def of_pattern({:<<>>, _meta, args} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    case reduce_ok(args, context, &of_binary(&1, stack, &2)) do
      {:ok, context} -> {:ok, :binary, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # left | []
  def of_pattern({:|, _meta, [left_expr, []]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)
    of_pattern(left_expr, stack, context)
  end

  # left | right
  def of_pattern({:|, _meta, [left_expr, right_expr]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    case of_pattern(left_expr, stack, context) do
      {:ok, left, context} ->
        case of_pattern(right_expr, stack, context) do
          {:ok, {:list, right}, context} ->
            {:ok, to_union([left, right], context), context}

          {:ok, right, context} ->
            {:ok, to_union([left, right], context), context}

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  # []
  def of_pattern([], _stack, context) do
    {:ok, {:list, :dynamic}, context}
  end

  # [expr, ...]
  def of_pattern(exprs, stack, context) when is_list(exprs) do
    stack = push_expr_stack(exprs, stack)

    case map_reduce_ok(exprs, context, &of_pattern(&1, stack, &2)) do
      {:ok, types, context} -> {:ok, {:list, to_union(types, context)}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # left ++ right
  def of_pattern({:++, _meta, [left_expr, right_expr]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    case of_pattern(left_expr, stack, context) do
      {:ok, {:list, left}, context} ->
        case of_pattern(right_expr, stack, context) do
          {:ok, {:list, right}, context} ->
            {:ok, {:list, to_union([left, right], context)}, context}

          {:ok, right, context} ->
            {:ok, {:list, to_union([left, right], context)}, context}

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  # _
  def of_pattern({:_, _meta, atom}, _stack, context) when is_atom(atom) do
    {:ok, :dynamic, context}
  end

  # var
  def of_pattern(var, _stack, context) when is_var(var) do
    {type, context} = new_var(var, context)
    {:ok, type, context}
  end

  # {left, right}
  def of_pattern({left, right}, stack, context) do
    of_pattern({:{}, [], [left, right]}, stack, context)
  end

  # {...}
  def of_pattern({:{}, _meta, exprs} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    case map_reduce_ok(exprs, context, &of_pattern(&1, stack, &2)) do
      {:ok, types, context} -> {:ok, {:tuple, types}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # left = right
  def of_pattern({:=, _meta, [left_expr, right_expr]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    with {:ok, left_type, context} <- of_pattern(left_expr, stack, context),
         {:ok, right_type, context} <- of_pattern(right_expr, stack, context),
         do: unify(left_type, right_type, stack, context)
  end

  # %{...}
  def of_pattern({:%{}, _meta, args} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    case of_pairs(args, stack, context) do
      {:ok, pairs, context} -> {:ok, {:map, pairs_to_unions(pairs, context)}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # %var{...}
  def of_pattern({:%, _meta1, [var, {:%{}, _meta2, args}]} = expr, stack, context)
      when is_var(var) do
    stack = push_expr_stack(expr, stack)

    with {:ok, pairs, context} <- of_pairs(args, stack, context),
         {var_type, context} = new_var(var, context),
         {:ok, _, context} <- unify(var_type, :atom, stack, context) do
      pairs = [{{:atom, :__struct__}, var_type} | pairs]
      {:ok, {:map, pairs}, context}
    end
  end

  # %Struct{...}
  def of_pattern({:%, _meta1, [module, {:%{}, _meta2, args}]} = expr, stack, context)
      when is_atom(module) do
    stack = push_expr_stack(expr, stack)

    case of_pairs(args, stack, context) do
      {:ok, pairs, context} ->
        pairs = [{{:atom, :__struct__}, {:atom, module}} | pairs]
        {:ok, {:map, pairs}, context}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp of_pairs(pairs, stack, context) do
    map_reduce_ok(pairs, context, fn {key, value}, context ->
      with {:ok, key_type, context} <- of_pattern(key, stack, context),
           {:ok, value_type, context} <- of_pattern(value, stack, context),
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

  @top_union [
    :atom,
    :binary,
    :float,
    :fun,
    :integer,
    {:list, :dynamic},
    {:map, []},
    :pid,
    :port,
    :reference,
    :tuple
  ]

  # TODO: Some guards can be changed to interesection types or higher order types

  @guard_functions %{
    {:is_atom, 1} => {[:atom], :boolean},
    {:is_binary, 1} => {[:binary], :boolean},
    {:is_bitstring, 1} => {[:binary], :boolean},
    {:is_boolean, 1} => {[:boolean], :boolean},
    {:is_float, 1} => {[:float], :boolean},
    {:is_function, 1} => {[:fun], :boolean},
    {:is_function, 2} => {[:fun, :integer], :boolean},
    {:is_integer, 1} => {[:integer], :boolean},
    {:is_list, 1} => {[{:list, :dynamic}], :boolean},
    {:is_map, 1} => {[{:map, []}], :boolean},
    {:is_number, 1} => {[:number], :boolean},
    {:is_pid, 1} => {[:pid], :boolean},
    {:is_port, 1} => {[:port], :boolean},
    {:is_reference, 1} => {[:reference], :boolean},
    {:is_tuple, 1} => {[:tuple], :boolean},
    {:<, 2} => {[:dynamic, :dynamic], :boolean},
    {:"=<", 2} => {[:dynamic, :dynamic], :boolean},
    {:>, 2} => {[:dynamic, :dynamic], :boolean},
    {:>=, 2} => {[:dynamic, :dynamic], :boolean},
    {:"/=", 2} => {[:dynamic, :dynamic], :boolean},
    {:"=/=", 2} => {[:dynamic, :dynamic], :boolean},
    {:==, 2} => {[:dynamic, :dynamic], :boolean},
    {:"=:=", 2} => {[:dynamic, :dynamic], :boolean},
    {:*, 2} => {[:number, :number], :number},
    {:+, 1} => {[:number], :number},
    {:+, 2} => {[:number, :number], :number},
    {:-, 1} => {[:number], :number},
    {:-, 2} => {[:number, :number], :number},
    {:/, 2} => {[:number, :number], :number},
    {:abs, 1} => {[:number], :number},
    {:ceil, 1} => {[:number], :integer},
    {:floor, 1} => {[:number], :integer},
    {:round, 1} => {[:number], :integer},
    {:trunc, 1} => {[:number], :integer},
    {:element, 2} => {[:integer, :tuple], :dynamic},
    {:hd, 1} => {[{:list, :dynamic}], :dynamic},
    {:length, 1} => {[{:list, :dynamic}], :integer},
    {:map_size, 1} => {[{:map, []}], :integer},
    {:tl, 1} => {[{:list, :dynamic}], :dynamic},
    {:tuple_size, 1} => {[:tuple], :integer},
    {:node, 1} => {[{:union, [:pid, :reference, :port]}], :atom},
    {:binary_part, 3} => {[:binary, :integer, :integer], :binary},
    {:bit_size, 1} => {[:binary], :integer},
    {:byte_size, 1} => {[:binary], :integer},
    {:div, 2} => {[:integer, :integer], :integer},
    {:rem, 2} => {[:integer, :integer], :integer},
    {:node, 0} => {[], :atom},
    {:self, 0} => {[], :pid},
    {:bnot, 1} => {[:integer], :integer},
    {:band, 2} => {[:integer, :integer], :integer},
    {:bor, 2} => {[:integer, :integer], :integer},
    {:bxor, 2} => {[:integer, :integer], :integer},
    {:bsl, 2} => {[:integer, :integer], :integer},
    {:bsr, 2} => {[:integer, :integer], :integer},
    {:xor, 2} => {[:boolean, :boolean], :boolean}

    # Following guards are matched explicitly to handle
    # type guard functions such as is_atom/1
    # {:andalso, 2} => {[:boolean, :boolean], :boolean}
    # {:orelse, 2} => {[:boolean, :boolean], :boolean}
    # {:not, 1} => {[:boolean], :boolean}
  }

  @type_guards [
    :is_atom,
    :is_binary,
    :is_bitstring,
    :is_boolean,
    :is_float,
    :is_function,
    :is_function,
    :is_integer,
    :is_list,
    :is_map,
    :is_number,
    :is_pid,
    :is_port,
    :is_reference,
    :is_tuple
  ]

  @doc """
  Refines the type variables in the typing context using type check guards
  such as `is_integer/1`.
  """
  def of_guard({{:., _, [:erlang, :andalso]}, _, [left, right]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)
    fresh_context = fresh_context(context)

    with {:ok, left_type, left_context} <- of_guard(left, stack, fresh_context),
         {:ok, right_type, right_context} <- of_guard(right, stack, fresh_context),
         {:ok, context} <- merge_context_and(context, stack, left_context, right_context),
         {:ok, _, context} <- unify(left_type, :boolean, stack, context),
         {:ok, _, context} <- unify(right_type, :boolean, stack, context),
         do: {:ok, :boolean, context}
  end

  def of_guard({{:., _, [:erlang, :orelse]}, _, [left, right]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)
    fresh_context = fresh_context(context)

    with {:ok, left_type, left_context} <- of_guard(left, stack, fresh_context),
         {:ok, right_type, right_context} <- of_guard(right, stack, fresh_context),
         {:ok, context} <- merge_context_or(context, stack, left_context, right_context),
         {:ok, _, context} <- unify(left_type, :boolean, stack, context),
         {:ok, _, context} <- unify(right_type, :boolean, stack, context),
         do: {:ok, :boolean, context}
  end

  def of_guard({{:., _, [:erlang, :not]}, _, [arg]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)
    fresh_context = fresh_context(context)

    with {:ok, type, arg_context} <- of_guard(arg, stack, fresh_context),
         arg_context = invert_types(stack, arg_context),
         {:ok, context} <- unify_new_types(context, stack, arg_context),
         {:ok, _, context} <- unify(type, :boolean, stack, context) do
      guard_sources = merge_guard_sources([context.guard_sources, arg_context.guard_sources])
      {:ok, :boolean, %{context | guard_sources: guard_sources}}
    end
  end

  def of_guard({{:., _, [:erlang, guard]}, _, args} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)
    {param_types, return_type} = guard_signature(guard, length(args))
    type_guard? = type_guard?(guard)

    # Only check type guards in the context of and/or/not,
    # a type guard in the context of is_tuple(x) > :foo
    # should not affect the inference of x
    if not type_guard? or stack.type_guards_enabled? do
      arg_stack = %{stack | type_guards_enabled?: type_guard?}

      with {:ok, arg_types, context} <-
             map_reduce_ok(args, context, &of_guard(&1, arg_stack, &2)),
           {:ok, context} <- unify_call(param_types, arg_types, stack, context) do
        {arg_types, guard_sources} =
          case arg_types do
            [{:var, index} | rest_arg_types] when type_guard? ->
              guard_sources =
                Map.update(context.guard_sources, index, [:guarded], &[:guarded | &1])

              {rest_arg_types, guard_sources}

            _ ->
              {arg_types, context.guard_sources}
          end

        guard_sources =
          Enum.reduce(arg_types, guard_sources, fn
            {:var, index}, guard_sources ->
              Map.update(guard_sources, index, [:fail], &[:fail | &1])

            _, guard_sources ->
              guard_sources
          end)

        {:ok, return_type, %{context | guard_sources: guard_sources}}
      end
    else
      {:ok, return_type, context}
    end
  end

  def of_guard(var, _stack, context) when is_var(var) do
    type = Map.fetch!(context.vars, var_name(var))
    {:ok, type, context}
  end

  def of_guard(expr, stack, context) do
    # Fall back to of_pattern/3 for literals
    of_pattern(expr, stack, context)
  end

  defp fresh_context(context) do
    types = Map.new(context.types, fn {var, _} -> {var, :unbound} end)
    traces = Map.new(context.traces, fn {var, _} -> {var, []} end)
    %{context | types: types, traces: traces}
  end

  defp unify_call(params, args, stack, context) do
    reduce_ok(Enum.zip(params, args), context, fn {param, arg}, context ->
      case unify(param, arg, stack, context) do
        {:ok, _, context} -> {:ok, context}
        {:error, reason} -> {:error, reason}
      end
    end)
  end

  defp merge_context_and(context, stack, left, right) do
    with {:ok, context} <- unify_new_types(context, stack, left),
         {:ok, context} <- unify_new_types(context, stack, right) do
      guard_sources = and_guard_sources(left.guard_sources, right.guard_sources)
      guard_sources = merge_guard_sources([context.guard_sources, guard_sources])
      {:ok, %{context | guard_sources: guard_sources}}
    end
  end

  defp unify_new_types(context, stack, new_context) do
    context = merge_traces(context, new_context)

    reduce_ok(Map.to_list(new_context.types), context, fn
      {_index, :unbound}, context ->
        {:ok, context}

      {index, new_type}, context ->
        case unify({:var, index}, new_type, %{stack | trace: false}, context) do
          {:ok, _, context} ->
            {:ok, context}

          {:error, reason} ->
            {:error, reason}
        end
    end)
  end

  defp merge_guard_sources(sources) do
    Enum.reduce(sources, fn left, right ->
      Map.merge(left, right, fn _index, left, right -> join_guard_source(left, right) end)
    end)
  end

  defp join_guard_source(left, right) do
    sources = left ++ right

    cond do
      :fail in sources -> [:fail]
      :guarded_fail in sources -> [:guarded_fail]
      :guarded in sources -> [:guarded]
      true -> []
    end
  end

  defp and_guard_sources(left, right) do
    Map.merge(left, right, fn _index, left, right ->
      # When the failing guard function wont fail due to type check function before it,
      # for example: is_list(x) and length(x)
      if :guarded in left and :fail in right do
        [:guarded_fail]
      else
        join_guard_source(left, right)
      end
    end)
  end

  defp merge_traces(context, new_context) do
    traces =
      :maps.fold(
        fn index, new_traces, traces ->
          :maps.update_with(index, &(new_traces ++ &1), new_traces, traces)
        end,
        context.traces,
        new_context.traces
      )

    %{context | traces: traces}
  end

  defp merge_context_or(context, stack, left, right) do
    context =
      case {Map.to_list(left.types), Map.to_list(right.types)} do
        {[{index, :unbound}], [{index, type}]} ->
          refine_var(index, type, stack, context)

        {[{index, type}], [{index, :unbound}]} ->
          refine_var(index, type, stack, context)

        {[{index, left_type}], [{index, right_type}]} ->
          # Only include right side if left side is from type guard such as is_list(x),
          # do not refine in case of length(x)
          left_guard_sources = Map.get(left.guard_sources, index, [])

          if :fail in left_guard_sources do
            guard_sources = Map.put(context.guard_sources, index, [:fail])
            context = %{context | guard_sources: guard_sources}
            refine_var(index, left_type, stack, context)
          else
            guard_sources =
              merge_guard_sources([
                context.guard_sources,
                left.guard_sources,
                right.guard_sources
              ])

            context = %{context | guard_sources: guard_sources}
            refine_var(index, to_union([left_type, right_type], context), stack, context)
          end

        {left_types, _right_types} ->
          Enum.reduce(left_types, context, fn {index, left_type}, context ->
            left_guard_sources = Map.get(left.guard_sources, index, [])

            if :fail in left_guard_sources do
              guard_sources =
                merge_guard_sources([
                  context.guard_sources,
                  left.guard_sources,
                  right.guard_sources
                ])

              context = %{context | guard_sources: guard_sources}
              refine_var(index, left_type, stack, context)
            else
              context
            end
          end)
      end

    {:ok, context}
  end

  defp invert_types(stack, context) do
    Enum.reduce(context.types, context, fn {index, type}, context ->
      sources = Map.get(context.guard_sources, index, [])

      cond do
        :guarded_fail in sources ->
          remove_var(index, context)

        :guarded in sources ->
          # Remove traces from inside `not(...)` when we invert the type
          # to avoid confusing error messages
          context = %{context | traces: Map.put(context.traces, index, [])}
          refine_var(index, invert_type(type, context), stack, context)

        true ->
          context
      end
    end)
  end

  defp invert_type({:union, _} = union, context) do
    union =
      Enum.flat_map(@top_union, fn top_type ->
        if subtype?(top_type, union, context) do
          []
        else
          [top_type]
        end
      end)

    to_union(union, context)
  end

  defp invert_type(type, context) do
    union =
      Enum.flat_map(@top_union, fn top_type ->
        if subtype?(type, top_type, context) do
          []
        else
          [top_type]
        end
      end)

    to_union(union, context)
  end

  # binary-pattern :: specifier
  defp of_binary({:"::", _meta, [expr, specifiers]} = full_expr, stack, context) do
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

    stack = push_expr_stack(full_expr, stack)

    # Special case utf specifiers with binary literals since they allow
    # both integer and binary literals but variables are always integer
    if is_binary(expr) and utf_specifier?(specifiers) do
      {:ok, context}
    else
      with {:ok, type, context} <- of_pattern(expr, stack, context),
           {:ok, _type, context} <- unify(type, expected_type, stack, context),
           do: {:ok, context}
    end
  end

  # binary-pattern
  defp of_binary(expr, stack, context) do
    case of_pattern(expr, stack, context) do
      {:ok, type, context} when type in [:integer, :float, :binary] ->
        {:ok, context}

      {:ok, type, _context} ->
        {:error, {:invalid_binary_type, type}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp utf_specifier?(specifiers) do
    :utf8 in specifiers or :utf16 in specifiers or :utf32 in specifiers
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
  def unify(left, right, stack, context) do
    case do_unify(left, right, stack, context) do
      {:ok, type, context} -> {:ok, type, context}
      {:error, reason} -> {:error, reason}
    end
  end

  defp do_unify(type, {:var, var}, stack, context) do
    case Map.fetch!(context.types, var) do
      :unbound ->
        context = refine_var(var, type, stack, context)
        stack = push_unify_stack(var, stack)

        if recursive_type?(type, [], context) do
          error({:unable_unify, type, {:var, var}}, stack, context)
        else
          {:ok, {:var, var}, context}
        end

      var_type ->
        # Only add trace if the variable wasn't already "expanded"
        context =
          if variable_expanded?(var, stack, context) do
            context
          else
            trace_var(var, type, stack, context)
          end

        stack = push_unify_stack(var, stack)

        case do_unify(type, var_type, stack, context) do
          {:ok, var_type, context} ->
            context = refine_var(var, var_type, stack, context)
            {:ok, {:var, var}, context}

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp do_unify({:var, var}, type, stack, context) do
    do_unify(type, {:var, var}, stack, context)
  end

  defp do_unify({:tuple, lefts}, {:tuple, rights}, stack, context)
       when length(lefts) == length(rights) do
    result =
      map_reduce_ok(Enum.zip(lefts, rights), context, fn {left, right}, context ->
        do_unify(left, right, stack, context)
      end)

    case result do
      {:ok, types, context} -> {:ok, {:tuple, types}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  defp do_unify({:list, left}, {:list, right}, stack, context) do
    case do_unify(left, right, stack, context) do
      {:ok, type, context} -> {:ok, {:list, type}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  defp do_unify({:map, left_pairs}, {:map, right_pairs}, stack, context) do
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
            case do_unify(left_value, right_value, stack, context) do
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

  defp do_unify(:dynamic, right, _stack, context) do
    {:ok, right, context}
  end

  defp do_unify(left, :dynamic, _stack, context) do
    {:ok, left, context}
  end

  defp do_unify(left, right, stack, context) do
    cond do
      subtype?(left, right, context) -> {:ok, left, context}
      subtype?(right, left, context) -> {:ok, right, context}
      true -> error({:unable_unify, left, right}, stack, context)
    end
  end

  # Check unify stack to see if variable was already expanded
  defp variable_expanded?(var, stack, context) do
    Enum.any?(stack.unify_stack, &variable_same?(var, &1, context))
  end

  # Find if two variables are the same or point to the other
  defp variable_same?(same, same, _context) do
    true
  end

  defp variable_same?(left, right, context) do
    case Map.fetch(context.types, left) do
      {:ok, {:var, new_left}} ->
        variable_same?(new_left, right, context)

      _ ->
        case Map.fetch(context.types, right) do
          {:ok, {:var, new_right}} -> variable_same?(left, new_right, context)
          _ -> false
        end
    end
  end

  defp push_unify_stack(var, stack) do
    %{stack | unify_stack: [var | stack.unify_stack]}
  end

  @doc """
  Adds a variable to the typing context and returns its type variables.
  If the variable has already been added, return the existing type variable.
  """
  def new_var(var, context) do
    case Map.fetch(context.vars, var_name(var)) do
      {:ok, type} ->
        {type, context}

      :error ->
        type = {:var, context.counter}
        vars = Map.put(context.vars, var_name(var), type)
        types_to_vars = Map.put(context.types_to_vars, context.counter, var)
        types = Map.put(context.types, context.counter, :unbound)
        traces = Map.put(context.traces, context.counter, [])

        context = %{
          context
          | vars: vars,
            types_to_vars: types_to_vars,
            types: types,
            traces: traces,
            counter: context.counter + 1
        }

        {type, context}
    end
  end

  defp refine_var(var, type, stack, context) do
    types = Map.put(context.types, var, type)
    context = %{context | types: types}
    trace_var(var, type, stack, context)
  end

  defp remove_var(var, context) do
    types = Map.delete(context.types, var)
    traces = Map.delete(context.traces, var)
    %{context | types: types, traces: traces}
  end

  defp trace_var(var, type, %{trace: true, expr_stack: expr_stack} = _stack, context) do
    line = get_meta(hd(expr_stack))[:line]
    trace = {type, expr_stack, {context.file, line}}
    traces = Map.update!(context.traces, var, &[trace | &1])
    %{context | traces: traces}
  end

  defp trace_var(_var, _type, %{trace: false} = _stack, context) do
    context
  end

  defp var_name({name, meta, context}), do: {name, var_context(meta, context)}

  defp var_context(meta, context) do
    case :lists.keyfind(:counter, 1, meta) do
      {:counter, counter} -> counter
      false -> context
    end
  end

  # Check if a variable is recursive and incompatible with itself
  # Bad: `{var} = var`
  # Good: `x = y; y = z; z = x`
  defp recursive_type?({:var, var} = parent, parents, context) do
    case Map.fetch!(context.types, var) do
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

  defp recursive_type?({:list, type} = parent, parents, context) do
    recursive_type?(type, [parent | parents], context)
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
  def subtype?({:atom, boolean}, :boolean, _context) when is_boolean(boolean), do: true
  def subtype?({:atom, atom}, :atom, _context) when is_atom(atom), do: true
  def subtype?(:boolean, :atom, _context), do: true
  def subtype?(:float, :number, _context), do: true
  def subtype?(:integer, :number, _context), do: true
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
  Returns a "simplified" union using `subtype?/3` to remove redundant types.

  Due to limitations in `subtype?/3` some overlapping types may still be
  included. For example unions with overlapping non-concrete types such as
  `{boolean()} | {atom()}` will not be merged or types with variables that
  are distinct but equivalent such as `a | b when a ~ b`.
  """
  # TODO: Translate union of all top types to dynamic()
  def to_union(types, context) when types != [] do
    if :dynamic in types do
      :dynamic
    else
      case unique_super_types(flatten_union(types), context) do
        [type] -> type
        types -> {:union, types}
      end
    end
  end

  defp flatten_union(types) do
    Enum.flat_map(types, fn
      {:union, types} -> flatten_union(types)
      type -> [type]
    end)
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
  defp error({:unable_unify, left, right}, stack, context) do
    {fun, arity} = context.function
    line = get_meta(hd(stack.expr_stack))[:line]
    location = {context.file, line, {context.module, fun, arity}}

    traces = type_traces(stack, context)
    common_expr = common_super_expr(traces)
    traces = simplify_traces(traces, context)

    {:error, {Module.Types, {:unable_unify, left, right, common_expr, traces}, [location]}}
  end

  # Collect relevant traces from context.traces using stack.unify_stack
  defp type_traces(stack, context) do
    stack = Enum.uniq(stack.unify_stack)

    Enum.flat_map(stack, fn var_index ->
      case Map.fetch(context.traces, var_index) do
        {:ok, traces} ->
          expr_var = Map.fetch!(context.types_to_vars, var_index)
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
          var2 = Map.fetch!(context.types_to_vars, var_index)
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

  defp get_meta({_fun, meta, _args}) when is_list(meta), do: meta
  defp get_meta(_other), do: []

  defp guard_signature(name, arity) do
    Map.fetch!(@guard_functions, {name, arity})
  end

  defp type_guard?(name) do
    name in @type_guards
  end
end
