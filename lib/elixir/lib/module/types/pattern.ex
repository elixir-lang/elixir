defmodule Module.Types.Pattern do
  @moduledoc false

  import Module.Types.{Helpers, Infer}

  @doc """
  Return the type and typing context of a pattern expression or an error
  in case of a typing conflict.
  """
  def of_pattern(pattern, %{context: stack_context} = stack, context)
      when stack_context != :pattern do
    of_pattern(pattern, %{stack | context: :pattern}, context)
  end

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

    result =
      reduce_ok(args, context, fn expr, context ->
        of_binary(expr, stack, context, &of_pattern/3)
      end)

    case result do
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
  def of_pattern(
        {{:., _meta1, [:erlang, :++]}, _meta2, [left_expr, right_expr]} = expr,
        stack,
        context
      ) do
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

  # ^var
  def of_pattern({:^, _meta, [var]}, stack, context) do
    of_pattern(var, stack, context)
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
      {:ok, pairs, context} ->
        pairs = pairs_to_unions(pairs, context) ++ [{:optional, :dynamic, :dynamic}]
        {:ok, {:map, pairs}, context}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # %_{...}
  def of_pattern(
        {:%, _meta1, [{:_, _meta2, var_context}, {:%{}, _meta3, args}]} = expr,
        stack,
        context
      )
      when is_atom(var_context) do
    stack = push_expr_stack(expr, stack)

    case of_pairs(args, stack, context) do
      {:ok, pairs, context} ->
        pairs =
          [{:required, {:atom, :__struct__}, :atom}] ++ pairs ++ [{:optional, :dynamic, :dynamic}]

        {:ok, {:map, pairs}, context}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # %^var{...}
  def of_pattern({:%, meta1, [{:^, _meta2, [var]}, args]}, stack, context) do
    of_pattern({:%, meta1, [var, args]}, stack, context)
  end

  # %var{...}
  def of_pattern({:%, _meta1, [var, {:%{}, _meta2, args}]} = expr, stack, context)
      when is_var(var) do
    stack = push_expr_stack(expr, stack)

    with {:ok, pairs, context} <- of_pairs(args, stack, context),
         {var_type, context} = new_var(var, context),
         {:ok, _, context} <- unify(var_type, :atom, stack, context) do
      pairs =
        [{:required, {:atom, :__struct__}, var_type}] ++
          pairs ++ [{:optional, :dynamic, :dynamic}]

      {:ok, {:map, pairs}, context}
    end
  end

  # %Struct{...}
  def of_pattern({:%, _meta1, [module, {:%{}, _meta2, args}]} = expr, stack, context)
      when is_atom(module) do
    stack = push_expr_stack(expr, stack)

    case of_pairs(args, stack, context) do
      {:ok, pairs, context} ->
        pairs = [{:required, {:atom, :__struct__}, {:atom, module}} | pairs]
        {:ok, {:map, pairs}, context}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp of_pairs(pairs, stack, context) do
    map_reduce_ok(pairs, context, fn {key, value}, context ->
      with {:ok, key_type, context} <- of_pattern(key, stack, context),
           {:ok, value_type, context} <- of_pattern(value, stack, context),
           do: {:ok, {:required, key_type, value_type}, context}
    end)
  end

  defp pairs_to_unions(pairs, context) do
    # Maps only allow simple literal keys in patterns so
    # we do not have to do subtype checking

    Enum.reduce(pairs, [], fn {kind_left, key, value_left}, pairs ->
      case List.keyfind(pairs, key, 1) do
        {kind_right, ^key, value_right} ->
          kind = unify_kinds(kind_left, kind_right)
          value = to_union([value_left, value_right], context)
          List.keystore(pairs, key, 1, {kind, key, value})

        nil ->
          [{kind_left, key, value_left} | pairs]
      end
    end)
  end

  ## GUARDS

  # TODO: Some guards can be changed to intersection types or higher order types

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
    {:is_map, 1} => {[{:map, [{:optional, :dynamic, :dynamic}]}], :boolean},
    {:is_map_key, 2} => {[:dynamic, {:map, [{:optional, :dynamic, :dynamic}]}], :dynamic},
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
    {:map_get, 2} => {[:dynamic, {:map, [{:optional, :dynamic, :dynamic}]}], :dynamic},
    {:map_size, 1} => {[{:map, [{:optional, :dynamic, :dynamic}]}], :integer},
    {:tl, 1} => {[{:list, :dynamic}], :dynamic},
    {:tuple_size, 1} => {[:tuple], :integer},
    {:node, 1} => {[{:union, [:pid, :reference, :port]}], :atom},
    {:binary_part, 3} => {[:binary, :integer, :integer], :binary},
    {:bit_size, 1} => {[:binary], :integer},
    {:byte_size, 1} => {[:binary], :integer},
    {:size, 1} => {[{:union, [:binary, :tuple]}], :boolean},
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
    {:or, 2} => {[:boolean, :boolean], :boolean},
    {:and, 2} => {[:boolean, :boolean], :boolean},
    {:xor, 2} => {[:boolean, :boolean], :boolean},
    {:not, 1} => {[:boolean], :boolean}

    # Following guards are matched explicitly to handle
    # type guard functions such as is_atom/1
    # {:andalso, 2} => {[:boolean, :boolean], :boolean}
    # {:orelse, 2} => {[:boolean, :boolean], :boolean}
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
  def of_guard(expr, %{context: stack_context} = stack, context) when stack_context != :pattern do
    of_guard(expr, %{stack | context: :pattern}, context)
  end

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
         {:ok, _right_type, right_context} <- of_guard(right, stack, fresh_context),
         {:ok, context} <- merge_context_or(context, stack, left_context, right_context),
         {:ok, _, context} <- unify(left_type, :boolean, stack, context),
         do: {:ok, :boolean, context}
  end

  # The unary operators + and - are special cased to avoid common warnings until
  # we add support for intersection types for the guard functions
  # -integer / +integer
  def of_guard({{:., _, [:erlang, guard]}, _, [integer]}, _stack, context)
      when guard in [:+, :-] and is_integer(integer) do
    {:ok, :integer, context}
  end

  # -float / +float
  def of_guard({{:., _, [:erlang, guard]}, _, [float]}, _stack, context)
      when guard in [:+, :-] and is_float(float) do
    {:ok, :float, context}
  end

  # fun(args)
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
           {:ok, context} <- unify_call(arg_types, param_types, stack, context) do
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

  # map.field
  def of_guard({{:., meta1, [map, field]}, meta2, []}, stack, context) do
    of_guard({{:., meta1, [:erlang, :map_get]}, meta2, [field, map]}, stack, context)
  end

  # var
  def of_guard(var, _stack, context) when is_var(var) do
    type = Map.fetch!(context.vars, var_name(var))
    {:ok, type, context}
  end

  # other literals
  def of_guard(expr, stack, context) do
    # Fall back to of_pattern/3 for literals
    of_pattern(expr, stack, context)
  end

  defp fresh_context(context) do
    types = Map.new(context.types, fn {var, _} -> {var, :unbound} end)
    traces = Map.new(context.traces, fn {var, _} -> {var, []} end)
    %{context | types: types, traces: traces}
  end

  defp unify_call(args, params, stack, context) do
    reduce_ok(Enum.zip(args, params), context, fn {arg, param}, context ->
      case unify(arg, param, stack, context) do
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
      :guarded in sources -> [:guarded]
      true -> []
    end
  end

  defp and_guard_sources(left, right) do
    Map.merge(left, right, fn _index, left, right ->
      # When the failing guard function wont fail due to type check function before it,
      # for example: is_list(x) and length(x)
      if :guarded in left and :fail in right do
        [:guarded]
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

  defp guard_signature(name, arity) do
    Map.fetch!(@guard_functions, {name, arity})
  end

  defp type_guard?(name) do
    name in @type_guards
  end
end
