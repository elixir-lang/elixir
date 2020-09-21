defmodule Module.Types.Pattern do
  @moduledoc false

  alias Module.Types.Of
  import Module.Types.{Helpers, Unify}

  @doc """
  Handles patterns and guards at once.
  """
  def of_head(patterns, guards, stack, context) do
    with {:ok, types, context} <-
           map_reduce_ok(patterns, context, &of_pattern(&1, stack, &2)),
         # TODO: Check that of_guard/3 returns boolean() | :fail
         {:ok, _, context} <- of_guard(guards_to_or(guards), stack, context),
         do: {:ok, types, context}
  end

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
  def of_pattern({:<<>>, _meta, args}, stack, context) do
    result = Of.binary(args, stack, context, &of_pattern/3)

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
  def of_pattern({:^, _meta, [var]}, _stack, context) do
    {:ok, get_var!(var, context), context}
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
      {:ok, types, context} -> {:ok, {:tuple, length(types), types}, context}
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
    Of.open_map(args, stack, context, &of_pattern/3)
  end

  # %Struct{...}
  def of_pattern({:%, meta1, [module, {:%{}, _meta2, args}]} = expr, stack, context)
      when is_atom(module) do
    stack = push_expr_stack(expr, stack)

    with {:ok, struct, context} <- Of.struct(module, meta1, context),
         {:ok, map, context} <- Of.open_map(args, stack, context, &of_pattern/3) do
      unify(map, struct, stack, context)
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

    with {:ok, {:map, pairs}, context} <- Of.open_map(args, stack, context, &of_pattern/3) do
      {:ok, {:map, [{:required, {:atom, :__struct__}, :atom} | pairs]}, context}
    end
  end

  # %var{...} and %^var{...}
  def of_pattern({:%, _meta1, [var, {:%{}, _meta2, args}]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    with {:ok, var_type, context} = of_pattern(var, stack, context),
         {:ok, _, context} <- unify(var_type, :atom, stack, context),
         {:ok, {:map, pairs}, context} <- Of.open_map(args, stack, context, &of_pattern/3) do
      {:ok, {:map, [{:required, {:atom, :__struct__}, var_type} | pairs]}, context}
    end
  end

  def unify_kinds(:required, _), do: :required
  def unify_kinds(_, :required), do: :required
  def unify_kinds(:optional, :optional), do: :optional

  ## GUARDS

  # TODO: Some guards can be changed to intersection types or higher order types
  @boolean {:union, [{:atom, true}, {:atom, false}]}
  @number {:union, [:integer, :float]}

  @guard_functions %{
    {:is_atom, 1} => {[:atom], @boolean},
    {:is_binary, 1} => {[:binary], @boolean},
    {:is_bitstring, 1} => {[:binary], @boolean},
    {:is_boolean, 1} => {[@boolean], @boolean},
    {:is_float, 1} => {[:float], @boolean},
    {:is_function, 1} => {[:fun], @boolean},
    {:is_function, 2} => {[:fun, :integer], @boolean},
    {:is_integer, 1} => {[:integer], @boolean},
    {:is_list, 1} => {[{:list, :dynamic}], @boolean},
    {:is_map, 1} => {[{:map, [{:optional, :dynamic, :dynamic}]}], @boolean},
    {:is_map_key, 2} => {[:dynamic, {:map, [{:optional, :dynamic, :dynamic}]}], :dynamic},
    {:is_number, 1} => {[@number], @boolean},
    {:is_pid, 1} => {[:pid], @boolean},
    {:is_port, 1} => {[:port], @boolean},
    {:is_reference, 1} => {[:reference], @boolean},
    {:is_tuple, 1} => {[:tuple], @boolean},
    {:<, 2} => {[:dynamic, :dynamic], @boolean},
    {:"=<", 2} => {[:dynamic, :dynamic], @boolean},
    {:>, 2} => {[:dynamic, :dynamic], @boolean},
    {:>=, 2} => {[:dynamic, :dynamic], @boolean},
    {:"/=", 2} => {[:dynamic, :dynamic], @boolean},
    {:"=/=", 2} => {[:dynamic, :dynamic], @boolean},
    {:==, 2} => {[:dynamic, :dynamic], @boolean},
    {:"=:=", 2} => {[:dynamic, :dynamic], @boolean},
    {:*, 2} => {[@number, @number], @number},
    {:+, 1} => {[@number], @number},
    {:+, 2} => {[@number, @number], @number},
    {:-, 1} => {[@number], @number},
    {:-, 2} => {[@number, @number], @number},
    {:/, 2} => {[@number, @number], @number},
    {:abs, 1} => {[@number], @number},
    {:ceil, 1} => {[@number], :integer},
    {:floor, 1} => {[@number], :integer},
    {:round, 1} => {[@number], :integer},
    {:trunc, 1} => {[@number], :integer},
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
    {:size, 1} => {[{:union, [:binary, :tuple]}], @boolean},
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
    {:or, 2} => {[@boolean, @boolean], @boolean},
    {:and, 2} => {[@boolean, @boolean], @boolean},
    {:xor, 2} => {[@boolean, @boolean], @boolean},
    {:not, 1} => {[@boolean], @boolean}

    # Following guards are matched explicitly to handle
    # type guard functions such as is_atom/1
    # {:andalso, 2} => {[@boolean, @boolean], @boolean}
    # {:orelse, 2} => {[@boolean, @boolean], @boolean}
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

    with {:ok, left_type, context} <- of_guard(left, stack, context),
         {:ok, _, context} <- unify(left_type, @boolean, stack, context),
         {:ok, right_type, context} <- of_guard(right, keep_guarded(stack), context),
         {:ok, _, context} <- unify(right_type, @boolean, stack, context),
         do: {:ok, @boolean, context}
  end

  def of_guard({{:., _, [:erlang, :orelse]}, _, [left, right]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)
    left_indexes = collect_var_indexes_from_expr(left, context)
    right_indexes = collect_var_indexes_from_expr(right, context)

    with {:ok, left_type, left_context} <- of_guard(left, stack, context),
         {:ok, _right_type, right_context} <- of_guard(right, stack, context),
         context =
           merge_context_or(
             left_indexes,
             right_indexes,
             context,
             stack,
             left_context,
             right_context
           ),
         {:ok, _, context} <- unify(left_type, @boolean, stack, context),
         do: {:ok, @boolean, context}
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
    {consider_type_guards?, keep_guarded?} = stack.type_guards

    # Only check type guards in the context of and/or/not,
    # a type guard in the context of is_tuple(x) > :foo
    # should not affect the inference of x
    if not type_guard? or consider_type_guards? do
      arg_stack = %{stack | type_guards: {false, keep_guarded?}}

      with {:ok, arg_types, context} <-
             map_reduce_ok(args, context, &of_guard(&1, arg_stack, &2)),
           {:ok, context} <- unify_call(arg_types, param_types, stack, context) do
        {arg_types, guard_sources} =
          case arg_types do
            [{:var, index} | rest_arg_types] when type_guard? ->
              guard_sources = Map.put_new(context.guard_sources, index, :guarded)
              {rest_arg_types, guard_sources}

            _ ->
              {arg_types, context.guard_sources}
          end

        guard_sources =
          Enum.reduce(arg_types, guard_sources, fn
            {:var, index}, guard_sources ->
              Map.update(guard_sources, index, :fail, &guarded_if_keep_guarded(&1, keep_guarded?))

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
    {:ok, get_var!(var, context), context}
  end

  # other literals
  def of_guard(expr, stack, context) do
    # Fall back to of_pattern/3 for literals
    of_pattern(expr, stack, context)
  end

  defp collect_var_indexes_from_expr(expr, context) do
    {_, vars} =
      Macro.prewalk(expr, %{}, fn
        var, acc when is_var(var) ->
          var_name = var_name(var)
          %{^var_name => type} = context.vars
          {var, collect_var_indexes(type, context, acc)}

        other, acc ->
          {other, acc}
      end)

    Map.keys(vars)
  end

  defp unify_call(args, params, stack, context) do
    reduce_ok(Enum.zip(args, params), context, fn {arg, param}, context ->
      case unify(arg, param, stack, context) do
        {:ok, _, context} -> {:ok, context}
        {:error, reason} -> {:error, reason}
      end
    end)
  end

  defp merge_context_or(left_indexes, right_indexes, context, stack, left, right) do
    left_different = filter_different_indexes(left_indexes, left, right)
    right_different = filter_different_indexes(right_indexes, left, right)

    case {left_different, right_different} do
      {[index], [index]} -> merge_context_or_equal(index, stack, left, right)
      {_, _} -> merge_context_or_diff(left_different, context, left)
    end
  end

  defp filter_different_indexes(indexes, left, right) do
    Enum.filter(indexes, fn index ->
      %{^index => left_type} = left.types
      %{^index => right_type} = right.types
      left_type != right_type
    end)
  end

  defp merge_context_or_equal(index, stack, left, right) do
    %{^index => left_type} = left.types
    %{^index => right_type} = right.types

    cond do
      left_type == :unbound ->
        refine_var!(index, right_type, stack, left)

      right_type == :unbound ->
        left

      true ->
        # Only include right side if left side is from type guard such as is_list(x),
        # do not refine in case of length(x)
        if left.guard_sources[index] == :fail do
          guard_sources = Map.put(left.guard_sources, index, :fail)
          left = %{left | guard_sources: guard_sources}
          refine_var!(index, left_type, stack, left)
        else
          guard_sources = merge_guard_sources([left.guard_sources, right.guard_sources])
          left = %{left | guard_sources: guard_sources}
          refine_var!(index, to_union([left_type, right_type], left), stack, left)
        end
    end
  end

  # If the variable failed, we can keep them from the left side as is.
  # If they didn't fail, then we need to restore them to their original value.
  defp merge_context_or_diff(indexes, old_context, new_context) do
    Enum.reduce(indexes, new_context, fn index, context ->
      if new_context.guard_sources[index] == :fail do
        context
      else
        restore_var!(index, new_context, old_context)
      end
    end)
  end

  defp merge_guard_sources(sources) do
    Enum.reduce(sources, fn left, right ->
      Map.merge(left, right, fn
        _index, :guarded, :guarded -> :guarded
        _index, _, _ -> :fail
      end)
    end)
  end

  defp guarded_if_keep_guarded(:guarded, true), do: :guarded
  defp guarded_if_keep_guarded(_, _), do: :fail

  defp keep_guarded(%{type_guards: {consider?, _}} = stack),
    do: %{stack | type_guards: {consider?, true}}

  defp guard_signature(name, arity) do
    Map.fetch!(@guard_functions, {name, arity})
  end

  defp type_guard?(name) do
    name in @type_guards
  end
end
