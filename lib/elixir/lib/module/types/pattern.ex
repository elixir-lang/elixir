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
         # TODO: Check that of_guard/4 returns boolean() | :fail
         {:ok, _, context} <- of_guard(guards_to_or(guards), :dynamic, stack, context),
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

  # left = right
  def of_pattern({:=, _meta, [left_expr, right_expr]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    with {:ok, left_type, context} <- of_pattern(left_expr, stack, context),
         {:ok, right_type, context} <- of_pattern(right_expr, stack, context),
         do: unify(left_type, right_type, stack, context)
  end

  # %_{...}
  def of_pattern(
        {:%, _meta1, [{:_, _meta2, var_context}, {:%{}, _meta3, args}]} = expr,
        stack,
        context
      )
      when is_atom(var_context) do
    stack = push_expr_stack(expr, stack)
    expected_fun = fn arg, _expected, stack, context -> of_pattern(arg, stack, context) end

    with {:ok, {:map, pairs}, context} <- Of.open_map(args, stack, context, expected_fun) do
      {:ok, {:map, [{:required, {:atom, :__struct__}, :atom} | pairs]}, context}
    end
  end

  # %var{...} and %^var{...}
  def of_pattern({:%, _meta1, [var, {:%{}, _meta2, args}]} = expr, stack, context)
      when not is_atom(var) do
    stack = push_expr_stack(expr, stack)
    expected_fun = fn arg, _expected, stack, context -> of_pattern(arg, stack, context) end

    with {:ok, var_type, context} = of_pattern(var, stack, context),
         {:ok, _, context} <- unify(var_type, :atom, stack, context),
         {:ok, {:map, pairs}, context} <- Of.open_map(args, stack, context, expected_fun) do
      {:ok, {:map, [{:required, {:atom, :__struct__}, var_type} | pairs]}, context}
    end
  end

  def of_pattern(expr, stack, context) do
    of_shared(expr, stack, context, &of_pattern/3)
  end

  ## GUARDS

  # TODO: Some guards can be changed to intersection types or higher order types
  @boolean {:union, [{:atom, true}, {:atom, false}]}
  @number {:union, [:integer, :float]}
  @unary_number_fun [{[:integer], :integer}, {[@number], :float}]
  @binary_number_fun [{[:integer, :integer], :integer}, {[@number, @number], :float}]

  @guard_functions %{
    {:is_atom, 1} => [{[:atom], @boolean}],
    {:is_binary, 1} => [{[:binary], @boolean}],
    {:is_bitstring, 1} => [{[:binary], @boolean}],
    {:is_boolean, 1} => [{[@boolean], @boolean}],
    {:is_float, 1} => [{[:float], @boolean}],
    {:is_function, 1} => [{[:fun], @boolean}],
    {:is_function, 2} => [{[:fun, :integer], @boolean}],
    {:is_integer, 1} => [{[:integer], @boolean}],
    {:is_list, 1} => [{[{:list, :dynamic}], @boolean}],
    {:is_map, 1} => [{[{:map, [{:optional, :dynamic, :dynamic}]}], @boolean}],
    {:is_map_key, 2} => [{[:dynamic, {:map, [{:optional, :dynamic, :dynamic}]}], :dynamic}],
    {:is_number, 1} => [{[@number], @boolean}],
    {:is_pid, 1} => [{[:pid], @boolean}],
    {:is_port, 1} => [{[:port], @boolean}],
    {:is_reference, 1} => [{[:reference], @boolean}],
    {:is_tuple, 1} => [{[:tuple], @boolean}],
    {:<, 2} => [{[:dynamic, :dynamic], @boolean}],
    {:"=<", 2} => [{[:dynamic, :dynamic], @boolean}],
    {:>, 2} => [{[:dynamic, :dynamic], @boolean}],
    {:>=, 2} => [{[:dynamic, :dynamic], @boolean}],
    {:"/=", 2} => [{[:dynamic, :dynamic], @boolean}],
    {:"=/=", 2} => [{[:dynamic, :dynamic], @boolean}],
    {:==, 2} => [{[:dynamic, :dynamic], @boolean}],
    {:"=:=", 2} => [{[:dynamic, :dynamic], @boolean}],
    {:*, 2} => @binary_number_fun,
    {:+, 1} => @unary_number_fun,
    {:+, 2} => @binary_number_fun,
    {:-, 1} => @unary_number_fun,
    {:-, 2} => @binary_number_fun,
    {:/, 2} => @binary_number_fun,
    {:abs, 1} => @unary_number_fun,
    {:ceil, 1} => [{[@number], :integer}],
    {:floor, 1} => [{[@number], :integer}],
    {:round, 1} => [{[@number], :integer}],
    {:trunc, 1} => [{[@number], :integer}],
    {:element, 2} => [{[:integer, :tuple], :dynamic}],
    {:hd, 1} => [{[{:list, :dynamic}], :dynamic}],
    {:length, 1} => [{[{:list, :dynamic}], :integer}],
    {:map_get, 2} => [{[:dynamic, {:map, [{:optional, :dynamic, :dynamic}]}], :dynamic}],
    {:map_size, 1} => [{[{:map, [{:optional, :dynamic, :dynamic}]}], :integer}],
    {:tl, 1} => [{[{:list, :dynamic}], :dynamic}],
    {:tuple_size, 1} => [{[:tuple], :integer}],
    {:node, 1} => [{[{:union, [:pid, :reference, :port]}], :atom}],
    {:binary_part, 3} => [{[:binary, :integer, :integer], :binary}],
    {:bit_size, 1} => [{[:binary], :integer}],
    {:byte_size, 1} => [{[:binary], :integer}],
    {:size, 1} => [{[{:union, [:binary, :tuple]}], @boolean}],
    {:div, 2} => [{[:integer, :integer], :integer}],
    {:rem, 2} => [{[:integer, :integer], :integer}],
    {:node, 0} => [{[], :atom}],
    {:self, 0} => [{[], :pid}],
    {:bnot, 1} => [{[:integer], :integer}],
    {:band, 2} => [{[:integer, :integer], :integer}],
    {:bor, 2} => [{[:integer, :integer], :integer}],
    {:bxor, 2} => [{[:integer, :integer], :integer}],
    {:bsl, 2} => [{[:integer, :integer], :integer}],
    {:bsr, 2} => [{[:integer, :integer], :integer}],
    {:or, 2} => [{[@boolean, @boolean], @boolean}],
    {:and, 2} => [{[@boolean, @boolean], @boolean}],
    {:xor, 2} => [{[@boolean, @boolean], @boolean}],
    {:not, 1} => [{[@boolean], @boolean}]

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
  def of_guard(expr, expected, %{context: stack_context} = stack, context)
      when stack_context != :pattern do
    of_guard(expr, expected, %{stack | context: :pattern}, context)
  end

  def of_guard({{:., _, [:erlang, :andalso]}, _, [left, right]} = expr, _expected, stack, context) do
    stack = push_expr_stack(expr, stack)

    with {:ok, left_type, context} <- of_guard(left, @boolean, stack, context),
         {:ok, _, context} <- unify(left_type, @boolean, stack, context),
         {:ok, right_type, context} <- of_guard(right, :dynamic, keep_guarded(stack), context),
         do: {:ok, to_union([@boolean, right_type], context), context}
  end

  def of_guard({{:., _, [:erlang, :orelse]}, _, [left, right]} = expr, _expected, stack, context) do
    stack = push_expr_stack(expr, stack)
    left_indexes = collect_var_indexes_from_expr(left, context)
    right_indexes = collect_var_indexes_from_expr(right, context)

    with {:ok, left_type, left_context} <- of_guard(left, @boolean, stack, context),
         {:ok, _right_type, right_context} <- of_guard(right, :dynamic, stack, context),
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
  def of_guard({{:., _, [:erlang, guard]}, _, [integer]}, _expected, _stack, context)
      when guard in [:+, :-] and is_integer(integer) do
    {:ok, :integer, context}
  end

  # -float / +float
  def of_guard({{:., _, [:erlang, guard]}, _, [float]}, _expected, _stack, context)
      when guard in [:+, :-] and is_float(float) do
    {:ok, :float, context}
  end

  # fun(args)
  def of_guard({{:., _, [:erlang, guard]}, _, args} = expr, expected, stack, context) do
    type_guard? = type_guard?(guard)
    {consider_type_guards?, keep_guarded?} = stack.type_guards
    signature = guard_signature(guard, length(args))

    # Only check type guards in the context of and/or/not,
    # a type guard in the context of is_tuple(x) > :foo
    # should not affect the inference of x
    if not type_guard? or consider_type_guards? do
      stack = push_expr_stack(expr, stack)
      param_unions = signature_to_param_unions(signature, context)
      arg_stack = %{stack | type_guards: {false, keep_guarded?}}
      mfa = {:erlang, guard, length(args)}

      with {:ok, arg_types, context} <-
             map_reduce_ok(Enum.zip(args, param_unions), context, fn {arg, param}, context ->
               of_guard(arg, param, arg_stack, context)
             end),
           {:ok, return_type, context} <-
             unify_call(arg_types, signature, expected, mfa, stack, context, type_guard?) do
        guard_sources = guard_sources(arg_types, type_guard?, keep_guarded?, context)
        {:ok, return_type, %{context | guard_sources: guard_sources}}
      end
    else
      # Assume that type guards always return boolean
      boolean = {:union, [atom: true, atom: false]}
      [{_params, ^boolean}] = signature
      {:ok, boolean, context}
    end
  end

  # map.field
  def of_guard({{:., meta1, [map, field]}, meta2, []}, expected, stack, context) do
    of_guard({{:., meta1, [:erlang, :map_get]}, meta2, [field, map]}, expected, stack, context)
  end

  # var
  def of_guard(var, _expected, _stack, context) when is_var(var) do
    {:ok, get_var!(var, context), context}
  end

  def of_guard(expr, _expected, stack, context) do
    of_shared(expr, stack, context, &of_guard(&1, :dynamic, &2, &3))
  end

  defp signature_to_param_unions(signature, context) do
    signature
    |> Enum.map(fn {params, _return} -> params end)
    |> zip_many()
    |> Enum.map(&to_union(&1, context))
  end

  # Collect guard sources from argument types, see type context documentation
  # for more information
  defp guard_sources(arg_types, type_guard?, keep_guarded?, context) do
    {arg_types, guard_sources} =
      case arg_types do
        [{:var, index} | rest_arg_types] when type_guard? ->
          guard_sources = Map.put_new(context.guard_sources, index, :guarded)
          {rest_arg_types, guard_sources}

        _ ->
          {arg_types, context.guard_sources}
      end

    Enum.reduce(arg_types, guard_sources, fn
      {:var, index}, guard_sources ->
        Map.update(guard_sources, index, :fail, &guarded_if_keep_guarded(&1, keep_guarded?))

      _, guard_sources ->
        guard_sources
    end)
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

  defp unify_call(args, clauses, _expected, _mfa, stack, context, true = _type_guard?) do
    unify_type_guard_call(args, clauses, stack, context)
  end

  defp unify_call(args, clauses, expected, mfa, stack, context, false = _type_guard?) do
    unify_call(args, clauses, expected, mfa, stack, context)
  end

  defp unify_call([], [{[], return}], _expected, _mfa, _stack, context) do
    {:ok, return, context}
  end

  defp unify_call(args, signature, expected, mfa, stack, context) do
    # Given the arguments:
    # foo | bar, {:ok, baz | bat}

    # Expand unions in arguments:
    # foo | bar, {:ok, baz} | {:ok, bat}

    # Permute arguments:
    # foo, {:ok, baz}
    # foo, {:ok, bat}
    # bar, {:ok, baz}
    # bar, {:ok, bat}

    expanded_args =
      args
      |> Enum.map(&flatten_union/1)
      |> cartesian_product()

    # Remove clauses that do not match the expected type
    # Ignore type variables in parameters by changing them to dynamic

    clauses =
      signature
      |> filter_clauses(expected, stack, context)
      |> Enum.map(fn {params, return} ->
        {Enum.map(params, &var_to_dynamic/1), return}
      end)

    # For each permuted argument find the clauses they match
    # All arguments must match at least one clause, but all clauses
    # do not need to match
    # Collect the return values from clauses that matched and collect
    # the type contexts from unifying argument and parameter to
    # infer type variables in arguments
    result =
      flat_map_ok(expanded_args, fn expanded_args ->
        result =
          Enum.flat_map(clauses, fn {params, return} ->
            result =
              map_ok(Enum.zip(expanded_args, params), fn {arg, param} ->
                case unify(arg, param, stack, context) do
                  {:ok, _type, context} -> {:ok, context}
                  {:error, reason} -> {:error, reason}
                end
              end)

            case result do
              {:ok, contexts} -> [{return, contexts}]
              {:error, _reason} -> []
            end
          end)

        if result != [] do
          {:ok, result}
        else
          {:error, args}
        end
      end)

    case result do
      {:ok, returns_contexts} ->
        {success_returns, contexts} = Enum.unzip(returns_contexts)
        contexts = Enum.concat(contexts)
        indexes = Enum.uniq(Enum.flat_map(args, &collect_var_indexes_from_type/1))

        # Build unions from collected type contexts to unify with
        # type variables from arguments
        result =
          map_reduce_ok(indexes, context, fn index, context ->
            union =
              contexts
              |> Enum.map(&Map.fetch!(&1.types, index))
              |> Enum.reject(&(&1 == :unbound))

            if union == [] do
              {:ok, {:var, index}, context}
            else
              unify({:var, index}, to_union(union, context), stack, context)
            end
          end)

        case result do
          {:ok, _types, context} -> {:ok, to_union(success_returns, context), context}
          {:error, reason} -> {:error, reason}
        end

      {:error, args} ->
        error(:unable_apply, {mfa, args, signature, stack}, context)
    end
  end

  defp unify_type_guard_call(args, [{params, return}], stack, context) do
    result =
      reduce_ok(Enum.zip(args, params), context, fn {arg, param}, context ->
        case unify(arg, param, stack, context) do
          {:ok, _, context} -> {:ok, context}
          {:error, reason} -> {:error, reason}
        end
      end)

    case result do
      {:ok, context} -> {:ok, return, context}
      {:error, reason} -> {:error, reason}
    end
  end

  defp cartesian_product(lists) do
    List.foldr(lists, [[]], fn list, acc ->
      for elem_list <- list,
          list_acc <- acc,
          do: [elem_list | list_acc]
    end)
  end

  defp var_to_dynamic(type) do
    {type, _acc} =
      walk(type, :ok, fn
        {:var, _index}, :ok ->
          {:dynamic, :ok}

        other, :ok ->
          {other, :ok}
      end)

    type
  end

  defp collect_var_indexes_from_type(type) do
    {_type, indexes} =
      walk(type, [], fn
        {:var, index}, indexes ->
          {{:var, index}, [index | indexes]}

        other, indexes ->
          {other, indexes}
      end)

    indexes
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

  defp filter_clauses(signature, expected, stack, context) do
    Enum.filter(signature, fn {_params, return} ->
      match?({:ok, _type, _context}, unify(return, expected, stack, context))
    end)
  end

  defp type_guard?(name) do
    name in @type_guards
  end

  ## Shared

  # :atom
  defp of_shared(atom, _stack, context, _fun) when is_atom(atom) do
    {:ok, {:atom, atom}, context}
  end

  # 12
  defp of_shared(literal, _stack, context, _fun) when is_integer(literal) do
    {:ok, :integer, context}
  end

  # 1.2
  defp of_shared(literal, _stack, context, _fun) when is_float(literal) do
    {:ok, :float, context}
  end

  # "..."
  defp of_shared(literal, _stack, context, _fun) when is_binary(literal) do
    {:ok, :binary, context}
  end

  # <<...>>>
  defp of_shared({:<<>>, _meta, args}, stack, context, fun) do
    expected_fun = fn arg, _expected, stack, context -> fun.(arg, stack, context) end

    case Of.binary(args, stack, context, expected_fun) do
      {:ok, context} -> {:ok, :binary, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # left | []
  defp of_shared({:|, _meta, [left_expr, []]} = expr, stack, context, fun) do
    stack = push_expr_stack(expr, stack)
    fun.(left_expr, stack, context)
  end

  # left | right
  defp of_shared({:|, _meta, [left_expr, right_expr]} = expr, stack, context, fun) do
    stack = push_expr_stack(expr, stack)

    case fun.(left_expr, stack, context) do
      {:ok, left, context} ->
        case fun.(right_expr, stack, context) do
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
  defp of_shared([], _stack, context, _fun) do
    {:ok, {:list, :dynamic}, context}
  end

  # [expr, ...]
  defp of_shared(exprs, stack, context, fun) when is_list(exprs) do
    stack = push_expr_stack(exprs, stack)

    case map_reduce_ok(exprs, context, &fun.(&1, stack, &2)) do
      {:ok, types, context} -> {:ok, {:list, to_union(types, context)}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # left ++ right
  defp of_shared(
         {{:., _meta1, [:erlang, :++]}, _meta2, [left_expr, right_expr]} = expr,
         stack,
         context,
         fun
       ) do
    stack = push_expr_stack(expr, stack)

    case fun.(left_expr, stack, context) do
      {:ok, {:list, left}, context} ->
        case fun.(right_expr, stack, context) do
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

  # {left, right}
  defp of_shared({left, right}, stack, context, fun) do
    of_shared({:{}, [], [left, right]}, stack, context, fun)
  end

  # {...}
  defp of_shared({:{}, _meta, exprs} = expr, stack, context, fun) do
    stack = push_expr_stack(expr, stack)

    case map_reduce_ok(exprs, context, &fun.(&1, stack, &2)) do
      {:ok, types, context} -> {:ok, {:tuple, length(types), types}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # %{...}
  defp of_shared({:%{}, _meta, args} = expr, stack, context, fun) do
    stack = push_expr_stack(expr, stack)
    expected_fun = fn arg, _expected, stack, context -> fun.(arg, stack, context) end
    Of.open_map(args, stack, context, expected_fun)
  end

  # %Struct{...}
  defp of_shared({:%, meta1, [module, {:%{}, _meta2, args}]} = expr, stack, context, fun)
       when is_atom(module) do
    stack = push_expr_stack(expr, stack)
    expected_fun = fn arg, _expected, stack, context -> fun.(arg, stack, context) end

    with {:ok, struct, context} <- Of.struct(module, meta1, context),
         {:ok, map, context} <- Of.open_map(args, stack, context, expected_fun) do
      unify(map, struct, stack, context)
    end
  end
end
