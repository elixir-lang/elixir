defmodule Module.Types.Expr do
  @moduledoc false

  alias Module.Types.{Of, Pattern}
  import Module.Types.{Helpers, Unify}

  def of_expr(expr, expected, %{context: stack_context} = stack, context)
      when stack_context != :expr do
    of_expr(expr, expected, %{stack | context: :expr}, context)
  end

  # :atom
  def of_expr(atom, _expected, _stack, context) when is_atom(atom) do
    {:ok, {:atom, atom}, context}
  end

  # 12
  def of_expr(literal, _expected, _stack, context) when is_integer(literal) do
    {:ok, :integer, context}
  end

  # 1.2
  def of_expr(literal, _expected, _stack, context) when is_float(literal) do
    {:ok, :float, context}
  end

  # "..."
  def of_expr(literal, _expected, _stack, context) when is_binary(literal) do
    {:ok, :binary, context}
  end

  # #PID<...>
  def of_expr(literal, _expected, _stack, context) when is_pid(literal) do
    {:ok, :dynamic, context}
  end

  # <<...>>>
  def of_expr({:<<>>, _meta, args}, _expected, stack, context) do
    case Of.binary(args, stack, context, &of_expr/4) do
      {:ok, context} -> {:ok, :binary, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # left | []
  def of_expr({:|, _meta, [left_expr, []]} = expr, _expected, stack, context) do
    stack = push_expr_stack(expr, stack)
    of_expr(left_expr, :dynamic, stack, context)
  end

  # left | right
  def of_expr({:|, _meta, [left_expr, right_expr]} = expr, _expected, stack, context) do
    stack = push_expr_stack(expr, stack)

    case of_expr(left_expr, :dynamic, stack, context) do
      {:ok, left, context} ->
        case of_expr(right_expr, :dynamic, stack, context) do
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
  def of_expr([], _expected, _stack, context) do
    {:ok, {:list, :dynamic}, context}
  end

  # [expr, ...]
  def of_expr(exprs, _expected, stack, context) when is_list(exprs) do
    stack = push_expr_stack(exprs, stack)

    case map_reduce_ok(exprs, context, &of_expr(&1, :dynamic, stack, &2)) do
      {:ok, types, context} -> {:ok, {:list, to_union(types, context)}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # __CALLER__
  def of_expr({:__CALLER__, _meta, var_context}, _expected, _stack, context)
      when is_atom(var_context) do
    struct_pair = {:required, {:atom, :__struct__}, {:atom, Macro.Env}}

    pairs =
      Enum.map(Map.from_struct(Macro.Env.__struct__()), fn {key, _value} ->
        {:required, {:atom, key}, :dynamic}
      end)

    {:ok, {:map, [struct_pair | pairs]}, context}
  end

  # __STACKTRACE__
  def of_expr({:__STACKTRACE__, _meta, var_context}, _expected, _stack, context)
      when is_atom(var_context) do
    file = {:tuple, 2, [{:atom, :file}, {:list, :integer}]}
    line = {:tuple, 2, [{:atom, :line}, :integer]}
    file_line = {:list, {:union, [file, line]}}
    type = {:list, {:tuple, 4, [:atom, :atom, :integer, file_line]}}
    {:ok, type, context}
  end

  # var
  def of_expr(var, _expected, _stack, context) when is_var(var) do
    {:ok, get_var!(var, context), context}
  end

  # {left, right}
  def of_expr({left, right}, expected, stack, context) do
    of_expr({:{}, [], [left, right]}, expected, stack, context)
  end

  # {...}
  def of_expr({:{}, _meta, exprs} = expr, _expected, stack, context) do
    stack = push_expr_stack(expr, stack)

    case map_reduce_ok(exprs, context, &of_expr(&1, :dynamic, stack, &2)) do
      {:ok, types, context} -> {:ok, {:tuple, length(types), types}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # left = right
  def of_expr({:=, _meta, [left_expr, right_expr]} = expr, _expected, stack, context) do
    # TODO: We might want to bring the expected type forward in case the type of this
    #       pattern is not useful. For example: 1 = _ = expr

    stack = push_expr_stack(expr, stack)

    with {:ok, left_type, context} <-
           Pattern.of_pattern(left_expr, stack, context),
         {:ok, right_type, context} <- of_expr(right_expr, left_type, stack, context),
         do: unify(right_type, left_type, stack, context)
  end

  # %{map | ...}
  def of_expr({:%{}, _, [{:|, _, [map, args]}]} = expr, _expected, stack, context) do
    stack = push_expr_stack(expr, stack)
    map_type = {:map, [{:optional, :dynamic, :dynamic}]}

    with {:ok, map_type, context} <- of_expr(map, map_type, stack, context),
         {:ok, {:map, arg_pairs}, context} <- Of.closed_map(args, stack, context, &of_expr/4),
         dynamic_value_pairs =
           Enum.map(arg_pairs, fn {:required, key, _value} -> {:required, key, :dynamic} end),
         args_type = {:map, dynamic_value_pairs ++ [{:optional, :dynamic, :dynamic}]},
         {:ok, type, context} <- unify(args_type, map_type, stack, context) do
      # Retrieve map type and overwrite with the new value types from the map update
      {:map, pairs} = resolve_var(type, context)

      updated_pairs =
        Enum.reduce(arg_pairs, pairs, fn {:required, key, value}, pairs ->
          List.keyreplace(pairs, key, 1, {:required, key, value})
        end)

      {:ok, {:map, updated_pairs}, context}
    end
  end

  # %Struct{map | ...}
  def of_expr(
        {:%, meta, [module, {:%{}, _, [{:|, _, [_, _]}]} = update]} = expr,
        _expected,
        stack,
        context
      ) do
    stack = push_expr_stack(expr, stack)
    map_type = {:map, [{:optional, :dynamic, :dynamic}]}

    with {:ok, struct, context} <- Of.struct(module, meta, context),
         {:ok, update, context} <- of_expr(update, map_type, stack, context) do
      unify(update, struct, stack, context)
    end
  end

  # %{...}
  def of_expr({:%{}, _meta, args} = expr, _expected, stack, context) do
    stack = push_expr_stack(expr, stack)
    Of.closed_map(args, stack, context, &of_expr/4)
  end

  # %Struct{...}
  def of_expr({:%, meta1, [module, {:%{}, _meta2, args}]} = expr, _expected, stack, context) do
    stack = push_expr_stack(expr, stack)

    with {:ok, struct, context} <- Of.struct(module, meta1, context),
         {:ok, map, context} <- Of.open_map(args, stack, context, &of_expr/4) do
      unify(map, struct, stack, context)
    end
  end

  # ()
  def of_expr({:__block__, _meta, []}, _expected, _stack, context) do
    {:ok, {:atom, nil}, context}
  end

  # (expr; expr)
  def of_expr({:__block__, _meta, exprs}, expected, stack, context) do
    expected_types = List.duplicate(:dynamic, length(exprs) - 1) ++ [expected]

    result =
      map_reduce_ok(Enum.zip(exprs, expected_types), context, fn {expr, expected}, context ->
        of_expr(expr, expected, stack, context)
      end)

    case result do
      {:ok, expr_types, context} -> {:ok, Enum.at(expr_types, -1), context}
      {:error, reason} -> {:error, reason}
    end
  end

  # case expr do pat -> expr end
  def of_expr({:case, _meta, [case_expr, [{:do, clauses}]]} = expr, _expected, stack, context) do
    stack = push_expr_stack(expr, stack)

    with {:ok, _expr_type, context} <- of_expr(case_expr, :dynamic, stack, context),
         {:ok, context} <- of_clauses(clauses, stack, context),
         do: {:ok, :dynamic, context}
  end

  # fn pat -> expr end
  def of_expr({:fn, _meta, clauses} = expr, _expected, stack, context) do
    stack = push_expr_stack(expr, stack)

    case of_clauses(clauses, stack, context) do
      {:ok, context} -> {:ok, :dynamic, context}
      {:error, reason} -> {:error, reason}
    end
  end

  @try_blocks [:do, :after]
  @try_clause_blocks [:catch, :else, :after]

  # try do expr end
  def of_expr({:try, _meta, [blocks]} = expr, _expected, stack, context) do
    stack = push_expr_stack(expr, stack)

    {result, context} =
      reduce_ok(blocks, context, fn
        {:rescue, clauses}, context ->
          reduce_ok(clauses, context, fn
            {:->, _, [[{:in, _, [var, _exceptions]}], body]}, context = acc ->
              {_type, context} = new_pattern_var(var, context)

              with {:ok, context} <- of_expr_context(body, :dynamic, stack, context) do
                {:ok, keep_warnings(acc, context)}
              end

            {:->, _, [[var], body]}, context = acc ->
              {_type, context} = new_pattern_var(var, context)

              with {:ok, context} <- of_expr_context(body, :dynamic, stack, context) do
                {:ok, keep_warnings(acc, context)}
              end
          end)

        {block, body}, context = acc when block in @try_blocks ->
          with {:ok, context} <- of_expr_context(body, :dynamic, stack, context) do
            {:ok, keep_warnings(acc, context)}
          end

        {block, clauses}, context when block in @try_clause_blocks ->
          of_clauses(clauses, stack, context)
      end)

    case result do
      :ok -> {:ok, :dynamic, context}
      :error -> {:error, context}
    end
  end

  # receive do pat -> expr end
  def of_expr({:receive, _meta, [blocks]} = expr, _expected, stack, context) do
    stack = push_expr_stack(expr, stack)

    {result, context} =
      reduce_ok(blocks, context, fn
        {:do, {:__block__, _, []}}, context ->
          {:ok, context}

        {:do, clauses}, context ->
          of_clauses(clauses, stack, context)

        {:after, [{:->, _meta, [head, body]}]}, context = acc ->
          with {:ok, _type, context} <- of_expr(head, :dynamic, stack, context),
               {:ok, _type, context} <- of_expr(body, :dynamic, stack, context),
               do: {:ok, keep_warnings(acc, context)}
      end)

    case result do
      :ok -> {:ok, :dynamic, context}
      :error -> {:error, context}
    end
  end

  # for pat <- expr do expr end
  def of_expr({:for, _meta, args} = expr, _expected, stack, context) do
    stack = push_expr_stack(expr, stack)
    {clauses, [[{:do, block} | opts]]} = Enum.split(args, -1)

    with {:ok, context} <- reduce_ok(clauses, context, &for_clause(&1, stack, &2)),
         {:ok, context} <- reduce_ok(opts, context, &for_option(&1, stack, &2)) do
      if Keyword.has_key?(opts, :reduce) do
        with {:ok, context} <- of_clauses(block, stack, context) do
          {:ok, :dynamic, context}
        end
      else
        with {:ok, _type, context} <- of_expr(block, :dynamic, stack, context) do
          {:ok, :dynamic, context}
        end
      end
    end
  end

  # with pat <- expr do expr end
  def of_expr({:with, _meta, clauses} = expr, _expected, stack, context) do
    stack = push_expr_stack(expr, stack)

    case reduce_ok(clauses, context, &with_clause(&1, stack, &2)) do
      {:ok, context} -> {:ok, :dynamic, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # fun.(args)
  def of_expr({{:., _meta1, [fun]}, _meta2, args} = expr, _expected, stack, context) do
    # TODO: Use expected type to infer intersection return type
    stack = push_expr_stack(expr, stack)

    with {:ok, _fun_type, context} <- of_expr(fun, :dynamic, stack, context),
         {:ok, _arg_types, context} <-
           map_reduce_ok(args, context, &of_expr(&1, :dynamic, stack, &2)) do
      {:ok, :dynamic, context}
    end
  end

  # expr.key_or_fun
  def of_expr({{:., _meta1, [expr1, key_or_fun]}, meta2, []} = expr2, _expected, stack, context)
      when not is_atom(expr1) do
    stack = push_expr_stack(expr2, stack)

    if Keyword.get(meta2, :no_parens, false) do
      with {:ok, expr_type, context} <- of_expr(expr1, :dynamic, stack, context),
           {value_var, context} = add_var(context),
           pair_type = {:required, {:atom, key_or_fun}, value_var},
           optional_type = {:optional, :dynamic, :dynamic},
           map_field_type = {:map, [pair_type, optional_type]},
           {:ok, _map_type, context} <- unify(map_field_type, expr_type, stack, context),
           do: {:ok, value_var, context}
    else
      # TODO: Use expected type to infer intersection return type
      with {:ok, expr_type, context} <- of_expr(expr1, :dynamic, stack, context),
           {:ok, _map_type, context} <- unify(expr_type, :atom, stack, context),
           do: {:ok, :dynamic, context}
    end
  end

  # expr.fun(arg)
  def of_expr({{:., meta1, [expr1, fun]}, _meta2, args} = expr2, _expected, stack, context) do
    # TODO: Use expected type to infer intersection return type

    context = Of.remote(expr1, fun, length(args), meta1, context)
    stack = push_expr_stack(expr2, stack)

    with {:ok, _expr_type, context} <- of_expr(expr1, :dynamic, stack, context),
         {:ok, _fun_type, context} <- of_expr(fun, :dynamic, stack, context),
         {:ok, _arg_types, context} <-
           map_reduce_ok(args, context, &of_expr(&1, :dynamic, stack, &2)) do
      {:ok, :dynamic, context}
    end
  end

  # &Foo.bar/1
  def of_expr(
        {:&, meta, [{:/, _, [{{:., _, [module, fun]}, _, []}, arity]}]},
        _expected,
        _stack,
        context
      )
      when is_atom(module) and is_atom(fun) do
    context = Of.remote(module, fun, arity, meta, context)
    {:ok, :dynamic, context}
  end

  # &foo/1
  # & &1
  def of_expr({:&, _meta, _arg}, _expected, _stack, context) do
    # TODO: Function type
    {:ok, :dynamic, context}
  end

  # fun(arg)
  def of_expr({fun, _meta, args} = expr, _expected, stack, context)
      when is_atom(fun) and is_list(args) do
    # TODO: Use expected type to infer intersection return type

    stack = push_expr_stack(expr, stack)

    case map_reduce_ok(args, context, &of_expr(&1, :dynamic, stack, &2)) do
      {:ok, _arg_types, context} -> {:ok, :dynamic, context}
      {:error, reason} -> {:error, reason}
    end
  end

  defp for_clause({:<-, _, [left, expr]}, stack, context) do
    {pattern, guards} = extract_head([left])

    with {:ok, _pattern_type, context} <- Pattern.of_head([pattern], guards, stack, context),
         {:ok, _expr_type, context} <- of_expr(expr, :dynamic, stack, context),
         do: {:ok, context}
  end

  defp for_clause({:<<>>, _, [{:<-, _, [pattern, expr]}]}, stack, context) do
    # TODO: the compiler guarantees pattern is a binary but we need to check expr is a binary
    with {:ok, _pattern_type, context} <- Pattern.of_pattern(pattern, stack, context),
         {:ok, _expr_type, context} <- of_expr(expr, :dynamic, stack, context),
         do: {:ok, context}
  end

  defp for_clause(list, stack, context) when is_list(list) do
    reduce_ok(list, context, &for_option(&1, stack, &2))
  end

  defp for_clause(expr, stack, context) do
    of_expr_context(expr, :dynamic, stack, context)
  end

  defp for_option({:into, expr}, stack, context) do
    of_expr_context(expr, :dynamic, stack, context)
  end

  defp for_option({:reduce, expr}, stack, context) do
    of_expr_context(expr, :dynamic, stack, context)
  end

  defp for_option({:uniq, _}, _stack, context) do
    {:ok, context}
  end

  defp with_clause({:<-, _, [left, expr]}, stack, context) do
    {pattern, guards} = extract_head([left])

    with {:ok, _pattern_type, context} <- Pattern.of_head([pattern], guards, stack, context),
         {:ok, _expr_type, context} <- of_expr(expr, :dynamic, stack, context),
         do: {:ok, context}
  end

  defp with_clause(list, stack, context) when is_list(list) do
    reduce_ok(list, context, &with_option(&1, stack, &2))
  end

  defp with_clause(expr, stack, context) do
    of_expr_context(expr, :dynamic, stack, context)
  end

  defp with_option({:do, body}, stack, context) do
    of_expr_context(body, :dynamic, stack, context)
  end

  defp with_option({:else, clauses}, stack, context) do
    of_clauses(clauses, stack, context)
  end

  defp of_clauses(clauses, stack, context) do
    reduce_ok(clauses, context, fn {:->, _meta, [head, body]}, context = acc ->
      {patterns, guards} = extract_head(head)

      with {:ok, _, context} <- Pattern.of_head(patterns, guards, stack, context),
           {:ok, _expr_type, context} <- of_expr(body, :dynamic, stack, context),
           do: {:ok, keep_warnings(acc, context)}
    end)
  end

  defp keep_warnings(context, %{warnings: warnings}) do
    %{context | warnings: warnings}
  end

  defp extract_head([{:when, _meta, args}]) do
    case Enum.split(args, -1) do
      {patterns, [guards]} -> {patterns, flatten_when(guards)}
      {patterns, []} -> {patterns, []}
    end
  end

  defp extract_head(other) do
    {other, []}
  end

  defp flatten_when({:when, _meta, [left, right]}) do
    [left | flatten_when(right)]
  end

  defp flatten_when(other) do
    [other]
  end

  defp of_expr_context(expr, expected, stack, context) do
    case of_expr(expr, expected, stack, context) do
      {:ok, _type, context} -> {:ok, context}
      {:error, reason} -> {:error, reason}
    end
  end

  defp new_pattern_var({:_, _meta, var_context}, context) when is_atom(var_context) do
    {:dynamic, context}
  end

  defp new_pattern_var(var, context) do
    new_var(var, context)
  end
end
