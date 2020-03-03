defmodule Module.Types.Expr do
  @moduledoc false

  import Module.Types.{Helpers, Infer}
  import Module.Types.Pattern, only: [of_guard: 3, of_pattern: 3]

  def of_expr(expr, %{context: stack_context} = stack, context) when stack_context != :expr do
    of_expr(expr, %{stack | context: :expr}, context)
  end

  # :atom
  def of_expr(atom, _stack, context) when is_atom(atom) do
    {:ok, {:atom, atom}, context}
  end

  # 12
  def of_expr(literal, _stack, context) when is_integer(literal) do
    {:ok, :integer, context}
  end

  # 1.2
  def of_expr(literal, _stack, context) when is_float(literal) do
    {:ok, :float, context}
  end

  # "..."
  def of_expr(literal, _stack, context) when is_binary(literal) do
    {:ok, :binary, context}
  end

  # #PID<...>
  def of_expr(literal, _stack, context) when is_pid(literal) do
    {:ok, :dynamic, context}
  end

  # <<...>>>
  def of_expr({:<<>>, _meta, args} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    result =
      reduce_ok(args, context, fn expr, context ->
        of_binary(expr, stack, context, &of_expr/3)
      end)

    case result do
      {:ok, context} -> {:ok, :binary, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # left | []
  def of_expr({:|, _meta, [left_expr, []]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)
    of_expr(left_expr, stack, context)
  end

  # left | right
  def of_expr({:|, _meta, [left_expr, right_expr]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    case of_expr(left_expr, stack, context) do
      {:ok, left, context} ->
        case of_expr(right_expr, stack, context) do
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
  def of_expr([], _stack, context) do
    {:ok, {:list, :dynamic}, context}
  end

  # [expr, ...]
  def of_expr(exprs, stack, context) when is_list(exprs) do
    stack = push_expr_stack(exprs, stack)

    case map_reduce_ok(exprs, context, &of_expr(&1, stack, &2)) do
      {:ok, types, context} -> {:ok, {:list, to_union(types, context)}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # __CALLER__
  def of_expr({:__CALLER__, _meta, var_context}, _stack, context) when is_atom(var_context) do
    # TODO: Full %Macro.Env{} struct
    {:ok, {:map, [{{:atom, :__struct__}, {:atom, Macro.Env}}]}, context}
  end

  # __STACKTRACE__
  def of_expr({:__STACKTRACE__, _meta, var_context}, _stack, context) when is_atom(var_context) do
    file = {:tuple, [{:atom, :file}, {:list, :integer}]}
    line = {:tuple, [{:atom, :line}, :integer]}
    file_line = {:list, {:union, [file, line]}}
    type = {:list, {:tuple, [:atom, :atom, :integer, file_line]}}
    {:ok, type, context}
  end

  # var
  def of_expr(var, _stack, context) when is_var(var) do
    {type, context} = new_var(var, context)
    {:ok, type, context}
  end

  # {left, right}
  def of_expr({left, right}, stack, context) do
    of_expr({:{}, [], [left, right]}, stack, context)
  end

  # {...}
  def of_expr({:{}, _meta, exprs} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    case map_reduce_ok(exprs, context, &of_expr(&1, stack, &2)) do
      {:ok, types, context} -> {:ok, {:tuple, types}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # left = right
  def of_expr({:=, _meta, [left_expr, right_expr]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    with {:ok, left_type, context} <-
           of_pattern(left_expr, stack, context),
         {:ok, right_type, context} <- of_expr(right_expr, stack, context),
         do: unify(right_type, left_type, stack, context)
  end

  # %{map | ...}
  def of_expr({:%{}, _, [{:|, _, [_map, args]}]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    case of_pairs(args, stack, context) do
      {:ok, _pairs, context} -> {:ok, {:map, []}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # %Struct{map | ...}
  def of_expr({:%, _, [module, {:%{}, _, [{:|, _, [_map, args]}]}]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    case of_pairs(args, stack, context) do
      {:ok, _pairs, context} ->
        pairs = [{{:atom, :__struct__}, {:atom, module}}]
        {:ok, {:map, pairs}, context}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # %{...}
  def of_expr({:%{}, _meta, args} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    case of_pairs(args, stack, context) do
      {:ok, pairs, context} -> {:ok, {:map, pairs_to_unions(pairs, context)}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # %Struct{...}
  def of_expr({:%, _meta1, [module, {:%{}, _meta2, args}]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    case of_pairs(args, stack, context) do
      {:ok, pairs, context} ->
        pairs = [{{:atom, :__struct__}, {:atom, module}} | pairs]
        {:ok, {:map, pairs}, context}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # ()
  def of_expr({:__block__, _meta, []}, _stack, context) do
    {:ok, {:atom, nil}, context}
  end

  # (expr; expr)
  def of_expr({:__block__, _meta, exprs}, stack, context) do
    case map_reduce_ok(exprs, context, &of_expr(&1, stack, &2)) do
      {:ok, expr_types, context} -> {:ok, Enum.at(expr_types, -1), context}
      {:error, reason} -> {:error, reason}
    end
  end

  # case expr do pat -> expr end
  def of_expr({:case, _meta, [case_expr, [{:do, clauses}]]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    with {:ok, _expr_type, context} <- of_expr(case_expr, stack, context),
         :ok <- of_clauses(clauses, stack, context),
         do: {:ok, :dynamic, context}
  end

  # fn pat -> expr end
  def of_expr({:fn, _meta, clauses} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    case of_clauses(clauses, stack, context) do
      :ok -> {:ok, :dynamic, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # &Foo.bar/1
  # &foo/1
  # & &1
  def of_expr({:&, _meta, _arg}, _stack, context) do
    # TODO: Function type
    {:ok, :dynamic, context}
  end

  @try_blocks [:do, :after]
  @try_clause_blocks [:catch, :else, :after]

  # try do expr end
  def of_expr({:try, _meta, [blocks]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    result =
      each_ok(blocks, fn
        {:rescue, clauses} ->
          each_ok(clauses, fn
            {:->, _, [[{:in, _, [var, _exceptions]}], body]} ->
              {_type, context} = new_pattern_var(var, context)
              of_expr_ok(body, stack, context)

            {:->, _, [[var], body]} ->
              {_type, context} = new_pattern_var(var, context)
              of_expr_ok(body, stack, context)
          end)

        {block, body} when block in @try_blocks ->
          of_expr_ok(body, stack, context)

        {block, clauses} when block in @try_clause_blocks ->
          of_clauses(clauses, stack, context)
      end)

    case result do
      :ok -> {:ok, :dynamic, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # receive do pat -> expr end
  def of_expr({:receive, _meta, [blocks]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    result =
      each_ok(blocks, fn
        {:do, clauses} ->
          of_clauses(clauses, stack, context)

        {:after, [{:->, _meta, [head, body]}]} ->
          with {:ok, _type, context} <- of_expr(head, stack, context),
               {:ok, _type, _context} <- of_expr(body, stack, context),
               do: :ok
      end)

    case result do
      :ok -> {:ok, :dynamic, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # for pat <- expr do expr end
  def of_expr({:for, _meta, args} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    case reduce_ok(args, context, &for_clause(&1, stack, &2)) do
      {:ok, _context} -> {:ok, :dynamic, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # for pat <- expr do expr end
  def of_expr({:with, _meta, clauses} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    case reduce_ok(clauses, context, &with_clause(&1, stack, &2)) do
      {:ok, _context} -> {:ok, :dynamic, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # fun.(arg)
  def of_expr({{:., _meta1, [_fun]}, _meta2, args} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    case map_reduce_ok(args, context, &of_expr(&1, stack, &2)) do
      {:ok, _arg_types, context} -> {:ok, :dynamic, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # Module.fun(arg)
  def of_expr({{:., _meta1, [_module, _fun]}, _meta2, args} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    case map_reduce_ok(args, context, &of_expr(&1, stack, &2)) do
      {:ok, _arg_types, context} -> {:ok, :dynamic, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # fun(arg)
  def of_expr({fun, _meta, args} = expr, stack, context) when is_atom(fun) and is_list(args) do
    stack = push_expr_stack(expr, stack)

    case map_reduce_ok(args, context, &of_expr(&1, stack, &2)) do
      {:ok, _arg_types, context} -> {:ok, :dynamic, context}
      {:error, reason} -> {:error, reason}
    end
  end

  defp of_pairs(pairs, stack, context) do
    map_reduce_ok(pairs, context, fn {key, value}, context ->
      with {:ok, key_type, context} <- of_expr(key, stack, context),
           {:ok, value_type, context} <- of_expr(value, stack, context),
           do: {:ok, {key_type, value_type}, context}
    end)
  end

  defp pairs_to_unions(pairs, context) do
    # We are currently creating overlapping key types

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

  defp for_clause({:<-, _, [left, expr]}, stack, context) do
    {pattern, guards} = extract_head([left])

    with {:ok, _pattern_type, context} <- of_pattern(pattern, stack, context),
         # TODO: Check that of_guard/3 returns a boolean
         {:ok, _guard_type, context} <- of_guard(guards_to_or(guards), stack, context),
         {:ok, _expr_type, context} <- of_expr(expr, stack, context),
         do: {:ok, context}
  end

  defp for_clause(list, stack, context) when is_list(list) do
    reduce_ok(list, context, &for_option(&1, stack, &2))
  end

  defp for_clause(expr, stack, context) do
    of_expr_context(expr, stack, context)
  end

  defp for_option({:into, expr}, stack, context) do
    of_expr_context(expr, stack, context)
  end

  defp for_option({:reduce, expr}, stack, context) do
    of_expr_context(expr, stack, context)
  end

  defp for_option({:uniq, _}, _stack, context) do
    {:ok, context}
  end

  defp for_option({:do, [{:->, _, [pattern, body]}]}, stack, context) do
    case of_pattern(pattern, stack, context) do
      {:ok, _pattern_type, context} -> of_expr_context(body, stack, context)
      {:error, reason} -> {:error, reason}
    end
  end

  defp for_option({:do, body}, stack, context) do
    of_expr_context(body, stack, context)
  end

  defp with_clause({:<-, _, [left, expr]}, stack, context) do
    {pattern, guards} = extract_head([left])

    with {:ok, _pattern_type, context} <- of_pattern(pattern, stack, context),
         # TODO: Check that of_guard/3 returns a boolean
         {:ok, _guard_type, context} <- of_guard(guards_to_or(guards), stack, context),
         {:ok, _expr_type, context} <- of_expr(expr, stack, context),
         do: {:ok, context}
  end

  defp with_clause(list, stack, context) when is_list(list) do
    reduce_ok(list, context, &with_option(&1, stack, &2))
  end

  defp with_clause(expr, stack, context) do
    of_expr_context(expr, stack, context)
  end

  defp with_option({:do, body}, stack, context) do
    of_expr_context(body, stack, context)
  end

  defp with_option({:else, clauses}, stack, context) do
    case of_clauses(clauses, stack, context) do
      :ok -> {:ok, context}
      {:error, reason} -> {:error, reason}
    end
  end

  defp of_clauses(clauses, stack, context) do
    each_ok(clauses, fn {:->, _meta, [head, body]} ->
      {patterns, guards} = extract_head(head)

      with {:ok, _pattern_types, context} <-
             map_reduce_ok(patterns, context, &of_pattern(&1, stack, &2)),
           # TODO: Check that of_guard/3 returns a boolean
           {:ok, _guard_type, context} <- of_guard(guards_to_or(guards), stack, context),
           {:ok, _expr_type, _context} <- of_expr(body, stack, context),
           do: :ok
    end)
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

  defp of_expr_context(expr, stack, context) do
    case of_expr(expr, stack, context) do
      {:ok, _type, context} -> {:ok, context}
      {:error, reason} -> {:error, reason}
    end
  end

  defp of_expr_ok(expr, stack, context) do
    case of_expr(expr, stack, context) do
      {:ok, _type, _context} -> :ok
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
