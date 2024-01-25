defmodule Module.Types.Expr do
  @moduledoc false

  alias Module.Types.{Of, Pattern}
  import Module.Types.{Helpers, Descr}

  defp of_expr(ast, _expected_expr, stack, context) do
    of_expr(ast, stack, context)
  end

  # :atom
  def of_expr(atom, _stack, context) when is_atom(atom) do
    {:ok, atom([atom]), context}
  end

  # 12
  def of_expr(literal, _stack, context) when is_integer(literal) do
    {:ok, integer(), context}
  end

  # 1.2
  def of_expr(literal, _stack, context) when is_float(literal) do
    {:ok, float(), context}
  end

  # "..."
  def of_expr(literal, _stack, context) when is_binary(literal) do
    {:ok, binary(), context}
  end

  # #PID<...>
  def of_expr(literal, _stack, context) when is_pid(literal) do
    {:ok, pid(), context}
  end

  # []
  def of_expr([], _stack, context) do
    {:ok, empty_list(), context}
  end

  # TODO: [expr, ...]
  def of_expr(exprs, stack, context) when is_list(exprs) do
    case map_reduce_ok(exprs, context, &of_expr(&1, stack, &2)) do
      {:ok, _types, context} -> {:ok, non_empty_list(), context}
      {:error, context} -> {:error, context}
    end
  end

  # {left, right}
  def of_expr({left, right}, stack, context) do
    of_expr({:{}, [], [left, right]}, stack, context)
  end

  # <<...>>>
  def of_expr({:<<>>, _meta, args}, stack, context) do
    case Of.binary(args, :expr, stack, context, &of_expr/4) do
      {:ok, context} -> {:ok, binary(), context}
      # It is safe to discard errors from binary inside expressions
      {:error, context} -> {:ok, binary(), context}
    end
  end

  # TODO: left | []
  def of_expr({:|, _meta, [left_expr, []]}, stack, context) do
    of_expr(left_expr, stack, context)
  end

  # TODO: left | right
  def of_expr({:|, _meta, [left_expr, right_expr]}, stack, context) do
    case of_expr(left_expr, stack, context) do
      {:ok, _left, context} ->
        of_expr(right_expr, stack, context)

      {:error, context} ->
        {:error, context}
    end
  end

  # TODO: __CALLER__
  def of_expr({:__CALLER__, _meta, var_context}, _stack, context)
      when is_atom(var_context) do
    {:ok, dynamic(), context}
  end

  # TODO: __STACKTRACE__
  def of_expr({:__STACKTRACE__, _meta, var_context}, _stack, context)
      when is_atom(var_context) do
    {:ok, dynamic(), context}
  end

  # TODO: {...}
  def of_expr({:{}, _meta, exprs}, stack, context) do
    case map_reduce_ok(exprs, context, &of_expr(&1, stack, &2)) do
      {:ok, _types, context} -> {:ok, tuple(), context}
      {:error, context} -> {:error, context}
    end
  end

  # TODO: left = right
  def of_expr({:=, _meta, [left_expr, right_expr]}, stack, context) do
    with {:ok, _left_type, context} <-
           Pattern.of_pattern(left_expr, stack, context),
         {:ok, right_type, context} <- of_expr(right_expr, stack, context) do
      {:ok, right_type, context}
    end
  end

  # TODO: %{map | ...}
  def of_expr({:%{}, _, [{:|, _, [map, args]}]}, stack, context) do
    with {:ok, _, context} <- of_expr(map, stack, context),
         {:ok, _, context} <- Of.closed_map(args, stack, context, &of_expr/3) do
      {:ok, map(), context}
    end
  end

  # TODO: %Struct{map | ...}
  def of_expr(
        {:%, meta, [module, {:%{}, _, [{:|, _, [_, _]}]} = update]},
        stack,
        context
      ) do
    with {:ok, _, context} <- Of.struct(module, meta, stack, context),
         {:ok, _, context} <- of_expr(update, stack, context) do
      {:ok, map(), context}
    end
  end

  # TODO: %{...}
  def of_expr({:%{}, _meta, args}, stack, context) do
    Of.closed_map(args, stack, context, &of_expr/3)
  end

  # TODO: %Struct{...}
  def of_expr({:%, meta1, [module, {:%{}, _meta2, args}]}, stack, context) do
    with {:ok, _, context} <- Of.struct(module, meta1, stack, context),
         {:ok, _, context} <- Of.open_map(args, stack, context, &of_expr/3) do
      {:ok, map(), context}
    end
  end

  # ()
  def of_expr({:__block__, _meta, []}, _stack, context) do
    {:ok, atom([nil]), context}
  end

  # (expr; expr)
  def of_expr({:__block__, _meta, exprs}, stack, context) do
    {pre, [post]} = Enum.split(exprs, -1)

    result =
      map_reduce_ok(pre, context, fn expr, context ->
        of_expr(expr, stack, context)
      end)

    case result do
      {:ok, _, context} -> of_expr(post, stack, context)
      {:error, context} -> {:error, context}
    end
  end

  # TODO: cond do pat -> expr end
  def of_expr({:cond, _meta, [[{:do, clauses}]]}, stack, context) do
    {result, context} =
      reduce_ok(clauses, context, fn {:->, _meta, [head, body]}, context ->
        with {:ok, _, context} <- of_expr(head, stack, context),
             {:ok, _, context} <- of_expr(body, stack, context),
             do: {:ok, context}
      end)

    case result do
      :ok -> {:ok, dynamic(), context}
      :error -> {:error, context}
    end
  end

  # TODO: case expr do pat -> expr end
  def of_expr({:case, _meta, [case_expr, [{:do, clauses}]]}, stack, context) do
    with {:ok, _expr_type, context} <- of_expr(case_expr, stack, context),
         {:ok, context} <- of_clauses(clauses, stack, context),
         do: {:ok, dynamic(), context}
  end

  # TODO: fn pat -> expr end
  def of_expr({:fn, _meta, clauses}, stack, context) do
    case of_clauses(clauses, stack, context) do
      {:ok, context} -> {:ok, dynamic(), context}
      {:error, context} -> {:error, context}
    end
  end

  @try_blocks [:do, :after]
  @try_clause_blocks [:catch, :else, :after]

  # TODO: try do expr end
  def of_expr({:try, _meta, [blocks]}, stack, context) do
    {result, context} =
      reduce_ok(blocks, context, fn
        {:rescue, clauses}, context ->
          reduce_ok(clauses, context, fn
            {:->, _, [[{:in, _, [var, _exceptions]} = expr], body]}, context ->
              # TODO: Vars are a union of the structs above
              {:ok, _type, context} = Pattern.of_pattern(var, {dynamic(), expr}, stack, context)
              of_expr_context(body, stack, context)

            {:->, _, [[var], body]}, context ->
              # TODO: Vars are structs with the exception field and that's it
              {:ok, _type, context} = Pattern.of_pattern(var, stack, context)
              of_expr_context(body, stack, context)
          end)

        {block, body}, context when block in @try_blocks ->
          of_expr_context(body, stack, context)

        {block, clauses}, context when block in @try_clause_blocks ->
          of_clauses(clauses, stack, context)
      end)

    case result do
      :ok -> {:ok, dynamic(), context}
      :error -> {:error, context}
    end
  end

  # TODO: receive do pat -> expr end
  def of_expr({:receive, _meta, [blocks]}, stack, context) do
    {result, context} =
      reduce_ok(blocks, context, fn
        {:do, {:__block__, _, []}}, context ->
          {:ok, context}

        {:do, clauses}, context ->
          of_clauses(clauses, stack, context)

        {:after, [{:->, _meta, [head, body]}]}, context ->
          with {:ok, _type, context} <- of_expr(head, stack, context),
               {:ok, _type, context} <- of_expr(body, stack, context),
               do: {:ok, context}
      end)

    case result do
      :ok -> {:ok, dynamic(), context}
      :error -> {:error, context}
    end
  end

  # TODO: for pat <- expr do expr end
  def of_expr({:for, _meta, [_ | _] = args}, stack, context) do
    {clauses, [[{:do, block} | opts]]} = Enum.split(args, -1)

    with {:ok, context} <- reduce_ok(clauses, context, &for_clause(&1, stack, &2)),
         {:ok, context} <- reduce_ok(opts, context, &for_option(&1, stack, &2)) do
      if Keyword.has_key?(opts, :reduce) do
        with {:ok, context} <- of_clauses(block, stack, context) do
          {:ok, dynamic(), context}
        end
      else
        with {:ok, _type, context} <- of_expr(block, stack, context) do
          {:ok, dynamic(), context}
        end
      end
    end
  end

  # TODO: with pat <- expr do expr end
  def of_expr({:with, _meta, [_ | _] = clauses}, stack, context) do
    case reduce_ok(clauses, context, &with_clause(&1, stack, &2)) do
      {:ok, context} -> {:ok, dynamic(), context}
      {:error, context} -> {:error, context}
    end
  end

  # TODO: fun.(args)
  def of_expr({{:., _meta1, [fun]}, _meta2, args}, stack, context) do
    with {:ok, _fun_type, context} <- of_expr(fun, stack, context),
         {:ok, _arg_types, context} <-
           map_reduce_ok(args, context, &of_expr(&1, stack, &2)) do
      {:ok, dynamic(), context}
    end
  end

  # TODO: expr.key_or_fun
  def of_expr({{:., _meta1, [expr1, _key_or_fun]}, meta2, []}, stack, context)
      when not is_atom(expr1) do
    if Keyword.get(meta2, :no_parens, false) do
      with {:ok, _, context} <- of_expr(expr1, stack, context) do
        {:ok, dynamic(), context}
      end
    else
      with {:ok, _, context} <- of_expr(expr1, stack, context) do
        {:ok, dynamic(), context}
      end
    end
  end

  # TODO: expr.fun(arg)
  def of_expr({{:., _meta1, [expr1, fun]}, meta2, args}, stack, context) do
    context = Of.remote(expr1, fun, length(args), meta2, stack, context)

    with {:ok, _expr_type, context} <- of_expr(expr1, stack, context),
         {:ok, _arg_types, context} <-
           map_reduce_ok(args, context, &of_expr(&1, stack, &2)) do
      {:ok, dynamic(), context}
    end
  end

  # TODO: &Foo.bar/1
  def of_expr(
        {:&, _, [{:/, _, [{{:., _, [module, fun]}, meta, []}, arity]}]},
        stack,
        context
      )
      when is_atom(module) and is_atom(fun) do
    context = Of.remote(module, fun, arity, meta, stack, context)
    {:ok, dynamic(), context}
  end

  # &foo/1
  # TODO: & &1
  def of_expr({:&, _meta, _arg}, _stack, context) do
    {:ok, dynamic(), context}
  end

  # TODO: fun(arg)
  def of_expr({fun, _meta, args}, stack, context)
      when is_atom(fun) and is_list(args) do
    with {:ok, _arg_types, context} <-
           map_reduce_ok(args, context, &of_expr(&1, stack, &2)) do
      {:ok, dynamic(), context}
    end
  end

  # var
  def of_expr(var, _stack, context) when is_var(var) do
    {:ok, Pattern.of_var(var, context), context}
  end

  defp for_clause({:<-, _, [left, expr]}, stack, context) do
    {pattern, guards} = extract_head([left])

    with {:ok, _pattern_type, context} <- Pattern.of_head([pattern], guards, stack, context),
         {:ok, _expr_type, context} <- of_expr(expr, stack, context),
         do: {:ok, context}
  end

  defp for_clause({:<<>>, _, [{:<-, _, [pattern, expr]}]}, stack, context) do
    # TODO: the compiler guarantees pattern is a binary but we need to check expr is a binary
    with {:ok, _pattern_type, context} <-
           Pattern.of_pattern(pattern, stack, context),
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

  defp with_clause({:<-, _, [left, expr]}, stack, context) do
    {pattern, guards} = extract_head([left])

    with {:ok, _pattern_type, context} <- Pattern.of_head([pattern], guards, stack, context),
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
    of_clauses(clauses, stack, context)
  end

  defp of_clauses(clauses, stack, context) do
    reduce_ok(clauses, context, fn {:->, _meta, [head, body]}, context ->
      {patterns, guards} = extract_head(head)

      with {:ok, _, context} <- Pattern.of_head(patterns, guards, stack, context),
           {:ok, _, context} <- of_expr(body, stack, context),
           do: {:ok, context}
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
      {:error, context} -> {:error, context}
    end
  end
end
