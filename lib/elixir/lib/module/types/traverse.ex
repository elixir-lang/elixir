# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

defmodule Module.Types.Traverse do
  @moduledoc false

  # Traverses expressions to find local calls when inference is disabled.

  # Literals
  def of_expr(literal, _stack, context)
      when is_atom(literal) or is_integer(literal) or is_float(literal) or is_binary(literal) or
             is_pid(literal) or literal == [] do
    context
  end

  # [expr, ...]
  def of_expr(list, stack, context) when is_list(list) do
    Enum.reduce(list, context, &of_expr(&1, stack, &2))
  end

  # {left, right}
  def of_expr({left, right}, stack, context) do
    context = of_expr(left, stack, context)
    of_expr(right, stack, context)
  end

  # <<...>>
  def of_expr({:<<>>, _meta, args}, stack, context) do
    Enum.reduce(args, context, fn
      {:"::", _meta, [left, _right]}, context ->
        of_expr(left, stack, context)

      expr, context ->
        of_expr(expr, stack, context)
    end)
  end

  def of_expr({:%, meta, [module, {:%{}, _, [{:|, _, [map, pairs]}]}]}, stack, context) do
    context = of_expr(map, stack, context)
    context = of_expr(pairs, stack, context)
    of_struct(module, pairs, :expr, meta, stack, context)
  end

  def of_expr({:%, meta, [module, {:%{}, _, pairs}]}, stack, context) do
    context = of_expr(pairs, stack, context)
    of_struct(module, pairs, :expr, meta, stack, context)
  end

  # Map update, tail operator
  def of_expr({:|, _meta, [left, right]}, stack, context) do
    context = of_expr(left, stack, context)
    of_expr(right, stack, context)
  end

  # Tuples, maps
  def of_expr({container, _meta, exprs}, stack, context) when container in [:{}, :%{}] do
    Enum.reduce(exprs, context, &of_expr(&1, stack, &2))
  end

  # left = right, left <- right
  def of_expr({op, _meta, [left, right]}, stack, context) when op in [:=, :<-] do
    context = of_pattern(left, stack, context)
    of_expr(right, stack, context)
  end

  # Blocks
  def of_expr({:__block__, _, args}, stack, context) do
    Enum.reduce(args, context, &of_expr(&1, stack, &2))
  end

  # cond do ... end
  def of_expr({:cond, _meta, [[{:do, clauses}]]}, stack, context) do
    Enum.reduce(clauses, context, fn {:->, _meta, [[head], body]}, context ->
      context = of_expr(head, stack, context)
      of_expr(body, stack, context)
    end)
  end

  # All non-handled -> are patterns
  def of_expr({:->, _, [head, body]}, stack, context) do
    context = of_pattern(head, stack, context)
    of_expr(body, stack, context)
  end

  # case expr do ... end
  def of_expr({:case, _meta, [case_expr, [{:do, clauses}]]}, stack, context) do
    context = of_expr(case_expr, stack, context)
    of_expr(clauses, stack, context)
  end

  # fn pat -> expr end
  def of_expr({:fn, _meta, clauses}, stack, context) do
    of_expr(clauses, stack, context)
  end

  # try do ... end
  def of_expr({:try, _meta, [blocks]}, stack, context) do
    Enum.reduce(blocks, context, fn {_, clauses_or_body}, context ->
      of_expr(clauses_or_body, stack, context)
    end)
  end

  # receive do ... end
  def of_expr({:receive, _meta, [blocks]}, stack, context) do
    Enum.reduce(blocks, context, fn
      {:do, clauses_or_empty_body}, context ->
        of_expr(clauses_or_empty_body, stack, context)

      {:after, [{:->, _meta, [[timeout], body]}]}, context ->
        context = of_expr(timeout, stack, context)
        of_expr(body, stack, context)
    end)
  end

  # for, with
  def of_expr({op, _meta, [_ | _] = args}, stack, context) when op in [:for, :with] do
    Enum.reduce(args, context, &of_expr(&1, stack, &2))
  end

  # fun.(args)
  def of_expr({{:., _meta, [fun]}, _call_meta, args}, stack, context) do
    context = of_expr(fun, stack, context)
    Enum.reduce(args, context, &of_expr(&1, stack, &2))
  end

  # remote.fun(args)
  def of_expr({{:., _, [remote, name]}, _meta, args}, stack, context)
      when is_atom(name) do
    context = of_expr(remote, stack, context)
    Enum.reduce(args, context, &of_expr(&1, stack, &2))
  end

  # &Mod.fun/arity
  def of_expr({:&, _, [{:/, _, [{{:., _, [_remote, name]}, _, []}, arity]}]}, _stack, context)
      when is_atom(name) and is_integer(arity) do
    context
  end

  # &fun/arity
  def of_expr({:&, meta, [{:/, _, [{name, _, _ctx}, arity]}]}, stack, context)
      when is_atom(name) and is_integer(arity) do
    local_fun(meta, name, arity, stack, context)
  end

  # super(args)
  def of_expr({:super, meta, args}, stack, context) when is_list(args) do
    {_kind, name} = Keyword.fetch!(meta, :super)
    context = local_fun(meta, name, length(args), stack, context)
    Enum.reduce(args, context, &of_expr(&1, stack, &2))
  end

  # local_fun(args)
  def of_expr({name, meta, args}, stack, context)
      when is_atom(name) and is_list(args) do
    context = local_fun(meta, name, length(args), stack, context)
    Enum.reduce(args, context, &of_expr(&1, stack, &2))
  end

  # var
  def of_expr({name, _meta, ctx}, _stack, context)
      when is_atom(name) and is_atom(ctx) do
    context
  end

  defp of_pattern({:%, meta, [module, {:%{}, _, pairs}]}, stack, context) when is_atom(module) do
    context = of_pattern(pairs, stack, context)
    of_struct(module, pairs, :expr, meta, stack, context)
  end

  defp of_pattern({left, _meta, right}, stack, context) do
    context = of_pattern(left, stack, context)
    of_pattern(right, stack, context)
  end

  defp of_pattern({left, right}, stack, context) do
    context = of_pattern(left, stack, context)
    of_pattern(right, stack, context)
  end

  defp of_pattern([_ | _] = list, stack, context) do
    Enum.reduce(list, context, &of_pattern(&1, stack, &2))
  end

  defp of_pattern(_, _stack, context) do
    context
  end

  defp of_struct(module, pairs, kind, meta, stack, context) do
    {info, context} = Module.Types.Of.struct_info(module, kind, meta, stack, context)

    if info do
      Enum.reduce(pairs, context, fn {key, _value}, context ->
        if Enum.any?(info, &(&1.field == key)) do
          context
        else
          Module.Types.Of.unknown_struct_field(module, key, kind, meta, stack, context)
        end
      end)
    else
      context
    end
  end

  defp local_fun(meta, fun, arity, stack, context) do
    case stack.local_handler.(meta, {fun, arity}, stack, context) do
      false -> context
      {_kind, _info, context} -> context
    end
  end
end
