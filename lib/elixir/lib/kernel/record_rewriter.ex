# This is an optimization Elixir runs on function clauses.
# Whenever a variables matches against a record (a tagged
# tuple), this information is stored in order to optimize
# record calls.
defmodule Kernel.RecordRewriter do
  @moduledoc false

  def optimize_clause(clause) do
    optimize_clause(clause, :orddict.new)
  end

  ## Clause

  defp optimize_clause({ :clause, line, args, guards, body }, dict) do
    { args, dict } = optimize_args(args, dict)
    { body, dict, res } = optimize_body(body, dict, [])
    { { :clause, line, args, guards, body }, dict, res }
  end

  defp optimize_args(args, dict) do
    Enum.map_reduce args, dict, fn(arg, acc) ->
      { new_arg, new_acc, _res } = optimize_expr(arg, acc)
      { new_arg, new_acc }
    end
  end

  defp optimize_body([], dict, _acc) do
    { [], dict, nil }
  end

  defp optimize_body([h], dict, acc) do
    { new_expr, new_dict, new_res } = optimize_expr(h, dict)
    { Enum.reverse([new_expr|acc]), new_dict, new_res }
  end

  defp optimize_body([h|t], dict, acc) do
    { new_expr, new_dict, _ } = optimize_expr(h, dict)
    optimize_body(t, new_dict, [new_expr|acc])
  end

  ## Record helpers

  defp record_fields(record) do
    if Code.ensure_loaded?(record) && function_exported?(record, :__record__, 1) do
      try do
        fields      = lc { k, _ } inlist record.__record__(:fields), do: k
        optimizable = record.__record__(:optimizable)
        { fields, optimizable }
      rescue
        [UndefinedFunctionError, FunctionClauseError] -> nil
      end
    end
  end

  defp record_field_info(function) do
    case atom_to_list(function) do
      'update' -> nil
      'update_' ++ field -> nil
      _ -> { :accessor, function }
    end
  end

  defp optimize_call(line, { record, _ } = res, left, { :atom, _, function }, args) do
    case record_fields(record) do
      { fields, optimizable } ->
        opt_call =
          if List.member?(optimizable, { function, length(args) + 1 }) do
            case record_field_info(function) do
              { kind, field } ->
                if index = Enum.find_index(fields, field == &1) do
                  optimize_record_accessor_call(line, res, kind, field, index, left, args)
                end
              nil ->
                optimize_record_other_call(line, record, res, function, left, args)
            end
          end

        opt_call || optimize_record_other_call(line, record, nil, function, left, args)
      nil ->
        nil
    end
  end

  defp optimize_call(_line, _res, _left, _right, _args) do
    nil
  end

  defp optimize_record_accessor_call(line, _res, :accessor, _field, index, left, []) do
    call = { :call, line,
      { :remote, line, { :atom, 0, :erlang }, { :atom, 0, :element } },
      [{ :integer, 0, index + 2 }, left]
    }
    { call, nil }
  end

  defp optimize_record_accessor_call(line, res, :accessor, _field, index, left, [arg]) do
    call = { :call, line,
      { :remote, line, { :atom, 0, :erlang }, { :atom, 0, :setelement } },
      [{ :integer, 0, index + 2 }, left, arg]
    }
    { call, res }
  end

  defp optimize_record_other_call(line, record, res, function, left, args) do
    call = { :call, line,
      { :remote, line, { :atom, line, record }, { :atom, 0, function } },
      args ++ [left]
    }
    { call, res }
  end

  ## Expr

  defp optimize_expr({ :call, call_line, { :remote, line, left, right }, args }, dict) do
    { left, dict, res } = optimize_expr(left, dict)
    { right, dict, _ } = optimize_expr(right, dict)
    { args, dict } = optimize_args(args, dict)

    case optimize_call(call_line, res, left, right, args) do
      { call, call_res } ->
        { call, dict, call_res }
      nil ->
        { { :call, call_line, { :remote, line, left, right }, args }, dict, nil }
    end
  end

  defp optimize_expr({ :call, line, expr, args }, dict) do
    { expr, dict, _ } = optimize_expr(expr, dict)
    { args, dict } = optimize_args(args, dict)
    { { :call, line, expr, args }, dict, nil }
  end

  defp optimize_expr({ :match, line, left, right }, dict) do
    { left,  dict, left_res }  = optimize_expr(left, dict)
    { right, dict, right_res } = optimize_expr(right, dict)

    match = { :match, line, left, right }

    if left_res do
      dict = assign_vars(extract_vars(right, []), dict, left_res)
    end

    if right_res do
      dict = assign_vars(extract_vars(left, []), dict, right_res)
    end

    { match, dict, right_res || left_res }
  end

  defp optimize_expr({ :op, line, op, left, right }, dict) do
    { left, dict, _ }  = optimize_expr(left, dict)
    { right, dict, _ } = optimize_expr(right, dict)
    { { :op, line, op, left, right }, dict, nil }
  end

  defp optimize_expr({ :op, line, op, expr }, dict) do
    { expr, dict, _ } = optimize_expr(expr, dict)
    { { :op, line, op, expr }, dict, nil }
  end

  defp optimize_expr({ :bin, line, elements }, dict) do
    { elements, dict } = optimize_args(elements, dict)
    { { :bin, line, elements }, dict, nil }
  end

  defp optimize_expr({ :bin_element, line, expr, type1, type2 }, dict) do
    { expr, dict, _ } = optimize_expr(expr, dict)
    { { :bin_element, line, expr, type1, type2 }, dict, nil }
  end

  defp optimize_expr({ :cons, line, left, right }, dict) do
    { left, dict, _ }  = optimize_expr(left, dict)
    { right, dict, _ } = optimize_expr(right, dict)
    { { :cons, line, left, right }, dict, nil }
  end

  defp optimize_expr({ :block, line, args }, dict) do
    { args, dict, res } = optimize_body(args, dict, [])
    { { :block, line, args }, dict, res }
  end

  defp optimize_expr({ :tuple, line, args }, dict) do
    { args, dict, args_res } = optimize_tuple_args(args, dict)
    args_res = if Enum.any?(args_res), do: args_res, else: nil

    res =
      case args do
        [{ :atom, _, atom }|t] -> atom
        _ -> nil
      end

    { { :tuple, line, args }, dict, { res, args_res } }
  end

  defp optimize_expr({ :var, _, name } = var, dict) do
    case :orddict.find(name, dict) do
      { :ok, res } -> { var, dict, res }
      :error       -> { var, dict, nil }
    end
  end

  defp optimize_expr({ :case, line, expr, clauses }, dict) do
    { expr, dict, _ } = optimize_expr(expr, dict)
    tuples  = lc clause inlist clauses, do: optimize_clause(clause, dict)
    clauses = lc { clause, _, _ } inlist tuples, do: clause
    dict    = join_dict(tuples)
    res     = join_result(tuples)
    { { :case, line, expr, clauses }, dict, res }
  end

  defp optimize_expr({ :receive, line, clauses }, dict) do
    tuples  = lc clause inlist clauses, do: optimize_clause(clause, dict)
    clauses = lc { clause, _, _ } inlist tuples, do: clause
    dict    = join_dict(tuples)
    res     = join_result(tuples)
    { { :receive, line, clauses }, dict, res }
  end

  defp optimize_expr({ :receive, line, clauses, after_key, after_value }, dict) do
    tuples  = lc clause inlist clauses, do: optimize_clause(clause, dict)
    clauses = lc { clause, _, _ } inlist tuples, do: clause

    { after_key, dict, _ } = optimize_expr(after_key, dict)
    { after_value, dict, res } = optimize_body(after_value, dict, [])

    dict = join_dict(tuples, dict)
    res  = join_result(tuples, res)

    { { :receive, line, clauses, after_key, after_value }, dict, res }
  end

  defp optimize_expr({ :try, line, body, elses, catches, try_after }, dict) do
    tuples  = lc clause inlist catches, do: optimize_clause(clause, dict)
    catches = lc { clause, _, _ } inlist tuples, do: clause

    tuples  = lc clause inlist elses, do: optimize_clause(clause, dict)
    elses   = lc { clause, _, _ } inlist tuples, do: clause

    { body, _, res } = optimize_body(body, dict, [])
    res = join_result(tuples, res)

    { try_after, _, _ } = optimize_body(try_after, dict, [])
    { { :try, line, body, elses, catches, try_after }, dict, res }
  end

  defp optimize_expr({ :fun, line, { :function, module, name, arity } }, dict) do
    { module, dict, _ } = optimize_expr(module, dict)
    { name, dict, _ }   = optimize_expr(name, dict)
    { arity, dict, _ }  = optimize_expr(arity, dict)
    { { :fun, line, { :function, module, name, arity } }, dict, nil }
  end

  defp optimize_expr({ :fun, line, { :clauses, clauses } }, dict) do
    clauses = lc clause inlist clauses do
      { clause, _, _ } = optimize_clause(clause, dict)
      clause
    end

    { { :fun, line, { :clauses, clauses } }, dict, nil }
  end

  defp optimize_expr({ comprehension, line, expr, args }, dict) when comprehension in [:lc, :bc] do
    { args, new_dict } = optimize_args(args, dict)
    { expr, _, _ } = optimize_expr(expr, new_dict)
    { { comprehension, line, expr, args }, dict, nil }
  end

  defp optimize_expr({ generate, line, left, right }, dict) when generate in [:generate, :b_generate] do
    { left, dict, _ }  = optimize_expr(left, dict)
    { right, dict, _ } = optimize_expr(right, dict)
    { { generate, line, left, right }, dict, nil }
  end

  defp optimize_expr(other, dict) when elem(other, 0) in [:string, :atom, :integer, :float, :nil, :fun] do
    { other, dict, nil }
  end

  ## Helpers

  defp optimize_tuple_args(args, dict) do
    { final_args, { final_dict, final_acc } } =
      Enum.map_reduce args, { dict, [] }, fn(arg, { acc_dict, acc_res }) ->
        { new_arg, new_acc, res } = optimize_expr(arg, acc_dict)
        { new_arg, { new_acc, [res|acc_res] } }
      end

    { final_args, final_dict, Enum.reverse(final_acc) }
  end

  defp assign_vars([key|t], dict, { _, value } = res) when is_list(key) and is_list(value) and length(key) == length(value) do
    assign_vars t, assign_nested_vars(key, dict, value), res
  end

  defp assign_vars([key|t], dict, { value, _ } = res) when is_atom(key) and value != nil do
    dict =
      case :orddict.find(key, dict) do
        { :ok, ^res } ->
          dict
        { :ok, { ^value, _ } } ->
          :orddict.store(key, { value, nil }, dict)
        { :ok, _ } ->
          # We are overriding a type of an existing variable,
          # which means the source code is invalid.
          :orddict.store(key, nil, dict)
        :error ->
          :orddict.store(key, res, dict)
      end

    assign_vars t, dict, res
  end

  defp assign_vars([_|t], dict, res) do
    assign_vars t, dict, res
  end

  defp assign_vars([], dict, _res) do
    dict
  end

  defp assign_nested_vars([vars|vt], dict, [res|rt]) do
    assign_nested_vars(vt, assign_vars(vars, dict, res), rt)
  end

  defp assign_nested_vars([], dict, []) do
    dict
  end

  defp extract_vars({ :match, _, left, right }, vars) do
    vars = extract_vars(right, vars)
    extract_vars(left, vars)
  end

  defp extract_vars({ :var, _, name }, vars) do
    [name|vars]
  end

  defp extract_vars({ :tuple, _, args }, vars) do
    [Enum.map(args, extract_vars(&1, []))|vars]
  end

  defp extract_vars(_, vars) do
    vars
  end

  defp join_dict([{ _, dict, _ }|t]) do
    join_dict(t, dict)
  end

  defp join_dict([{ _, dict, _ }|t], other) do
    other = Enum.reduce other, other, fn
      { key, { value, _ } = res }, acc ->
        case :orddict.find(key, dict) do
          { :ok, ^res } -> acc
          { :ok, { ^value, _ } } -> :orddict.store(key, { value, nil }, acc)
          { :ok, _ } -> :orddict.store(key, nil, acc)
          :error -> :orddict.erase(key, acc)
        end
    end

    join_dict(t, other)
  end

  defp join_dict([], other) do
    other
  end

  defp join_result([{ _, _, res }|t]) do
    join_result(t, res)
  end

  defp join_result([{ _, _, res }|t], res) do
    join_result(t, res)
  end

  defp join_result([{ _, _, { res, _ } }|t], { res, _ }) do
    join_result(t, { res, nil })
  end

  defp join_result([{ _, _, _ }|_], _res) do
    nil
  end

  defp join_result([], res) do
    res
  end
end