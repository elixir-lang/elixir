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

  ## Expr

  defp optimize_expr({ :call, call_line, { :remote, line, left, right }, args }, dict) do
    { left, dict, _ }  = optimize_expr(left, dict)
    { right, dict, _ } = optimize_expr(right, dict)
    { args, dict } = optimize_args(args, dict)
    { { :call, call_line, { :remote, line, left, right }, args }, dict, nil }
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

    res =
      case args do
        [{ :atom, _, atom }|t] -> atom
        _ -> nil
      end

    { { :tuple, line, args }, dict, { res, args_res } }
  end

  defp optimize_expr({ :var, _, name } = var, dict) do
    case :orddict.find(name, dict) do
      { :ok, res } -> { var, dict, { res, nil } }
      :error       -> { var, dict, nil }
    end
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

  defp optimize_expr(other, dict) when elem(other, 0) in [:string, :atom, :integer, :float, :nil] do
    { other, dict, nil }
  end

  ## Match related

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
    if is_record?(value) do
      dict =
        case :orddict.find(key, dict) do
          { :ok, ^value } ->
            dict
          { :ok, _ } ->
            # We are overriding a type of an existing variable,
            # which means the source code is invalid.
            :orddict.store(key, nil, dict)
          :error ->
            :orddict.store(key, value, dict)
        end
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

  ## Record helpers

  # TODO: Implement proper record check
  defp is_record?(_h) do
    true
  end
end