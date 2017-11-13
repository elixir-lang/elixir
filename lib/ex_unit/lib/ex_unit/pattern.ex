defmodule ExUnit.Pattern do
  defstruct [:val, :vars, :pins, :meta]

  @no_value :ex_unit_no_meaningful_value

  @type t :: %__MODULE__{
          val: any(),
          meta: :list | :%{} | :{} | :|,
          vars: [key: atom()],
          pins: [key: atom()],
        }

  def new(lh_pattern, pins, unbound_vars) when is_list(pins) and is_list(unbound_vars) do
    %__MODULE__{
      val: lh_pattern,
      meta: get_meta(lh_pattern),
      pins: pins,
      vars: unbound_vars,
    }
  end

  def new(lh_pattern, %__MODULE__{} = parent, meta)  do
    %__MODULE__{
      val: lh_pattern,
      meta: meta,
      pins: parent.pins,
      vars: parent.vars,
    }
  end

  def new(lh_pattern, %__MODULE__{} = parent)  do
    %__MODULE__{
      val: lh_pattern,
      meta: get_meta(lh_pattern),
      pins: parent.pins,
      vars: parent.vars,
    }
  end


  defp get_meta(node) when is_list(node), do: :list
  defp get_meta(_), do: nil

  # def next(pattern, lhs, rhs) do
  #   loggit("---- next #{inspect lhs}- #{inspect rhs}----")

  #   %__MODULE__{
  #     lh: lhs,
  #     rh: rhs,
  #     pins: pattern.pins,
  #     vars: pattern.vars,
  #     meta: pattern.meta,
  #     diff: pattern.diff,
  #     parent: pattern.parent
  #   }
  #   |> diff
  # end

  # def diff(%__MODULE__{lh: {:^, _, rhs}, rh: right} = pattern) do
  #   loggit(pattern, label: ":^")
  #   [{var, _, _}] = rhs

  #   val = Keyword.get(pattern.pins, var)

  #   diff =
  #     if val == right,
  #       do: [{:eq, "^#{var}"}],
  #       else: [del: "^#{var}", ins: "#{right}"]

  #   append_diff(pattern, diff)
  # end

  # def diff(%__MODULE__{lh: [[{:|, _, [h | rest]}]], rh: [h_rhs | t_rhs]} = pattern) do
  #   loggit(pattern, label: "[:|]")
  #   IO.inspect(rest, label: "rest")
  #   IO.inspect(t_rhs, label: "t_rhs")
  #   pattern
  #   |> set_meta(:|)
  #   |> next(h, h_rhs)
  #   |> next(rest, t_rhs)
  #   |> wrap
  # end
  # def diff(%__MODULE__{lh: [{:|, _, [h | rest]}], rh: [h_rhs | t_rhs]} = pattern) do
  #   loggit(pattern, label: ":|")
  #   IO.inspect(rest, label: "rest")
  #   IO.inspect(t_rhs, label: "t_rhs")
  #   pattern
  #   |> set_meta(:|)
  #   |> next(h, h_rhs)
  #   |> next(rest, t_rhs)
  #   |> wrap
  # end

  # def diff(%__MODULE__{lh: {:%{}, _, _}, rh: right} = pattern) do
  #   loggit(pattern, label: ":%{}")
  #   members = get_members(pattern.lh)

  #   pattern
  #   |> set_meta(:%{})
  #   |> next(members, right)
  #   |> wrap
  # end

  # def diff(%__MODULE__{lh: {:{}, _, _}, rh: right} = pattern) do
  #   loggit(pattern, label: ":{}")
  #   lhs = get_members(pattern.lh)
  #   rhs = Tuple.to_list(right)

  #   pattern
  #   |> set_meta(:{})
  #   |> next(lhs, rhs)
  #   |> wrap
  # end

  # def diff(%__MODULE__{lh: {key, val}, rh: {key, @no_value}, meta: meta} = pattern)
  #     when meta == :%{} or meta == :list do
  #   loggit(pattern, label: "kv?: true, no right value")

  #   if is_atom(key) || meta == :list do
  #     %{pattern | diff: [{:del, "#{key}: #{inspect(val)}"}]}
  #   else
  #     %{pattern | diff: [{:del, "#{format_key(key)} => #{inspect(val)}"}]}
  #   end
  # end

  # def diff(%__MODULE__{lh: {key, val}, rh: {key, val}, meta: meta} = pattern)
  #     when meta == :%{} or meta == :list do
  #   loggit(pattern, label: "kv?: true")

  #   if is_atom(key) || meta == :list do
  #     append_diff(pattern, [{:eq, "#{key}: #{inspect(val)}"}])
  #   else
  #     append_diff(pattern, [{:eq, "#{inspect(key)} => #{inspect(val)}"}])
  #   end
  # end

  # def diff(%__MODULE__{lh: {key, val1}, rh: {key, val2}, meta: meta} = pattern)
  #     when meta == :%{} or meta == :list do
  #   loggit(pattern, label: "adding key :%{}")

  #   pre_diff =
  #     if is_atom(key) || meta == :list do
  #       [{:eq, "#{key}: "}]
  #     else
  #       [{:eq, "#{inspect(key)} => "}]
  #     end

  #   pattern
  #   |> append_diff(pre_diff)
  #   |> push(val1, val2)
  #   |> pop
  # end

  # # two-element tuples are passed in as is
  # def diff(%__MODULE__{lh: {_, _}, rh: right} = pattern) when is_tuple(right) do
  #   loggit(pattern, label: "{_, _}")
  #   lhs = Tuple.to_list(pattern.lh)
  #   rhs = Tuple.to_list(right)

  #   pattern
  #   |> set_meta(:{})
  #   |> next(lhs, rhs)
  #   |> wrap
  # end

  # def diff(%__MODULE__{lh: [h_lhs | t_lhs], rh: rhs, meta: :%{}} = pattern) when is_map(rhs) do
  #   loggit(pattern, label: "is_map(rhs)")
  #   key = h_lhs
  #   |> elem(0)
  #   |> build_key(pattern.pins)

  #   {val, t_rhs} = Map.pop(rhs, key, @no_value)

  #   pattern
  #   |> next({key, elem(h_lhs, 1)}, {key, val})
  #   |> next(t_lhs, t_rhs)
  # end

  # def diff(%__MODULE__{lh: lhs, rh: rhs, meta: nil} = pattern) when is_list(lhs) and is_list(rhs) do
  #   loggit(pattern, label: "new list")

  #   pattern
  #   |> set_meta(:list)
  #   |> next(lhs, rhs)
  #   |> wrap
  # end

  # def diff(%__MODULE__{lh: [h_lhs | t_lhs], rh: [h_rhs | t_rhs]} = pattern) do
  #   loggit(pattern, label: "[h | t]")

  #   if is_list(h_lhs) && is_list(h_rhs) do
  #     meta = pattern.meta
  #     p = %{pattern | meta: nil}
  #     result = next(p, h_lhs, h_rhs)
  #     next(%{result | meta: meta}, t_lhs, t_rhs)
  #   else
  #     pattern
  #     |> next(h_lhs, h_rhs)
  #     |> next(t_lhs, t_rhs)
  #   end
  # end

  # def diff(%__MODULE__{lh: [], rh: []} = pattern), do: pattern

  # def diff(%__MODULE__{lh: [], rh: map} = pattern) when map == %{} do
  #   loggit(pattern, label: "[] & %{}")
  #   pattern
  # end

  # def diff(%__MODULE__{lh: [], rh: remaining} = pattern) when is_list(remaining) do
  #   loggit(pattern, label: "[], remaining")

  #   diff =
  #     for r <- remaining do
  #       {:ins, inspect(r)}
  #     end

  #   ret = append_diff(pattern, diff, :ins)
  #   ret
  # end

  # def diff(%__MODULE__{lh: [], rh: remaining} = pattern) when is_map(remaining) do
  #   loggit(pattern, label: "%{}, remaining")
  #   append_diff(pattern, [{:ins, "..."}], :ins)
  # end

  # def diff(%__MODULE__{lh: [h | t], rh: []} = pattern) do
  #   pattern
  #   |> next(h, @no_value)
  #   |> next(t, [])
  # end

  # def diff(%__MODULE__{lh: {var, _, _}, rh: val} = pattern) when is_atom(var) do
  #   loggit(pattern, label: "var")

  #   pattern =
  #     case test_bound_value(pattern, var, val) do
  #       {:ok, pattern} ->
  #         append_diff(pattern, [{:equiv, {"#{var}", "#{inspect(val)}"}}])

  #       {:error, pattern, _other_val} ->
  #         append_diff(pattern, [{:del, "#{var}"}, {:ins, "#{inspect(val)}"}])
  #     end

  #   pattern
  # end

  # def diff(%__MODULE__{lh: @no_value, rh: val} = pattern) do
  #   loggit(pattern, label: "no value left")
  #   append_diff(pattern, [{:ins, "#{inspect(val)}"}], :ins)
  # end

  # def diff(%__MODULE__{lh: val, rh: @no_value} = pattern) do
  #   loggit(pattern, label: "no value right")
  #   append_diff(pattern, [{:del, "#{inspect(val)}"}], :del)
  # end

  # def diff(%__MODULE__{lh: val, rh: val} = pattern) do
  #   loggit(pattern, label: "format equal")

  #   pattern
  #   |> append_diff([{:eq, "#{inspect(val)}"}])
  # end

  # def diff(%__MODULE__{lh: lhs, rh: rhs} = pattern) do
  #   loggit(pattern, label: "format")

  #   pattern
  #   |> append_diff([{:del, "#{inspect(lhs)}"}, {:ins, "#{inspect(rhs)}"}])
  # end

  # def build_key({:%{}, _, members}, pins) do
  #   members
  #   |> Enum.map(&(build_key(&1, pins)))
  #   |> Map.new()
  # end

  # def build_key({:^, _, value}, pins) do
  #   [{var, _, _}] = value
  #   pins[var]
  # end

  # def build_key(m, _pins), do: m

  # def format_key({:%{}, _, members}) do
  #   members
  #   |> Map.new()
  #   |> inspect
  # end

  # def format_key(key), do: inspect(key)

  # defp set_meta(pattern, type) do
  #   loggit(pattern, label: "set_meta - #{type}")
  #   pattern = %{pattern | meta: type}
  #   push(pattern)
  # end

  # defp push(pattern) do
  #   loggit("push")

  #   %__MODULE__{
  #     lh: pattern.lh,
  #     rh: pattern.rh,
  #     pins: pattern.pins,
  #     vars: pattern.vars,
  #     meta: pattern.meta,
  #     parent: pattern,
  #     diff: []
  #   }
  # end

  # defp push(pattern, lhs, rhs) do
  #   loggit("push")

  #   %__MODULE__{
  #     lh: lhs,
  #     rh: rhs,
  #     pins: pattern.pins,
  #     vars: pattern.vars,
  #     meta: pattern.meta,
  #     parent: pattern,
  #     diff: []
  #   }
  #   |> diff
  # end

  # defp pop(%__MODULE__{parent: p} = pattern) do
  #   %{p | diff: p.diff ++ pattern.diff}
  # end

  # defp wrap(pattern, key \\ :eq)

  # defp wrap(%__MODULE__{meta: :%{}, parent: p} = pattern, key) do
  #   loggit("wrap :%{}")
  #   append_diff(p, [{key, "%{"}] ++ pattern.diff ++ [{key, "}"}])
  # end

  # defp wrap(%__MODULE__{meta: :{}, parent: p} = pattern, key) do
  #   loggit("wrap :{}")
  #   append_diff(p, [{key, "{"}] ++ pattern.diff ++ [{key, "}"}])
  # end

  # defp wrap(%__MODULE__{meta: :list, parent: p} = pattern, key) do
  #   loggit("wrap :list")
  #   append_diff(p, [{key, "["}] ++ pattern.diff ++ [{key, "]"}])
  # end

  # defp wrap(%__MODULE__{meta: :|, parent: p} = pattern, key) do
  #   loggit("wrap :list")
  #   append_diff(p, [{key, "["}] ++ pattern.diff ++ [{key, "]"}])
  # end
  # def get_diff(pattern) do
  #   pattern.diff
  # end

  # defp append_diff(pattern, diff, comma_type \\ :eq)
  # defp append_diff(pattern, nil, _), do: pattern
  # defp append_diff(pattern, [], _), do: pattern

  # defp append_diff(%__MODULE__{diff: []} = pattern, diff, _) do
  #   loggit(diff, label: "no_comma")
  #   %{pattern | diff: pattern.diff ++ diff}
  # end

  # defp append_diff(pattern, diff, :none) do
  #   loggit(diff, label: "comma suppressed")
  #   %{pattern | diff: pattern.diff ++ diff}
  # end

  # defp append_diff(pattern, diff, comma_type) do
  #   loggit(diff, label: "comma #{comma_type}")
  #   %{pattern | diff: pattern.diff ++ [{comma_type, ", "} | diff]}
  # end

  # defp test_bound_value(%__MODULE__{vars: vars} = pattern, key, value) do
  #   case Keyword.get(vars, key) do
  #     :ex_unit_unbound_var ->
  #       new_vars = Keyword.put(vars, key, value)
  #       {:ok, %{pattern | vars: new_vars}}

  #     ^value ->
  #       {:ok, pattern}

  #     other_val ->
  #       {:error, pattern, other_val}
  #   end
  # end

  # defp get_members({:%{}, _, members}), do: members
  # defp get_members({:{}, _, members}), do: members

  # # TODO - remove 
  # defp loggit(msg, arg \\ []), do: IO.inspect(msg, arg)
  # #defp loggit(msg, _arg \\ []), do: msg
end
