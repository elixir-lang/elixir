defmodule ExUnit.MatchPatternFormat do
  @moduledoc "Formats the output of PatternDiff"

  alias ExUnit.{ContainerDiff, PatternDiff}

  @no_value :ex_unit_no_meaningful_value

  def format(diff, ctx \\ nil)

  def format(%ContainerDiff{type: :map, items: items}, _) do
    ctx = if Enum.all?(items, &keyword_tuple?/1), do: :atom_keys, else: :non_atom_keys
    items =
      items
      |> sort
      |> Enum.map(&format(&1, ctx))
      |> commaize_keys

    List.flatten([{:eq, "%{"}, items, {:eq, "}"}])
  end

  def format(%ContainerDiff{type: :tuple, items: [l, r]}, :atom_keys) do
    [
      format_key(l, :atom_key),
      format(r)
    ]
    |> List.flatten()
  end

  def format(%ContainerDiff{type: :tuple, items: [l, r]}, :non_atom_keys) do
    [
      format_key(l, :non_atom_key),
      format(r)
    ]
    |> List.flatten()
  end

  def format(%ContainerDiff{type: :tuple, items: items}, _) do
    [h | t] = items
    ret = [{:eq, "{"}, format(h), Enum.map(t, &[format_comma(&1), format(&1)]), {:eq, "}"}]
    List.flatten(ret)
  end

  def format(%ContainerDiff{type: :list, items: items}, _) do
    ctx = if Enum.all?(items, &keyword_tuple?/1), do: :atom_keys, else: :list
    [h | t] = items

    ret = [
      {:eq, "["},
      format(h, ctx),
      Enum.map(t, &[format_comma(&1), format(&1, ctx)]),
      {:eq, "]"}
    ]

    List.flatten(ret)
  end

  def format(%PatternDiff{lh: %{ast: {var, _, var_ctx}}, diff_result: :eq} = diff, _)
      when is_atom(var) and is_atom(var_ctx) do
    [equiv: {"#{var}", inspect(diff.rh)}]
  end

  def format(%PatternDiff{lh: %{ast: {var, _, var_ctx}}, diff_result: :neq} = diff, _)
      when is_atom(var) and is_atom(var_ctx) do
    [del: "#{var}", ins: inspect(diff.rh)]
  end

  def format(%PatternDiff{lh: %{ast: {:^, _, var}}, diff_result: :eq} = diff, _) do
    [equiv: {format_pin(var), inspect(diff.rh)}]
  end

  def format(%PatternDiff{lh: %{ast: {:^, _, var}}, diff_result: :neq} = diff, _) do
    [del: format_pin(var), ins: inspect(diff.rh)]
  end

  def format(%PatternDiff{type: :value, diff_result: :eq} = diff, _) do
    [eq: inspect(diff.rh)]
  end

  def format(%PatternDiff{type: :value, diff_result: :neq} = diff, _) do
    [del: inspect(diff.lh.ast), ins: inspect(diff.rh)]
  end

  def format(
        %PatternDiff{type: :different, rh: @no_value, lh: %{ast: {key, value}}},
        :atom_keys
      ) do
    [del: "#{textify_ast(key)}: ", del: "#{inspect(value)}"]
  end

  def format(
        %PatternDiff{type: :different, rh: @no_value, lh: %{ast: {key, value}}},
        :non_atom_keys
      ) do
    [del: "#{textify_ast(key)} => ", del: "#{textify_ast(value)}"]
  end

  def format(%PatternDiff{type: :different, rh: @no_value} = diff, _ctx) do
    [del: inspect(diff.lh.ast)]
  end

  def format(%PatternDiff{type: :different, rh: {key, value}, lh: @no_value}, :atom_keys) do
    [ins: "#{key}: ", ins: "#{inspect(value)}"]
  end

  def format(
        %PatternDiff{type: :different, rh: {key, value}, lh: @no_value},
        :non_atom_keys
      ) do
    [ins: "#{inspect(key)} => ", ins: "#{inspect(value)}"]
  end

  def format(%PatternDiff{type: :different, lh: @no_value} = diff, _) do
    [ins: inspect(diff.rh)]
  end

  def format(%PatternDiff{type: :different, lh: %{ast: {_, _, _}}} = diff, _) do
    [del: textify_ast(diff.lh.ast), ins: inspect(diff.rh)]
  end

  def format(%PatternDiff{type: :different} = diff, _) do
    [del: inspect(diff.lh.ast), ins: inspect(diff.rh)]
  end

  def format(%PatternDiff{} = unhandled, ctx) do
    IO.inspect(ctx, label: "unhandeld ctx")
    IO.inspect(unhandled, label: "pattern_diff_unhandled")
  end

  def format_key(%PatternDiff{type: :value, diff_result: :eq} = diff, :atom_key) do
    [eq: "#{diff.rh}: "]
  end

  def format_key(%PatternDiff{type: :value, diff_result: :neq} = diff, :atom_key) do
    [del: "#{diff.lh.ast}: ", ins: "#{diff.rh}: "]
  end

  def format_key(%PatternDiff{type: :value, diff_result: :eq, lh: %{ast: {:^, _, pin}}} = diff, :non_atom_key) do
    [equiv: {"#{format_pin(pin)} => ", "#{inspect(diff.rh)} => "}]
  end
  def format_key(%PatternDiff{type: :value, diff_result: :eq} = diff, :non_atom_key) do
    [eq: "#{inspect(diff.rh)} => "]
  end

  def format_key(%PatternDiff{type: :value, diff_result: :neq} = diff, :non_atom_key) do
    [del: "#{inspect(diff.lh.ast)} => ", ins: "#{diff.rh} => "]
  end

  defp format_pin([{var, _, _}]), do: "^#{var}"

  defp format_comma(%{rh: @no_value}), do: {:del, ", "}
  defp format_comma(%{lh: @no_value}), do: {:ins, ", "}
  defp format_comma(%{lh: %{type: :cons_r}}), do: {:eq, " | "}
  defp format_comma(_), do: {:eq, ", "}

  defp keyword_tuple?(%ContainerDiff{items: [key, _value], type: :tuple}) do
    l_matches = key.lh == @no_value || is_atom(key.lh.ast)
    r_matches = key.rh == @no_value || is_atom(key.rh)
    l_matches && r_matches
  end

  defp keyword_tuple?(%PatternDiff{rh: @no_value, lh: %{ast: {key, _value}}}) when is_atom(key) do
    true
  end

  defp keyword_tuple?(%PatternDiff{rh: {key, _value}, lh: @no_value}) when is_atom(key) do
    true
  end

  defp keyword_tuple?(_) do
    false
  end

  defp textify_ast({:%{}, _, elements}) do
    inspect(Map.new(Enum.map(elements, &textify_ast/1)))
  end

  defp textify_ast(elem) when is_atom(elem), do: elem
  defp textify_ast(elem), do: elem

  ### Order items by matching first, then by inserted, then by deleted
  defp sort(items) when is_list(items) do
    Enum.sort_by(items, fn
      %PatternDiff{diff_result: :eq, type: :value} -> 0
      %ContainerDiff{items: [%{diff_result: :eq}, %{diff_result: :eq}]} -> 0
      %ContainerDiff{items: [%{diff_result: :eq}, %{diff_result: :neq}]} -> 1
      %ContainerDiff{items: [%{diff_result: :eq} | _]} -> 1
      %PatternDiff{diff_result: :neq, type: :value} -> 1
      %PatternDiff{type: :different} -> 2
    end)
  end

  defp sort(items), do: items

  defp commaize_keys(items) do
    [h | rest] = items

    comma = rest
    |> Enum.map(&commaize_key(&1))

    comma = case h do
      [{:del, _} | _] -> remove_ins_comma(comma)
      _ -> comma
    end
    [h | comma]
  end

  defp commaize_key([key | _values] = fmt) do
    comma = case key do
              {:equiv, _} -> {:eq, ", "}
              {k, _} -> {k, ", "}
    end
    [comma | fmt]
  end

  defp remove_ins_comma(formatted) do
    {ret, _}  = Enum.reduce(formatted, {[], false}, fn x, acc ->
      case acc do
        {lst, false} ->
          case x do
            [{:ins, ", "} | rest] ->
              {[rest | lst], true}
            rest ->
              {[rest | lst], false}
          end
        {lst, true} ->
          {[x | lst], true}
      end
    end)
    Enum.reverse(ret)
  end

end
