defmodule ExUnit.PatternFormat do
  @moduledoc "Formats the output of PatternDiff"

  alias ExUnit.{ContainerDiff, PatternDiff, WhenDiff}

  @no_value :ex_unit_no_meaningful_value

  def format(diff, ctx \\ nil)

  def format(%ContainerDiff{type: :map, items: items}, _) do
    ctx = if Enum.all?(items, &keyword_tuple?/1), do: :atom_keys, else: :non_atom_keys
    # IO.inspect(items, label: "items #{ctx}")
    [h | t] = items
    ret = [{:eq, "%{"},
           format(h, ctx),
           Enum.map(t, &([format_comma(&1), format(&1, ctx)])),
           {:eq, "}"}]
    List.flatten(ret)
  end

  def format(%ContainerDiff{type: :tuple, items: [l, r]}, :atom_keys) do
    [
      format(l, :atom_key),
      format(r)
    ]
  end

  def format(%ContainerDiff{type: :tuple, items: [l, r]}, :non_atom_keys) do
    [
      format(l, :non_atom_key),
      format(r)
    ]
  end

  def format(%ContainerDiff{type: :tuple, items: items}, _) do
    [h | t] = items
    ret = [{:eq, "{"},
           format(h),
           Enum.map(t, &([format_comma(&1), format(&1)])),
           {:eq, "}"}]
    List.flatten(ret)
  end

  def format(%ContainerDiff{type: :list, items: items}, _) do
    ctx = if Enum.all?(items, &keyword_tuple?/1), do: :atom_keys, else: :list
    [h | t] = items
    ret = [{:eq, "["},
           format(h, ctx),
           Enum.map(t, &([format_comma(&1), format(&1, ctx)])),
           {:eq, "]"}]
    List.flatten(ret)
  end

  def format(%PatternDiff{lh: %{ast: {var, _, var_ctx}}, diff_result: :eq} = diff, _) when is_atom(var) and is_atom(var_ctx) do
    [equiv: {"#{var}", inspect(diff.rh)}]
  end

  def format(%PatternDiff{lh: %{ast: {var, _, var_ctx}}, diff_result: :neq} = diff, _) when is_atom(var) and is_atom(var_ctx) do
    [del: "#{var}", ins: inspect(diff.rh)]
  end

  def format(%PatternDiff{lh: %{ast: {:^, _, var}}, diff_result: :eq} = diff, _) do
    [equiv: {format_pin(var), inspect(diff.rh)}]
  end

  def format(%PatternDiff{lh: %{ast: {:^, _, var}}, diff_result: :neq} = diff, _) do
    [del: format_pin(var), ins: inspect(diff.rh)]
  end

  def format(%PatternDiff{type: :value, diff_result: :eq} = diff, :atom_key) do
    [eq: "#{diff.rh}: "]
  end

  def format(%PatternDiff{type: :value, diff_result: :neq} = diff, :atom_key) do
    [del: "#{diff.lh.ast}: ", ins: "#{diff.rh}: "]
  end

  def format(%PatternDiff{type: :value, diff_result: :eq} = diff, :non_atom_key) do
    [eq: "#{inspect diff.rh} => "]
  end

  def format(%PatternDiff{type: :value, diff_result: :neq} = diff, :non_atom_key) do
    [del: "#{inspect diff.lh.ast} => ", ins: "#{diff.rh} => "]
  end

  def format(%PatternDiff{type: :value, diff_result: :eq} = diff, _) do
    [eq: inspect(diff.rh)]
  end

  def format(%PatternDiff{type: :value, diff_result: :neq} = diff, _) do
    [del: inspect(diff.lh.ast), ins: inspect(diff.rh)]
  end

  def format(%PatternDiff{type: :different, rh: @no_value, lh: %{ast: {key, value}}} = diff, :atom_keys) do
    [del: "#{textify_ast key}: ", del: "#{inspect value}"]
  end
  def format(%PatternDiff{type: :different, rh: @no_value, lh: %{ast: {key, value}}} = diff, :non_atom_keys) do
    [del: "#{textify_ast key} => ", del: "#{textify_ast value}"]
  end

  def format(%PatternDiff{type: :different, rh: @no_value} = diff, ctx) do
    [del: inspect(diff.lh.ast)]
  end

  def format(%PatternDiff{type: :different, rh: {key, value}, lh: @no_value} = diff, :atom_keys) do
    [ins: "#{key}: ", ins: "#{inspect value}"]
  end

  def format(%PatternDiff{type: :different, rh: {key, value}, lh: @no_value} = diff, :non_atom_keys) do
    [ins: "#{inspect key} => ", ins: "#{inspect value}"]
  end

  def format(%PatternDiff{type: :different, lh: @no_value} = diff, _) do
    [ins: inspect(diff.rh)]
  end

  def format(%PatternDiff{type: :different} = diff, _) do
    [del: inspect(diff.lh.ast), ins: inspect(diff.rh)]
  end

  def format(%PatternDiff{} = unhandled, ctx) do
    IO.inspect(ctx, label: "unhandeld ctx")
    IO.inspect(unhandled, label: "pattern_diff_unhandled")
  end

  defp format_pin([{var, _, _}]), do: "^#{var}"

  defp format_comma(%{rh: @no_value}), do: {:del, ", "}
  defp format_comma(%{lh: @no_value}), do: {:ins, ", "}
  defp format_comma(_), do: {:eq, ", "}


  defp keyword_tuple?(%ContainerDiff{items: [key, _value], type: :tuple}) do
    l_matches = key.lh == @no_value || is_atom(key.lh.ast)
    r_matches = key.rh == @no_value || is_atom(key.rh)
    #IO.inspect("#{inspect key}: #{l_matches} && #{r_matches}", label: "keyword_tuple?")
    l_matches && r_matches
  end
  defp keyword_tuple?(%PatternDiff{rh: @no_value, lh: %{ast: {key, value}}}) when is_atom(key) do
    true
  end
  defp keyword_tuple?(%PatternDiff{rh: {key, value}, lh: @no_value}) when is_atom(key) do
    true
  end
  defp keyword_tuple?(n) do
    #IO.inspect(n, label: "not keyword;")
    false
  end

  defp textify_ast({:%{}, _, elements} = dump) do
    inspect Map.new(Enum.map(elements, &textify_ast/1))
  end
  defp textify_ast(elem), do: elem
end
