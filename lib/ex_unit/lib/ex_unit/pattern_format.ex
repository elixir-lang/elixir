defmodule ExUnit.PatternFormat do
  @moduledoc "Formats the output of PatternDiff"

  alias ExUnit.{ContainerDiff, PatternDiff, WhenDiff}

  def format(diff, ctx \\ nil)

  def format(%ContainerDiff{type: :tuple, items: [%{rh: key} = l, r]}, :list) when is_atom(key) do
    [
      format(l, :key),
      format(r)
    ]
  end

  def format(%ContainerDiff{type: :tuple, items: items}, _) do
    [h | t] = items
    ret = [{:eq, "{"},
           format(h),
           Enum.map(t, &([{:eq, ", "}, format(&1)])),
           {:eq, "}"}]
    List.flatten(ret)
  end

  def format(%ContainerDiff{type: :list, items: items}, _) do
    [h | t] = items
    ret = [{:eq, "["},
           format(h, :list),
           Enum.map(t, &([{:eq, ", "}, format(&1, :list)])),
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

  def format(%PatternDiff{type: :value, diff_result: :eq} = diff, :key) do
    [eq: "#{diff.rh}: "]
  end

  def format(%PatternDiff{type: :value, diff_result: :neq} = diff, :key) do
    [del: "#{diff.lh.ast}: ", ins: "#{diff.rh}: "]
  end

  def format(%PatternDiff{type: :value, diff_result: :eq} = diff, _) do
    [eq: inspect(diff.rh)]
  end

  def format(%PatternDiff{type: :value, diff_result: :neq} = diff, _) do
    [del: inspect(diff.lh.ast), ins: inspect(diff.rh)]
  end

  def format(%PatternDiff{} = unhandled, _) do
    IO.inspect(unhandled, label: "pattern_diff_unhandled")
  end

  defp format_pin([{var, _, _}]), do: "^#{var}"
end
