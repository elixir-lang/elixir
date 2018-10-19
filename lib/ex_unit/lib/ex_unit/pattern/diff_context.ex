defmodule ExUnit.Pattern.DiffContext do
  alias ExUnit.{ContainerDiff, PatternDiff}

  @no_value :ex_unit_no_meaningful_value

  def create_context(%ContainerDiff{type: :list, items: items}, old_ctx) do
    if Enum.all?(items, &keyword_tuple?/1),
      do: new_context(:atom_keys, old_ctx),
      else: new_context(:list, old_ctx)
  end

  def create_context(%ContainerDiff{type: :map, items: items}, old_ctx) do
    keys = if Enum.all?(items, &keyword_tuple?/1), do: :atom_keys, else: :non_atom_keys

    new_context(keys, old_ctx)
  end

  def new_context(keys, %{vars: vars, pins: pins}), do: new_context(keys, vars, pins)

  def new_context(keys, vars, pins) do
    %{comma: "", keys: keys, print_when?: true, vars: vars, pins: pins}
  end

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
end
