defmodule ExUnit.PatternFormat2 do
  @moduledoc """
  Formats the output of Pattern diff.
  1) Matches will be printed in green, conflicts in red, and neutral in the default color
  2) If pattern and target are maps, structs or a keyword list
     and all keywords of the pattern are present, only the keywords in the pattern will be listed
     e.g.  left: %{a: 1, b: 2}
           right: %{a: 2, b: 2, ...}
  3) If a key is missing, then all will be printed.
  4) Example coloring:
     [eq: "working", diff_insert: " hello", diff_delete: " good bye"]
      = (normal)message: %{(green)Hello: (red)Goodbye(normal)}
  """

  alias ExUnit.{ContainerDiff, PatternDiff}

  @no_value :ex_unit_no_meaningful_value

  def script(left, right) do
    _diff =
      left
      |> ExUnit.PatternDiff.cmp(right)
      |> IO.inspect(label: "Calling format")
      |> format

    # [eq: "working", diff_insert: " hello", diff_delete: " good bye"]
  end

  def format(diff, ctx \\ nil)

  def format(%ContainerDiff{type: :map, items: items}, %{keys: :atom_keys, missing: false} = ctx) do
    IO.inspect("full map, here goes", label: "ContainerDiff")
    [h | rest] = items

    [
      "%{",
      format(h, ctx),
      Enum.map(rest, &format(&1, %{ctx | comma: ", "})),
      "}"
    ]
    |> List.flatten()
  end

  def format(%ContainerDiff{type: :map, items: _items} = diff, _) do
    IO.inspect("format map", label: "ContainerDiff")
    ctx = create_context(diff)
    format(diff, ctx)
  end

  def format(
        %ContainerDiff{type: :list, items: items},
        %{keys: :list, missing: false} = ctx
      ) do
    IO.inspect("formatting a list-list", label: "ContainerDiff")
    [h | rest] = items

    [
      "[",
      format(h, ctx),
      Enum.map(rest, &format(&1, %{ctx | comma: ", "})),
      "]"
    ]
    |> List.flatten()
  end

  def format(%ContainerDiff{type: :list, items: _items} = _diff, :atom_keys) do
    raise "Not done yet"
  end

  def format(%ContainerDiff{type: :list, items: _items} = diff, _) do
    ctx = create_context(diff)

    format(diff, ctx)
  end

  def format(%ContainerDiff{type: :tuple, items: [l, r]}, %{keys: :atom_keys, comma: comma}) do
    IO.inspect("format tuple", label: "ContainerDiff")

    IO.inspect(binding())

    left =
      case l do
        %PatternDiff{diff_result: :eq} ->
          match("#{l.rh}: ")

        _ ->
          delete(inspect(l.rh))
      end

    right =
      case r do
        %PatternDiff{diff_result: :eq} ->
          match("#{r.rh}")

        _ ->
          delete(inspect(r.rh))
      end

    [
      comma,
      left,
      right
    ]
    |> IO.inspect(label: "tuple return")
  end

  def format(diff, nil) do
    format(diff, %{comma: "", keys: :none, missing: false})
  end

  def format(%PatternDiff{diff_result: :eq, rh: rh}, %{comma: comma}) do
    IO.inspect("eq", label: "PatternDiff")
    [comma, match(inspect(rh))]
  end

  def format(%PatternDiff{diff_result: :neq, rh: @no_value, lh: lh}, %{comma: comma}) do
    IO.inspect("neq", label: "PatternDiff")
    [comma, insert(textify_ast(lh))]
  end

  def format(%PatternDiff{diff_result: :neq, rh: {key, value}, lh: @no_value}, %{
        comma: comma,
        keys: :atom_keys
      }) do
    IO.inspect("delete", label: "PatternDiff")
    [comma, delete("#{to_string(key)}: #{inspect(value)}")]
  end

  def format(%PatternDiff{diff_result: :neq, rh: rh, lh: _lh}, %{comma: comma}) do
    IO.inspect("delete", label: "PatternDiff")
    [comma, delete(inspect(rh))]
  end

  # def format(%PatternDiff{diff_result: :neq, rh: rh, lh: lh})
  def format(other, context) do
    IO.inspect(context, label: "context")
    IO.inspect(other, label: "No matching format")
  end

  defp create_context(%ContainerDiff{type: :list, items: items}) do
    if Enum.all?(items, &keyword_tuple?/1),
      do: %{keys: :atom_keys, missing: false, comma: ""},
      else: %{keys: :list, missing: false, comma: ""}
  end

  defp create_context(%ContainerDiff{type: :map, items: items}) do
    keys = if Enum.all?(items, &keyword_tuple?/1), do: :atom_keys, else: :non_atom_keys
    missing = Enum.all?(items, &right_missing_keys?/1)

    %{keys: keys, missing: missing, comma: ""}
    |> IO.inspect(label: "create contect for a map")
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

  defp right_missing_keys?(%ContainerDiff{items: [_key, _value], type: :tuple}), do: false
  defp right_missing_keys?(%PatternDiff{rh: {_key, _value}, lh: @no_value}), do: false
  defp right_missing_keys?(_), do: true

  defp textify_ast(%{ast: ast}), do: textify_ast(ast)

  defp textify_ast({:%{}, _, elements}) do
    inspect(Map.new(Enum.map(elements, &textify_ast/1)))
  end

  defp textify_ast(elem) when is_atom(elem), do: elem
  defp textify_ast(elem), do: to_string(elem)

  defp delete(term) do
    {:diff_delete, term}
  end

  defp insert(term) do
    {:diff_insert, term}
  end

  defp match(term) do
    term
  end
end
