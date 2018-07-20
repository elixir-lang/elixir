defmodule ExUnit.ReceivePatternFormat do
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

  alias ExUnit.{ContainerDiff, PatternDiff, WhenDiff}

  @no_value :ex_unit_no_meaningful_value

  # JOWENS
  # Create context function that includes vars & pins from `left`
  def script(left, right) do
    # IO.inspect(binding(), label: "script")

    _diff =
      left
      |> ExUnit.PatternDiff.cmp(right)
      # |> IO.inspect(label: "Compare")
      |> format
      |> Enum.reject(&(&1 == ""))

    # |> IO.inspect(label: "Diff")
  end

  def format(diff, ctx \\ nil)

  def format(
        %ContainerDiff{type: :map, items: items},
        %{keys: key_type, missing: false, comma: comma} = ctx
      )
      when key_type == :atom_keys or key_type == :non_atom_keys do
    # IO.inspect("Container Map")
    [h | rest] = items

    [
      comma,
      "%{",
      format(h, ctx),
      Enum.map(rest, &format(&1, %{ctx | comma: ", "})),
      "}"
    ]
    |> List.flatten()
  end

  def format(%ContainerDiff{type: :map, items: _items} = diff, ctx) do
    IO.inspect(ctx, label: "Container Map - ")
    #    IO.inspect(_items, label: "items")
    map_ctx = create_context(diff)

    comma =
      case ctx do
        %{comma: comma} -> comma
        _ -> ""
      end

    [comma, format(diff, map_ctx)]
    |> List.flatten()
  end

  def format(
        %ContainerDiff{type: :list, items: items},
        %{keys: :list, missing: false} = ctx
      ) do
    IO.inspect("Container list")
    [h | rest] = items

    [
      "[",
      format(h, ctx),
      Enum.map(rest, &format(&1, %{ctx | comma: ", "})),
      "]"
    ]
    |> List.flatten()
  end

  def format(%ContainerDiff{type: :when, items: [l, r]}, %{comma: comma} = ctx) do
    IO.inspect("Container when")

    [comma, format(l, ctx), " ", format(r, ctx)]
    |> List.flatten()
  end

  def format(%ContainerDiff{type: :list, items: []} = diff, %{comma: comma}) do
    [comma, "[]"]
  end

  def format(%ContainerDiff{type: :list, items: _items} = diff, %{comma: comma}) do
    IO.inspect("Cointainer diff list")

    ctx = create_context(diff)

    [comma, format(diff, ctx)]
    |> List.flatten()

    # |> IO.inspect(label: "Format list")
  end

  def format(%ContainerDiff{type: :tuple, items: items}, %{keys: :none} = ctx) do
    IO.inspect("Container tuple")
    [h | rest] = items

    [
      "{",
      format(h, ctx),
      Enum.map(rest, &format(&1, %{ctx | comma: ", "})),
      "}"
    ]
    |> List.flatten()
  end

  def format(%ContainerDiff{type: :tuple, items: [l, r]}, %{keys: :non_atom_keys, comma: comma}) do
    # IO.inspect(l, label: "Container non-atom tuple pair: L")
    # IO.inspect(r, label: "Container non-atom tuple pair: R")

    left =
      case l do
        %PatternDiff{diff_result: :eq} ->
          match("#{inspect(l.rh)} => ")

        _ ->
          delete(inspect(l.rh))
      end

    right =
      case r do
        %PatternDiff{diff_result: :eq} ->
          match("#{inspect(r.rh)}")

        %ContainerDiff{} ->
          format(r)

        _ ->
          delete(inspect(r.rh))
      end

    [
      comma,
      left,
      right
    ]

    # |> IO.inspect(label: "Returning")
  end

  def format(%ContainerDiff{type: :tuple, items: [l, r]}, %{keys: :atom_keys, comma: comma}) do
    IO.inspect("Container keyword tuple")

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
          match("#{inspect(r.rh)}")

        %ContainerDiff{} ->
          format(r)

        _ ->
          delete(inspect(r.rh))
      end

    [
      comma,
      left,
      right
    ]
  end

  def format(diff, nil) do
    IO.inspect("Format with a nil context")
    # Jowens - extract context to it's own deal, this is repeated 3 times now
    format(diff, %{comma: "", keys: :none, missing: false, print_when?: true})
  end

  def format(%PatternDiff{diff_result: :eq, rh: rh}, %{comma: comma}) do
    IO.inspect("pattern equal")
    [comma, match(inspect(rh))]
  end

  def format(%PatternDiff{diff_result: :neq, rh: @no_value, lh: lh}, %{comma: comma} = ctx) do
    # IO.inspect(lh, label: "pattern not equal, missing right")
    # IO.inspect(ctx, label: "context")
    [comma, insert(textify_ast(lh, ctx))]
  end

  def format(%PatternDiff{diff_result: :neq, rh: {key, value}, lh: @no_value}, %{
        comma: comma,
        keys: :atom_keys
      }) do
    IO.inspect("pattern not equal, missing left")
    [comma, match("#{to_string(key)}: #{inspect(value)}")]
  end

  def format(%PatternDiff{diff_result: :neq, rh: {key, value}, lh: @no_value}, %{
        comma: comma,
        keys: :non_atom_keys
      }) do
    IO.inspect("pattern not equal, missing left")
    [comma, "#{inspect(key)} => #{inspect(value)}"]
  end

  def format(%PatternDiff{diff_result: :neq, rh: rh, lh: _lh}, %{comma: comma} = ctx) do
    # IO.inspect(ctx, label: "pattern not equal, but not part of match")
    [comma, delete(inspect(rh))]
  end

  def format(%WhenDiff{result: res, op: :and, bindings: keys}, ctx) do
    IO.inspect("When Diff")
    [left, right] = keys
    prefix = if ctx.print_when?, do: "when ", else: ""
    ctx = %{ctx | print_when?: false}
    and_val = " and "
    and_val = if res == :neq, do: delete(and_val), else: and_val
    [prefix, format(left, ctx), and_val, format(right, ctx)]
  end

  def format(%WhenDiff{result: res, op: :or, bindings: keys}, ctx) do
    [left, right] = keys
    ctx = %{ctx | print_when?: false}
    or_val = " or "
    or_val = if res == :neq, do: delete(or_val), else: or_val
    [format(left, ctx), or_val, format(right, ctx)]
  end

  def format(%WhenDiff{result: res, op: op, bindings: keys}, %{print_when?: print_when?}) do
    [{key, _value}] = keys
    prefix = if print_when?, do: "when ", else: ""
    ret = "#{prefix}#{to_string(op)}(#{key})"
    ret = if res == :neq, do: delete(ret), else: ret
    [ret]
  end

  # def format(%PatternDiff{diff_result: :neq, rh: rh, lh: lh})
  def format(other, context) do
    IO.inspect(context, label: "context")
    IO.inspect(other, label: "No matching format")
  end

  defp create_context(%ContainerDiff{type: :list, items: items}) do
    if Enum.all?(items, &keyword_tuple?/1),
      do: %{keys: :atom_keys, missing: false, comma: "", print_when?: true},
      else: %{keys: :list, missing: false, comma: "", print_when?: true}
  end

  defp create_context(%ContainerDiff{type: :map, items: items}) do
    keys = if Enum.all?(items, &keyword_tuple?/1), do: :atom_keys, else: :non_atom_keys
    missing = Enum.all?(items, &right_missing_keys?/1)

    %{keys: keys, missing: missing, comma: "", print_when?: true}
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

  defp textify_ast(%{ast: ast}, ctx), do: textify_ast(ast, ctx)

  defp textify_ast({:^, _, [{var, _, _}]}, ctx) do
    # IO.inspect(var, label: "var")
    # IO.inspect(ctx, label: "ctx")
    "^#{var}"
  end

  defp textify_ast({:%{}, _, elements}, ctx) do
    inspect(Map.new(Enum.map(elements, &textify_ast(&1, ctx))))
  end

  defp textify_ast({key, value}, %{keys: :atom_keys} = ctx) when is_atom(key) do
    "#{key}: #{textify_ast(value, ctx)}"
  end

  defp textify_ast({key, value}, %{keys: :non_atom_keys} = ctx) when is_atom(key) do
    "#{key} => #{textify_ast(value, ctx)}"
  end

  defp textify_ast(elem, _ctx) when is_atom(elem), do: elem
  defp textify_ast(elem, _ctx), do: inspect(elem)

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
