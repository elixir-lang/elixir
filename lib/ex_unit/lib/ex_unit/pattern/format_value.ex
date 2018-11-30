defmodule ExUnit.Pattern.FormatValue do
  @moduledoc """
  Formats the output of Pattern diff.
  1) Matches will be printed in green, conflicts in red, and neutral in the default color
  """

  alias ExUnit.{ContainerDiff, PatternDiff, PinDiff, WhenDiff}
  alias ExUnit.Pattern.DiffContext

  @no_value :ex_unit_no_meaningful_value

  def script(left, right) do
    ctx = DiffContext.new_context(:none, left)

    left
    |> ExUnit.PatternDiff.compare(right)
    |> format(ctx)
    |> Enum.reject(&(&1 == ""))
  end

  def format(
        %ContainerDiff{type: :map, items: items},
        %{keys: key_type} = ctx
      )
      when key_type == :atom_keys or key_type == :non_atom_keys do
    [
      "%{",
      items
      |> Enum.map(&format(&1, ctx))
      |> Enum.intersperse(", "),
      "}"
    ]
    |> List.flatten()
  end

  def format(%ContainerDiff{type: :map, items: _items} = diff, ctx) do
    map_ctx = DiffContext.create_context(diff, ctx)

    [
      diff
      |> format(map_ctx)
      |> Enum.intersperse(", ")
    ]
    |> List.flatten()
  end

  def format(
        %ContainerDiff{type: :list, items: [%{lh: %{type: :cons_l}} = l, r]},
        ctx
      ) do
    new_ctx = DiffContext.new_context(:none, ctx)

    ["[", format(l, new_ctx), " | ", format(r, new_ctx), "]"]
    |> List.flatten()
  end

  def format(
        %ContainerDiff{type: :list, items: items},
        %{keys: :list} = ctx
      ) do
    [
      "[",
      items
      |> Enum.map(&format(&1, ctx))
      |> Enum.intersperse(", "),
      "]"
    ]
    |> List.flatten()
  end

  def format(%ContainerDiff{type: :when, items: [l, r]}, ctx) do
    [format(l, ctx), " ", format(r, ctx)]
    |> List.flatten()
  end

  def format(%ContainerDiff{type: :list, items: []}, _ctx) do
    ["[]"]
  end

  def format(%ContainerDiff{type: :list, items: _items} = diff, ctx) do
    ctx = DiffContext.create_context(diff, ctx)

    diff
    |> format(ctx)
    |> Enum.intersperse(", ")
    |> List.flatten()
  end

  def format(%ContainerDiff{type: :tuple, items: items}, %{keys: :none} = ctx) do
    [
      "{",
      items
      |> Enum.map(&format(&1, ctx)),
      "}"
    ]
    |> List.flatten()
  end

  def format(
        %ContainerDiff{type: :tuple, items: [l, r]},
        %{keys: :non_atom_keys} = ctx
      ) do
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
          new_ctx = DiffContext.new_context(r, ctx)
          format(r, new_ctx)

        _ ->
          delete(inspect(r.rh))
      end

    [
      left,
      right
    ]
  end

  def format(%ContainerDiff{type: :tuple, items: [l, r]}, %{keys: :atom_keys} = ctx) do
    left =
      case l do
        %PatternDiff{diff_result: :eq} ->
          match("#{l.rh}: ")

        %ContainerDiff{} ->
          new_ctx = DiffContext.create_context(r, ctx)
          delete(format(l, new_ctx))

        _ ->
          delete(inspect(l.rh))
      end

    right =
      case r do
        %PatternDiff{diff_result: :eq} ->
          match("#{inspect(r.rh)}")

        %ContainerDiff{} ->
          new_ctx = DiffContext.create_context(r, ctx)
          format(r, new_ctx)

        _ ->
          delete(inspect(r.rh))
      end

    [
      left,
      right
    ]
  end

  def format(diff, nil) do
    format(diff, DiffContext.new_context(:none, [], []))
  end

  def format(%PatternDiff{diff_result: :eq, rh: rh}, _ctx) do
    [match(inspect(rh))]
  end

  def format(%PatternDiff{diff_result: :neq, rh: @no_value, lh: lh}, ctx) do
    [insert(textify_ast(lh, ctx))]
  end

  def format(%PatternDiff{diff_result: :neq, rh: {key, value}, lh: @no_value}, %{
        keys: :atom_keys
      }) do
    [match("#{to_string(key)}: #{inspect(value)}")]
  end

  def format(%PatternDiff{diff_result: :neq, rh: {key, value}, lh: @no_value}, %{
        keys: :non_atom_keys
      }) do
    ["#{inspect(key)} => #{inspect(value)}"]
  end

  def format(%PatternDiff{diff_result: :neq, rh: rh, lh: _lh}, _ctx) do
    [delete(inspect(rh))]
  end

  def format(%WhenDiff{result: res, op: :and, bindings: keys}, ctx) do
    [left, right] = keys
    prefix = if ctx.print_when?, do: "when ", else: ""
    ctx = %{ctx | print_when?: false}
    and_val = " and "
    and_val = if res == :neq, do: delete(and_val), else: and_val
    [prefix, format(left, ctx), and_val, format(right, ctx)]
  end

  def format(%WhenDiff{result: res, op: :or, bindings: keys}, ctx) do
    [left, right] = keys
    prefix = if ctx.print_when?, do: "when ", else: ""
    ctx = %{ctx | print_when?: false}
    or_val = " or "
    or_val = if res == :neq, do: delete(or_val), else: or_val
    [prefix, format(left, ctx), or_val, format(right, ctx)]
  end

  def format(%WhenDiff{result: res, op: op, bindings: keys}, %{print_when?: print_when?}) do
    [{{key, _}, _value}] = Map.to_list(keys)
    prefix = if print_when?, do: "when ", else: ""
    ret = "#{prefix}#{to_string(op)}(#{key})"
    ret = if res == :neq, do: delete(ret), else: ret
    [ret]
  end

  # def format(%PatternDiff{diff_result: :neq, rh: rh, lh: lh})
  def format(%PinDiff{diff: diff}, context) do
    format(diff, context)
  end

  def format(other, context) do
    IO.inspect(context, label: "context")
    IO.inspect(other, label: "No matching format")
    raise "Missing format for #{other}-#{context}"
  end

  defp textify_ast(%{ast: ast}, ctx) do
    textify_ast(ast, ctx)
  end

  defp textify_ast({:^, _, [{var, _, _}]}, ctx) do
    inspect(ctx.pins[var])
  end

  defp textify_ast({:%{}, _, _elements} = map, _ctx) do
    Macro.to_string(map)
  end

  defp textify_ast({key, value}, %{keys: :atom_keys} = ctx) when is_atom(key) do
    "#{key}: #{textify_ast(value, ctx)}"
  end

  defp textify_ast({key, value}, %{keys: :non_atom_keys} = ctx) do
    "#{inspect(key)} => #{textify_ast(value, ctx)}"
  end

  defp textify_ast(elem, _ctx) do
    Macro.to_string(elem)
  end

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
