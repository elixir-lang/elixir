defmodule ExUnit.Diff do
  @moduledoc false

  @doc """
  Formats the difference between `left` and `right`.

  Returns `nil` if they are not the same data type,
  or if the given data type is not supported.
  """
  def format(left, right, formatter)

  def format(left, right, formatter)
      when is_binary(left) and is_binary(right) do
    if String.printable?(left) and String.printable?(right) do
      left = Inspect.BitString.escape(left, ?\")
      right = Inspect.BitString.escape(right, ?\")
      "\"" <> format_string(left, right, formatter) <> "\""
    end
  end

  def format(%name{} = left, %name{} = right, formatter) do
    left = Map.from_struct(left)
    right = Map.from_struct(right)
    format_map(left, right, inspect(name), formatter)
  end

  def format(%{} = left, %{} = right, formatter) do
    if match?(%_{}, left) or match?(%_{}, right) do
      nil
    else
      format_map(left, right, "", formatter)
    end
  end

  def format(left, right, formatter) when is_list(left) and is_list(right) do
    if Inspect.List.printable?(left) and Inspect.List.printable?(right) do
      left = List.to_string(left) |> Inspect.BitString.escape(?')
      right = List.to_string(right) |> Inspect.BitString.escape(?')
      "'" <> format_string(left, right, formatter) <> "'"
    else
      keyword? = Inspect.List.keyword?(left) and Inspect.List.keyword?(right)
      format_list(left, right, formatter, keyword?, [])
    end
  end

  def format(left, right, formatter)
      when is_integer(left) and is_integer(right)
      when is_float(left) and is_float(right) do
    {kind, skew} =
      case to_string(right - left) do
        "-" <> _ = result ->
          {:diff_delete, result}
        result ->
          {:diff_insert, "+" <> result}
      end
    value_diff = formatter.(kind, "(off by " <> skew <> ")")
    format_string(inspect(left), inspect(right), formatter) <> " " <> value_diff
  end

  def format(left, right, formatter)
      when is_tuple(left) and is_tuple(right) do
    left = {left, tuple_size(left) - 1}
    right = {right, tuple_size(right) - 1}
    format_tuple(left, right, formatter, [])
  end

  def format(_left, _right, _formatter), do: nil

  defp format_string(string1, string2, formatter) do
    String.myers_difference(string1, string2)
    |> Enum.map_join(&format_fragment(&1, formatter))
  end

  defp format_list([], [], _formatter, _keyword?, acc) do
    result = Enum.reverse(acc)
    "[" <> Enum.join(result, ", ") <> "]"
  end

  defp format_list([], [elem | rest], formatter, keyword?, acc) do
    elem_diff = formatter.(:diff_insert, format_list_elem(elem, keyword?))
    format_list([], rest, formatter, keyword?, [elem_diff | acc])
  end

  defp format_list([elem | rest], [], formatter, keyword?, acc) do
    elem_diff = formatter.(:diff_delete, format_list_elem(elem, keyword?))
    format_list(rest, [], formatter, keyword?, [elem_diff | acc])
  end

  defp format_list([elem | rest1], [elem | rest2], formatter, keyword?, acc) do
    elem_diff = format_list_elem(elem, keyword?)
    format_list(rest1, rest2, formatter, keyword?, [elem_diff | acc])
  end

  defp format_list([{key1, val1} | rest1], [{key2, val2} | rest2], formatter, true, acc) do
    key_diff =
      if key1 != key2 do
        format_string(Atom.to_string(key1), Atom.to_string(key2), formatter)
      else
        Atom.to_string(key1)
      end
    value_diff = format_inner(val1, val2, formatter)
    elem_diff = format_key_value(key_diff, value_diff, true)
    format_list(rest1, rest2, formatter, true, [elem_diff | acc])
  end

  defp format_list([elem1 | rest1], [elem2 | rest2], formatter, false, acc) do
    elem_diff = format_inner(elem1, elem2, formatter)
    format_list(rest1, rest2, formatter, false, [elem_diff | acc])
  end

  defp format_list_elem(elem, false), do: inspect(elem)

  defp format_list_elem({key, val}, true) do
    format_key_value(Atom.to_string(key), inspect(val), true)
  end

  defp format_tuple({_tuple1, -1}, {_tuple2, -1}, _formatter, acc) do
    "{" <> Enum.join(acc, ", ") <> "}"
  end

  defp format_tuple({tuple1, index1}, {_, index2} = right, formatter, acc)
      when index1 > index2 do
    elem = elem(tuple1, index1)
    elem_diff = formatter.(:diff_delete, inspect(elem))
    format_tuple({tuple1, index1 - 1}, right, formatter, [elem_diff | acc])
  end

  defp format_tuple({_, index1} = left, {tuple2, index2}, formatter, acc)
      when index1 < index2 do
    elem = elem(tuple2, index2)
    elem_diff = formatter.(:diff_insert, inspect(elem))
    format_tuple(left, {tuple2, index2 - 1}, formatter, [elem_diff | acc])
  end

  defp format_tuple({tuple1, index}, {tuple2, index}, formatter, acc) do
    elem1 = elem(tuple1, index)
    elem2 = elem(tuple2, index)
    elem_diff =
      if elem1 != elem2 do
        format_inner(elem1, elem2, formatter)
      else
        inspect(elem1)
      end
    format_tuple({tuple1, index - 1}, {tuple2, index - 1}, formatter, [elem_diff | acc])
  end

  defp format_map(left, right, name, formatter) do
    {surplus, altered, missing} = map_difference(left, right)

    keyword? =
      Inspect.List.keyword?(surplus) and
      Inspect.List.keyword?(altered) and
      Inspect.List.keyword?(missing)

    result =
      if map_size(right) > length(altered) + length(missing),
        do: ["..."],
        else: []
    result = Enum.reduce(missing, result, fn({key, val}, acc) ->
      map_pair = format_key_value(inspect(key), inspect(val), keyword?)
      [formatter.(:diff_insert, map_pair) | acc]
    end)
    result = Enum.reduce(surplus, result, fn({key, val}, acc) ->
      map_pair = format_key_value(inspect(key), inspect(val), keyword?)
      [formatter.(:diff_delete, map_pair) | acc]
    end)
    result = Enum.reduce(altered, result, fn({key, {val1, val2}}, acc) ->
      value_diff = format_inner(val1, val2, formatter)
      [format_key_value(inspect(key), value_diff, keyword?) | acc]
    end)
    "%" <> name <> "{" <> Enum.join(result, ", ") <> "}"
  end

  defp map_difference(map1, map2) do
    {surplus, altered} =
      Enum.reduce(map1, {[], []}, fn({key, val1}, {surplus, altered} = acc) ->
        case Map.fetch(map2, key) do
          {:ok, ^val1} ->
            acc
          {:ok, val2} ->
            {surplus, [{key, {val1, val2}} | altered]}
          :error ->
            {[{key, val1} | surplus], altered}
        end
      end)
    missing = Enum.reduce(map2, [], fn({key, _} = pair, acc) ->
      if Map.has_key?(map1, key), do: acc, else: [pair | acc]
    end)
    {surplus, altered, missing}
  end

  defp format_key_value(key, value, false) do
    key <> " => " <> value
  end

  defp format_key_value(":" <> rest, value, true) do
    format_key_value(rest, value, true)
  end

  defp format_key_value(key, value, true) do
    key <> ": " <> value
  end

  defp format_inner(left, right, formatter) do
    if result = format(left, right, formatter) do
      result
    else
      formatter.(:diff_delete, inspect(left)) <>
      formatter.(:diff_insert, inspect(right))
    end
  end

  defp format_fragment({:eq, content}, _), do: content

  defp format_fragment({:del, content}, formatter) do
    formatter.(:diff_delete, content)
  end

  defp format_fragment({:ins, content}, formatter) do
    formatter.(:diff_insert, content)
  end
end
