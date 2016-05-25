defmodule ExUnit.Diff do
  @moduledoc false

  @doc """
  Returns an edit script representing the difference between `left` and `right`.

  Returns `nil` if they are not the same data type,
  or if the given data type is not supported.
  """
  def script(left, right)

  # Binaries
  def script(left, right) when is_binary(left) and is_binary(right) do
    if String.printable?(left) and String.printable?(right) do
      length1 = String.length(left)
      length2 = String.length(right)
      if bag_distance(left, right) / max(length1, length2) <= 0.6 do
        left = Inspect.BitString.escape(left, ?\")
        right = Inspect.BitString.escape(right, ?\")
        [{:eq, "\""}, script_string(left, right), {:eq, "\""}]
      end
    end
  end

  # Structs
  def script(%name{} = left, %name{} = right) do
    left = Map.from_struct(left)
    right = Map.from_struct(right)
    script_map(left, right, inspect(name))
  end

  # Maps
  def script(%{} = left, %{} = right) do
    if match?(%_{}, left) or match?(%_{}, right) do
      nil
    else
      script_map(left, right, "")
    end
  end

  # Char lists and lists
  def script(left, right) when is_list(left) and is_list(right) do
    if Inspect.List.printable?(left) and Inspect.List.printable?(right) do
      left = List.to_string(left) |> Inspect.BitString.escape(?')
      right = List.to_string(right) |> Inspect.BitString.escape(?')
      length1 = String.length(left)
      length2 = String.length(left)

      if bag_distance(left, right) / max(length1, length2) <= 0.6 do
        [{:eq, "'"}, script_string(left, right), {:eq, "'"}]
      end
    else
      keyword? = Inspect.List.keyword?(left) and Inspect.List.keyword?(right)
      script_list(left, right, keyword?, [])
    end
  end

  # Numbers
  def script(left, right)
      when is_integer(left) and is_integer(right)
      when is_float(left) and is_float(right) do
    script_string(inspect(left), inspect(right))
  end

  # Tuples
  def script(left, right)
      when is_tuple(left) and is_tuple(right) do
    left = {left, tuple_size(left) - 1}
    right = {right, tuple_size(right) - 1}
    script_tuple(left, right, [])
  end

  def script(_left, _right), do: nil

  defp script_string(string1, string2) do
    String.myers_difference(string1, string2)
  end

  # The algorithm is outlined in the
  # "String Matching with Metric Trees Using an Approximate Distance"
  # paper by Ilaria Bartolini, Paolo Ciaccia, and Marco Patella.
  defp bag_distance(string1, string2) do
    bag1 = string_to_bag(string1)
    bag2 = string_to_bag(string2)

    diff1 = bag_difference(bag1, bag2)
    diff2 = bag_difference(bag2, bag1)

    max(diff1, diff2)
  end

  defp string_to_bag(string) do
    string_to_bag(string, %{}, &(&1 + 1))
  end

  defp string_to_bag(string, bag, fun) do
    case String.next_grapheme(string) do
      {char, rest} ->
        bag = Map.update(bag, char, 1, fun)
        string_to_bag(rest, bag, fun)
      nil ->
        bag
    end
  end

  defp bag_difference(bag1, bag2) do
    Enum.reduce(bag1, 0, fn {char, count1}, sum ->
      case Map.fetch(bag2, char) do
        {:ok, count2} ->
          sum + max(count1 - count2, 0)
        :error ->
          sum + count1
      end
    end)
  end

  defp script_list([], [], _keyword?, acc) do
    [[_ | elem_diff] | rest] = Enum.reverse(acc)
    [{:eq, "["}, [elem_diff | rest], {:eq, "]"}]
  end

  defp script_list([], [elem | rest], keyword?, acc) do
    elem_diff = [ins: format_list_elem(elem, keyword?)]
    script_list([], rest, keyword?, [[ins: ", "] ++ elem_diff | acc])
  end

  defp script_list([elem | rest], [], keyword?, acc) do
    elem_diff = [del: format_list_elem(elem, keyword?)]
    script_list(rest, [], keyword?, [[del: ", "] ++ elem_diff | acc])
  end

  defp script_list([elem | rest1], [elem | rest2], keyword?, acc) do
    elem_diff = [eq: format_list_elem(elem, keyword?)]
    script_list(rest1, rest2, keyword?, [[eq: ", "] ++ elem_diff | acc])
  end

  defp script_list([{key1, val1} | rest1], [{key2, val2} | rest2], true, acc) do
    key_diff =
      if key1 != key2 do
        script_string(Atom.to_string(key1), Atom.to_string(key2))
      else
        [eq: Atom.to_string(key1)]
      end
    value_diff = script_inner(val1, val2)
    elem_diff = [[key_diff, {:eq, ": "}], value_diff]
    script_list(rest1, rest2, true, [[eq: ", "] ++ elem_diff | acc])
  end

  defp script_list([elem1 | rest1], [elem2 | rest2], false, acc) do
    elem_diff = script_inner(elem1, elem2)
    script_list(rest1, rest2, false, [[eq: ", "] ++ elem_diff | acc])
  end

  defp script_list(last, [elem | rest], keyword?, acc) do
    joiner_diff = [del: " |", ins: ",", eq: " "]
    elem_diff = script_inner(last, elem)
    new_acc = [joiner_diff ++ elem_diff | acc]
    script_list([], rest, keyword?, new_acc)
  end

  defp script_list([elem | rest], last, keyword?, acc) do
    joiner_diff = [del: ",", ins: " |", eq: " "]
    elem_diff = script_inner(elem, last)
    new_acc = [joiner_diff ++ elem_diff | acc]
    script_list(rest, [], keyword?, new_acc)
  end

  defp script_list(last1, last2, keyword?, acc) do
    elem_diff =
      cond do
        last1 == [] ->
          [ins: " | " <> inspect(last2)]
        last2 == [] ->
          [del: " | " <> inspect(last1)]
        true ->
          [eq: " | "] ++ script_inner(last1, last2)
      end
    script_list([], [], keyword?, [elem_diff | acc])
  end

  defp format_list_elem(elem, false), do: inspect(elem)

  defp format_list_elem({key, val}, true) do
    format_key_value(Atom.to_string(key), inspect(val), true)
  end

  defp script_tuple({_tuple1, -1}, {_tuple2, -1}, acc) do
    [[_ | elem_diff] | rest] = acc
    [{:eq, "{"}, [elem_diff | rest], {:eq, "}"}]
  end

  defp script_tuple({tuple1, index1}, {_, index2} = right, acc)
      when index1 > index2 do
    elem = elem(tuple1, index1)
    elem_diff = [del: ", ", del: inspect(elem)]
    script_tuple({tuple1, index1 - 1}, right, [elem_diff | acc])
  end

  defp script_tuple({_, index1} = left, {tuple2, index2}, acc)
      when index1 < index2 do
    elem = elem(tuple2, index2)
    elem_diff = [ins: ", ", ins: inspect(elem)]
    script_tuple(left, {tuple2, index2 - 1}, [elem_diff | acc])
  end

  defp script_tuple({tuple1, index}, {tuple2, index}, acc) do
    elem1 = elem(tuple1, index)
    elem2 = elem(tuple2, index)
    elem_diff = script_inner(elem1, elem2)
    script_tuple({tuple1, index - 1}, {tuple2, index - 1}, [[eq: ", "] ++ elem_diff | acc])
  end

  defp script_map(left, right, name) do
    {surplus, altered, missing, same} = map_difference(left, right)

    keyword? =
      Inspect.List.keyword?(surplus) and
      Inspect.List.keyword?(altered) and
      Inspect.List.keyword?(missing) and
      Inspect.List.keyword?(same)

    result = Enum.reduce(same, [], fn({key, val}, acc) ->
      map_pair = format_key_value(inspect(key), inspect(val), keyword?)
      [[eq: ", ", eq: map_pair] | acc]
    end)
    result = Enum.reduce(missing, result, fn({key, val}, acc) ->
      map_pair = format_key_value(inspect(key), inspect(val), keyword?)
      [[ins: ", ", ins: map_pair] | acc]
    end)
    result = Enum.reduce(surplus, result, fn({key, val}, acc) ->
      map_pair = format_key_value(inspect(key), inspect(val), keyword?)
      [[del: ", ", del: map_pair] | acc]
    end)
    result = Enum.reduce(altered, result, fn({key, {val1, val2}}, acc) ->
      value_diff = script_inner(val1, val2)
      [[{:eq, ", "}, script_key(key, keyword?), value_diff] | acc]
    end)
    [[_ | elem_diff] | rest] = result
    [{:eq, "%" <> name <> "{"}, [elem_diff | rest], {:eq, "}"}]
  end

  defp map_difference(map1, map2) do
    {surplus, altered, same} =
      Enum.reduce(map1, {[], [], []}, fn({key, val1}, {surplus, altered, same}) ->
        case Map.fetch(map2, key) do
          {:ok, ^val1} ->
            {surplus, altered, [{key, val1} | same]}
          {:ok, val2} ->
            {surplus, [{key, {val1, val2}} | altered], same}
          :error ->
            {[{key, val1} | surplus], altered, same}
        end
      end)
    missing = Enum.reduce(map2, [], fn({key, _} = pair, acc) ->
      if Map.has_key?(map1, key), do: acc, else: [pair | acc]
    end)
    {surplus, altered, missing, same}
  end

  defp script_key(key, false) do
    [eq: inspect(key) <> " => "]
  end

  defp script_key(key, true) do
    [eq: Atom.to_string(key) <> ": "]
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

  defp script_inner(term, term) do
    [eq: inspect(term)]
  end

  defp script_inner(left, right) do
    if result = script(left, right) do
      result
    else
      [del: inspect(left), ins: inspect(right)]
    end
  end
end
