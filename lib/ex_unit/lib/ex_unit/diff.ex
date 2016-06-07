defmodule ExUnit.Diff do
  @moduledoc false

  @doc """
  Returns an edit script representing the difference between `left` and `right`.

  Returns `nil` if they are not the same data type,
  or if the given data type is not supported.
  """
  def script(left, right)

  def script(term, term)
      when is_binary(term) or is_number(term)
      when is_map(term) or is_list(term) or is_tuple(term) do
    [eq: inspect(term)]
  end

  # Binaries
  def script(left, right) when is_binary(left) and is_binary(right) do
    if String.printable?(left) and String.printable?(right) do
      script_string(left, right, ?\")
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
    cond do
      Inspect.List.printable?(left) and Inspect.List.printable?(right) ->
        script_string(List.to_string(left), List.to_string(right), ?')
      Inspect.List.keyword?(left) and Inspect.List.keyword?(right) ->
        script_keyword(left, right)
      true ->
        script_list(left, right, [])
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

  defp script_string(string1, string2, token) do
    length1 = String.length(string1)
    length2 = String.length(string2)
    if bag_distance(string1, string2) / max(length1, length2) <= 0.6 do
      string1 = Inspect.BitString.escape(string1, token)
      string2 = Inspect.BitString.escape(string2, token)
      [{:eq, <<token>>}, script_string(string1, string2), {:eq, <<token>>}]
    end
  end

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

  defp script_keyword(list1, list2) do
    path = {0, 0, list1, list2, []}
    result =
      find_script(0, length(list1) + length(list2), [path])
      |> format_each_fragment([])
    [{:eq, "["}, result, {:eq, "]"}]
  end

  defp format_each_fragment([{:diff, script}], []),
    do: script

  defp format_each_fragment([{kind, elems}], []),
    do: [format_fragment(kind, elems)]

  defp format_each_fragment([_, _] = fragments, acc) do
    result =
      case fragments do
        [diff: script1, diff: script2] ->
          [script1, {:eq, ", "}, script2]

        [{:diff, script}, {kind, elems}] ->
          [script, {kind, ", "}, format_fragment(kind, elems)]

        [{kind, elems}, {:diff, script}] ->
          [format_fragment(kind, elems), {kind, ", "}, script]

        [del: elems1, ins: elems2] ->
          [format_fragment(:del, elems1), format_fragment(:ins, elems2)]

        [{:eq, elems1}, {kind, elems2}] ->
          [format_fragment(:eq, elems1), {kind, ", "}, format_fragment(kind, elems2)]

        [{kind, elems1}, {:eq, elems2}] ->
          [format_fragment(kind, elems1), {kind, ", "}, format_fragment(:eq, elems2)]
      end
    Enum.reverse(acc, result)
  end

  defp format_each_fragment([{:diff, script} | rest], acc) do
    format_each_fragment(rest, [{:eq, ", "}, script | acc])
  end

  defp format_each_fragment([{kind, elems} | rest], acc) do
    new_acc = [{kind, ", "}, format_fragment(kind, elems) | acc]
    format_each_fragment(rest, new_acc)
  end

  defp format_fragment(kind, elems) do
    formatter = fn {key, val} ->
      format_key_value(key, val, true)
    end
    {kind, Enum.map_join(elems, ", ", formatter)}
  end

  defp find_script(envelope, max, _paths) when envelope > max do
    nil
  end

  defp find_script(envelope, max, paths) do
    case each_diagonal(-envelope, envelope, paths, []) do
      {:done, edits} ->
        compact_reverse(edits, [])
      {:next, paths} -> find_script(envelope + 1, max, paths)
    end
  end

  defp compact_reverse([], acc),
    do: acc

  defp compact_reverse([{:diff, _} = fragment | rest], acc),
    do: compact_reverse(rest, [fragment | acc])

  defp compact_reverse([{kind, char} | rest], [{kind, chars} | acc]),
    do: compact_reverse(rest, [{kind, [char | chars]} | acc])

  defp compact_reverse([{kind, char} | rest], acc),
    do: compact_reverse(rest, [{kind, [char]} | acc])

  defp each_diagonal(diag, limit, _paths, next_paths) when diag > limit do
    {:next, Enum.reverse(next_paths)}
  end

  defp each_diagonal(diag, limit, paths, next_paths) do
    {path, rest} = proceed_path(diag, limit, paths)
    with {:cont, path} <- follow_snake(path) do
      each_diagonal(diag + 2, limit, rest, [path | next_paths])
    end
  end

  defp proceed_path(0, 0, [path]), do: {path, []}

  defp proceed_path(diag, limit, [path | _] = paths) when diag == -limit do
    {move_down(path), paths}
  end

  defp proceed_path(diag, limit, [path]) when diag == limit do
    {move_right(path), []}
  end

  defp proceed_path(_diag, _limit, [path1, path2 | rest]) do
    if elem(path1, 1) > elem(path2, 1) do
      {move_right(path1), [path2 | rest]}
    else
      {move_down(path2), [path2 | rest]}
    end
  end

  defp script_keyword_inner({key, val1}, {key, val2}),
    do: [{:eq, format_key(key, true)}, script_inner(val1, val2)]

  defp script_keyword_inner(_pair1, _pair2),
    do: nil

  defp move_right({x, x, [elem1 | rest1] = list1, [elem2 | rest2], edits}) do
    if result = script_keyword_inner(elem1, elem2) do
      {x + 1, x + 1, rest1, rest2, [{:diff, result} | edits]}
    else
      {x + 1, x, list1, rest2, [{:ins, elem2} | edits]}
    end
  end

  defp move_right({x, y, list1, [elem | rest], edits}) do
    {x + 1, y, list1, rest, [{:ins, elem} | edits]}
  end

  defp move_right({x, y, list1, [], edits}) do
    {x + 1, y, list1, [], edits}
  end

  defp move_down({x, x, [elem1 | rest1], [elem2 | rest2] = list2, edits}) do
    if result = script_keyword_inner(elem1, elem2) do
      {x + 1, x + 1, rest1, rest2, [{:diff, result} | edits]}
    else
      {x, x + 1, rest1, list2, [{:del, elem1} | edits]}
    end
  end

  defp move_down({x, y, [elem | rest], list2, edits}) do
    {x, y + 1, rest, list2, [{:del, elem} | edits]}
  end

  defp move_down({x, y, [], list2, edits}) do
    {x, y + 1, [], list2, edits}
  end

  defp follow_snake({x, y, [elem | rest1], [elem | rest2], edits}) do
    follow_snake({x + 1, y + 1, rest1, rest2, [{:eq, elem} | edits]})
  end

  defp follow_snake({_x, _y, [], [], edits}) do
    {:done, edits}
  end

  defp follow_snake(path) do
    {:cont, path}
  end

  defp script_list([], [], acc) do
    [[_ | elem_diff] | rest] = Enum.reverse(acc)
    [{:eq, "["}, [elem_diff | rest], {:eq, "]"}]
  end

  defp script_list([], [elem | rest], acc) do
    elem_diff = [ins: inspect(elem)]
    script_list([], rest, [[ins: ", "] ++ elem_diff | acc])
  end

  defp script_list([elem | rest], [], acc) do
    elem_diff = [del: inspect(elem)]
    script_list(rest, [], [[del: ", "] ++ elem_diff | acc])
  end

  defp script_list([elem | rest1], [elem | rest2], acc) do
    elem_diff = [eq: inspect(elem)]
    script_list(rest1, rest2, [[eq: ", "] ++ elem_diff | acc])
  end

  defp script_list([elem1 | rest1], [elem2 | rest2], acc) do
    elem_diff = script_inner(elem1, elem2)
    script_list(rest1, rest2, [[eq: ", "] ++ elem_diff | acc])
  end

  defp script_list(last, [elem | rest], acc) do
    joiner_diff = [del: " |", ins: ",", eq: " "]
    elem_diff = script_inner(last, elem)
    new_acc = [joiner_diff ++ elem_diff | acc]
    script_list([], rest, new_acc)
  end

  defp script_list([elem | rest], last, acc) do
    joiner_diff = [del: ",", ins: " |", eq: " "]
    elem_diff = script_inner(elem, last)
    new_acc = [joiner_diff ++ elem_diff | acc]
    script_list(rest, [], new_acc)
  end

  defp script_list(last1, last2, acc) do
    elem_diff =
      cond do
        last1 == [] ->
          [ins: " | " <> inspect(last2)]
        last2 == [] ->
          [del: " | " <> inspect(last1)]
        true ->
          [eq: " | "] ++ script_inner(last1, last2)
      end
    script_list([], [], [elem_diff | acc])
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

    result = Enum.reduce(missing, [], fn({key, val}, acc) ->
      map_pair = format_key_value(key, val, keyword?)
      [[ins: ", ", ins: map_pair] | acc]
    end)
    result = Enum.reduce(surplus, result, fn({key, val}, acc) ->
      map_pair = format_key_value(key, val, keyword?)
      [[del: ", ", del: map_pair] | acc]
    end)
    result = Enum.reduce(altered, result, fn({key, {val1, val2}}, acc) ->
      value_diff = script_inner(val1, val2)
      [[{:eq, ", "}, {:eq, format_key(key, keyword?)}, value_diff] | acc]
    end)
    result = Enum.reduce(same, result, fn({key, val}, acc) ->
      map_pair = format_key_value(key, val, keyword?)
      [[eq: ", ", eq: map_pair] | acc]
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

  defp format_key(key, false) do
    inspect(key) <> " => "
  end

  defp format_key(key, true) do
    Atom.to_string(key) <> ": "
  end

  defp format_key_value(key, value, keyword?) do
    format_key(key, keyword?) <> inspect(value)
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
