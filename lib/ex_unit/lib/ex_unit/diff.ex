defmodule ExUnit.Diff do
  @moduledoc """
  A Diff struct and functions.

  The Diff struct contains the fields `:equivalent?`, `:left`, `:right`.
  The `:equivalent?` field represents if the `:left` and `:right` side are
  equivalents and contain no diffs. The `:left` and `:right` represent the sides
  of the comparison and contain ASTs with some special metas: `:diff` and
  `:diff_container`.

  When meta `:diff` is `true`, the AST inside of it has no equivalent on the
  other side and should be rendered in a different color. If the AST is a
  literal and doesn't contain meta, the `:diff` meta will be place in a wrapping
  block.

  The `:diff_container` meta appears in blocks and contains a list of strings
  that must be rendered in sequence and surrounded by the char in the value or
  nothing in case of `:number`.
  """

  alias Code.Identifier
  alias Inspect.Algebra

  defstruct equivalent?: true,
            left: nil,
            right: nil

  defmodule Env do
    @moduledoc """
    Defines the Diff.Env struct used to calculate the diffs between to sides
    and keep the context, the pins, and the vars evaluated during the comparison.
    """

    defstruct pins: %{},
              current_vars: %{},
              context: nil
  end

  @doc """
  Returns the diff between `left` and `right` and env after the comparison.

  The `left` side can be a literal or an AST, the `right` should be always a
  value. The `pins` are a keyword list with the name of the pin as key and its
  value. The `context` should be `:match` for pattern matching and `nil` or
  any other case.
  """
  def compare_quoted(left, right, pins, context) do
    env = %Env{pins: Map.new(pins), context: context}
    compare_quoted(left, right, env)
  end

  defp compare_quoted({:_, _, _} = left, right, env) do
    diff_right = value_to_result(right, false)
    diff = %__MODULE__{equivalent?: true, left: left, right: diff_right}
    {diff, env}
  end

  defp compare_quoted({:^, _, [{name, _, context}]} = left, right, env)
       when is_atom(name) and is_atom(context) do
    compare_pin(left, right, env)
  end

  defp compare_quoted({name, _, context} = left, right, env)
       when is_atom(name) and is_atom(context) do
    compare_var(left, right, env)
  end

  defp compare_quoted({:-, _, [number]}, right, env)
       when is_integer(number) and is_integer(right)
       when is_float(number) and is_float(right) do
    compare_quoted(-number, right, env)
  end

  defp compare_quoted({:+, _, [number]}, right, env)
       when is_integer(number) and is_integer(right)
       when is_float(number) and is_float(right) do
    compare_quoted(number, right, env)
  end

  defp compare_quoted(literal, literal, env)
       when is_atom(literal) or is_integer(literal) or is_float(literal) do
    diff = %__MODULE__{equivalent?: true, left: literal, right: literal}
    {diff, env}
  end

  defp compare_quoted(left, right, env)
       when is_integer(left) and is_integer(right)
       when is_float(left) and is_float(right) do
    compare_number(left, right, env)
  end

  defp compare_quoted({:++, _, _} = left, right, env) when is_list(right) do
    compare_maybe_improper_list(left, right, env)
  end

  defp compare_quoted(left, right, env) when is_list(left) and is_list(right) do
    compare_maybe_list(left, right, env)
  end

  defp compare_quoted({:{}, _, _} = left, right, env) when is_tuple(right) do
    compare_tuple(left, right, env)
  end

  defp compare_quoted({_, _} = left, right, env) when is_tuple(right) do
    compare_tuple(left, right, env)
  end

  defp compare_quoted(left, right, %{context: nil} = env)
       when is_tuple(left) and is_tuple(right) do
    compare_tuple(left, right, env)
  end

  defp compare_quoted({:%, _, [_, {:%{}, _, items}]} = left, %{} = right, env)
       when is_list(items) do
    compare_struct(left, right, env)
  end

  defp compare_quoted({:%{}, _, [{:__struct__, _} | _]} = left, %{} = right, env) do
    compare_struct(left, right, env)
  end

  defp compare_quoted({:%{}, _, items} = left, %struct{} = right, env) when is_list(items) do
    compare_map(left, right, nil, struct, env)
  end

  defp compare_quoted({:%{}, _, items} = left, %{} = right, env) when is_list(items) do
    compare_map(left, right, nil, nil, env)
  end

  defp compare_quoted(%_{} = left, %{} = right, env) do
    compare_struct(left, right, env)
  end

  defp compare_quoted(%{} = left, %{} = right, env) do
    compare_map(left, right, nil, nil, env)
  end

  defp compare_quoted(left, right, env) when is_binary(left) and is_binary(right) do
    compare_string(left, right, ?\", env)
  end

  defp compare_quoted({:<>, _, _} = left, right, env) when is_binary(right) do
    compare_string_concat(left, right, env)
  end

  defp compare_quoted({_, [{:expanded, expanded} | _], _} = left, right, env) do
    macro = Macro.update_meta(left, &Keyword.delete(&1, :expanded))
    compare_macro(macro, expanded, right, env)
  end

  defp compare_quoted(left, right, %Env{context: :match} = env) do
    diff_left = update_diff_meta(left, true)
    diff_right = value_to_result(right, true)
    diff = %__MODULE__{equivalent?: false, left: diff_left, right: diff_right}
    {diff, env}
  end

  defp compare_quoted(left, right, env) do
    diff_left = value_to_result(left, true)
    diff_right = value_to_result(right, true)
    diff = %__MODULE__{equivalent?: false, left: diff_left, right: diff_right}
    {diff, env}
  end

  # Macros

  defp compare_macro(macro, expanded, right, env) do
    {diff, post_env} = compare_quoted(expanded, right, env)

    diff_left = update_diff_meta(macro, !diff.equivalent?)
    {%{diff | left: diff_left}, post_env}
  end

  # Pins

  defp compare_pin({:^, _, [{name, _, _}]} = pin, right, %{pins: pins} = env) do
    %{^name => pin_value} = pins
    {diff, post_env} = compare_quoted(pin_value, right, env)

    diff_left = update_diff_meta(pin, !diff.equivalent?)
    {%{diff | left: diff_left}, post_env}
  end

  # Vars

  defp compare_var({name, meta, context} = left, right, env) do
    identifier = {name, meta[:counter] || context}

    case env.current_vars do
      %{^identifier => ^right} ->
        diff_right = value_to_result(right, false)
        diff = %__MODULE__{equivalent?: true, left: left, right: diff_right}
        {diff, env}

      %{^identifier => _} ->
        diff_left = update_diff_meta(left, true)
        diff_right = value_to_result(right, true)
        diff = %__MODULE__{equivalent?: false, left: diff_left, right: diff_right}
        {diff, env}

      current_vars = %{} ->
        updated_vars = Map.put(current_vars, identifier, right)
        diff_right = value_to_result(right, false)
        diff = %__MODULE__{equivalent?: true, left: left, right: diff_right}
        {diff, %{env | current_vars: updated_vars}}
    end
  end

  # Tuples

  def compare_tuple({:{}, _, left_list}, right, %{context: :match} = env) do
    compare_tuple_items(left_list, right, env)
  end

  def compare_tuple(left, right, env) do
    compare_tuple_items(Tuple.to_list(left), right, env)
  end

  def compare_tuple_items(list_left, right, env) do
    list_right = Tuple.to_list(right)

    {diff, list_post_env} = myers_difference_list(list_left, list_right, env)
    diff_left = {:{}, [], diff.left}
    diff_right = {:{}, [], diff.right}

    {%{diff | left: diff_left, right: diff_right}, list_post_env}
  end

  # Lists

  def compare_maybe_list(left, right, env) do
    if List.ascii_printable?(left) and List.ascii_printable?(right) do
      compare_string(List.to_string(left), List.to_string(right), ?', env)
    else
      compare_maybe_improper_list(left, right, env)
    end
  end

  # Compare two lists, removing all the operators (`|` and `++`) before and
  # adding them back in the end. When the `left` contains a improper element
  # it will extract forcefully a improper element on the `right` for matching
  # purposes.
  def compare_maybe_improper_list(left, right, env) do
    {parsed_left, improper_left, operators_left, length_left} = parse_list(left, 0)
    {parsed_right, improper_right, operators_right, _} = parse_list(right, 0)

    {parsed_right, improper_right, split?} =
      split_list(parsed_right, length_left, improper_right, improper_left)

    {parsed_diff, parsed_post_env} = myers_difference_list(parsed_left, parsed_right, env)

    {improper_diff, improper_post_env, improper_diff?} =
      compare_improper(improper_left, improper_right, parsed_post_env, split?)

    diff =
      merge_diff(parsed_diff, improper_diff, fn left1, left2, right1, right2 ->
        left = rebuild_list(left1, left2, operators_left, improper_diff?)

        right =
          if split? do
            rebuild_split_lists(right1, right2)
          else
            rebuild_list(right1, right2, operators_right, improper_diff?)
          end

        {left, right}
      end)

    {diff, improper_post_env}
  end

  defp compare_improper({:element, left}, {:element, right}, env, split?) do
    {diff, post_env} = compare_quoted(left, right, env)
    {diff, post_env, split?}
  end

  defp compare_improper({:element, left}, :empty, env, _split?) do
    diff_left = update_diff_meta(left, true)
    diff = %__MODULE__{equivalent?: false, left: diff_left}
    {diff, env, true}
  end

  defp compare_improper(:empty, {:element, right}, env, _split?) do
    diff_right = value_to_result(right, true)
    diff = %__MODULE__{equivalent?: false, right: diff_right}
    {diff, env, true}
  end

  defp compare_improper(:empty, :empty, env, _split?) do
    diff = %__MODULE__{equivalent?: true}
    {diff, env, false}
  end

  defp parse_list([], _index) do
    {[], :empty, nil, 0}
  end

  defp parse_list({:++, _, [left, right]}, _index) do
    {parsed_left, :empty, operators_left, length_left} = parse_list(left, 0)

    case parse_list(right, 0) do
      {:improper, improper} ->
        operators = {:++, length_left, [operators_left]}
        {parsed_left, {:element, improper}, operators, length_left}

      {parsed_right, improper_right, operators_right, length_right} ->
        operators = {:++, length_left, [operators_left, operators_right]}
        length = length_right + length_left
        {parsed_left ++ parsed_right, improper_right, operators, length}
    end
  end

  defp parse_list([{:|, _, [head, tail]}], index) do
    case parse_list(tail, 0) do
      {:improper, improper} ->
        operator = {:|, index, []}
        {[head], {:element, improper}, operator, 1}

      {parsed_tail, improper_tail, operators_tail, length_tail} ->
        operators = {:|, index, [operators_tail]}
        {[head | parsed_tail], improper_tail, operators, length_tail + 1}
    end
  end

  defp parse_list([head | tail], index) do
    case parse_list(tail, index + 1) do
      {:improper, improper} ->
        operator = {:|, index, []}
        {[head], {:element, improper}, operator, 1}

      {parsed_tail, improper_tail, operators_tail, length_tail} ->
        {[head | parsed_tail], improper_tail, operators_tail, length_tail + 1}
    end
  end

  defp parse_list(element, _index) do
    {:improper, element}
  end

  defp rebuild_list(list, _improper = nil, _operators = nil, _improper_diff?) do
    list
  end

  defp rebuild_list(list, improper, {:|, index, []}, _improper_diff?) do
    {left, [head]} = Enum.split(list, index)

    left ++ [{:|, [], [head, improper]}]
  end

  defp rebuild_list(list, improper, {:|, index, [operators]}, improper_diff?) do
    {left, [head | tail]} = Enum.split(list, index)

    rebuilt_tail = rebuild_list(tail, improper, operators, improper_diff?)

    rebuilt_tail =
      if is_nil(operators) do
        update_diff_meta(rebuilt_tail, improper_diff?)
      else
        rebuilt_tail
      end

    left ++ [{:|, [], [head, rebuilt_tail]}]
  end

  defp rebuild_list(list, improper, {:++, _index, [operators]}, _improper_diff?) do
    rebuilt_list = rebuild_list(list, nil, operators, false)

    {:++, [], [rebuilt_list, improper]}
  end

  defp rebuild_list(list, improper, {:++, index, operators}, improper_diff?) do
    [operators_left, operators_right] = operators
    {left, right} = Enum.split(list, index)

    rebuilt_left = rebuild_list(left, nil, operators_left, false)
    rebuilt_right = rebuild_list(right, improper, operators_right, improper_diff?)

    rebuilt_right =
      if is_nil(operators) do
        update_diff_meta(rebuilt_right, improper_diff?)
      else
        rebuilt_right
      end

    {:++, [], [rebuilt_left, rebuilt_right]}
  end

  defp split_list(list, index, :empty, {:element, _element}) do
    case Enum.split(list, index) do
      {left, []} -> {left, :empty, false}
      {left, right} -> {left, {:element, right}, true}
    end
  end

  defp split_list(list, _index, improper, _improper_left) do
    {list, improper, false}
  end

  defp rebuild_split_lists(left, right) do
    updated_right =
      case extract_diff_meta(right) do
        {list, true} -> Enum.map(list, &update_diff_meta(&1, true))
        {list, false} -> list
      end

    left ++ updated_right
  end

  def myers_difference_list(left, right, env) do
    path = {0, left, right, {[], [], env}}
    find_diff(0, length(left) + length(right), [path])
  end

  defp find_diff(envelope, max, paths) do
    case each_diagonal(-envelope, envelope, paths, []) do
      {:done, {edit1, edit2, env}} ->
        list_script_to_result(Enum.reverse(edit1), Enum.reverse(edit2), env)

      {:next, paths} ->
        find_diff(envelope + 1, max, paths)
    end
  end

  defp each_diagonal(diag, limit, _paths, next_paths) when diag > limit do
    {:next, Enum.reverse(next_paths)}
  end

  defp each_diagonal(diag, limit, paths, next_paths) do
    {path, rest} = proceed_path(diag, limit, paths)

    case follow_snake(path) do
      {:cont, path} -> each_diagonal(diag + 2, limit, rest, [path | next_paths])
      {:done, edits} -> {:done, edits}
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
    if elem(path1, 0) > elem(path2, 0) do
      {move_right(path1), [path2 | rest]}
    else
      {move_down(path2), [path2 | rest]}
    end
  end

  defp move_right({y, list1, [elem2 | rest2], {edit1, edit2, env}}) do
    {y, list1, rest2, {edit1, [{:ins, elem2} | edit2], env}}
  end

  defp move_right({y, list1, [], edits}) do
    {y, list1, [], edits}
  end

  defp move_down({y, [elem1 | rest1], list2, {edit1, edit2, env}}) do
    {y + 1, rest1, list2, {[{:del, elem1} | edit1], edit2, env}}
  end

  defp move_down({y, [], list2, edits}) do
    {y + 1, [], list2, edits}
  end

  defp follow_snake({y, [elem1 | rest1], [elem2 | rest2], {edit1, edit2, env}} = path) do
    {diff, post_env} = compare_quoted(elem1, elem2, env)

    if diff.equivalent? do
      new_edit1 = [{:eq, diff.left} | edit1]
      new_edit2 = [{:eq, diff.right} | edit2]

      follow_snake({y + 1, rest1, rest2, {new_edit1, new_edit2, post_env}})
    else
      {:cont, path}
    end
  end

  defp follow_snake({_y, [], [], edits}) do
    {:done, edits}
  end

  defp follow_snake(path) do
    {:cont, path}
  end

  defp list_script_to_result([], [], env) do
    diff = %__MODULE__{equivalent?: true, left: [], right: []}
    {diff, env}
  end

  defp list_script_to_result([{:del, elem1} | rest1], [{:ins, elem2} | rest2], env) do
    {elem_diff, elem_post_env} = compare_quoted(elem1, elem2, env)
    {rest_diff, rest_post_env} = list_script_to_result(rest1, rest2, elem_post_env)

    {prepend_diff(elem_diff, rest_diff), rest_post_env}
  end

  defp list_script_to_result([{:del, elem1} | rest1], list2, env) do
    diff_left = update_diff_meta(elem1, true)

    elem_diff = %__MODULE__{equivalent?: false, left: diff_left}
    {rest_diff, rest_post_env} = list_script_to_result(rest1, list2, env)

    {prepend_diff(elem_diff, rest_diff), rest_post_env}
  end

  defp list_script_to_result(list1, [{:ins, elem2} | rest2], env) do
    diff_right = value_to_result(elem2, true)

    elem_diff = %__MODULE__{equivalent?: false, right: diff_right}
    {rest_diff, rest_post_env} = list_script_to_result(list1, rest2, env)

    {prepend_diff(elem_diff, rest_diff), rest_post_env}
  end

  defp list_script_to_result([{:eq, elem1} | rest1], [{:eq, elem2} | rest2], env) do
    elem_diff = %__MODULE__{equivalent?: true, left: elem1, right: elem2}
    {rest_diff, rest_post_env} = list_script_to_result(rest1, rest2, env)

    {prepend_diff(elem_diff, rest_diff), rest_post_env}
  end

  # Maps

  defp compare_map(%{} = left, right, struct1, struct2, env) do
    compare_map_items(left, right, struct1, struct2, env)
  end

  defp compare_map({:%{}, _, items}, right, struct1, struct2, env) do
    compare_map_items(items, right, struct1, struct2, env)
  end

  # Compare items based on the keys of `left_items` and add the `:diff` meta to
  # the element that it wasn't able to compare.
  defp compare_map_items(left_items, right, struct1, struct2, env) do
    {non_comparable_by_key, remaining, compared, struct1, by_key_post_env} =
      compare_map_items_by_key(left_items, right, struct1, env)

    remaining_diff = compare_map_remaining_pairs(non_comparable_by_key, remaining, env)

    struct_diff = build_struct_result(struct1, struct2)
    map_diff = build_map_result(compared, remaining_diff)

    {prepend_diff(struct_diff, map_diff), by_key_post_env}
  end

  defp compare_map_items_by_key(items, right, defined_struct, env) do
    {non_comparable, remaining, compared, items_struct, post_env} =
      Enum.reduce(items, {[], right, [], nil, env}, fn
        {:__struct__, name}, acc ->
          put_elem(acc, 3, name)

        {key, _} = item, {non_comparable, remaining, compared, struct, acc_env} ->
          literal_key = literal_key(key, env)

          case Map.pop(remaining, literal_key) do
            {nil, ^remaining} ->
              {[item | non_comparable], remaining, [nil | compared], struct, acc_env}

            {popped, new_remaining} ->
              {diff, diff_post_env} = compare_map_pair(item, {literal_key, popped}, acc_env)
              {non_comparable, new_remaining, [diff | compared], struct, diff_post_env}
          end
      end)

    non_comparable = Enum.reverse(non_comparable)
    remaining = Map.delete(remaining, :__struct__)
    compared = Enum.reverse(compared)

    defined_struct =
      case defined_struct || items_struct do
        {_, [{:expanded, name} | _], _} -> name
        other -> other
      end

    {non_comparable, remaining, compared, defined_struct, post_env}
  end

  defp compare_map_pair({key1, value1}, {key2, value2}, env) do
    {diff, post_env} = compare_quoted(value1, value2, env)
    diff_left = {key1, diff.left}
    diff_right = {key2, diff.right}

    {%{diff | left: diff_left, right: diff_right}, post_env}
  end

  defp literal_key({:^, _, [{name, _, _}]}, %{pins: pins}) do
    %{^name => pin_value} = pins
    pin_value
  end

  defp literal_key(literal, _env) do
    literal
  end

  # Can't compare using `myers_difference_list` because if key and value are
  # equivalent, it gives strange results. It just mark them as different
  # depending on the context, if `:match` only left side, otherwise both sides.
  defp compare_map_remaining_pairs(remaining, right, env) do
    list_left = Enum.map(remaining, &update_diff_meta(&1, true))

    list_right =
      if env.context == :match do
        Map.to_list(right)
      else
        Enum.map(right, &update_diff_meta(&1, true))
      end

    diff_left = {:%{}, [], list_left}
    diff_right = {:%{}, [], list_right}

    equivalent? =
      if env.context == :match do
        remaining == []
      else
        remaining == [] && right == %{}
      end

    %__MODULE__{equivalent?: equivalent?, left: diff_left, right: diff_right}
  end

  defp build_map_result([], remaining_diff) do
    remaining_diff
  end

  defp build_map_result([nil | tail], remaining_diff) do
    {popped, new_remaining_diff} = pop_diff(remaining_diff)
    tail_result = build_map_result(tail, new_remaining_diff)
    prepend_diff(popped, tail_result)
  end

  defp build_map_result([head | tail], remaining_diff) do
    tail_result = build_map_result(tail, remaining_diff)
    prepend_diff(head, tail_result)
  end

  # Structs

  defp compare_struct({:%, _, [struct1, left_map]}, %struct2{} = right, env) do
    compare_struct(left_map, Map.from_struct(right), struct1, struct2, env)
  end

  defp compare_struct({:%, _, [struct1, left_map]}, right, env) do
    compare_struct(left_map, right, struct1, nil, env)
  end

  defp compare_struct({:%{}, _, [{:__struct__, struct1} | left_items]}, %struct2{} = right, env) do
    compare_struct({:%{}, [], left_items}, right, struct1, struct2, env)
  end

  defp compare_struct(%struct1{} = left, %struct2{} = right, env) do
    compare_struct(left, right, struct1, struct2, env)
  end

  defp compare_struct(%struct1{} = left, right, env) do
    compare_struct(left, right, struct1, nil, env)
  end

  defp compare_struct(left, %struct2{} = right, env) do
    compare_map(left, right, nil, struct2, env)
  end

  defp compare_struct(left, right, env) do
    compare_map(left, right, nil, nil, env)
  end

  defp compare_struct(%{} = left, right, struct1, struct2, env) do
    if Inspect.impl_for(left) not in [Inspect.Any, Inspect.Map] do
      inspect_left = inspect(left)
      inspect_right = inspect(right)

      if inspect_left != inspect_right do
        compare_string(inspect_left, inspect_right, ?\", env)
      else
        compare_map(Map.from_struct(left), right, struct1, struct2, env)
      end
    else
      compare_map(Map.from_struct(left), right, struct1, struct2, env)
    end
  end

  defp compare_struct(left_map, right, struct1, struct2, env) do
    compare_map(left_map, right, struct1, struct2, env)
  end

  defp build_struct_result(nil, nil) do
    %__MODULE__{equivalent?: true}
  end

  defp build_struct_result(struct1, nil) do
    diff_left = update_diff_meta({:__struct__, struct1}, true)
    %__MODULE__{equivalent?: true, left: diff_left}
  end

  defp build_struct_result(nil, struct2) do
    diff_right = update_diff_meta({:__struct__, struct2}, true)
    %__MODULE__{equivalent?: true, right: diff_right}
  end

  defp build_struct_result(struct, struct) do
    struct_pair = {:__struct__, struct}
    %__MODULE__{equivalent?: true, left: struct_pair, right: struct_pair}
  end

  defp build_struct_result(struct1, struct2) do
    diff_left = update_diff_meta({:__struct__, struct1}, true)
    diff_right = update_diff_meta({:__struct__, struct2}, true)
    %__MODULE__{equivalent?: true, left: diff_left, right: diff_right}
  end

  # Strings

  defp compare_string(left, right, delimiter, env) do
    diff =
      cond do
        diff_string?(left, right) ->
          {escaped_left, _} = Code.Identifier.escape(left, delimiter)
          {escaped_right, _} = Code.Identifier.escape(right, delimiter)
          left = IO.iodata_to_binary(escaped_left)
          right = IO.iodata_to_binary(escaped_right)

          String.myers_difference(left, right) |> string_script_to_diff(delimiter)

        left == right ->
          string_script_to_diff([eq: left], delimiter)

        true ->
          string_script_to_diff([del: left, ins: right], delimiter)
      end

    {diff, env}
  end

  # Concat all the literals on `left` and split `right` based on the size of
  # that, comparing them and the remaining AST from `left` and the remaining
  # string from `right`.
  def compare_string_concat(left, right, env) do
    {parsed_left, quoted, indexes, parsed_left_length} = parse_string(left)

    compare_string_concat(parsed_left, quoted, indexes, parsed_left_length, right, env)
  end

  def compare_string_concat(left, nil, indexes, _left_length, right, env) do
    {parsed_diff, parsed_post_env} = compare_string(left, right, ?\", env)
    left_diff = rebuild_concat_string(parsed_diff.left, nil, indexes)

    diff = %__MODULE__{parsed_diff | left: left_diff}
    {diff, parsed_post_env}
  end

  def compare_string_concat(left, quoted, indexes, left_length, right, env) do
    {parsed_right, continue_right} = String.split_at(right, left_length)

    {parsed_diff, parsed_post_env} = compare_string(left, parsed_right, ?\", env)
    {quoted_diff, quoted_post_env} = compare_quoted(quoted, continue_right, parsed_post_env)

    diff =
      merge_diff(parsed_diff, quoted_diff, fn left1, left2, right1, right2 ->
        new_left = rebuild_concat_string(left1, left2, indexes)
        new_right = rebuild_split_strings(right1, right2)

        {new_left, new_right}
      end)

    {diff, quoted_post_env}
  end

  defp diff_string?(left, right) do
    length_left = String.length(left)
    length_right = String.length(right)

    case max(length_left, length_right) do
      0 -> false
      other -> bag_distance(left, right) / other <= 0.6
    end
  end

  # The algorithm is outlined in the
  # "String Matching with Metric Trees Using an Approximate Distance"
  # paper by Ilaria Bartolini, Paolo Ciaccia, and Marco Patella.
  defp bag_distance(left, right) do
    bag_left = string_to_bag(left)
    bag_right = string_to_bag(right)

    diff_left = bag_difference(bag_left, bag_right)
    diff_right = bag_difference(bag_right, bag_left)

    max(diff_left, diff_right)
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

  defp bag_difference(bag_left, bag_right) do
    Enum.reduce(bag_left, 0, fn {char, count_a}, sum ->
      case Map.fetch(bag_right, char) do
        {:ok, count_b} ->
          sum + max(count_a - count_b, 0)

        :error ->
          sum + count_a
      end
    end)
  end

  defp parse_string({:<>, _, [literal, rest]}) do
    {parsed, quoted, indexes, parsed_length} = parse_string(rest)
    literal_length = String.length(literal)
    length = literal_length + parsed_length

    {literal <> parsed, quoted, [literal_length | indexes], length}
  end

  defp parse_string(literal) when is_binary(literal) do
    {literal, nil, [], String.length(literal)}
  end

  defp parse_string(pattern) do
    {"", pattern, [], 0}
  end

  defp rebuild_split_strings(left, "") do
    left
  end

  defp rebuild_split_strings({:__block__, meta, left_list}, {:__block__, _, right_list}) do
    {:__block__, meta, left_list ++ right_list}
  end

  defp rebuild_split_strings({:__block__, meta, left_list}, right) do
    {:__block__, meta, left_list ++ [right]}
  end

  defp rebuild_concat_string(literal, nil, []) do
    literal
  end

  defp rebuild_concat_string(_literal, quoted, []) do
    quoted
  end

  defp rebuild_concat_string(literal, quoted, [index | rest]) do
    {next, continue} = next_concat_result(literal, index)
    rebuilt_right = rebuild_concat_string(continue, quoted, rest)

    {:<>, [], [next, rebuilt_right]}
  end

  defp next_concat_result({:__block__, [{:diff_container, _} | _] = meta, list}, index) do
    {next, continue} = next_concat_result(list, index)
    {{:__block__, meta, next}, {:__block__, meta, continue}}
  end

  defp next_concat_result([head | tail], index) do
    {string, diff_meta?} = extract_diff_meta(head)
    length = String.length(string)

    cond do
      length > index ->
        {next, continue} = String.split_at(string, index)
        next = [update_diff_meta(next, diff_meta?)]
        continue = [update_diff_meta(continue, diff_meta?) | tail]

        {next, continue}

      length < index ->
        {next, continue} = next_concat_result(tail, index - length)
        {[head | next], continue}

      true ->
        {[head], tail}
    end
  end

  defp string_script_to_diff([], delimiter) do
    container = {:__block__, [diff_container: delimiter], []}
    %__MODULE__{equivalent?: true, left: container, right: container}
  end

  defp string_script_to_diff([{:eq, string} | tail], delimiter) do
    head_diff = %__MODULE__{equivalent?: true, left: string, right: string}
    tail_diff = string_script_to_diff(tail, delimiter)

    prepend_diff(head_diff, tail_diff)
  end

  defp string_script_to_diff([{:del, string} | tail], delimiter) do
    left = update_diff_meta(string, true)
    head_diff = %__MODULE__{equivalent?: false, left: left, right: nil}
    tail_diff = string_script_to_diff(tail, delimiter)

    prepend_diff(head_diff, tail_diff)
  end

  defp string_script_to_diff([{:ins, string} | tail], delimiter) do
    right = update_diff_meta(string, true)
    head_diff = %__MODULE__{equivalent?: false, left: nil, right: right}
    tail_diff = string_script_to_diff(tail, delimiter)

    prepend_diff(head_diff, tail_diff)
  end

  def update_diff_container_meta({:__block__, [{:diff_container, _} | meta], list}, type) do
    {:__block__, [{:diff_container, type} | meta], list}
  end

  # Numbers

  def compare_number(left, right, env) do
    {diff, post_env} = compare_string(inspect(left), inspect(right), ?\", env)
    diff_left = update_diff_container_meta(diff.left, :number)
    diff_right = update_diff_container_meta(diff.right, :number)

    {%{diff | left: diff_left, right: diff_right}, post_env}
  end

  # Algebra

  def to_algebra(quoted, diff_wrapper) do
    wrap_on_diff(quoted, &safe_to_algebra/2, diff_wrapper)
  end

  defp safe_to_algebra({:__block__, [diff_container: :number], list}, diff_wrapper) do
    content_docs = Enum.map(list, &string_to_algebra(&1, diff_wrapper))

    Algebra.concat(content_docs)
  end

  defp safe_to_algebra({:__block__, [diff_container: type], list}, diff_wrapper) do
    content_docs = Enum.map(list, &string_to_algebra(&1, diff_wrapper))

    delimiter = to_string([type])
    Algebra.concat([delimiter] ++ content_docs ++ [delimiter])
  end

  defp safe_to_algebra(list, diff_wrapper) when is_list(list) do
    container_to_algebra("[", list, "]", diff_wrapper, select_list_item_algebra(list))
  end

  defp safe_to_algebra({op, _, [left, right]}, diff_wrapper) when op in [:<>, :++, :|] do
    binary_op_to_algebra(left, " #{op} ", right, diff_wrapper)
  end

  defp safe_to_algebra({:{}, _, args}, diff_wrapper) do
    container_to_algebra("{", args, "}", diff_wrapper, &to_algebra/2)
  end

  defp safe_to_algebra({a, b}, diff_wrapper) do
    container_to_algebra("{", [a, b], "}", diff_wrapper, &to_algebra/2)
  end

  defp safe_to_algebra({:%{}, _, [head | tail]}, diff_wrapper) do
    {struct, list} =
      case extract_diff_meta(head) do
        {{:__struct__, name}, true} -> {update_diff_meta(name, true), tail}
        {{:__struct__, name}, false} -> {name, tail}
        _other -> {nil, [head | tail]}
      end

    open =
      if struct do
        Algebra.concat(["%", struct_to_algebra(struct, diff_wrapper), "{"])
      else
        "%{"
      end

    container_to_algebra(open, list, "}", diff_wrapper, select_map_item_to_algebra(list))
  end

  defp safe_to_algebra({_, _, _} = quoted, _diff_wrapper) do
    Macro.to_string(quoted)
  end

  defp safe_to_algebra(literal, _diff_wrapper) do
    inspect(literal)
  end

  def string_to_algebra(quoted, diff_wrapper) do
    wrap_on_diff(quoted, &safe_string_to_algebra/2, diff_wrapper)
  end

  def safe_string_to_algebra(literal, _diff_wrapper) do
    literal
  end

  defp keyword_to_algebra(quoted, diff_wrapper) do
    wrap_on_diff(quoted, &safe_keyword_to_algebra/2, diff_wrapper)
  end

  defp safe_keyword_to_algebra({:{}, _, [key, value]}, diff_wrapper) do
    keyword_to_algebra({key, value}, diff_wrapper)
  end

  defp safe_keyword_to_algebra({key, value}, diff_wrapper) do
    key_doc = key_to_algebra(key, diff_wrapper)
    value_doc = to_algebra(value, diff_wrapper) |> Algebra.nest(:cursor)

    key_doc
    |> Algebra.glue(" ", value_doc)
    |> Algebra.group()
  end

  defp key_to_algebra(quoted, diff_wrapper) do
    wrap_on_diff(quoted, &safe_key_to_algebra/2, diff_wrapper)
  end

  defp safe_key_to_algebra(key, _diff_wrapper) do
    Identifier.inspect_as_key(key)
  end

  defp map_item_to_algebra(quoted, diff_wrapper) do
    wrap_on_diff(quoted, &safe_map_item_to_algebra/2, diff_wrapper)
  end

  defp safe_map_item_to_algebra({:{}, _, [key, value]}, diff_wrapper) do
    safe_map_item_to_algebra({key, value}, diff_wrapper)
  end

  defp safe_map_item_to_algebra({key, value}, diff_wrapper) do
    key_doc = to_algebra(key, diff_wrapper)
    value_doc = to_algebra(value, diff_wrapper) |> Algebra.nest(:cursor)

    key_doc
    |> Algebra.glue(" => ", value_doc)
    |> Algebra.group()
  end

  defp binary_op_to_algebra(left, op, right, diff_wrapper) do
    left_doc = to_algebra(left, diff_wrapper)
    right_doc = to_algebra(right, diff_wrapper) |> Algebra.nest(:cursor)

    left_doc
    |> Algebra.glue(op, right_doc)
    |> Algebra.group()
  end

  defp container_to_algebra(open, list, close, diff_wrapper, item_to_algebra) do
    docs =
      list
      |> Enum.map(&item_to_algebra.(&1, diff_wrapper))
      |> Algebra.fold_doc(&Algebra.flex_glue(&1, ", ", &2))
      |> Algebra.nest(1)

    [open, docs, close]
    |> Algebra.concat()
    |> Algebra.group()
  end

  defp struct_to_algebra(quoted, diff_wrapper) do
    wrap_on_diff(quoted, &safe_struct_to_algebra/2, diff_wrapper)
  end

  defp safe_struct_to_algebra(name, _diff_wrapper) do
    Code.Identifier.inspect_as_atom(name)
  end

  defp select_list_item_algebra(list) do
    short? = Enum.all?(list, &keyword?/1)

    if short?, do: &keyword_to_algebra/2, else: &to_algebra/2
  end

  defp select_map_item_to_algebra(list) do
    short? = Enum.all?(list, &keyword?/1)

    if short?, do: &keyword_to_algebra/2, else: &map_item_to_algebra/2
  end

  defp wrap_on_diff(quoted, fun, wrapper) do
    case extract_diff_meta(quoted) do
      {expr, true} -> fun.(expr, & &1) |> wrapper.()
      {expr, false} -> fun.(expr, wrapper)
    end
  end

  # Diff helpers

  defp value_to_result(reference, diff_meta?) when is_reference(reference) do
    update_diff_meta(reference, diff_meta?)
  end

  defp value_to_result(value, diff_meta?) do
    value
    |> Macro.escape()
    |> update_diff_meta(diff_meta?)
  end

  defp merge_diff(%__MODULE__{} = result1, %__MODULE__{} = result2, fun) do
    {left, right} = fun.(result1.left, result2.left, result1.right, result2.right)

    %__MODULE__{
      equivalent?: result1.equivalent? && result2.equivalent?,
      left: left,
      right: right
    }
  end

  defp prepend_diff(%__MODULE__{} = head_result, %__MODULE__{} = tail_result) do
    merge_diff(head_result, tail_result, fn pattern1, pattern2, value1, value2 ->
      {prepend_result(pattern1, pattern2), prepend_result(value1, value2)}
    end)
  end

  defp pop_diff(%__MODULE__{} = diff) do
    {popped_left, remaining_left} = pop_result(diff.left)
    {popped_right, remaining_right} = pop_result(diff.right)

    remaining_diff = %{diff | left: remaining_left, right: remaining_right}

    case {popped_left, popped_right} do
      {:empty, :empty} ->
        {nil, remaining_diff}

      {{:element, elem1}, :empty} ->
        {%{diff | left: elem1, right: nil}, remaining_diff}

      {:empty, {:element, elem2}} ->
        {%{diff | left: nil, right: elem2}, remaining_diff}

      {{:element, elem1}, {:element, elem2}} ->
        {%{diff | left: elem1, right: elem2}, remaining_diff}
    end
  end

  defp prepend_result(nil, quoted) do
    quoted
  end

  defp prepend_result(item, quoted) do
    {extracted, diff_meta?} = extract_diff_meta(quoted)

    safe_prepend_result(item, extracted) |> update_diff_meta(diff_meta?)
  end

  defp safe_prepend_result(item, {type, meta, list}), do: {type, meta, [item | list]}
  defp safe_prepend_result(item, list), do: [item | list]

  defp pop_result(quoted) do
    {extracted, _diff_meta?} = extract_diff_meta(quoted)
    safe_pop_result(extracted)
  end

  defp safe_pop_result({_, _, []} = quoted) do
    {:empty, quoted}
  end

  defp safe_pop_result({left, meta, [head | tail]}) do
    {{:element, head}, {left, meta, tail}}
  end

  defp update_diff_meta({:__block__, [diff: true], [literal]}, false) do
    literal
  end

  defp update_diff_meta({:__block__, [diff: true], _} = quoted, true) do
    quoted
  end

  defp update_diff_meta({:__block__, [diff_container: type, diff: true], args}, false) do
    {:__block__, [diff_container: type], args}
  end

  defp update_diff_meta({:__block__, [diff_container: type], args}, true) do
    {:__block__, [diff_container: type, diff: true], args}
  end

  defp update_diff_meta({left, [{:diff, true} | meta], right}, false) do
    {left, meta, right}
  end

  defp update_diff_meta({left, meta, right}, true) do
    {left, [{:diff, true} | meta], right}
  end

  defp update_diff_meta(literal, false) do
    literal
  end

  defp update_diff_meta(literal, true) do
    {:__block__, [diff: true], [literal]}
  end

  defp update_diff_meta(other, _diff_meta?) do
    other
  end

  defp extract_diff_meta({:__block__, [diff: true], [literal]}) do
    {literal, true}
  end

  defp extract_diff_meta({:__block__, [diff_container: type, diff: true], args}) do
    {{:__block__, [diff_container: type], args}, true}
  end

  defp extract_diff_meta({left, [{:diff, true} | meta], right}) do
    {{left, meta, right}, true}
  end

  defp extract_diff_meta(other) do
    {other, false}
  end

  defp keyword?(quoted) do
    {pair, _} = extract_diff_meta(quoted)
    safe_keyword?(pair)
  end

  defp safe_keyword?({key, _value}), do: key_is_atom?(key)
  defp safe_keyword?({:{}, _meta, [key, _value]}), do: key_is_atom?(key)
  defp safe_keyword?(_other), do: false

  def key_is_atom?(quoted) do
    {key, _} = extract_diff_meta(quoted)
    :erlang.is_atom(key)
  end
end
