Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.PatternMatchTest do
  use ExUnit.Case

  alias ExUnit.Pattern

  # test "simple pattern match" do
  #   simple =
  #     quote do
  #       :a
  #     end

  #   expected = [del: ":a", ins: ":b"]
  #   pattern = ExUnit.Pattern.new(simple, :b, [], [])

  #   actual =
  #     pattern
  #     |> Pattern.diff()
  #     |> Pattern.get_diff()

  #   assert actual == expected
  # end

  # test "simple pinned pattern match" do
  #   pinned_var =
  #     quote do
  #       ^a
  #     end

  #   expected = [del: "^a", ins: "2"]
  #   pattern = ExUnit.Pattern.new(pinned_var, 2, [a: 1], [])

  #   actual =
  #     pattern
  #     |> Pattern.diff()
  #     |> Pattern.get_diff()

  #   assert actual == expected
  # end

  # test "Two pins, one matches" do
  #   pinned_var =
  #     quote do
  #       {^a, ^b}
  #     end

  #   pattern = Pattern.new(pinned_var, {1, 3}, [a: 1, b: 2], [])
  #   expected = [{:eq, "{"}, {:eq, "^a"}, {:eq, ", "}, {:del, "^b"}, {:ins, "3"}, {:eq, "}"}]

  #   actual =
  #     pattern
  #     |> Pattern.diff()
  #     |> Pattern.get_diff()

  #   assert actual == expected
  # end

  # test "single element tuple match" do
  #   tuple =
  #     quote do
  #       {1}
  #     end

  #   pattern = ExUnit.Pattern.new(tuple, {2}, [], [])
  #   expected = [{:eq, "{"}, {:del, "1"}, {:ins, "2"}, {:eq, "}"}]

  #   actual =
  #     pattern
  #     |> Pattern.diff()
  #     |> Pattern.get_diff()

  #   assert actual == expected
  # end

  # test "two element tuple match" do
  #   tuple =
  #     quote do
  #       {1, 2}
  #     end

  #   pattern = ExUnit.Pattern.new(tuple, {1, 3}, [], [])
  #   expected = [{:eq, "{"}, {:eq, "1"}, {:eq, ", "}, {:del, "2"}, {:ins, "3"}, {:eq, "}"}]

  #   actual =
  #     pattern
  #     |> Pattern.diff()
  #     |> Pattern.get_diff()

  #   assert actual == expected
  # end

  # test "two element tuple, bound_vars" do
  #   tuple =
  #     quote do
  #       {a, a}
  #     end

  #   pattern = ExUnit.Pattern.new(tuple, {1, 2}, [], a: :ex_unit_unbound_var)

  #   expected = [
  #     {:eq, "{"},
  #     {:equiv, {"a", "1"}},
  #     {:eq, ", "},
  #     {:del, "a"},
  #     {:ins, "2"},
  #     {:eq, "}"}
  #   ]

  #   actual =
  #     pattern
  #     |> Pattern.diff()
  #     |> Pattern.get_diff()

  #   assert actual == expected
  # end

  # test "uneven tuples left match" do
  #   tuple =
  #     quote do
  #       {1, 2}
  #     end

  #   expected = [{:eq, "{"}, {:eq, "1"}, {:del, ", "}, {:del, "2"}, {:eq, "}"}]
  #   pattern = ExUnit.Pattern.new(tuple, {1}, [], [])

  #   actual =
  #     pattern
  #     |> Pattern.diff()
  #     |> Pattern.get_diff()

  #   assert actual == expected
  # end

  # test "uneven tuples right match" do
  #   tuple =
  #     quote do
  #       {1}
  #     end

  #   expected = [{:eq, "{"}, {:eq, "1"}, {:ins, ", "}, {:ins, "2"}, {:eq, "}"}]
  #   pattern = ExUnit.Pattern.new(tuple, {1, 2}, [], [])

  #   actual =
  #     pattern
  #     |> Pattern.diff()
  #     |> Pattern.get_diff()

  #   assert actual == expected
  # end

  # test "nested tuple match" do
  #   tuple =
  #     quote do
  #       {1, 2, {1, 2, 3}}
  #     end

  #   pattern = ExUnit.Pattern.new(tuple, {1, 2, {1, 2, 4}}, [], [])

  #   expected = [
  #     {:eq, "{"},
  #     {:eq, "1"},
  #     {:eq, ", "},
  #     {:eq, "2"},
  #     {:eq, ", "},
  #     {:eq, "{"},
  #     {:eq, "1"},
  #     {:eq, ", "},
  #     {:eq, "2"},
  #     {:eq, ", "},
  #     {:del, "3"},
  #     {:ins, "4"},
  #     {:eq, "}"},
  #     {:eq, "}"}
  #   ]

  #   actual =
  #     pattern
  #     |> Pattern.diff()
  #     |> Pattern.get_diff()

  #   assert actual == expected
  # end

  # test "tuple with nested map and list" do
  #   tuple =
  #     quote do
  #       {1, %{a: [1, 2, 3]}, 3}
  #     end

  #   pattern = Pattern.new(tuple, {1, %{a: [1, 2, 3]}, 4}, [], [])

  #   expected = [
  #     eq: "%{",
  #     eq: "1",
  #     eq: ", ",
  #     eq: "%{",
  #     eq: "a: [1, 2, 3]",
  #     eq: "}",
  #     eq: ", ",
  #     del: "3",
  #     ins: "4",
  #     eq: "}"
  #   ]

  #   actual =
  #     pattern
  #     |> Pattern.diff()
  #     |> Pattern.get_diff()

  #   assert actual == expected
  # end

  # test "simple map match" do
  #   map =
  #     quote do
  #       %{a: 1}
  #     end

  #   pattern = ExUnit.Pattern.new(map, %{a: 2}, [], [])

  #   expected = [{:eq, "%{"}, {:eq, "a: "}, {:del, "1"}, {:ins, "2"}, {:eq, "}"}]

  #   actual =
  #     pattern
  #     |> Pattern.diff()
  #     |> Pattern.get_diff()

  #   assert actual == expected

  #   map =
  #     quote do
  #       %{a: 1, b: 2}
  #     end

  #   pattern = ExUnit.Pattern.new(map, %{a: 1, b: 1}, [], [])

  #   expected = [
  #     {:eq, "%{"},
  #     {:eq, "a: 1"},
  #     {:eq, ", "},
  #     {:eq, "b: "},
  #     {:del, "2"},
  #     {:ins, "1"},
  #     {:eq, "}"}
  #   ]

  #   actual =
  #     pattern
  #     |> Pattern.diff()
  #     |> Pattern.get_diff()

  #   assert actual == expected
  # end

  # test "simple map with no shared keys" do
  #   map =
  #     quote do
  #       %{a: 1}
  #     end

  #   pattern = ExUnit.Pattern.new(map, %{b: 1}, [], [])

  #   expected = [
  #     {:eq, "%{"},
  #     {:del, "a: 1"},
  #     {:ins, ", "},
  #     {:ins, "..."},
  #     {:eq, "}"}
  #   ]

  #   actual =
  #     pattern
  #     |> Pattern.diff()
  #     |> Pattern.get_diff()

  #   assert actual == expected
  # end

  # test "simple two element map, keys match" do
  #   map =
  #     quote do
  #       %{a: 1, b: 2}
  #     end

  #   pattern = Pattern.new(map, %{a: 1, b: 1}, [], [])
  #   expected = [eq: "%{", eq: "a: 1", eq: ", ", eq: "b: ", del: "2", ins: "1", eq: "}"]

  #   actual =
  #     pattern
  #     |> Pattern.diff()
  #     |> Pattern.get_diff()

  #   assert actual == expected
  # end

  # test "simple three element map, keys match" do
  #   map =
  #     quote do
  #       %{a: 1, b: 2, c: 1}
  #     end

  #   pattern = Pattern.new(map, %{a: 1, b: 1, c: 1}, [], [])

  #   expected = [
  #     eq: "%{",
  #     eq: "a: 1",
  #     eq: ", ",
  #     eq: "b: ",
  #     del: "2",
  #     ins: "1",
  #     eq: ", ",
  #     eq: "c: 1",
  #     eq: "}"
  #   ]

  #   actual =
  #     pattern
  #     |> Pattern.diff()
  #     |> Pattern.get_diff()

  #   assert actual == expected
  # end

  # test "map with a pin" do
  #   map =
  #     quote do
  #       %{a: ^a}
  #     end

  #   pattern = ExUnit.Pattern.new(map, %{a: 2}, [a: 1], [])

  #   expected = [
  #     {:eq, "%{"},
  #     {:eq, "a: "},
  #     {:del, "^a"},
  #     {:ins, "2"},
  #     {:eq, "}"}
  #   ]

  #   actual =
  #     pattern
  #     |> Pattern.diff()
  #     |> Pattern.get_diff()

  #   assert actual == expected
  # end

  # test "map with string keys" do
  #   map =
  #     quote do
  #       %{"hello" => "world", "good" => "morning"}
  #     end

  #   pattern = ExUnit.Pattern.new(map, %{"hello" => "world", "good" => "night"}, [], [])

  #   expected = [
  #     eq: "%{",
  #     eq: "\"hello\" => \"world\"",
  #     eq: ", ",
  #     eq: "\"good\" => ",
  #     del: "\"morning\"",
  #     ins: "\"night\"",
  #     eq: "}"
  #   ]

  #   actual =
  #     pattern
  #     |> Pattern.diff()
  #     |> Pattern.get_diff()

  #   assert actual == expected
  # end

  # test "simple list" do
  #   map =
  #     quote do
  #       [1, 2, :a]
  #     end

  #   pattern = Pattern.new(map, [1, 2, :b], [], [])

  #   expected = [eq: "[", eq: "1", eq: ", ", eq: "2", eq: ", ", del: ":a", ins: ":b", eq: "]"]

  #   actual =
  #     pattern
  #     |> Pattern.diff()
  #     |> Pattern.get_diff()

  #   assert actual == expected
  # end

  # test "keyword list" do
  #   map =
  #     quote do
  #       [a: 1, b: 2, c: 3]
  #     end

  #   pattern = Pattern.new(map, [a: 1, b: 3, c: 3], [], [])

  #   expected = [
  #     eq: "[",
  #     eq: "a: 1",
  #     eq: ", ",
  #     eq: "b: ",
  #     del: "2",
  #     ins: "3",
  #     eq: ", ",
  #     eq: "c: 3",
  #     eq: "]"
  #   ]

  #   actual =
  #     pattern
  #     |> Pattern.diff()
  #     |> Pattern.get_diff()

  #   assert actual == expected
  # end

  # test "map with map key" do
  #   map = quote do
  #     %{ %{a: 1} => 2}
  #   end

  #   pattern = Pattern.new(map, %{ %{a: 1} => 3}, [], [])
  #   expected = [eq: "%{", eq: "%{a: 1} => ", del: "2", ins: "3", eq: "}"]

  #     actual =
  #       pattern
  #       |> Pattern.diff()
  #       |> Pattern.get_diff()

  #     assert actual == expected
  # end

  test "decomposed list" do
    assert match?([3 | [t | [t]]], [3, 2, 3])
    list = quote do
      [1 | 2 | t]
    end
    pattern = Pattern.new(list, [1, 2, 3], [], [])
    expected = []

    actual =
      pattern
      |> Pattern.diff()
      |> Pattern.get_diff()

    assert actual == expected
  end

  # test "map with pinned key" do
  #   a = :a
  #   assert match?(%{^a => 0}, %{a: 1})
  # end

  # test "map with pinned key doesn't match" do
  #   a = :a
  #   assert match?(%{^a => 0}, %{b: 1})
  # end

end

