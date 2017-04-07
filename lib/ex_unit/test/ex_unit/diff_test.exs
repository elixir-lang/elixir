Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.DiffTest do
  use ExUnit.Case, async: true

  import ExUnit.Diff

  defmodule User do
    defstruct [:age]
  end

  test "numbers" do
    int1 = 491512235
    int2 = 490512035
    expected = [eq: "49", del: "1", ins: "0", eq: "512", del: "2", ins: "0", eq: "35"]
    assert script(int1, int2) == expected
    assert script(42.0, 43.0) == [eq: "4", del: "2", ins: "3", eq: ".0"]
    assert script(int1, 43.0) == nil
  end

  test "strings" do
    string1 = "fox hops over \"the dog"
    string2 = "fox jumps over the lazy cat"
    expected = [
      {:eq, "\""},
      [eq: "fox ", del: "ho", ins: "jum", eq: "ps over ", del: "\\\"", eq: "the ", del: "dog", ins: "lazy cat"],
      {:eq, "\""}
    ]
    assert script(string1, string2) == expected
    assert script(string1, <<193, 31>>) == nil

    # Filtered due to bag distance
    assert script("aaa", "bba") == nil
    assert script("aaa", "baa") == [{:eq, "\""}, [ins: "b", eq: "aa", del: "a"], {:eq, "\""}]

    assert script("", "") == [eq: "\"\""]
  end

  test "lists" do
    list1 = ["Tvo", nil, :ok, {}, :ok]
    list2 = ["Two", :ok, self(), {true}]

    expected = [
      {:eq, "["},
      [
        [{:eq, "\""}, [eq: "T", del: "v", ins: "w", eq: "o"], {:eq, "\""}], {:eq, ", "},
        {:del, "nil"}, {:del, ", "},
        {:eq, ":ok"}, {:eq, ", "},
        {:ins, inspect(self())}, {:ins, ", "},
        [{:eq, "{"}, [[ins: "true"]], {:eq, "}"}], {:del, ", "}, {:del, ":ok"}
      ],
      {:eq, "]"}
    ]
    assert script(list1, list2) == expected

    list1 = [1, "2", 1]
    list2 = [1, 1, 2]
    expected = [
      {:eq, "["},
      [eq: "1", eq: ", ", del: "\"2\"", del: ", ", eq: "1", ins: ", ", ins: "2"],
      {:eq, "]"}
    ]
    assert script(list1, list2) == expected

    list1 = [1, 1, "1", 2]
    list2 = [1, 1, 2]
    expected = [
      {:eq, "["},
      [eq: "1, 1", eq: ", ", del: "\"1\"", del: ", ", eq: "2"],
      {:eq, "]"}
    ]
    assert script(list1, list2) == expected

    list1 = [1, 2]
    list2 = [1, 1, 2]
    expected = [
      {:eq, "["},
      [{:eq, "1"}, {:eq, ", "}, [del: "2", ins: "1"], {:ins, ", "}, {:ins, "2"}],
      {:eq, "]"}
    ]
    assert script(list1, list2) == expected

    list1 = []
    list2 = [1, 2]
    expected = [{:eq, "["}, [ins: "1, 2"], {:eq, "]"}]
    assert script(list1, list2) == expected

    list1 = [1, 2]
    list2 = []
    expected = [{:eq, "["}, [del: "1, 2"], {:eq, "]"}]
    assert script(list1, list2) == expected

    assert script([], []) == [eq: "[]"]
  end

  test "charlists" do
    charlist1 = 'fox hops over \'the dog'
    charlist2 = 'fox jumps over the lazy cat'
    expected = [
      {:eq, "'"},
      [eq: "fox ", del: "ho", ins: "jum", eq: "ps over ", del: "\\'", eq: "the ", del: "dog", ins: "lazy cat"],
      {:eq, "'"}
    ]
    assert script(charlist1, charlist2) == expected
  end

  test "keyword lists" do
    keyword1 = [file: "nofile", line: 1]
    keyword2 = [file: nil, lime: 1]
    expected = [
      {:eq, "["},
      [
        [{:eq, "file: "}, [del: "\"nofile\"", ins: "nil"]],
        {:eq, ", "},
        {:del, "line: 1"}, {:ins, "lime: 1"}
      ],
      {:eq, "]"}
    ]
    assert script(keyword1, keyword2) == expected

    keyword1 = [file: nil, line: 1]
    keyword2 = [file: "nofile"]
    expected = [
      {:eq, "["},
      [
        [{:eq, "file: "}, [del: "nil", ins: "\"nofile\""]],
        {:del, ", "},
        {:del, "line: 1"}
      ],
      {:eq, "]"}
    ]
    assert script(keyword1, keyword2) == expected

    keyword1 = [file: "nofile"]
    keyword2 = [file: nil, line: 1]
    expected = [
      {:eq, "["},
      [
        [{:eq, "file: "}, [del: "\"nofile\"", ins: "nil"]],
        {:ins, ", "},
        {:ins, "line: 1"}
      ],
      {:eq, "]"}
    ]
    assert script(keyword1, keyword2) == expected

    keyword1 = [file: "nofile", line: 1]
    keyword2 = [file: nil, line: 1]
    expected = [
      {:eq, "["},
      [
        [{:eq, "file: "}, [del: "\"nofile\"", ins: "nil"]],
        {:eq, ", "},
        {:eq, "line: 1"}
      ],
      {:eq, "]"}
    ]
    assert script(keyword1, keyword2) == expected

    keyword1 = [line: 1, file: "nofile"]
    keyword2 = [line: 1, file: nil]
    expected = [
      {:eq, "["},
      [
        {:eq, "line: 1"},
        {:eq, ", "},
        [{:eq, "file: "}, [del: "\"nofile\"", ins: "nil"]]
      ],
      {:eq, "]"}
    ]
    assert script(keyword1, keyword2) == expected

    keyword1 = [file: "one", line: 1]
    keyword2 = [file: "two", line: 2]
    expected = [
      {:eq, "["},
      [
        [{:eq, "file: "}, [del: "\"one\"", ins: "\"two\""]],
        {:eq, ", "},
        [{:eq, "line: "}, [del: "1", ins: "2"]]
      ],
      {:eq, "]"}
    ]
    assert script(keyword1, keyword2) == expected

    keyword1 = [file: "nofile"]
    keyword2 = [file: nil]
    expected = [
      {:eq, "["},
      [{:eq, "file: "}, [del: "\"nofile\"", ins: "nil"]],
      {:eq, "]"}
    ]
    assert script(keyword1, keyword2) == expected

    keyword1 = [file: nil, line: 1]
    keyword2 = [line: 1]
    expected = [
      {:eq, "["},
      [{:del, "file: nil"}, {:del, ", "}, {:eq, "line: 1"}],
      {:eq, "]"}
    ]
    assert script(keyword1, keyword2) == expected

    keyword1 = [file: nil]
    keyword2 = []
    expected = [{:eq, "["}, [{:del, "file: nil"}], {:eq, "]"}]
    assert script(keyword1, keyword2) == expected

    keyword1 = []
    keyword2 = [file: nil]
    expected = [{:eq, "["}, [{:ins, "file: nil"}], {:eq, "]"}]
    assert script(keyword1, keyword2) == expected

    keyword1 = [port: 4000, max_connections: 1000]
    keyword2 = [max_connections: 1000, port: 4000]
    expected = [
      {:eq, "["},
      [del: "port: 4000", del: ", ", eq: "max_connections: 1000", ins: ", ", ins: "port: 4000"],
      {:eq, "]"}
    ]
    assert script(keyword1, keyword2) == expected

    assert script(["foo-bar": 1], []) == [{:eq, "["}, [{:del, "\"foo-bar\": 1"}], {:eq, "]"}]
  end

  test "improper lists" do
    expected = [{:eq, "["}, [[eq: "1"], [eq: ", ", eq: "2"], [ins: " | 3"]], {:eq, "]"}]
    assert script([1, 2], [1, 2 | 3]) == expected
    expected = [{:eq, "["}, [[eq: "1"], [eq: ", ", eq: "2"], [del: " | 3"]], {:eq, "]"}]
    assert script([1, 2 | 3], [1, 2]) == expected

    expected = [{:eq, "["}, [[eq: "1"], [del: ",", ins: " |", eq: " ", del: "\"a\"", ins: "\"b\""]], {:eq, "]"}]
    assert script([1, "a"], [1 | "b"]) == expected
    expected = [{:eq, "["}, [[eq: "1"], [del: " |", ins: ",", eq: " ", del: "\"b\"", ins: "\"a\""]], {:eq, "]"}]
    assert script([1 | "b"], [1, "a"]) == expected

    expected = [{:eq, "["}, [[eq: "1"], [eq: " | ", del: "2", ins: "3"]], {:eq, "]"}]
    assert script([1 | 2], [1 | 3]) == expected

    expected = [{:eq, "["}, [[eq: "1"], [eq: ", ", del: "'b'", ins: "'a'"], [eq: " | ", eq: "3"]], {:eq, "]"}]
    assert script([1, 'b' | 3], [1, 'a' | 3]) == expected
    expected = [{:eq, "["}, [[del: "'a'", ins: "'b'"], [eq: ", ", eq: "2"], [eq: " | ", eq: "3"]], {:eq, "]"}]
    assert script(['a', 2 | 3], ['b', 2 | 3]) == expected
  end

  test "tuples" do
    tuple1 = {:hex, '1.1'}
    tuple2 = {:hex, '0.1', [{:ex_doc}]}
    expected = [
      {:eq, "{"},
      [[eq: ":hex"], [{:eq, ", "}, {:eq, "'"}, [del: "1", ins: "0", eq: ".1"], {:eq, "'"}],[ins: ", ", ins: "[{:ex_doc}]"]],
      {:eq, "}"}
    ]
    assert script(tuple1, tuple2) == expected
    assert script(tuple1, {}) == [{:eq, "{"}, [[del: ":hex"], [del: ", ", del: "'1.1'"]], {:eq, "}"}]
    assert script({}, tuple1) == [{:eq, "{"}, [[ins: ":hex"], [ins: ", ", ins: "'1.1'"]], {:eq, "}"}]

    assert script({}, {}) == [eq: "{}"]
  end

  test "maps" do
    map1 = Enum.into(1..15, %{}, &{&1, &1}) |> Map.delete(13)
    map2 = Enum.reduce(5..10, map1, &Map.delete(&2, &1)) |> Map.put(13, 13) |> Map.put(12, 32)
    expected = [
      {:eq, "%{"},
      [
        [eq: "1 => 1"], [eq: ", ", eq: "2 => 2"], [eq: ", ", eq: "3 => 3"],
        [eq: ", ", eq: "4 => 4"], [eq: ", ", eq: "11 => 11"], [eq: ", ", eq: "14 => 14"],
        [eq: ", ", eq: "15 => 15"],
        [{:eq, ", "}, {:eq, "12 => "}, [del: "1", ins: "3", eq: "2"]],
        [del: ", ", del: "5 => 5"], [del: ", ", del: "6 => 6"], [del: ", ", del: "7 => 7"],
        [del: ", ", del: "8 => 8"], [del: ", ", del: "9 => 9"], [del: ", ", del: "10 => 10"],
        [ins: ", ", ins: "13 => 13"],
      ],
      {:eq, "}"}
    ]
    assert script(map1, map2) == expected

    map1 = %{baz: 12}
    map2 = %{foo: 12, bar: 12, baz: 12}
    expected = [{:eq, "%{"}, [[eq: "baz: 12"], [ins: ", ", ins: "bar: 12"], [ins: ", ", ins: "foo: 12"]], {:eq, "}"}]
    assert script(map1, map2) == expected
    expected = [{:eq, "%{"}, [[eq: "baz: 12"], [del: ", ", del: "bar: 12"], [del: ", ", del: "foo: 12"]], {:eq, "}"}]
    assert script(map2, map1) == expected
    assert script(map1, %{}) == [{:eq, "%{"}, [[del: "baz: 12"]], {:eq, "}"}]
    assert script(%{}, map1) == [{:eq, "%{"}, [[ins: "baz: 12"]], {:eq, "}"}]
    expected = [{:eq, "%{"}, [[del: "baz: 12"], [ins: "foo: 12"]], {:eq, "}"}]
    assert script(map1, %{foo: 12}) == expected

    assert script(%{"foo-bar": 1}, %{}) == [{:eq, "%{"}, [[del: "\"foo-bar\": 1"]], {:eq, "}"}]
    assert script(%{}, %{}) == [eq: "%{}"]

    assert script(%{nil: 42}, %{}) == [{:eq, "%{"}, [[del: "nil: 42"]], {:eq, "}"}]
    assert script(%{true: 42}, %{}) == [{:eq, "%{"}, [[del: "true: 42"]], {:eq, "}"}]
    assert script(%{false: 42}, %{}) == [{:eq, "%{"}, [[del: "false: 42"]], {:eq, "}"}]
  end

  test "structs" do
    user1 = %User{age: 16}
    user2 = %User{age: 21}
    expected = [{:eq, "%ExUnit.DiffTest.User{"}, [[{:eq, "age: "}, [ins: "2", eq: "1", del: "6"]]], {:eq, "}"}]
    assert script(user1, user2) == expected
    assert script(%User{}, %{}) == nil
    assert script(%User{}, %ExUnit.Test{}) == nil
  end

  test "not supported" do
    bin1 = <<147, 1, 2, 31>>
    bin2 = <<193, 1, 31>>
    assert script(bin1, bin2) == nil
    assert script(:foo, :bar) == nil
    assert script(:foo, "bar") == nil
  end
end
