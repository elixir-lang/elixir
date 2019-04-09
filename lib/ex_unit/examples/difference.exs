ExUnit.start [seed: 0]

defmodule Difference do
  @moduledoc """
  This module contains failing tests to see
  difference highlighting in action.
  """
  use ExUnit.Case

  defmodule User do
    defstruct [:age]
  end

  defmacrop one, do: 1

  describe "regular context" do
    test "integers" do
      assert 491512235 == 490512035
    end

    test "floats" do
      assert 42.0 == 43.0
    end

    test "strings" do
      string1 = "fox hops over \"the dog"
      string2 = "fox  jumps over the lazy cat"
      assert string1 == string2
    end

    test "whitespace" do
      list1 = [%{a: "abc "}, %{a: "def"}, %{c: "gh"}]
      list2 = [%{a: "abc"}, %{a: " def"}, %{c: "hi"}]
      assert list1 == list2
    end

    test "large strings" do
      string1 = "short"
      string2 = "really long string that should not emit diff"
      assert string1 == string2
    end

    test "large strings; inner" do
      tuple1 = {"short"}
      tuple2 = {"really long string that should not emit diff"}
      assert tuple1 == tuple2
    end

    test "lists" do
      list1 = ["Tvo", make_ref(), :ok, {}]
      list2 = ["Two", :ok, self(), {true}]
      assert list1 == list2
    end

    test "lists; missing entries" do
      assert [] == [1, 2, 3]
    end

    test "lists; surplus entries" do
      assert [1, 2, 3] == []
    end

    test "improper lists" do
      list1 = [1 | "b"]
      list2 = [1, "a"]
      assert list1 == list2
    end

    test "charlists" do
      charlist1 = 'fox hops over \'the dog'
      charlist2 = 'fox jumps over the lazy cat'
      assert charlist1 == charlist2
    end

    test "keyword lists" do
      assert [file: "nofile", line: 12] == [file: nil, lime: 10]
    end

    test "keyword lists; reverse order" do
      keyword1 = [port: 4000, max_connections: 1000]
      keyword2 = [max_connections: 1000, port: 4000]
      assert keyword1 == keyword2
    end

    test "tuples" do
      tuple1 = {:hex, "0.1", [{:ex_doc}]}
      tuple2 = {:hex, "1.1"}
      assert tuple1 == tuple2
    end

    test "maps; mixed diff" do
      map1 = Enum.into(1..15, %{}, &{&1, &1}) |> Map.delete(13)
      map2 = Enum.reduce(5..10, map1, &Map.delete(&2, &1)) |> Map.put(13, 13) |> Map.put(12, 32)
      assert map1 == map2
    end

    test "maps; missing pairs" do
      map1 = %{baz: 12}
      map2 = %{foo: 12, bar: 12, baz: 12}
      assert map1 == map2
    end

    test "maps; surplus pairs" do
      map1 = %{foo: 12, bar: 12, baz: 12}
      map2 = %{baz: 12}
      assert map1 == map2
    end

    test "maps; missing pair" do
      assert %{} == %{baz: 12}
    end

    test "maps; surplus pair" do
      assert %{baz: 12} == %{}
    end

    test "structs" do
      assert %User{age: 16} == %User{age: 21}
    end
  end

  describe "match context" do
    test "pins" do
      x = 1
      assert ^x = 2
    end

    test "vars" do
      assert {x, x} = {1, 2}
    end

    test "strings; concat" do
      assert "fox hops over the" <> x = "fox jumps over the lazy cat"
    end

    test "lists; head and tail" do
      assert ["Tvo", 1 | [:ok , {}]] = ["Two", :ok, self(), {true}]
    end

    test "lists; concat" do
      assert ["Tvo", 1] ++ [:ok , {}] = ["Two", :ok, self(), {true}]
    end

    test "maps" do
      assert %{baz: 12} = %{foo: 13, bar: 13, baz: 13}
    end

    test "macro" do
      assert one() = 2
    end

    test "underscore" do
      assert {_, 1} = {1, 2}
    end
  end
end
