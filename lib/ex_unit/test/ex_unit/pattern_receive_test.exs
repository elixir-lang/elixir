Code.require_file("../test_helper.exs", __DIR__)

# Jowens - do we need a special formatter, similar to diff to do the patterns?
# jowens - reveiw this output, and then start coding up a better solution?

defmodule ExUnit.PatternReceiveTest do
  use ExUnit.Case

  alias ExUnit.{Pattern, PatternDiff, ReceivePatternFormat}

  # test "message recevied, no messages" do
  #   assert %{a: 2} == %{a: 1, b: 1, d: 2, c: 3}
  #   assert_receive 1
  # end

  # test "Two primatives" do
  #   #   send(self(), 2)
  #   #   assert_receive 1

  #   left =
  #     quote do
  #       1
  #     end

  #   pattern = Pattern.new(left, [], [])
  #   actual = ReceivePatternFormat.script(pattern, 2)

  #   expected = [
  #     diff_delete: "2"
  #   ]

  #   assert actual == expected
  # end

  # test "message received, map" do
  #   # send(self(), %{a: 1, b: 1, d: 2, c: 3})
  #   # assert_receive %{a: 2}

  #   left =
  #     quote do
  #       %{a: 2}
  #     end

  #   pattern = Pattern.new(left, [], [])
  #   actual = ReceivePatternFormat.script(pattern, %{a: 1, b: 1, d: 2, c: 3})

  #   expected = [
  #     "%{",
  #     "a: ",
  #     {:diff_delete, "1"},
  #     ", ",
  #     {:diff_delete, "b: 1"},
  #     ", ",
  #     {:diff_delete, "c: 3"},
  #     ", ",
  #     {:diff_delete, "d: 2"},
  #     "}"
  #   ]

  #   assert actual == expected
  # end

  # test "message received, map with mulitble keys" do
  #   send(self(), %{a: 1, b: 1, d: 2, c: 3})
  #   assert_receive %{a: 1, b: 2}
  # end

  # test "message received, map with multible keys don't match" do
  #   send(self(), %{a: 2, b: 1})
  #   assert_receive %{a: 1, b: 2}
  # end

  # test "message received, list" do
  #   send(self(), [1, 2, 3])
  #   send(self(), [1, 2])
  #   send(self(), [1, 2, 3, 4, 5])
  #   assert_receive [1, 2, 3, 4]
  # end

  # test "cons list" do
  #   send(self(), [1, 2, 3])
  #   assert_receive [2 | _rest]
  # end

  # test "multiple cons list" do
  #   send(self(), [1, 2, 3])
  #   b = 3
  #   assert_receive [1 | [^b | _rest]]
  # end

  # test "Large, nested map, doesn't match" do
  #   send(self(), %{a: 1, b: %{a: 2, c: %{d: 4, f: "world"}}, e: "hello"})
  #   assert_receive %{a: 1, b: %{a: 2, c: %{d: 4, f: "worl"}}, e: "hello"}
  # end

  # test "Large, nested map, missing" do
  #   send(self(), %{a: 1, b: %{a: 2, c: %{d: 4}}, e: "hello"})
  #   assert_receive %{a: 1, b: %{a: 2, c: %{d: 4, f: "world"}}, e: "hello"}
  # end

  # test "Large, nested map, extra" do
  #   send(self(), %{a: 1, b: %{a: 2, c: %{d: 4, f: "world"}}, e: "hello"})
  #   assert_receive %{a: 1, b: %{a: 2, c: %{d: 4}}, e: "hello"}
  # end

  # test "using variables" do
  #   send(self(), [1, 2, 3])
  #   assert_receive([a, a, 3])
  # end

  # test "using guards" do
  #   send(self(), [1, 2, 3])
  #   assert_receive [a, 2, 3] when is_binary(a)
  # end

  # test "using guards, but guard matches" do
  #   send(self(), [1, 2, 3])
  #   assert_receive [a, b, 3, 4] when is_integer(a)
  # end

  # test "using multiple when and clauses" do
  #   send(self(), [1, 2, 3])
  #   assert_receive [a, b, 3] when is_binary(a) and is_integer(b)
  # end

  test "using multiple when or clauses" do
    send(self(), [1, 2, 3])
    assert_receive [a, b, 3] when is_binary(a) or is_binary(b)
  end

  test "Using a list" do
    send(self(), [1, 2, 3])
    assert_receive [1, 2, 4]
  end
end
