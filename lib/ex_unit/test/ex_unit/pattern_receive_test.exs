Code.require_file("../test_helper.exs", __DIR__)

# Jowens - do we need a special formatter, similar to diff to do the patterns?
# jowens - reveiw this output, and then start coding up a better solution?

defmodule ExUnit.PatternReceiveTest do
  use ExUnit.Case

  # test "message recevied, no messages" do
  #   assert %{a: 2} == %{a: 1, b: 1, d: 2, c: 3}
  #   assert_receive 1
  # end

  # test "message recevied, non-matching message" do
  #   send(self(), 2)
  #   assert_receive 1
  # end

  # test "message received, map" do
  #   send(self(), %{a: 1, b: 1, d: 2, c: 3})
  #   assert_receive %{a: 2}
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

  # test "Hi" do
  #   send(self(), [1, 2, 3])
  #   assert_receive [1 | rest]
  # end

  test "toss it" do
    send(self(), [1, 2, 3])
    assert_receive [1 | _rest]
  end
end
