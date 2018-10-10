ExUnit.start [seed: 0]

defmodule TestOneOfEach do
  @moduledoc """
  This module contains one of each type of failing test.
  It is used simply to document the style of each.
  """
  use ExUnit.Case

  @one 1
  @two 2

  @long_data_1  [field1: "one", field2: {:two1, :two2}, field3: 'three', field4: [1, 2, 3, 4]]
  @long_data_2  [field1: "one", field2: {:two1, :two3}, field3: 'three', field4: [1, 2, 3, 4]]

  setup do
    {:ok, user_id: 1, post_id: 2, many_ids: Enum.to_list(1..50)}
  end

  test "1. assert with a match" do
    assert [@one] = [@two]
  end

  test "2. assert with a binary operator" do
    assert @one * 4 > @two *3
  end

  test "3. assert match with a long argument" do
    assert @long_data_1 = @long_data_2
  end

  test "4. assert equality with a long argument" do
    assert @long_data_1 == @long_data_2
  end

  test "5. refute with a match" do
    refute [@one] = [@one]
  end

  test "6. refute with a binary operator" do
    refute @one * 6 > @two *2
  end

  test "7. refute match with a long argument" do
    refute @long_data_1 = @long_data_1
  end

  test "8. refute equality with a long argument" do
    refute @long_data_1 == @long_data_1
  end

  test "9. assert of an expression with a message" do
    assert 1 > 2, "is one greater than two?"
  end

  test "10. assert with explicit expected and actual values" do
    assert @one > @two, left: @one, right: @two, message: "one should be greater than two"
  end

  test "11. assert that a message is ready to be received" do
    assert_received :no_message_there
  end

  test "12. assert that a message is received within a timeout" do
    send self(), {:ok, 1}
    send self(), :message_in_my_inbox
    send self(), {:ok, 2}
    send self(), :another_message
    assert_receive :no_message_after_timeout
  end

  test "13. assert an exception with a given message is raised, but no exception" do
    assert_raise(SomeException, "some message", fn -> nil end)
  end

  test "14. assert an exception with a given message is raised" do
    assert_raise(SomeException, "some message", fn ->
                                                     raise "other exception"
                                                end)
  end

  test "15. assert an exception with a given message is raised, but the message is wrong" do
    assert_raise(RuntimeError, "some message", fn ->
                                                    raise "other error"
                                               end)
  end

  test "16. assert an exception is raised" do
    assert_raise(SomeException, fn -> nil end)
  end

  test "17. assert two values are within some delta" do
    assert_in_delta 3.1415926, 22.0/7, 0.001
  end

  test "18. refute a value with a message" do
    refute @one != @two, "one should equal two"
  end

  test "19. refute a message is received within a timeout" do
    send self(), {:hello, "Dave"}
    refute_receive {:hello, _}, 1000
  end

  test "20. refute a message is ready to be received" do
    send self(), :hello_again
    refute_received :hello_again
  end

  test "21. refute two values are within delta" do
    refute_in_delta @one, @two, 1.5
  end

  test "22. refute two values are within delta with message" do
    refute_in_delta @one, @two, 1.5, "they shouldn't be this close"
  end

  test "23. flunk" do
    flunk "we failed. totally"
  end

  test "24. exception raised while running test" do
    assert blows_up()
  end

  test "25. error due to exit" do
    spawn_link fn -> raise "oops" end
    receive do
    end
  end

  test "26. multi error" do
    error1 =
      try do
        assert [@one] = [@two]
      rescue e in ExUnit.AssertionError ->
        {:error, e, System.stacktrace}
      end

    error2 =
      try do
        assert @one * 4 > @two *3
      rescue e in ExUnit.AssertionError ->
        {:error, e, System.stacktrace}
      end

    raise ExUnit.MultiError, errors: [error1, error2]
  end

  @tag capture_log: true
  test "27. log capturing" do
    require Logger
    Logger.debug "this will be logged"
    flunk "oops"
  end

  test "28. function clause error" do
    Access.fetch(:foo, :bar)
  end

  test "29. function call arguments" do
    assert some_vars(1 + 2, 3 + 4)
  end

  defp some_vars(_a, _b) do
    false
  end

  defp blows_up do
    ignite(0) + 1
  end

  defp ignite(val) do
    1/val
  end
end
