Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.AssertionsTest.Value do
  def tuple, do: {2, 1}
  def falsy, do: false
  def truthy, do: true
end

alias ExUnit.AssertionsTest.Value

defmodule ExUnit.AssertionsTest do
  use ExUnit.Case, async: true

  test "assert with true value" do
    true = assert Value.truthy
  end

  test "assert with message when value is false" do
    try do
      "This should never be tested" = assert false, "This should be true"
    rescue
      error in [ExUnit.AssertionError] ->
        "This should be true" = error.message
    end
  end

  test "assert when value evaluates to false" do
    try do
      "This should never be tested" = assert Value.falsy
    rescue
      error in [ExUnit.AssertionError] ->
        "Value.falsy()" = error.expr |> Macro.to_string
        "Expected truthy, got false" = error.message
    end
  end

  test "assert with equality" do
    try do
      "This should never be tested" = assert 1 + 1 == 1
    rescue
      error in [ExUnit.AssertionError] ->
        1 = error.right
        2 = error.left
        "1 + 1 == 1" = error.expr |> Macro.to_string
    end
  end

  test "assert with equality in reverse" do
    try do
      "This should never be tested" = assert 1 == 1 + 1
    rescue
      error in [ExUnit.AssertionError] ->
        1 = error.left
        2 = error.right
        "1 == 1 + 1" = error.expr |> Macro.to_string
    end
  end

  test "refute when value is false" do
    false = refute false
  end

  test "refute when value evaluates to true" do
    try do
      refute Value.truthy
      raise "refute was supposed to fail"
    rescue
      error in [ExUnit.AssertionError] ->
        "Value.truthy()"   = error.expr |> Macro.to_string
        "Expected false or nil, got true" = error.message
    end
  end

  test "assert match when equal" do
    {2, 1} = (assert {2, 1} = Value.tuple)
  end

  test "assert receive waits" do
    parent = self
    spawn fn -> send parent, :hello end
    :hello = assert_receive :hello
  end

  test "assert received does not wait" do
    send self, :hello
    :hello = assert_received :hello
  end

  test "assert received when empty mailbox" do
    try do
      "This should never be tested" = assert_received :hello
    rescue
      error in [ExUnit.AssertionError] ->
        "No message matching :hello after 0ms. The process mailbox is empty." = error.message
    end
  end

  test "assert received when different message" do
    send self, {:message, :not_expected, :at_all}
    try do
      "This should never be tested" = assert_received :hello
    rescue
      error in [ExUnit.AssertionError] ->
        "No message matching :hello after 0ms. Process mailbox:\n{:message, :not_expected, :at_all}" = error.message
    end
  end

  test "assert received when different message having more than 10 on mailbox" do
    for i <- 1..11, do: send(self, {:message, i})
    try do
      "This should never be tested" = assert_received x when x == :hello
    rescue
      error in [ExUnit.AssertionError] ->
        "No message matching x when x == :hello after 0ms. Process mailbox:\n" <>
        "{:message, 1}\n{:message, 2}\n{:message, 3}\n{:message, 4}\n" <>
        "{:message, 5}\n{:message, 6}\n{:message, 7}\n{:message, 8}\n" <>
        "{:message, 9}\n{:message, 10}\nShowing only 10 of 11 messages." = error.message
    end
  end

  test "assert received leaks" do
    send self, {:hello, :world}
    assert_received {:hello, world}
    :world = world
  end

  test "refute received does not wait" do
    false = refute_received :hello
  end

  test "refute receive waits" do
    false = refute_receive :hello
  end

  test "refute received when equal" do
    send self, :hello
    try do
      "This should never be tested" = refute_received :hello
    rescue
      error in [ExUnit.AssertionError] ->
        "Unexpectedly received message :hello (which matched :hello)" = error.message
    end
  end

  test "assert in when member" do
    true = assert 'foo' in ['foo', 'bar']
  end

  test "assert in when is not member" do
    try do
      "This should never be tested" = assert 'foo' in 'bar'
    rescue
      error in [ExUnit.AssertionError] ->
        'foo' = error.left
        'bar' = error.right
        "'foo' in 'bar'" = error.expr |> Macro.to_string
    end
  end

  test "refute in when is not member" do
    false = refute 'baz' in ['foo', 'bar']
  end

  test "refute in when is member" do
    try do
      "This should never be tested" = refute 'foo' in ['foo', 'bar']
    rescue
      error in [ExUnit.AssertionError] ->
        'foo'          = error.left
        ['foo', 'bar'] = error.right
        "'foo' in ['foo', 'bar']" = error.expr |> Macro.to_string
    end
  end

  test "assert match" do
    {:ok, true} = assert {:ok, _} = {:ok, true}
  end

  test "assert match when no match" do
    try do
      "This should never be tested" = assert {:ok, _} = "bar"
    rescue
      error in [ExUnit.AssertionError] ->
        "match (=) failed"   = error.message
        "{:ok, _} = \"bar\"" = error.expr |> Macro.to_string
        "bar"                = error.right
    end
  end

  test "refute match when no match" do
    try do
      "This should never be tested" = refute _ = "bar"
    rescue
      error in [ExUnit.AssertionError] ->
        "bar"         = error.right
        "_ = \"bar\"" = error.expr |> Macro.to_string
        "match (=) succeeded, but should have failed" = error.message
    end
  end

  test "assert regex match" do
    true = assert "foo" =~ ~r(o)
  end

  test "assert regex match when no match" do
    try do
      "This should never be tested" = assert "foo" =~ ~r(a)
    rescue
      error in [ExUnit.AssertionError] ->
        "foo" = error.left
        ~r{a} = error.right
    end
  end

  test "refute regex match" do
    false = refute "foo" =~ ~r(a)
  end

  test "refute regex match when match" do
    try do
      "This should never be tested" = refute "foo" =~ ~r(o)
    rescue
      error in [ExUnit.AssertionError] ->
        "foo" = error.left
        ~r"o" = error.right
    end
  end

  test "assert raise with no error" do
    "This should never be tested" = assert_raise ArgumentError, fn ->
      # nothing
    end
  rescue
    error in [ExUnit.AssertionError] ->
      "Expected exception ArgumentError but nothing was raised" = error.message
  end

  test "assert raise with error" do
    error = assert_raise ArgumentError, fn ->
      raise ArgumentError, "test error"
    end

    "test error" = error.message
  end

  test "assert raise with some other error" do
    "This should never be tested" = assert_raise ArgumentError, fn ->
      Not.Defined.function(1, 2, 3)
    end
  rescue
    error in [ExUnit.AssertionError] ->
      "Expected exception ArgumentError but got UndefinedFunctionError " <>
      "(undefined function: Not.Defined.function/3 (module Not.Defined is not available))" = error.message
  end

  test "assert raise with erlang error" do
    assert_raise SyntaxError, fn ->
      List.flatten(1)
    end
  rescue
    error in [ExUnit.AssertionError] ->
      "Expected exception SyntaxError but got FunctionClauseError (no function clause matching in :lists.flatten/1)" = error.message
  end

  test "assert greater than operator" do
    true = assert 2 > 1
  end

  test "assert greater than operator error" do
    "This should never be tested" = assert 1 > 2
  rescue
    error in [ExUnit.AssertionError] ->
      1       = error.left
      2       = error.right
      "1 > 2" = error.expr |> Macro.to_string
  end

  test "assert less or equal than operator" do
    true = assert 1 <= 2
  end

  test "assert less or equal than operator error" do
    "This should never be tested" = assert 2 <= 1
  rescue
    error in [ExUnit.AssertionError] ->
      "2 <= 1" = error.expr |> Macro.to_string
      2 = error.left
      1 = error.right
  end

  test "assert operator with expressions" do
    greater = 5
    true = assert 1 + 2 < greater
  end

  test "assert operator with custom message" do
    "This should never be tested" = assert 1 > 2, "assertion"
  rescue
    error in [ExUnit.AssertionError] ->
      "assertion" = error.message
  end

  test "assert in delta" do
    true = assert_in_delta(1.1, 1.2, 0.2)
  end

  test "assert in delta error" do
    "This should never be tested" = assert_in_delta(10, 12, 1)
  rescue
    error in [ExUnit.AssertionError] ->
      "Expected the difference between 10 and 12 (2) to be less than 1" = error.message
  end

  test "assert in delta with message" do
    "This should never be tested" = assert_in_delta(10, 12, 1, "test message")
  rescue
    error in [ExUnit.AssertionError] ->
      "test message" = error.message
  end

  test "refute in delta" do
    false = refute_in_delta(1.1, 1.5, 0.2)
  end

  test "refute in delta error" do
    "This should never be tested" = refute_in_delta(10, 11, 2)
  rescue
    error in [ExUnit.AssertionError] ->
      "Expected the difference between 10 and 11 (1) to be more than 2" = error.message
  end

  test "refute in delta with message" do
    "This should never be tested" = refute_in_delta(10, 11, 2, "test message")
  rescue
    error in [ExUnit.AssertionError] ->
      "test message (difference between 10 and 11 is less than 2)" = error.message
  end

  test "catch_throw with no throw" do
    catch_throw(1)
  rescue
    error in [ExUnit.AssertionError] ->
      "Expected to catch throw, got nothing" = error.message
  end

  test "catch_error with no error" do
    catch_error(1)
  rescue
    error in [ExUnit.AssertionError] ->
      "Expected to catch error, got nothing" = error.message
  end

  test "catch_exit with no exit" do
    catch_exit(1)
  rescue
    error in [ExUnit.AssertionError] ->
      "Expected to catch exit, got nothing" = error.message
  end

  test "catch_throw with throw" do
    1 = catch_throw(throw 1)
  end

  test "catch_exit with exit" do
    1 = catch_exit(exit 1)
  end

  test "catch_error with error" do
    :function_clause = catch_error(List.flatten(1))
  end

  test "flunk" do
    "This should never be tested" = flunk
  rescue
    error in [ExUnit.AssertionError] ->
      "Flunked!" = error.message
  end

  test "flunk with message" do
    "This should never be tested" = flunk "This should raise an error"
  rescue
    error in [ExUnit.AssertionError] ->
      "This should raise an error" = error.message
  end
end
