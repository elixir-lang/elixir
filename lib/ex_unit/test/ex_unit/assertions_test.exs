Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.AssertionsTest.Value do
  def tuple, do: {2, 1}
  def falsy, do: false
  def truthy, do: true
end

alias ExUnit.AssertionsTest.Value

defmodule ExUnit.AssertionsTest do
  use ExUnit.Case, async: true

  defp assert_error do
    [{:error, error, _}] = ExUnit.FailureCollector.get_failures(self)
    error
  end

  test "assert with true value" do
    true = assert Value.truthy
  end

  test "assert with message when value is false" do
    assert false, "This should be true"
    "This should be true" = assert_error().message
  end

  test "assert when value evaluates to false" do
    assert Value.falsy

    error = assert_error()
    "Value.falsy()" = error.expr |> Macro.to_string
    "Expected truthy, got false" = error.message
  end

  test "assert with equality" do
    assert 1 + 1 == 1

    error = assert_error()
    1 = error.right
    2 = error.left
    "1 + 1 == 1" = error.expr |> Macro.to_string
  end

  test "assert with equality in reverse" do
    assert 1 == 1 + 1

    error = assert_error()
    1 = error.left
    2 = error.right
    "1 == 1 + 1" = error.expr |> Macro.to_string
  end

  test "refute when value is false" do
    false = refute false
  end

  test "refute when value evaluates to true" do
    refute Value.truthy
    error = assert_error()
    "Value.truthy()"   = error.expr |> Macro.to_string
    "Expected false or nil, got true" = error.message
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
    assert_received :hello
    error = assert_error()
    "No message matching :hello after 0ms. The process mailbox is empty." = error.message
  end

  test "assert received when different message" do
    send self, {:message, :not_expected, :at_all}
    assert_received :hello

    error = assert_error()
    "No message matching :hello after 0ms. Process mailbox:\n{:message, :not_expected, :at_all}" = error.message
  end

  test "assert received when different message having more than 10 on mailbox" do
    for i <- 1..11, do: send(self, {:message, i})
    assert_received x when x == :hello

    error = assert_error()
      "No message matching x when x == :hello after 0ms. Process mailbox:\n" <>
      "{:message, 1}\n{:message, 2}\n{:message, 3}\n{:message, 4}\n" <>
      "{:message, 5}\n{:message, 6}\n{:message, 7}\n{:message, 8}\n" <>
      "{:message, 9}\n{:message, 10}\nShowing only 10 of 11 messages." = error.message
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
    refute_received :hello

    error = assert_error()
    "Unexpectedly received message :hello (which matched :hello)" = error.message
  end

  test "assert in when member" do
    true = assert 'foo' in ['foo', 'bar']
  end

  test "assert in when is not member" do
    assert 'foo' in 'bar'

    error = assert_error()
    'foo' = error.left
    'bar' = error.right
    "'foo' in 'bar'" = error.expr |> Macro.to_string
  end

  test "refute in when is not member" do
    false = refute 'baz' in ['foo', 'bar']
  end

  test "refute in when is member" do
    refute 'foo' in ['foo', 'bar']

    error = assert_error()
    'foo'          = error.left
    ['foo', 'bar'] = error.right
    "'foo' in ['foo', 'bar']" = error.expr |> Macro.to_string
  end

  test "assert match" do
    {:ok, true} = assert {:ok, _} = {:ok, true}
  end

  test "assert match when no match" do
    assert {:ok, _} = "bar"

    error = assert_error()
    "match (=) failed"   = error.message
    "{:ok, _} = \"bar\"" = error.expr |> Macro.to_string
    "bar"                = error.right
  end

  test "refute match when no match" do
    refute _ = "bar"

    error = assert_error()
    "bar"         = error.right
    "_ = \"bar\"" = error.expr |> Macro.to_string
    "match (=) succeeded, but should have failed" = error.message
  end

  test "assert regex match" do
    true = assert "foo" =~ ~r(o)
  end

  test "assert regex match when no match" do
    assert "foo" =~ ~r(a)

    error = assert_error()
    "foo" = error.left
    ~r{a} = error.right
  end

  test "refute regex match" do
    false = refute "foo" =~ ~r(a)
  end

  test "refute regex match when match" do
    refute "foo" =~ ~r(o)

    error = assert_error()
    "foo" = error.left
    ~r"o" = error.right
  end

  test "assert raise with no error" do
    assert_raise ArgumentError, fn ->
      # nothing
    end
    error = assert_error()
    "Expected exception ArgumentError but nothing was raised" = error.message
  end

  test "assert raise with error" do
    error = assert_raise ArgumentError, fn ->
      raise ArgumentError, "test error"
    end
    "test error" = error.message
  end

  test "assert raise with some other error" do
    assert_raise ArgumentError, fn ->
      Not.Defined.function(1, 2, 3)
    end
    error = assert_error()
    "Expected exception ArgumentError but got UndefinedFunctionError " <>
    "(undefined function: Not.Defined.function/3 (module Not.Defined is not available))" = error.message
  end

  test "assert raise with erlang error" do
    assert_raise SyntaxError, fn ->
      List.flatten(1)
    end
    error = assert_error()
    "Expected exception SyntaxError but got FunctionClauseError (no function clause matching in :lists.flatten/1)" = error.message
  end

  test "assert greater than operator" do
    true = assert 2 > 1
  end

  test "assert greater than operator error" do
    assert 1 > 2

    error = assert_error()
    1       = error.left
    2       = error.right
    "1 > 2" = error.expr |> Macro.to_string
  end

  test "assert less or equal than operator" do
    true = assert 1 <= 2
  end

  test "assert less or equal than operator error" do
    assert 2 <= 1

    error = assert_error()
    "2 <= 1" = error.expr |> Macro.to_string
    2 = error.left
    1 = error.right
  end

  test "assert operator with expressions" do
    greater = 5
    true = assert 1 + 2 < greater
  end

  test "assert operator with custom message" do
    assert 1 > 2, "assertion"

    error = assert_error()
    "assertion" = error.message
  end

  test "assert in delta" do
    true = assert_in_delta(1.1, 1.2, 0.2)
  end

  test "assert in delta error" do
    assert_in_delta(10, 12, 1)

    error = assert_error()
    "Expected the difference between 10 and 12 (2) to be less than 1" = error.message
  end

  test "assert in delta with message" do
    assert_in_delta(10, 12, 1, "test message")

    error = assert_error()
    "test message" = error.message
  end

  test "refute in delta" do
    false = refute_in_delta(1.1, 1.5, 0.2)
  end

  test "refute in delta error" do
    refute_in_delta(10, 11, 2)

    error = assert_error()
    "Expected the difference between 10 and 11 (1) to be more than 2" = error.message
  end

  test "refute in delta with message" do
    refute_in_delta(10, 11, 2, "test message")

    error = assert_error()
    "test message (difference between 10 and 11 is less than 2)" = error.message
  end

  test "catch_throw with no throw" do
    catch_throw(1)

    error = assert_error()
    "Expected to catch throw, got nothing" = error.message
  end

  test "catch_error with no error" do
    catch_error(1)

    error = assert_error()
    "Expected to catch error, got nothing" = error.message
  end

  test "catch_exit with no exit" do
    catch_exit(1)

    error = assert_error()
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
    flunk

    error = assert_error()
    "Flunked!" = error.message
  end

  test "flunk with message" do
    flunk "This should raise an error"

    error = assert_error()
    "This should raise an error" = error.message
  end

  test "flunk with wrong argument type" do
    "This should never be tested" = flunk ["flunk takes a binary, not a list"]
  rescue
    error ->
      "no function clause matching in ExUnit.Assertions.flunk/1" = FunctionClauseError.message error
  end
end
