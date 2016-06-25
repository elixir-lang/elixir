Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.AssertionsTest.Value do
  def tuple, do: {2, 1}
  def falsy, do: false
  def truthy, do: true
end

alias ExUnit.AssertionsTest.Value

defmodule ExUnit.AssertionsTest do
  use ExUnit.Case, async: true

  defmacrop assert_ok(arg) do
    quote do
      assert {:ok, val} = ok(unquote(arg))
    end
  end

  test "assert inside macro" do
    assert_ok 42
  end

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

  test "assert match with pinned variable" do
    a = 1
    {2, 1} = (assert {2, ^a} = Value.tuple)

    try do
      assert {^a, 1} = Value.tuple
    rescue
      error in [ExUnit.AssertionError] ->
        "match (=) failed\n" <>
        "The following variables were pinned:\n" <>
        "  a = 1" = error.message
        "{^a, 1} = Value.tuple()" = Macro.to_string(error.expr)
    end
  end

  test "assert match?" do
    true = assert match?({2, 1}, Value.tuple)

    try do
      "This should never be tested" = assert match?({:ok, _}, error(true))
    rescue
      error in [ExUnit.AssertionError] ->
        "match (match?) failed" = error.message
        "match?({:ok, _}, error(true))" = Macro.to_string(error.expr)
        "{:error, true}" = Macro.to_string(error.right)
    end
  end

  test "refute match?" do
    false = refute match?({1, 1}, Value.tuple)

    try do
      "This should never be tested" = refute match?({:error, _}, error(true))
    rescue
      error in [ExUnit.AssertionError] ->
        "match (match?) succeeded, but should have failed" = error.message
        "match?({:error, _}, error(true))" = Macro.to_string(error.expr)
        "{:error, true}" = Macro.to_string(error.right)
    end
  end

  test "assert match? with pinned variable" do
    a = 1
    try do
      "This should never be tested" = assert(match?({^a, 1}, Value.tuple))
    rescue
      error in [ExUnit.AssertionError] ->
        "match (match?) failed\n" <>
        "The following variables were pinned:\n" <>
        "  a = 1" = error.message
        "match?({^a, 1}, Value.tuple())" = Macro.to_string(error.expr)
    end
  end

  test "refute match? with pinned variable" do
    a = 2
    try do
      "This should never be tested" = refute(match?({^a, 1}, Value.tuple))
    rescue
      error in [ExUnit.AssertionError] ->
        "match (match?) succeeded, but should have failed\n" <>
        "The following variables were pinned:\n" <>
        "  a = 2" = error.message
        "match?({^a, 1}, Value.tuple())" = Macro.to_string(error.expr)
    end
  end

  test "assert receive waits" do
    parent = self()
    spawn fn -> send parent, :hello end
    :hello = assert_receive :hello
  end

  test "assert received does not wait" do
    send self(), :hello
    :hello = assert_received :hello
  end

  @received :hello

  test "assert received with module attribute" do
    send self(), :hello
    :hello = assert_received @received
  end

  test "assert received with pinned variable" do
    status = :valid
    send self(), {:status, :invalid}
    try do
      "This should never be tested" = assert_received {:status, ^status}
    rescue
      error in [ExUnit.AssertionError] ->
        "No message matching {:status, ^status} after 0ms.\n" <>
        "The following variables were pinned:\n" <>
        "  status = :valid\n" <>
        "Process mailbox:\n" <>
        "  {:status, :invalid}" = error.message
    end
  end

  test "assert received with multiple identical pinned variables" do
    status = :valid
    send self(), {:status, :invalid, :invalid}
    try do
      "This should never be tested" = assert_received {
        :status,
        ^status,
        ^status
      }
    rescue
      error in [ExUnit.AssertionError] ->
        "No message matching {:status, ^status, ^status} after 0ms.\n" <>
        "The following variables were pinned:\n" <>
        "  status = :valid\n" <>
        "Process mailbox:\n" <>
        "  {:status, :invalid, :invalid}" = error.message
    end
  end

  test "assert received with multiple unique pinned variables" do
    status = :valid
    other_status = :invalid
    send self(), {:status, :invalid, :invalid}
    try do
      "This should never be tested" = assert_received {
        :status,
        ^status,
        ^other_status
      }
    rescue
      error in [ExUnit.AssertionError] ->
        "No message matching {:status, ^status, ^other_status} after 0ms.\n" <>
        "The following variables were pinned:\n" <>
        "  status = :valid\n" <>
        "  other_status = :invalid\n" <>
        "Process mailbox:\n" <>
        "  {:status, :invalid, :invalid}" = error.message
    end
  end

  test "assert received when empty mailbox" do
    try do
      "This should never be tested" = assert_received :hello
    rescue
      error in [ExUnit.AssertionError] ->
        "No message matching :hello after 0ms.\nThe process mailbox is empty." = error.message
    end
  end

  test "assert received when different message" do
    send self(), {:message, :not_expected, :at_all}
    try do
      "This should never be tested" = assert_received :hello
    rescue
      error in [ExUnit.AssertionError] ->
        "No message matching :hello after 0ms.\n" <>
        "Process mailbox:\n" <>
        "  {:message, :not_expected, :at_all}" = error.message
    end
  end

  test "assert received when different message having more than 10 on mailbox" do
    for i <- 1..11, do: send(self(), {:message, i})
    try do
      "This should never be tested" = assert_received x when x == :hello
    rescue
      error in [ExUnit.AssertionError] ->
        "No message matching x when x == :hello after 0ms.\nProcess mailbox:" <>
        "\n  {:message, 1}\n  {:message, 2}\n  {:message, 3}" <>
        "\n  {:message, 4}\n  {:message, 5}\n  {:message, 6}" <>
        "\n  {:message, 7}\n  {:message, 8}\n  {:message, 9}" <>
        "\n  {:message, 10}\nShowing only 10 of 11 messages." = error.message
    end
  end

  test "assert received leaks" do
    send self(), {:hello, :world}
    assert_received {:hello, world}
    :world = world
  end

  test "assert received does not leak external variables used in guards" do
    send self(), {:hello, :world}
    guard_world = :world
    assert_received {:hello, world} when world == guard_world
    :world = world
  end

  test "refute received does not wait" do
    false = refute_received :hello
  end

  test "refute receive waits" do
    false = refute_receive :hello
  end

  test "refute received when equal" do
    send self(), :hello
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
    {:ok, true} = assert {:ok, _} = ok(true)
  end

  test "assert match with bitstrings" do
    "foobar" = assert "foo" <> bar = "foobar"
    "bar" = bar
  end

  test "assert match when no match" do
    try do
      assert {:ok, _} = error(true)
    rescue
      error in [ExUnit.AssertionError] ->
        "match (=) failed"       = error.message
        "{:ok, _} = error(true)" = error.expr |> Macro.to_string
        "{:error, true}"         = error.right |> Macro.to_string
    end
  end

  test "assert match when falsy but not match" do
    try do
      assert {:ok, _x} = nil
    rescue
      error in [ExUnit.AssertionError] ->
        "match (=) failed" = error.message
        "{:ok, _x} = nil"   = error.expr |> Macro.to_string
        "nil"              = error.right |> Macro.to_string
    end
  end

  test "assert match when falsy" do
    try do
      assert _x = nil
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected truthy, got nil" = error.message
        "_x = nil" = error.expr |> Macro.to_string
    end
  end

  test "refute match when no match" do
    try do
      "This should never be tested" = refute _ = ok(true)
    rescue
      error in [ExUnit.AssertionError] ->
        "_ = ok(true)" = error.expr |> Macro.to_string
        "Expected false or nil, got {:ok, true}" = error.message
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
      nil
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
      "(function Not.Defined.function/3 is undefined (module Not.Defined is not available))" = error.message
  end

  test "assert raise with some other error includes stacktrace from original error" do
    "This should never be tested" = assert_raise ArgumentError, fn ->
      Not.Defined.function(1, 2, 3)
    end
  rescue
    ExUnit.AssertionError ->
      stacktrace = System.stacktrace
      [{Not.Defined, :function, [1, 2, 3], _} | _] = stacktrace
  end

  test "assert raise with Erlang error" do
    assert_raise SyntaxError, fn ->
      List.flatten(1)
    end
  rescue
    error in [ExUnit.AssertionError] ->
      "Expected exception SyntaxError but got FunctionClauseError (no function clause matching in :lists.flatten/1)" = error.message
  end

  test "assert raise comparing messages (for equality)" do
    assert_raise RuntimeError, "foo", fn ->
      raise RuntimeError, "bar"
    end
  rescue
    error in [ExUnit.AssertionError] ->
      "Wrong message for RuntimeError" <>
      "\nexpected:" <>
      "\n  \"foo\"" <>
      "\nactual:" <>
      "\n  \"bar\"" = error.message
  end

  test "assert raise comparing messages (with a regex)" do
    assert_raise RuntimeError, ~r/ba[zk]/, fn ->
      raise RuntimeError, "bar"
    end
  rescue
    error in [ExUnit.AssertionError] ->
      "Wrong message for RuntimeError" <>
      "\nexpected:" <>
      "\n  ~r/ba[zk]/" <>
      "\nactual:" <>
      "\n  \"bar\"" = error.message
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
    "This should never be tested" = flunk()
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

  test "flunk with wrong argument type" do
    "This should never be tested" = flunk ["flunk takes a binary, not a list"]
  rescue
    error ->
      "no function clause matching in ExUnit.Assertions.flunk/1" = FunctionClauseError.message error
  end

  test "AssertionError message should include nice formatting" do
    assert :a = :b
  rescue
    error in [ExUnit.AssertionError] ->
      """


      match (=) failed
      code: :a = :b
      rhs:  :b
      """ = Exception.message(error)
  end

  defp ok(val), do: {:ok, val}
  defp error(val), do: {:error, val}
end
