Code.require_file "../../test_helper", __FILE__

defmodule ExUnit.AssertionsTest.Value do
  def tuple, do: { 2, 1 }
end

alias ExUnit.AssertionsTest.Value

defmodule ExUnit.AssertionsTest do
  use ExUnit.Case, sync: false

  test :assert_when_value_is_true do
    true = assert true
  end

  test :assert_when_value_is_false do
    try do
      "This should never be tested" = assert false
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected false to be true" = error.message
    end
  end

  test :assert_with_message_when_value_is_false do
    try do
      "This should never be tested" = assert false, "This should be true"
    rescue
      error in [ExUnit.AssertionError] ->
        "This should be true" = error.message
    end
  end

  test :assert_with_equality do
    try do
      "This should never be tested" = assert 1 + 1 == 1
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected 2 to be equal to (==) 1" = error.message
    end
  end

  test :assert_with_equality_in_reverse do
    try do
      "This should never be tested" = assert 1 == 1 + 1
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected 1 to be equal to (==) 2" = error.message
    end
  end

  test :refute_when_value_is_false do
    false = refute false
  end

  test :refute_when_value_is_true do
    try do
      "This should never be tested" = refute true
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected true to be false" = error.message
    end
  end

  test :refute_with_message_when_value_is_true do
    try do
      "This should never be tested" = refute true, "This should be false"
    rescue
      error in [ExUnit.AssertionError] ->
        "This should be false" = error.message
    end
  end

  test :assert_match_when_equal do
    assert { 2, 1 } = Value.tuple
    true = assert { 2, 1 } = Value.tuple
  end

  test :assert_match_when_different do
    try do
      "This should never be tested" = assert {_, 2} = Value.tuple
    rescue
      error in [ExUnit.AssertionError] ->
        "no match of right hand side value: {2,1}" = error.message
    end
  end

  test :assert_in_when_is_member do
    true = assert 'foo' in ['foo', 'bar']
  end

  test :assert_in_when_is_not_member do
    try do
      "This should never be tested" = assert 'foo' in 'bar'
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected 'foo' to be in 'bar'" = error.message
    end
  end

  test :refute_in_when_is_not_member do
    false = refute 'baz' in ['foo', 'bar']
  end

  test :refute_in_when_is_member do
    try do
      "This should never be tested" = refute 'foo' in ['foo', 'bar']
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected 'foo' to not be in ['foo','bar']" = error.message
    end
  end

  test :assert_match_when_matches do
    true = assert "foo" =~ %r(o)
  end

  test :assert_match_when_no_match do
    try do
      "This should never be tested" = assert "foo" =~ %r(a)
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected \"foo\" to match (=~) %r\"a\"" = error.message
    end
  end

  test :refute_match_when_is_matches do
    false = refute "foo" =~ %r(a)
  end

  test :refute_match_when_no_match do
    try do
      "This should never be tested" = refute "foo" =~ %r(o)
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected \"foo\" to not match %r\"o\"" = error.message
    end
  end

  test :assert_raise_when_no_error do
    "This should never be tested" = assert_raise ArgumentError, fn ->
      # nothing
    end
  rescue
    error in [ExUnit.AssertionError] ->
      "Expected ArgumentError exception but nothing was raised" = error.message
  end

  test :assert_raise_when_error do
    error = assert_raise ArgumentError, fn ->
      raise ArgumentError, message: "test error"
    end

    "test error" = error.message
  end

  test :assert_raise_when_other_error do
    try do
      "This should never be tested" = assert_raise ArgumentError, fn ->
        Certainly.Undefined.function(1,2,3)
      end
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected exception ArgumentError, got UndefinedFunctionError" = error.message
    end
  end

  test :assert_raise_when_erlang_error do
    try do
      assert_raise SyntaxError, fn ->
        List.flatten(1)
      end
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected exception SyntaxError, got FunctionClauseError" = error.message
    end
  end

  test :assert_operator_greater_pass do
    true = assert 2 > 1
  end

  test :assert_operator_greater_fail do
    try do
      "This should never be tested" = assert 1 > 2
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected 1 to be more than 2" = error.message
    end
  end

  test :assert_operator_less_or_equal_than_pass do
    true = assert 1 <= 2
  end

  test :assert_operator_less_or_equal_than_fail do
    try do
      "This should never be tested" = assert 2 <= 1
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected 2 to be less than or equal to 1" = error.message
    end
  end

  test :assert_operator_with_expressions do
    greater = 5
    true = assert 1 + 2 < greater
  end

  test :assert_operator_with_message do
    try do
      "This should never be tested" = assert 1 > 2, "assertion"
    rescue
      error in [ExUnit.AssertionError] ->
        "assertion" = error.message
    end
  end

  test :assert_empty_when_empty do
    true = assert_empty []
  end

  test :assert_empty_when_not_empty do
    try do
      "This should never be tested" = assert_empty [1, 2]
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected [1,2] to be empty" = error.message
    end
  end

  test :assert_empty_with_message do
    try do
      "This should never be tested" = assert_empty [1, 2], "test message"
    rescue
      error in [ExUnit.AssertionError] ->
        "test message" = error.message
    end
  end

  test :refute_empty_when_not_empty do
    false = refute_empty [1, 2]
  end

  test :refute_empty_when_empty do
    try do
      "This should never be tested" = refute_empty []
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected [] to not be empty" = error.message
    end
  end

  test :refute_empty_with_message do
    try do
      "This should never be tested" = refute_empty [], "test message"
    rescue
      error in [ExUnit.AssertionError] ->
        "test message" = error.message
    end
  end

  test :assert_nil_when_nil do
    true = assert_nil nil
  end

  test :assert_nil_when_not_nil do
    try do
      "This should never be tested" = assert_nil true
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected true to be nil" = error.message
    end
  end

  test :assert_nil_with_message do
    try do
      "This should never be tested" = assert_nil false, "test message"
    rescue
      error in [ExUnit.AssertionError] ->
        "test message" = error.message
    end
  end

  test :refute_nil_when_not_nil do
    false = refute_nil true
  end

  test :refute_nil_when_nil do
    try do
      "This should never be tested" = refute_nil nil
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected nil to not be nil" = error.message
    end
  end

  test :refute_nil_with_message do
    try do
      "This should never be tested" = refute_nil nil, "test message"
    rescue
      error in [ExUnit.AssertionError] ->
        "test message" = error.message
    end
  end

  test :assert_in_delta_pass do
    true = assert_in_delta(1.1, 1.2, 0.2)
  end

  test :assert_in_delta_fail do
    try do
      "This should never be tested" = assert_in_delta(10, 12, 1)
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected |10 - 12| (2) to be < 1" = error.message
    end
  end

  test :assert_in_delta_with_message do
    try do
      "This should never be tested" = assert_in_delta(10, 12, 1, "test message")
    rescue
      error in [ExUnit.AssertionError] ->
        "test message" = error.message
    end
  end

  test :refute_in_delta_pass do
    false = refute_in_delta(1.1, 1.5, 0.2)
  end

  test :refute_in_delta_fail do
    try do
      "This should never be tested" = refute_in_delta(10, 11, 2)
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected |10 - 11| (1) to not be < 2" = error.message
    end
  end

  test :refute_in_delta_with_message do
    try do
      "This should never be tested" = refute_in_delta(10, 11, 2, "test message")
    rescue
      error in [ExUnit.AssertionError] ->
        "test message" = error.message
    end
  end

  test :assert_throw_when_no_throw do
    try do
      "This should never be tested" = assert_throw 1, fn ->
        # nothing
      end
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected throw 1, got nothing" = error.message
    end
  end

  test :assert_throw_when_throw do
    1 = assert_throw 1, fn ->
      throw 1
    end
  end

  test :assert_throw_when_other_throw do
    try do
      "This should never be tested" = assert_throw 1, fn ->
        throw 2
      end
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected throw 1, got 2" = error.message
    end
  end

  test :assert_exit_when_no_exit do
    try do
      "This should never be tested" = assert_exit 1, fn ->
        # nothing
      end
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected exit 1, got nothing" = error.message
    end
  end

  test :assert_exit_when_exit do
    1 = assert_exit 1, fn ->
      exit 1
    end
  end

  test :assert_exit_when_other_exit do
    try do
      "This should never be tested" = assert_exit 1, fn ->
        exit 2
      end
    rescue
      error in [ExUnit.AssertionError] ->
        "Expected exit 1, got 2" = error.message
    end
  end

  test :assert_error_when_error do
    :function_clause = assert_error :function_clause, fn ->
      List.flatten(1)
    end
  end

  test :flunk do
    try do
      "This should never be tested" = flunk
    rescue
      error in [ExUnit.AssertionError] ->
        "Epic Fail!" = error.message
    end
  end

  test :flunk_with_message do
    try do
      "This should never be tested" = flunk "This should raise an error"
    rescue
      error in [ExUnit.AssertionError] ->
        "This should raise an error" = error.message
    end
  end
end
