Code.require_file "../../test_helper", __FILE__

defmodule ExUnit::AssertionsTest::Value do
  def tuple, do: { 2, 1 }
end

defmodule ExUnit::AssertionsTest do
  use ExUnit::Case

  test :assert_when_value_is_true do
    true = assert true
  end

  test :assert_when_value_is_false do
    "This should never be tested" = assert false
  rescue: error in [ExUnit::AssertionError]
    "Expected false to be true" = error.message
  end

  test :assert_with_message_when_value_is_false do
    "This should never be tested" = assert false, "This should be true"
  rescue: error in [ExUnit::AssertionError]
    "This should be true" = error.message
  end

  test :refute_when_value_is_false do
    false = refute false
  end

  test :refute_when_value_is_true do
    "This should never be tested" = refute true
  rescue: error in [ExUnit::AssertionError]
    "Expected true to be false" = error.message
  end

  test :refute_with_message_when_value_is_true do
    "This should never be tested" = refute true, "This should be false"
  rescue: error in [ExUnit::AssertionError]
    "This should be false" = error.message
  end

  test :assert_match_when_equal do
    assert_match({ 2, 1 }, Value.tuple)
    true = assert_match({ 2, 1 }, Value.tuple)
  end

  test :assert_match_when_different do
    "This should never be tested" = assert_match({_, 2}, Value.tuple)
  rescue: error in [ExUnit::AssertionError]
    "no match of right hand side value: {2,1}" = error.message
  end

  test :assert_equal_when_equal do
    true = assert_equal(0, 0)
  end

  test :assert_equal_when_different do
    "This should never be tested" = assert_equal(0, 1)
  rescue: error in [ExUnit::AssertionError]
    "Expected 1 to be equal to 0" = error.message
  end

  test :assert_equal_with_message_when_different do
    "This should never be tested" = assert_equal(0, 1, "This should be equal")
  rescue: error in [ExUnit::AssertionError]
    "This should be equal" = error.message
  end

  test :assert_member_when_is_member do
    true = assert_member('foo', 'foobar')
  end

  test :assert_member_when_is_not_member do
    "This should never be tested" = assert_member('foo', 'bar')
  rescue: error in [ExUnit::AssertionError]
    "Expected 'bar' to include 'foo'" = error.message
  end

  test :assert_member_with_message_when_is_not_member do
    "This should never be tested" = assert_member('foo', 'bar', "This should be included")
  rescue: error in [ExUnit::AssertionError]
    "This should be included" = error.message
  end

  test :assert_raises_when_no_error do
    "This should never be tested" = assert_raises ArgumentError, fn ->
      # nothing
    end
  rescue: error in [ExUnit::AssertionError]
    "::ArgumentError exception expected but nothing was raised" = error.message
  end

  test :assert_raises_when_error do
    error = assert_raises ArgumentError, fn ->
      raise ArgumentError, message: "test error"
    end

    "test error" = error.message
  end

  test :assert_raises_when_other_error do
    "This should never be tested" = assert_raises ArgumentError, fn ->
      raise MatchError, message: "test error"
    end
  rescue: error in [ExUnit::AssertionError]
    "::ArgumentError exception expected, not ::MatchError" = error.message
  end

  test :refute_equal_when_equal do
    "This should never be tested" = refute_equal(1, 1)
  rescue: error in [ExUnit::AssertionError]
    "Expected 1 to not be equal to 1" = error.message
  end

  test :refute_equal_when_different do
    false = refute_equal(0, 1)
  end

  test :flunk do
    "This should never be tested" = flunk
  rescue: error in [ExUnit::AssertionError]
    "Epic Fail!" = error.message
  end

  test :flunk_with_message do
    "This should never be tested" = flunk "This should raise an error"
  rescue: error in [ExUnit::AssertionError]
    "This should raise an error" = error.message
  end
end
