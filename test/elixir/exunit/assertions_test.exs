Code.require_file "../../test_helper", __FILE__

defmodule ExUnit::AssertionsTest do
  use ExUnit::Case

  def test_assert_when_value_is_true do
    true = assert true
  end

  def test_assert_when_value_is_false do
    assert false
  rescue: error in [ExUnit::AssertionError]
    "Expected false to be true" = error.message
  end

  def test_assert_with_message_when_value_is_false do
    assert false, "This should be true"
  rescue: error in [ExUnit::AssertionError]
    "This should be true" = error.message
  end

  def test_refute_when_value_is_false do
    false = refute false
  end

  def test_refute_when_value_is_true do
    refute true
  rescue: error in [ExUnit::AssertionError]
    "Expected true to be false" = error.message
  end

  def test_refute_with_message_when_value_is_true do
    refute true, "This should be false"
  rescue: error in [ExUnit::AssertionError]
    "This should be false" = error.message
  end


  def test_assert_equal_when_equal do
    true = assert_equal(0, 0)
  end

  def test_assert_equal_when_different do
    assert_equal(0, 1)
  rescue: error in [ExUnit::AssertionError]
    "Expected 1 to be equal to 0" = error.message
  end

  def test_assert_equal_with_message_when_different do
    assert_equal(0, 1, "This should be equal")
  rescue: error in [ExUnit::AssertionError]
    "This should be equal" = error.message
  end

  def test_assert_included_when_included do
    true = assert_included('foo', 'foobar')
  end

  def test_assert_included_when_not_included do
    assert_included('foo', 'bar')
  rescue: error in [ExUnit::AssertionError]
    "Expected 'bar' to include 'foo'" = error.message
  end

  def test_assert_included_with_message_when_not_included do
    assert_included('foo', 'bar', "This should be included")
  rescue: error in [ExUnit::AssertionError]
    "This should be included" = error.message
  end
end
