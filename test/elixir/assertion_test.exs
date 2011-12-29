module AssertionTest do
  use ExUnit::Case

  def test_always_pass do
    true = true
  end
end