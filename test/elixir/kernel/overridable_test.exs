Code.require_file "../../test_helper", __FILE__

defmodule Kernel::Overridable do
  @overridable true
  def sample do
    1
  end

  @overridable true
  def with_super do
    1
  end

  @overridable true
  def without_super do
    1
  end

  @overridable true
  def explicit_nested_super do
    { super?, 2 }
  end

  @overridable true
  def explicit_nested_super do
    { super, super?, 1 }
  end

  def without_super do
    :without_super
  end

  def with_super do
    super() + 2
  end

  def no_overridable do
    { :no_overridable, super? }
  end

  def explicit_nested_super do
    { super, super?, 0 }
  end
end

defmodule Kernel::OverridableTest do
  use ExUnit::Case

  test "overridable function is made concrete if no other is defined" do
    assert_equal 1, Overridable.sample
  end

  test "overridable overridden with super" do
    assert_equal 3, Overridable.with_super
  end

  test "overridable overridden without super" do
    assert_equal :without_super, Overridable.without_super
  end

  test "overridable overriden with nested super" do
    assert_equal { { { false, 2 }, true, 1 }, true, 0 }, Overridable.explicit_nested_super
  end

  test "function without overridable returns false for super?" do
    assert_equal { :no_overridable, false }, Overridable.no_overridable
  end
end