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

  @overridable true
  def implicit_nested_super do
    { super?, 1 }
  end

  @overridable true
  def implicit_nested_super do
    { super, super?, 0 }
  end

  @overridable true
  def super_with_explicit_args(x, y) do
    x + y
  end

  @overridable true
  def super_with_implicit_args(x, y) do
    x + y
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

  def super_with_explicit_args(x, y) do
    super x, y * 2
  end

  def super_with_implicit_args(x, y) do
    x + y + super
  end
end

defmodule Kernel::OverridableTest do
  use ExUnit::Case

  test "overridable is made concrete if no other is defined" do
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

  test "overridable node overriden with nested super" do
    assert_equal { { false, 1 }, true, 0 }, Overridable.implicit_nested_super
  end

  test "calling super with explicit args" do
    assert_equal 5, Overridable.super_with_explicit_args(1, 2)
  end

  test "calling super with implicit args" do
    assert_equal 6, Overridable.super_with_implicit_args(1, 2)
  end

  test "function without overridable returns false for super?" do
    assert_equal { :no_overridable, false }, Overridable.no_overridable
  end

  test "invalid super call" do
    try do
      Erlang.elixir.eval 'defmodule Foo::Forwarding do\n@overridable true\ndef bar, do: 1\ndef foo, do: super\nend', []
      flunk "expected eval to fail"
    rescue: error
      assert_equal "nofile:4: no super defined for foo/0 in module '__MAIN__::Foo::Forwarding'. " <>
        "Overridable functions available are: bar/0", error.message
    end
  end
end