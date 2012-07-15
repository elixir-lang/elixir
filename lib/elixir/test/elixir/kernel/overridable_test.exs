Code.require_file "../../test_helper", __FILE__

defmodule Kernel.Overridable do
  def sample do
    1
  end

  def with_super do
    1
  end

  def without_super do
    1
  end

  def explicit_nested_super do
    { super?, 2 }
  end

  defoverridable [sample: 0, with_super: 0, without_super: 0, explicit_nested_super: 0]

  def explicit_nested_super do
    { super, super?, 1 }
  end

  defoverridable [explicit_nested_super: 0]

  def implicit_nested_super do
    { super?, 1 }
  end

  defoverridable [implicit_nested_super: 0]

  def implicit_nested_super do
    { super, super?, 0 }
  end

  def super_with_explicit_args(x, y) do
    x + y
  end

  def super_with_implicit_args(x, y) do
    x + y
  end

  def many_clauses(0) do
    11
  end

  def many_clauses(1) do
    13
  end

  defoverridable [implicit_nested_super: 0, super_with_implicit_args: 2,
    super_with_explicit_args: 2, many_clauses: 1]

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
    if true do
      x + y + super
    else
      raise :NEVER
    end
  end

  def many_clauses(2) do
    17
  end

  def many_clauses(_) do
    super
  end
end

defmodule Kernel.OverridableTest do
  require Kernel.Overridable, as: Overridable
  use ExUnit.Case, async: true

  test "overridable is made concrete if no other is defined" do
    assert Overridable.sample == 1
  end

  test "overridable overridden with super" do
    assert Overridable.with_super == 3
  end

  test "overridable overridden without super" do
    assert Overridable.without_super == :without_super
  end

  test "overridable overriden with nested super" do
    assert Overridable.explicit_nested_super == { { { false, 2 }, true, 1 }, true, 0 }
  end

  test "overridable node overriden with nested super" do
    assert Overridable.implicit_nested_super == { { false, 1 }, true, 0 }
  end

  test "calling super with explicit args" do
    assert Overridable.super_with_explicit_args(1, 2) == 5
  end

  test "calling super with implicit args" do
    assert Overridable.super_with_implicit_args(1, 2) == 6
  end

  test "function without overridable returns false for super?" do
    assert Overridable.no_overridable == { :no_overridable, false }
  end

  test "overridable with many clauses" do
    assert Overridable.many_clauses(0) == 11
    assert Overridable.many_clauses(1) == 13
    assert Overridable.many_clauses(2) == 17
  end

  test "overridable definitions are private" do
    refute {:"OVERRIDABLE-0-with_super",0} in Overridable.__info__(:exports)
  end

  test "invalid super call" do
    try do
      Erlang.elixir.eval 'defmodule Foo.Forwarding do\ndef bar, do: 1\ndefoverridable [bar: 0]\ndef foo, do: super\nend', []
      flunk "expected eval to fail"
    rescue
      error ->
        assert error.message == ("nofile:4: no super defined for foo/0 in module 'Foo.Forwarding'. " <>
          "Overridable functions available are: bar/0")
    end
  end
end