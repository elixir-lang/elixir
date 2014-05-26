Code.require_file "test_helper.exs", __DIR__

defmodule AccessTest do
  use ExUnit.Case, async: true

  # Test nil at compilation time does not fail
  # and that @config[:foo] has proper precedence.
  @config nil
  nil = @config[:foo]

  @config [foo: :bar]
  :bar = @config[:foo]

  @mod :lists
  [1, 2, 3] = @mod.flatten([1, [2], 3])

  @mod -13
  -13 = @mod

  test "for nil" do
    assert nil[:foo] == nil
    assert Access.get(nil, :foo) == nil
    assert Access.update(nil, :foo, fn nil -> :bar end) == :bar
  end

  test "for keywords" do
    assert [foo: :bar][:foo] == :bar
    assert [foo: [bar: :baz]][:foo][:bar] == :baz
    assert [foo: [bar: :baz]][:fuu][:bar] == nil

    assert Access.get([foo: :bar], :foo) == :bar
    assert Access.update([], :foo, fn nil -> :baz end) == [foo: :baz]
    assert Access.update([foo: :bar], :foo, fn :bar -> :baz end) == [foo: :baz]
  end

  test "for maps" do
    assert %{foo: :bar}[:foo] == :bar
    assert %{1 => 1}[1] == 1
    assert %{1.0 => 1.0}[1.0] == 1.0
    assert %{1 => 1}[1.0] == nil

    assert Access.get(%{foo: :bar}, :foo) == :bar
    assert Access.update(%{}, :foo, fn nil -> :baz end) == %{foo: :baz}
    assert Access.update(%{foo: :bar}, :foo, fn :bar -> :baz end) == %{foo: :baz}
  end

  test "for atoms" do
    assert_raise Protocol.UndefinedError, ~r"protocol Access not implemented for :foo", fn ->
      Access.get(:foo, :bar)
    end

    assert_raise Protocol.UndefinedError, ~r"protocol Access not implemented for :foo", fn ->
      Access.update(:foo, :bar, fn _ -> :baz end)
    end
  end
end
