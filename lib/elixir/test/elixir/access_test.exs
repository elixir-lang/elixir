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
    assert_raise ArgumentError, "could not put/update key :foo on a nil value", fn ->
      Access.get_and_update(nil, :foo, fn nil -> {:ok, :bar} end)
    end
  end

  test "for keywords" do
    assert [foo: :bar][:foo] == :bar
    assert [foo: [bar: :baz]][:foo][:bar] == :baz
    assert [foo: [bar: :baz]][:fuu][:bar] == nil

    assert Access.get([foo: :bar], :foo) == :bar
    assert Access.get_and_update([], :foo, fn nil -> {:ok, :baz} end) == {:ok, [foo: :baz]}
    assert Access.get_and_update([foo: :bar], :foo, fn :bar -> {:ok, :baz} end) == {:ok, [foo: :baz]}
  end

  test "for maps" do
    assert %{foo: :bar}[:foo] == :bar
    assert %{1 => 1}[1] == 1
    assert %{1.0 => 1.0}[1.0] == 1.0
    assert %{1 => 1}[1.0] == nil

    assert Access.get(%{foo: :bar}, :foo) == :bar
    assert Access.get_and_update(%{}, :foo, fn nil -> {:ok, :baz} end) == {:ok, %{foo: :baz}}
    assert Access.get_and_update(%{foo: :bar}, :foo, fn :bar -> {:ok, :baz} end) == {:ok, %{foo: :baz}}
  end
end
