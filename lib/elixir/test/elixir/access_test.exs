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

  test :nil do
    assert nil[:foo] == nil
  end

  test :list do
    assert [foo: :bar][:foo] == :bar
    assert [foo: [bar: :baz]][:foo][:bar] == :baz
    assert [foo: [bar: :baz]][:fuu][:bar] == nil
  end

  test :map do
    assert %{ foo: :bar }[:foo] == :bar
    assert %{ 1 => 1 }[1] == 1
    assert %{ 1.0 => 1.0 }[1.0] == 1.0
    assert %{ 1 => 1 }[1.0] == nil
  end

  test :atom do
    exception = assert_raise RuntimeError, fn ->
      foo = :foo
      foo[:atom]
    end
    assert exception.message == "The access protocol can only be invoked for atoms " <>
                                "at compilation time, tried to invoke it for :foo"
  end
end
