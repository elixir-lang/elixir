Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.CaseTest do
  use ExUnit.Case, async: true

  ExUnit.Case.register_attribute __MODULE__, :foo
  ExUnit.Case.register_attribute __MODULE__, :bar, accumulate: true
  ExUnit.Case.register_attribute __MODULE__, :baz

  @moduletag :moduletag

  test "defines __ex_unit__" do
    assert %ExUnit.TestModule{name: __MODULE__, tests: tests} = __ex_unit__()
    assert length(tests) > 0
  end

  @tag hello: false
  @tag :hello
  @tag world: :bad
  @tag world: :good
  test "tags", context do
    line = __ENV__.line - 1
    assert context[:module] == __MODULE__
    assert context[:test] == __ENV__.function |> elem(0)
    assert context[:line] == line
    assert context[:async] == true
    assert context[:hello] == true
    assert context[:world] == :good
  end

  test "reset tags", context do
    assert is_nil(context[:hello])
    assert is_nil(context[:world])
  end

  test "module tags", context do
    assert context[:moduletag] == true
  end

  @tag moduletag: :overridden
  test "module tags can be overridden", context do
    assert context[:moduletag] == :overridden
  end

  @foo :hello
  @bar :world
  test "registered attributes are in context", context do
    assert context.registered.foo == :hello
    assert context.registered.bar == [:world]
    assert context.registered.baz == nil
  end

  test "registered attributes are set per test", context do
    assert context.registered.foo == nil
    assert context.registered.bar == []
  end

  describe "describe block" do
    test "sets describe tag and line", context do
      describe_line = __ENV__.line - 2
      assert context.describe == "describe block"
      assert context.describe_line == describe_line
    end

    test "is prepended to title", context do
      assert context.test == :"test describe block is prepended to title"
    end
  end
end
