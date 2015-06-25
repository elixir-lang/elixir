Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.CaseTest do
  use ExUnit.Case, async: true

  @moduletag :moduletag

  test "defines test case info" do
    assert %ExUnit.TestCase{name: __MODULE__, tests: tests} = __ex_unit__(:case)
    assert length(tests) > 0
  end

  @tag hello: false
  @tag :hello
  @tag world: :bad
  @tag world: :good
  test "tags", context do
    line = __ENV__.line - 1
    assert context[:case] == __MODULE__
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
end
