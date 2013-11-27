Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.CaseTest do
  use ExUnit.Case, async: true

  test "defines test case info" do
    assert ExUnit.TestCase[name: __MODULE__, tests: tests] =
           __MODULE__.__ex_unit__(:case)
    assert length(tests) > 0
  end

  @tag hello: false
  @tag :hello
  @tag world: :bad
  @tag world: :good
  test "tags", config do
    assert config[:test].tags[:hello] == true
    assert config[:test].tags[:world] == :good
  end
end
