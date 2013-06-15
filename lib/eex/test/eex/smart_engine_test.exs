Code.require_file "../test_helper.exs", __DIR__

defmodule EEx.SmartEngineTest do
  use ExUnit.Case, async: true

  test "evaluates simple string" do
    assert_eval "foo bar", "foo bar"
  end

  test "evaluates with assigns" do
    assert_eval "1", "<%= @foo %>", assigns: [foo: 1]
  end

  test "evaluates with loops" do
    assert_eval "1\n2\n3\n", "<%= lc x inlist [1, 2, 3] do %><%= x %>\n<% end %>"
  end

  defp assert_eval(expected, actual, binding // []) do
    result = EEx.eval_string(actual, binding, file: __FILE__)
    assert result == expected
  end
end
