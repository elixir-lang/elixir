Code.require_file "../test_helper.exs", __DIR__

defmodule EEx.SmartEngineTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

  test "evaluates simple string" do
    assert_eval "foo bar", "foo bar"
  end

  test "evaluates with assigns as keywords" do
    assert_eval "1", "<%= @foo %>", assigns: [foo: 1]
  end

  test "evaluates with assigns as a map" do
    assert_eval "1", "<%= @foo %>", assigns: %{foo: 1}
  end

  test "error with missing assigns" do
    stderr = capture_io(:stderr, fn ->
      assert_eval "", "<%= @foo %>", assigns: %{}
    end)
    assert stderr =~ "assign @foo not available in EEx template"
  end

  test "evaluates with loops" do
    assert_eval "1\n2\n3\n", "<%= for x <- [1, 2, 3] do %><%= x %>\n<% end %>"
  end

  test "preserves line numbers" do
    result = EEx.compile_string("<%= @hello %>", engine: EEx.SmartEngine)
    Macro.prewalk(result, fn
      {_left, meta, _right} ->
        assert Keyword.get(meta, :line, 0) in [0, 1]
      _ ->
        :ok
    end)
  end

  defp assert_eval(expected, actual, binding \\ []) do
    result = EEx.eval_string(actual, binding, file: __ENV__.file, engine: EEx.SmartEngine)
    assert result == expected
  end
end
