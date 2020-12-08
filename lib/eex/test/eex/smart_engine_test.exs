Code.require_file("../test_helper.exs", __DIR__)

defmodule EEx.SmartEngineTest do
  use ExUnit.Case, async: true

  test "evaluates simple string" do
    assert_eval("foo bar", "foo bar")
  end

  test "evaluates with assigns as keywords" do
    assert_eval("1", "<%= @foo %>", assigns: [foo: 1])
  end

  test "evaluates with assigns as a map" do
    assert_eval("1", "<%= @foo %>", assigns: %{foo: 1})
  end

  test "error with missing assigns" do
    stderr =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        assert_eval("", "<%= @foo %>", assigns: %{})
      end)

    assert stderr =~ "assign @foo not available in EEx template"
  end

  test "evaluates with loops" do
    assert_eval("1\n2\n3\n", "<%= for x <- [1, 2, 3] do %><%= x %>\n<% end %>")
  end

  test "show warning when opening block" do
    stderr =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        assert_eval("", "<% if true do %>Foo<% else %>Bar<% end %>")
      end)

    assert stderr =~ "contains blocks"
  end

  test "doesn't show warning when opening inline" do
    assert_eval("", ~s/<% if true, do: "Foo", else: "Bar" %>/)
  end

  test "doesn't show warning when opening a block for an assignment" do
    stderr =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        assert_eval("Foo", ~s/<%a = if true do %>Foo<% else %>Bar<% end %><%= a %>/)
      end)

    assert stderr == ""
  end

  test "show warning when opens a block and then does an assignment" do 
    stderr =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        assert_eval("Foo", ~s/<%if true do %>Foo<% else %>Bar<% end %><% a = "Foo" %><%= a %>/)
      end)

    assert stderr =~ "contains blocks"
  end

  test "preserves line numbers in assignments" do
    result = EEx.compile_string("foo\n<%= @hello %>", engine: EEx.SmartEngine)

    Macro.prewalk(result, fn
      {_left, meta, [_, :hello]} ->
        assert Keyword.get(meta, :line) == 2
        send(self(), :found)

      node ->
        node
    end)

    assert_received :found
  end

  defp assert_eval(expected, actual, binding \\ []) do
    result = EEx.eval_string(actual, binding, file: __ENV__.file, engine: EEx.SmartEngine)
    assert result == expected
  end
end
