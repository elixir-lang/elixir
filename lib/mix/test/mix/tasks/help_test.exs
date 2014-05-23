Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.HelpTest do
  use MixTest.Case

  import ExUnit.CaptureIO

  test "help lists all tasks" do
    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Help.run []
      assert_received {:mix_shell, :info, ["mix" <> _]}
      assert_received {:mix_shell, :info, ["mix help" <> _]}
      assert_received {:mix_shell, :info, ["mix compile" <> _]}
    end
  end

  test "help list default task" do
    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Help.run []

      {_, _, [output]} =
        assert_received {:mix_shell, :info, [_]}
      assert output =~ ~r/^mix\s+# Run the default task \(current: mix run\)/m
    end
  end

  test "help TASK" do
    in_fixture "no_mixfile", fn ->
      output =
        capture_io fn ->
          Mix.Tasks.Help.run ["compile"]
        end

      assert output =~ "# mix compile"
      assert output =~ "## Command line options"
      assert output =~ ~r/^Location:/m
    end
  end

  test "bad arguments" do
    assert_raise Mix.Error, "Unexpected arguments, expected `mix help` or `mix help TASK`", fn ->
      Mix.Tasks.Help.run ["foo", "bar"]
    end
  end
end
