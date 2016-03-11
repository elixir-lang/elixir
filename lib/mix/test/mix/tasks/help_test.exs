Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.HelpTest do
  use MixTest.Case

  import ExUnit.CaptureIO

  test "help lists all tasks", context do
    in_tmp context.test, fn ->
      Mix.Tasks.Help.run []
      assert_received {:mix_shell, :info, ["mix" <> _]}
      assert_received {:mix_shell, :info, ["mix help" <> _]}
      assert_received {:mix_shell, :info, ["mix compile" <> _]}
    end
  end

  test "help list default task", context do
    in_tmp context.test, fn ->
      Mix.Tasks.Help.run []

      {_, _, [output]} =
        assert_received {:mix_shell, :info, [_]}
      assert output =~ ~r/^mix\s+# Runs the default task \(current: \"mix run\"\)/m
    end
  end

  defmodule Aliases do
    def project do
      [aliases: [h: "hello", c: "compile"]]
    end
  end

  test "help --names", context do
    Mix.Project.push Aliases

    in_tmp context.test, fn ->
      Mix.Tasks.Help.run ["--names"]
      assert_received {:mix_shell, :info, ["c"]}
      assert_received {:mix_shell, :info, ["compile"]}
      assert_received {:mix_shell, :info, ["h"]}
      assert_received {:mix_shell, :info, ["help"]}
      assert_received {:mix_shell, :info, ["escript.build"]}
      refute_received {:mix_shell, :info, ["compile.all"]}
    end
  end

  test "help TASK", context do
    in_tmp context.test, fn ->
      output =
        capture_io(fn ->
          Mix.Tasks.Help.run ["compile"]
        end)

      assert output =~ "# mix compile\n"
      assert output =~ "## Command line options"
      assert output =~ ~r/^Location:/m

      output =
        capture_io(fn ->
          Mix.Tasks.Help.run ["compile.all"]
        end)

      assert output =~ "# mix compile.all\n"
      assert output =~ "There is no documentation for this task"
    end
  end

  test "help --search PATTERN", context do
    in_tmp context.test, fn ->
      Mix.Tasks.Help.run ["--search", "deps"]
      assert_received {:mix_shell, :info, ["mix deps" <> _]}
      assert_received {:mix_shell, :info, ["mix deps.clean" <> _]}
    end
  end

  test "help --search without pattern" do
    assert_raise Mix.Error, "Unexpected arguments, expected \"mix help --search PATTERN\"", fn ->
      Mix.Tasks.Help.run ["--search"]
    end
  end

  test "help --search without results", context do
    in_tmp context.test, fn ->
      output =
        capture_io fn ->
          Mix.Tasks.Help.run ["--search", "foo"]
        end

      assert output == ""
    end
  end

  test "bad arguments" do
    assert_raise Mix.Error, "Unexpected arguments, expected \"mix help\" or \"mix help TASK\"", fn ->
      Mix.Tasks.Help.run ["foo", "bar"]
    end
  end
end
