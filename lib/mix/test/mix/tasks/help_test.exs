Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.HelpTest do
  use MixTest.Case

  import ExUnit.CaptureIO

  test "help lists all tasks", context do
    in_tmp(context.test, fn ->
      Mix.Tasks.Help.run([])
      assert_received {:mix_shell, :info, ["mix" <> _]}
      assert_received {:mix_shell, :info, ["mix help" <> _]}
      assert_received {:mix_shell, :info, ["mix compile" <> _]}
    end)
  end

  test "help list default task", context do
    in_tmp(context.test, fn ->
      Mix.Tasks.Help.run([])

      assert_received {:mix_shell, :info, [output]}
      assert output =~ ~r/^mix\s+# Runs the default task \(current: \"mix run\"\)/m
    end)
  end

  defmodule Aliases do
    def project do
      [aliases: [h: "hello", c: "compile"]]
    end
  end

  test "help lists all aliases", context do
    Mix.Project.push(Aliases)

    in_tmp(context.test, fn ->
      Mix.Tasks.Help.run([])

      assert_received {:mix_shell, :info, ["mix h" <> message]}
      assert message =~ ~r/# Alias defined in mix.exs/

      assert_received {:mix_shell, :info, ["mix c" <> message]}
      assert message =~ ~r/# Alias defined in mix.exs/
    end)
  end

  test "help --names", context do
    Mix.Project.push(Aliases)

    in_tmp(context.test, fn ->
      Mix.Tasks.Help.run(["--names"])
      assert_received {:mix_shell, :info, ["c"]}
      assert_received {:mix_shell, :info, ["compile"]}
      assert_received {:mix_shell, :info, ["h"]}
      assert_received {:mix_shell, :info, ["help"]}
      assert_received {:mix_shell, :info, ["escript.build"]}
      refute_received {:mix_shell, :info, ["compile.all"]}
    end)
  end

  defmodule ComplexAliases do
    def project do
      [
        aliases: [
          h: "hello",
          p: &inspect/1,
          help: ["help", "hello"],
          "nested.h": [&Mix.shell().info(inspect(&1)), "h foo bar"]
        ]
      ]
    end
  end

  test "help ALIAS", context do
    Mix.Project.push(ComplexAliases)

    in_tmp(context.test, fn ->
      output =
        capture_io(fn ->
          Mix.Tasks.Help.run(["h"])
        end)

      assert output =~ "mix h\n\n"
      assert output =~ "Alias for \"hello\"\n"
      assert output =~ ~r/^Location: mix.exs/m

      output =
        capture_io(fn ->
          Mix.Tasks.Help.run(["p"])
        end)

      assert output =~ "mix p\n\n"
      assert output =~ "Alias for &Kernel.inspect/1\n"
      assert output =~ ~r/^Location: mix.exs/m

      output =
        capture_io(fn ->
          Mix.Tasks.Help.run(["help"])
        end)

      assert output =~ "mix help\n\n"
      assert output =~ "Alias for [\"help\", \"hello\"]\n"
      assert output =~ ~r/^Location: mix.exs/m

      output =
        capture_io(fn ->
          Mix.Tasks.Help.run(["nested.h"])
        end)

      assert output =~ "mix nested.h\n\n"
      assert output =~ ~r/Alias for \[#Function/
      assert output =~ ~r/^Location: mix.exs/m
    end)
  end

  test "help TASK", context do
    in_tmp(context.test, fn ->
      output =
        capture_io(fn ->
          Mix.Tasks.Help.run(["compile"])
        end)

      assert output =~ "mix compile\n"
      assert output =~ "## Command line options"
      assert output =~ ~r/^Location:/m

      output =
        capture_io(fn ->
          Mix.Tasks.Help.run(["compile.all"])
        end)

      assert output =~ "mix compile.all\n"
      assert output =~ "There is no documentation for this task"
    end)
  end

  defmodule ShadowedAliases do
    def project do
      [aliases: [compile: "compile"]]
    end
  end

  test "help TASK && ALIAS", context do
    Mix.Project.push(ShadowedAliases)

    in_tmp(context.test, fn ->
      output =
        capture_io(fn ->
          Mix.Tasks.Help.run(["compile"])
        end)

      assert output =~ "mix compile\n\n"
      assert output =~ "Alias for \"compile\"\n"
      assert output =~ ~r/^Location: mix.exs/m

      assert output =~
               "\nThere is also a task named \"compile\". The documentation is shown next.\n"

      assert output =~ "## Command line options"
      assert output =~ ~r/^Location:/m
    end)
  end

  test "help --search PATTERN", context do
    in_tmp(context.test, fn ->
      Mix.Tasks.Help.run(["--search", "deps"])
      assert_received {:mix_shell, :info, ["mix deps" <> _]}
      assert_received {:mix_shell, :info, ["mix deps.clean" <> _]}
    end)

    Mix.Project.push(Aliases)

    in_tmp(context.test, fn ->
      Mix.Tasks.Help.run(["--search", "h"])
      assert_received {:mix_shell, :info, ["mix h" <> message]}
      assert message =~ ~r/# Alias defined in mix.exs/
    end)
  end

  test "help --search without pattern" do
    assert_raise Mix.Error, "Unexpected arguments, expected \"mix help --search PATTERN\"", fn ->
      Mix.Tasks.Help.run(["--search"])
    end
  end

  test "help --search without results", context do
    in_tmp(context.test, fn ->
      output =
        capture_io(fn ->
          Mix.Tasks.Help.run(["--search", "foo"])
        end)

      assert output == ""
    end)
  end

  test "bad arguments" do
    message = "Unexpected arguments, expected \"mix help\" or \"mix help TASK\""

    assert_raise Mix.Error, message, fn ->
      Mix.Tasks.Help.run(["foo", "bar"])
    end
  end
end
