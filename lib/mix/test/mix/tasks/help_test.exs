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

  test "help --names", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(Aliases)

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
          foo: &foo/1,
          bar: fn _ -> :ok end,
          help: ["help", "hello"],
          "nested.h": [&Mix.shell().info(inspect(&1)), "h foo bar"],
          other: [
            "format --check-formatted",
            fn _ -> :ok end,
            &foo/1,
            "help"
          ]
        ]
      ]
    end

    defp foo(_), do: :ok
  end

  test "help lists all aliases", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(ComplexAliases)

      Mix.Tasks.Help.run([])

      assert_received {:mix_shell, :info, ["mix h" <> message]}
      assert message =~ ~r/# Alias for hello/

      assert_received {:mix_shell, :info, ["mix p" <> message]}
      assert message =~ ~r/# Alias for &inspect\/1/

      assert_received {:mix_shell, :info, ["mix foo" <> message]}
      assert message =~ ~r/# Alias for &foo\/1/

      assert_received {:mix_shell, :info, ["mix bar" <> message]}
      assert message =~ ~r/# Alias for a function/

      assert_received {:mix_shell, :info, ["mix help" <> message]}
      assert message =~ ~r/# Alias for help, hello/

      assert_received {:mix_shell, :info, ["mix nested.h" <> message]}
      assert message =~ ~r/# Alias for a function, h foo bar/

      assert_received {:mix_shell, :info, ["mix other" <> message]}
      assert message =~ ~r/# Alias for format --check-formatted, a function, &foo\/1, help/
    end)
  end

  test "help ALIAS", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(ComplexAliases)

      output =
        capture_io(fn ->
          Mix.Tasks.Help.run(["h"])
        end)

      assert output =~ "mix h\n\n"
      assert output =~ "Alias for\n\n"
      assert output =~ "    \"hello\"\n"
      assert output =~ ~r/^Location: mix.exs/m

      output =
        capture_io(fn ->
          Mix.Tasks.Help.run(["p"])
        end)

      assert output =~ "mix p\n\n"
      assert output =~ "Alias for\n\n"
      assert output =~ "    &Kernel.inspect/1\n"
      assert output =~ ~r/^Location: mix.exs/m

      output =
        capture_io(fn ->
          Mix.Tasks.Help.run(["help"])
        end)

      assert output =~ "mix help\n\n"
      assert output =~ "Alias for\n\n"
      assert output =~ "    [\"help\",\n"
      assert output =~ "    \"hello\"]\n"
      assert output =~ ~r/^Location: mix.exs/m

      output =
        capture_io(fn ->
          Mix.Tasks.Help.run(["nested.h"])
        end)

      assert output =~ "mix nested.h\n\n"
      assert output =~ "Alias for\n\n"
      assert output =~ ~r/    \[#Function/
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
    in_tmp(context.test, fn ->
      Mix.Project.push(ShadowedAliases)

      output =
        capture_io(fn ->
          Mix.Tasks.Help.run(["compile"])
        end)

      assert output =~ "mix compile\n\n"
      assert output =~ "Alias for\n\n"
      assert output =~ "\"compile\"\n"
      assert output =~ ~r/^Location: mix.exs/m

      assert output =~
               "\nThere is also a task named \"compile\". The documentation is shown next.\n"

      assert output =~ "## Command line options"
      assert output =~ ~r/^Location:/m
    end)
  end

  test "help Elixir MODULE", context do
    in_tmp(context.test, fn ->
      output =
        capture_io(fn ->
          Mix.Tasks.Help.run(["Mix"])
        end)

      assert output =~
               "Mix is a build tool that provides tasks for creating, compiling, and testing\nElixir projects, managing its dependencies, and more."
    end)
  end

  test "help Elixir NESTED MODULE", context do
    in_tmp(context.test, fn ->
      output =
        capture_io(fn ->
          Mix.Tasks.Help.run(["IO.ANSI"])
        end)

      assert output =~ "Functionality to render ANSI escape sequences."
    end)
  end

  test "help Erlang MODULE", context do
    otp_docs? = match?({:docs_v1, _, _, _, _, _, _}, Code.fetch_docs(:math))

    in_tmp(context.test, fn ->
      output =
        capture_io(fn ->
          Mix.Tasks.Help.run([":math"])
        end)

      if otp_docs? do
        assert output =~
                 "This module provides an interface to a number of mathematical functions."
      else
        assert output =~ ":math was not compiled with docs"
      end
    end)
  end

  test "help FUNCTION/0", context do
    in_tmp(context.test, fn ->
      output =
        capture_io(fn ->
          Mix.Tasks.Help.run(["DateTime.utc_now()"])
        end)

      assert output =~ "Returns the current datetime in UTC"
    end)
  end

  test "help FUNCTION/1", context do
    in_tmp(context.test, fn ->
      output =
        capture_io(fn ->
          Mix.Tasks.Help.run(["Enum.all?/1"])
        end)

      assert output =~ "Returns `true` if all elements in `enumerable` are truthy."
    end)
  end

  test "help NESTED MODULE FUNCTION/3", context do
    in_tmp(context.test, fn ->
      output =
        capture_io(fn ->
          Mix.Tasks.Help.run(["IO.ANSI.color/3"])
        end)

      assert output =~ "Sets the foreground color from individual RGB values"
    end)
  end

  test "help ERROR" do
    assert_raise Mix.Error, "Invalid expression: Foo.bar(~s[baz])", fn ->
      Mix.Tasks.Help.run(["Foo.bar(~s[baz])"])
    end
  end

  test "help --search PATTERN", context do
    in_tmp(context.test, fn ->
      Mix.Tasks.Help.run(["--search", "deps"])
      assert_received {:mix_shell, :info, ["mix deps" <> _]}
      assert_received {:mix_shell, :info, ["mix deps.clean" <> _]}
    end)

    in_tmp(context.test, fn ->
      Mix.Project.push(Aliases)

      Mix.Tasks.Help.run(["--search", "h"])
      assert_received {:mix_shell, :info, ["mix h" <> message]}
      assert message =~ ~r/# Alias for hello/
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

  test "help --aliases", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(Aliases)

      Mix.Tasks.Help.run(["--aliases"])
      assert_received {:mix_shell, :info, ["mix h" <> message]}
      assert message =~ ~r/# Alias for hello/

      assert_received {:mix_shell, :info, ["mix c" <> message]}
      assert message =~ ~r/# Alias for compile/

      refute_received {:mix_shell, :info, ["mix deps" <> _]}
    end)
  end

  test "bad arguments" do
    message = "Unexpected arguments, expected \"mix help\" or \"mix help TASK\""

    assert_raise Mix.Error, message, fn ->
      Mix.Tasks.Help.run(["foo", "bar"])
    end
  end
end
