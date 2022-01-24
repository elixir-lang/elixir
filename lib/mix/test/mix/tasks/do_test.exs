Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.DoTest do
  use MixTest.Case

  test "runs given tasks", context do
    in_tmp(context.test, fn ->
      Mix.Tasks.Do.run(["compile", "--list,", "help"])
      assert_received {:mix_shell, :info, ["mix help" <> _]}
      assert_received {:mix_shell, :info, ["mix compile.app" <> _]}
    end)
  end

  test "gather_command returns a list of commands" do
    import Mix.Tasks.Do, only: [gather_commands: 1]
    assert gather_commands(["compile", "--list,", "help"]) == [["compile", "--list"], ["help"]]
    assert gather_commands(["help,", "compile", "--list"]) == [["help"], ["compile", "--list"]]

    assert gather_commands(["compile,", "run", "-e", "IO.puts :hello"]) ==
             [["compile"], ["run", "-e", "IO.puts :hello"]]

    assert gather_commands(["compile,", "run", "-e", "[1, 2]"]) ==
             [["compile"], ["run", "-e", "[1, 2]"]]

    assert gather_commands(["test", ",", "help"]) == [["test"], ["help"]]
  end

  test "runs given tasks for a single app specified by app flag" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        Mix.Tasks.Do.run(["--app", "bar", "compile", "--list,", "cmd", "echo", "hello"])

        nl = os_newline()
        assert_received {:mix_shell, :info, ["==> bar"]}
        assert_received {:mix_shell, :run, ["hello" <> ^nl]}
        refute_received {:mix_shell, :info, ["==> foo"]}
        refute_received {:mix_shell, :run, ["hello" <> ^nl]}
      end)
    end)
  end

  test "runs given tasks for each app specified by app flag" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        Mix.Tasks.Do.run([
          "--app",
          "bar",
          "--app",
          "foo",
          "compile",
          "--list,",
          "cmd",
          "echo",
          "hello"
        ])

        nl = os_newline()
        assert_received {:mix_shell, :info, ["==> bar"]}
        assert_received {:mix_shell, :run, ["hello" <> ^nl]}
        assert_received {:mix_shell, :info, ["==> foo"]}
        assert_received {:mix_shell, :run, ["hello" <> ^nl]}
      end)
    end)
  end

  test "runs non-recursive tasks at project root level" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        Mix.Tasks.Do.run(["--app", "bar", "--app", "foo", "compile", "--list,", "help"])

        assert_received {:mix_shell, :info,
                         [
                           "warning: running \"help\" at root level because it is not a recursive task (ignoring apps options)"
                         ]}
      end)
    end)
  end

  test "runs given tasks ignoring apps argument when project is not an umbrella" do
    Mix.Tasks.Do.run(["--app", "bar", "--app", "foo", "cmd", "echo", "hello"])

    assert_received {:mix_shell, :info,
                     [
                       "warning: running \"cmd\" at root level because this is not an umbrella project"
                     ]}
  end

  test "runs given aliases for each app specified by app flag" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      aliases = [
        e: ["cmd echo hello"],
        p: fn val -> Mix.shell().info(inspect(val)) end
      ]

      Mix.Project.in_project(:umbrella, ".", [aliases: aliases], fn _ ->
        Mix.Tasks.Do.run(["compile"])
        Mix.Tasks.Do.run(["--app", "bar", "--app", "foo", "e,", "p", "Foo"])

        nl = os_newline()
        assert_received {:mix_shell, :info, ["==> bar"]}
        assert_received {:mix_shell, :run, ["hello" <> ^nl]}
        assert_received {:mix_shell, :info, ["[\"Foo\"]"]}

        assert_received {:mix_shell, :info, ["==> foo"]}
        assert_received {:mix_shell, :run, ["hello" <> ^nl]}
        assert_received {:mix_shell, :info, ["[\"Foo\"]"]}
      end)
    end)
  end

  test "runs non-recursive aliases at project root level" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      aliases = [
        h: "help"
      ]

      Mix.Project.in_project(:umbrella, ".", [aliases: aliases], fn _ ->
        Mix.Tasks.Do.run(["--app", "bar", "--app", "foo", "h"])

        assert_received {:mix_shell, :info,
                         [
                           "warning: running \"help\" at root level because it is not a recursive task (ignoring apps options)"
                         ]}
      end)
    end)
  end

  test "runs given aliases ignoring apps argument when project is not an umbrella" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      aliases = [
        e: ["cmd echo hello"],
        p: fn val -> Mix.shell().info(inspect(val)) end
      ]

      Mix.Project.in_project(:foo, "apps/foo", [aliases: aliases], fn _ ->
        Mix.Tasks.Do.run(["--app", "bar", "--app", "foo", "e,", "p", "Foo"])

        assert_received {:mix_shell, :info,
                         [
                           "warning: running \"cmd\" at root level because this is not an umbrella project"
                         ]}

        assert_received {:mix_shell, :info,
                         [
                           "warning: running \"p\" at root level because this is not an umbrella project"
                         ]}
      end)
    end)
  end
end
