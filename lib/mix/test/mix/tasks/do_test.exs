Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.DoTest do
  use MixTest.Case

  import Mix.Tasks.Do, only: [gather_commands: 1]

  test "runs given tasks", context do
    in_tmp(context.test, fn ->
      Mix.Tasks.Do.run(["compile", "--list", "+", "help"])
      assert_received {:mix_shell, :info, ["mix help" <> _]}
      assert_received {:mix_shell, :info, ["mix compile.app" <> _]}
    end)
  end

  test "gather_command returns a list of commands" do
    assert gather_commands(["help", "+", "compile"]) ==
             [["help"], ["compile"]]

    assert gather_commands(["help", "+", "compile", "--list"]) ==
             [["help"], ["compile", "--list"]]

    assert gather_commands(["help", "--list", "+", "compile", "--list"]) ==
             [["help", "--list"], ["compile", "--list"]]
  end

  test "gather_command supports deprecated comma commands" do
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
        Mix.Tasks.Do.run(~w(--app bar compile --list + cmd echo hello))

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
        Mix.Tasks.Do.run(~w(--app bar --app foo compile --list + cmd echo hello))

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
        Mix.Tasks.Do.run(~w(--app bar --app foo compile --list + help))

        assert_received {:mix_shell, :info, ["mix help" <> _]}
        assert_received {:mix_shell, :info, ["mix compile.app" <> _]}
      end)
    end)
  end

  test "raises when -app is given but the project is not an umbrella" do
    assert_raise Mix.Error,
                 "Could not run \"cmd\" with the --app option because this is not an umbrella project",
                 fn ->
                   Mix.Tasks.Do.run(~w(--app bar --app foo cmd echo hello))
                 end
  end

  test "runs given aliases for each app specified by app flag" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      aliases = [
        e: ["cmd echo hello"],
        p: fn val -> Mix.shell().info(inspect(val)) end
      ]

      Mix.Project.in_project(:umbrella, ".", [aliases: aliases], fn _ ->
        Mix.Tasks.Do.run(["compile"])
        Mix.Tasks.Do.run(["--app", "bar", "--app", "foo", "e", "+", "p", "Foo"])

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

        assert_received {:mix_shell, :info, ["mix help" <> _]}
      end)
    end)
  end

  test "raise with aliases when -app is given but the project is not an umbrella" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      aliases = [
        e: ["cmd echo hello"],
        p: fn val -> Mix.shell().info(inspect(val)) end
      ]

      Mix.Project.in_project(:foo, "apps/foo", [aliases: aliases], fn _ ->
        assert_raise Mix.Error,
                     "Could not run \"e\" with the --app option because this is not an umbrella project",
                     fn ->
                       Mix.Tasks.Do.run(~w(--app bar --app foo e + p Foo))
                     end
      end)
    end)
  end
end
