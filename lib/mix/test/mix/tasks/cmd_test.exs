# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.CmdTest do
  use MixTest.Case

  test "can be called multiple times" do
    Mix.Task.run("cmd", ["echo", "hello"])
    assert_received {:mix_shell, :run, ["hello\n"]}
    Mix.Task.run("cmd", ["echo", "hello world"])
    assert_received {:mix_shell, :run, ["hello world\n"]}
  end

  test "can be invoked as a shell" do
    nl = os_newline()
    Mix.Task.run("cmd", ["--shell", "echo", "hello"])
    assert_received {:mix_shell, :run, ["hello" <> ^nl]}
  end

  @tag :unix
  test "supports relative paths" do
    in_tmp("cmd-relative", fn ->
      File.mkdir_p!("priv")
      File.write!("priv/world.sh", "#!/bin/sh\necho world")
      File.chmod!("priv/world.sh", 0o755)

      Mix.Task.run("cmd", ["priv/world.sh"])
      assert_received {:mix_shell, :run, ["world\n"]}

      Mix.Task.run("cmd", ["--cd", "priv", "./world.sh"])
      assert_received {:mix_shell, :run, ["world\n"]}
    end)
  end

  test "runs the command for each app" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        Mix.Task.run("cmd", ["echo", "hello"])
        assert_received {:mix_shell, :info, ["==> bar"]}
        assert_received {:mix_shell, :run, ["hello\n"]}
        assert_received {:mix_shell, :info, ["==> foo"]}
        assert_received {:mix_shell, :run, ["hello\n"]}
      end)
    end)
  end

  test "runs the command for a single app specified by app flag" do
    ExUnit.CaptureIO.capture_io(:stderr, fn ->
      in_fixture("umbrella_dep/deps/umbrella", fn ->
        Mix.Project.in_project(:umbrella, ".", fn _ ->
          Mix.Task.run("cmd", ["--app", "bar", "echo", "hello"])
          assert_received {:mix_shell, :info, ["==> bar"]}
          assert_received {:mix_shell, :run, ["hello\n"]}
          refute_received {:mix_shell, :info, ["==> foo"]}
          refute_received {:mix_shell, :run, ["hello\n"]}
        end)
      end)
    end)
  end

  test "runs the command for each app specified by app flag" do
    ExUnit.CaptureIO.capture_io(:stderr, fn ->
      in_fixture("umbrella_dep/deps/umbrella", fn ->
        Mix.Project.in_project(:umbrella, ".", fn _ ->
          Mix.Task.run("cmd", ["--app", "bar", "--app", "foo", "echo", "hello"])
          assert_received {:mix_shell, :info, ["==> bar"]}
          assert_received {:mix_shell, :run, ["hello\n"]}
          assert_received {:mix_shell, :info, ["==> foo"]}
          assert_received {:mix_shell, :run, ["hello\n"]}
        end)
      end)
    end)
  end

  test "only runs the cmd for specified apps and in specific directory" do
    ExUnit.CaptureIO.capture_io(:stderr, fn ->
      in_fixture("umbrella_dep/deps/umbrella", fn ->
        Mix.Project.in_project(:umbrella, ".", fn _ ->
          Mix.Task.run("cmd", ["--app", "bar", "--cd", "lib", "pwd"])
          assert_received {:mix_shell, :info, ["==> bar"]}
          {pwd, 0} = System.cmd("pwd", [], cd: Path.join(["apps", "bar", "lib"]))
          assert_received {:mix_shell, :run, [^pwd]}
          refute_received {:mix_shell, :info, ["==> foo"]}
        end)
      end)
    end)
  end
end
