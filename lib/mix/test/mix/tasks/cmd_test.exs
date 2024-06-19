Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.CmdTest do
  use MixTest.Case

  test "can be called multiple times" do
    nl = os_newline()
    Mix.Task.run("cmd", ["echo", "hello"])
    assert_received {:mix_shell, :run, ["hello" <> ^nl]}
    Mix.Task.run("cmd", ["echo", "hello"])
    assert_received {:mix_shell, :run, ["hello" <> ^nl]}
  end

  test "runs the command for each app" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        Mix.Task.run("cmd", ["echo", "hello"])
        nl = os_newline()
        assert_received {:mix_shell, :info, ["==> bar"]}
        assert_received {:mix_shell, :run, ["hello" <> ^nl]}
        assert_received {:mix_shell, :info, ["==> foo"]}
        assert_received {:mix_shell, :run, ["hello" <> ^nl]}
      end)
    end)
  end

  test "runs the command for a single app specified by app flag" do
    ExUnit.CaptureIO.capture_io(:stderr, fn ->
      in_fixture("umbrella_dep/deps/umbrella", fn ->
        Mix.Project.in_project(:umbrella, ".", fn _ ->
          Mix.Task.run("cmd", ["--app", "bar", "echo", "hello"])
          nl = os_newline()
          assert_received {:mix_shell, :info, ["==> bar"]}
          assert_received {:mix_shell, :run, ["hello" <> ^nl]}
          refute_received {:mix_shell, :info, ["==> foo"]}
          refute_received {:mix_shell, :run, ["hello" <> ^nl]}
        end)
      end)
    end)
  end

  test "runs the command for each app specified by app flag" do
    ExUnit.CaptureIO.capture_io(:stderr, fn ->
      in_fixture("umbrella_dep/deps/umbrella", fn ->
        Mix.Project.in_project(:umbrella, ".", fn _ ->
          Mix.Task.run("cmd", ["--app", "bar", "--app", "foo", "echo", "hello"])
          nl = os_newline()
          assert_received {:mix_shell, :info, ["==> bar"]}
          assert_received {:mix_shell, :run, ["hello" <> ^nl]}
          assert_received {:mix_shell, :info, ["==> foo"]}
          assert_received {:mix_shell, :run, ["hello" <> ^nl]}
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
