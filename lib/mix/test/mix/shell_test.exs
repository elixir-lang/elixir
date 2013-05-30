Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.ShellTest do
  use MixTest.Case

  test "shell process" do
    Mix.shell.info "abc"
    Mix.shell.error "def"
    assert_received { :mix_shell, :info, ["abc"] }
    assert_received { :mix_shell, :error, ["def"] }

    self <- { :mix_shell_input, :yes?, true }
    assert Mix.shell.yes?("hello?")
    assert_received { :mix_shell, :yes?, ["hello?"] }

    assert Mix.shell.cmd("echo first") == 0
    assert_received { :mix_shell, :run, ["first\n"] }
  end

  test "shell io" do
    import ExUnit.CaptureIO

    Mix.shell(Mix.Shell.IO)
    assert Mix.shell == Mix.Shell.IO

    assert capture_io(fn -> Mix.shell.info "abc" end) ==
           "abc\n"

    assert capture_io(:stderr, fn -> Mix.shell.error "def" end) ==
           (IO.ANSI.escape "%{red,bright}def") <> "\n"

    capture_io("Yes", fn -> assert Mix.shell.yes?("hello?") end)

    assert capture_io(fn -> assert Mix.shell.cmd("echo first") == 0 end) ==
           "first\n"
  after
    Mix.shell(Mix.Shell.Process)
  end

  test "shell cmd" do
    assert Mix.shell.cmd("echo first && echo second") == 0
    assert_received { :mix_shell, :run, ["first\nsecond\n"] }
  end
end
