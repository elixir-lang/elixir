Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.ShellTest do
  use MixTest.Case

  defp capture_io(somefunc) do
    ExUnit.CaptureIO.capture_io(somefunc) |> String.replace("\r\n","\n")
  end

  defp capture_io(from, somefunc) do
    ExUnit.CaptureIO.capture_io(from, somefunc) |> String.replace("\r\n","\n")
  end

  setup do
    on_exit fn ->
      Mix.shell(Mix.Shell.Process)
    end
    :ok
  end

  test "shell process" do
    Mix.shell.info "abc"
    Mix.shell.error "def"
    assert_received {:mix_shell, :info, ["abc"]}
    assert_received {:mix_shell, :error, ["def"]}

    send self, {:mix_shell_input, :prompt, "world"}
    assert Mix.shell.prompt("hello?") == "world"
    assert_received {:mix_shell, :prompt, ["hello?"]}

    send self, {:mix_shell_input, :yes?, true}
    assert Mix.shell.yes?("hello?")
    assert_received {:mix_shell, :yes?, ["hello?"]}

    assert Mix.shell.cmd("echo first") == 0

    nl = os_newline
    assert_received {:mix_shell, :run, ["first" <> ^nl]}
  end

  test "shell io" do
    Mix.shell Mix.Shell.IO

    assert capture_io(fn -> Mix.shell.info "abc" end) ==
           "abc\n"

    if IO.ANSI.enabled? do
      assert capture_io(:stderr, fn -> Mix.shell.error "def" end) ==
             "#{IO.ANSI.red}#{IO.ANSI.bright}def#{IO.ANSI.reset}\n"
    else
      assert capture_io(:stderr, fn -> Mix.shell.error "def" end) ==
             "def\n"
    end

    assert capture_io("world", fn -> assert Mix.shell.prompt("hello?") == "world" end) ==
           "hello? "

    assert capture_io("Yes", fn -> assert Mix.shell.yes?("hello?") end) ==
           "hello? [Yn] "

    assert capture_io(fn -> assert Mix.shell.cmd("echo first") == 0 end) ==
           "first\n"
  end

  test "shell cmd supports expressions" do
    Mix.shell Mix.Shell.IO

    assert (capture_io(fn ->
      assert Mix.shell.cmd("echo first && echo second") == 0
    end) |> String.replace(" \n", "\n")) == "first\nsecond\n"
  end

  test "shell cmd ignores output if desired" do
    Mix.shell Mix.Shell.IO

    assert capture_io(fn ->
      assert Mix.shell.cmd("echo first && echo second", quiet: true) == 0
    end) == ""
  end
end
