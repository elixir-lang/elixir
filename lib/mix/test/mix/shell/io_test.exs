Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Shell.IOTest do
  use MixTest.Case

  import ExUnit.CaptureIO
  import Mix.Shell.IO

  test "prints info message to stdio" do
    assert capture_io(fn ->
             info("hello")
           end) == "hello\n"
  end

  test "prints error message to stderr" do
    assert capture_io(:stderr, fn ->
             error("hello")
           end) =~ "hello"
  end

  test "asks the user with yes? with implicit default option" do
    assert capture_io("\n", fn -> yes?("Ok?") end) == "Ok? [Yn] "
    assert capture_io("\n", fn -> assert yes?("Ok?") end)
    assert capture_io("Yes", fn -> assert yes?("Ok?") end)
    assert capture_io("yes", fn -> assert yes?("Ok?") end)
    assert capture_io("y", fn -> assert yes?("Ok?") end)

    assert capture_io("n", fn -> refute yes?("Ok?") end)
    assert capture_io("", fn -> refute yes?("Ok?") end)
  end

  test "asks the user with yes? using :yes as default option" do
    assert capture_io("\n", fn -> yes?("Ok?", default: :yes) end) == "Ok? [Yn] "
    assert capture_io("\n", fn -> assert yes?("Ok?", default: :yes) end)
    assert capture_io("Yes", fn -> assert yes?("Ok?", default: :yes) end)
    assert capture_io("yes", fn -> assert yes?("Ok?", default: :yes) end)
    assert capture_io("y", fn -> assert yes?("Ok?", default: :yes) end)

    assert capture_io("n", fn -> refute yes?("Ok?") end)
    assert capture_io("", fn -> refute yes?("Ok?") end)
  end

  test "asks the user with yes? using :no as default option" do
    assert capture_io("\n", fn -> yes?("Ok?", default: :no) end) == "Ok? [yN] "
    assert capture_io("Yes", fn -> assert yes?("Ok?", default: :no) end)
    assert capture_io("yes", fn -> assert yes?("Ok?", default: :no) end)
    assert capture_io("y", fn -> assert yes?("Ok?", default: :no) end)

    assert capture_io("\n", fn -> refute yes?("Ok?", default: :no) end)
    assert capture_io("n", fn -> refute yes?("Ok?", default: :no) end)
    assert capture_io("", fn -> refute yes?("Ok?", default: :no) end)
  end

  test "asks the user with yes? and raise exception where default option is not valid" do
    message = "expected :default to be either :yes or :no, got: :other"

    assert_raise ArgumentError, message, fn ->
      capture_io("\n", fn -> yes?("Ok?", default: :other) end)
    end
  end

  test "runs a given command" do
    nl = os_newline()

    assert capture_io("", fn -> assert cmd("echo hello") == 0 end) == "hello" <> nl

    will_print_sample()

    assert capture_io("", fn -> assert cmd("echo hello", print_app: false) == 0 end) ==
             "hello" <> nl

    assert capture_io("", fn -> assert cmd("echo hello") == 0 end) == "==> sample\nhello" <> nl
  end

  defp will_print_sample do
    Mix.Project.push(nil)
    Mix.Project.push(MixTest.Case.Sample)
  end
end
