Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Shell.QuietTest do
  use MixTest.Case

  import ExUnit.CaptureIO
  import Mix.Shell.Quiet

  test "prints nothing to stdio when info is invoked" do
    assert capture_io(fn -> info("hello") end) == ""
  end

  test "prints error message to stderr" do
    assert capture_io(:stderr, fn -> error("hello") end) =~ "hello"
  end

  test "asks the user with yes?" do
    assert capture_io("\n", fn -> yes?("Ok?") end) == "Ok? [Yn] "
    assert capture_io("\n", fn -> assert yes?("Ok?") end)
    assert capture_io("Yes", fn -> assert yes?("Ok?") end)
    assert capture_io("yes", fn -> assert yes?("Ok?") end)
    assert capture_io("y", fn -> assert yes?("Ok?") end)

    assert capture_io("n", fn -> refute yes?("Ok?") end)
    assert capture_io("", fn -> refute yes?("Ok?") end)
  end

  test "runs a given command" do
    assert capture_io("", fn -> assert cmd("echo hello") == 0 end) == ""

    wont_print_sample()
    assert capture_io("", fn -> assert cmd("echo hello", print_app: false) == 0 end) == ""
    assert capture_io("", fn -> assert cmd("echo hello") == 0 end) == ""
  end

  defp wont_print_sample do
    Mix.Project.push(nil)
    Mix.Project.push(MixTest.Case.Sample)
  end
end
