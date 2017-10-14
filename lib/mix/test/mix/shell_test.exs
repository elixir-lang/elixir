Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.ShellTest do
  use MixTest.Case

  defp capture_io(fun) do
    fun |> ExUnit.CaptureIO.capture_io() |> String.replace("\r\n", "\n")
  end

  test "executes cmd with expressions" do
    Mix.shell(Mix.Shell.IO)

    assert capture_io(fn ->
             assert Mix.shell().cmd("echo first && echo second") == 0
           end)
           |> String.replace(" \n", "\n") == "first\nsecond\n"
  after
    Mix.shell(Mix.Shell.Process)
  end
end
