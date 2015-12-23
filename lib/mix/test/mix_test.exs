Code.require_file "test_helper.exs", __DIR__

defmodule MixTest do
  use MixTest.Case

  test "shell" do
    assert Mix.shell == Mix.Shell.Process
  end

  test "env" do
    assert Mix.env == :dev
    Mix.env(:prod)
    assert Mix.env == :prod
  end

  test "debug" do
    refute Mix.debug?
    Mix.debug(true)
    assert Mix.debug?
    Mix.debug(false)
  end
end
