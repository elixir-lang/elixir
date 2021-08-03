Code.require_file("test_helper.exs", __DIR__)

defmodule Elixir.CLITest do
  use ExUnit.Case

  import PathHelpers

  test "--help smoke test" do
    output = elixir(~w[--help])
    assert output =~ "Usage: elixir"
  end

  test "--version smoke test" do
    output = elixir(~w[--version])
    assert output =~ ~r/Erlang\/OTP [0-9]+ \[.+]/
    assert output =~ ~r/Elixir [1-9]\.[0-9]+\.[0-9]+.*\(compiled with Erlang\/OTP [0-9]+\)/
  end

  test "--short-version smoke test" do
    output = elixir(~w[--short-version])
    assert output =~ ~r/^[1-9]\.[0-9]+\.[0-9]+(?:-dev)?\n$/m
    refute output =~ "Erlang"
  end
end
