Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.RunnerTest do
  use ExUnit.Case

  test "it set the timeout to infinity and max cases to one with trace enabled" do
    opts = Keyword.merge(ExUnit.configuration, trace: true)

    {_, config} = ExUnit.Runner.configure(opts)

    assert config[:timeout] == :infinity
    assert config[:max_cases] == 1
  end

  test "it does not set timeout to infinity and the max cases to 1 with trace disabled" do
    opts = Keyword.merge(ExUnit.configuration, trace: false)

    {_, config} = ExUnit.Runner.configure(opts)

    assert config[:max_cases] == :erlang.system_info(:schedulers_online)
    assert config[:timeout] == 30_000
  end
end
