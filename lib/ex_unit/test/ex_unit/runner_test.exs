Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.RunnerTest do
  use ExUnit.Case

  test "it sets max cases to one with trace enabled" do
    opts = Keyword.merge(ExUnit.configuration, trace: true)

    {_, config} = ExUnit.Runner.configure(opts)

    assert config.trace
    assert config.timeout == 60_000
    assert config.max_cases == 1
  end

  test "it does not set timeout to infinity and the max cases to 1 with trace disabled" do
    opts = Keyword.merge(ExUnit.configuration, trace: false)

    {_, config} = ExUnit.Runner.configure(opts)

    refute config.trace
    assert config.max_cases == :erlang.system_info(:schedulers_online)
    assert config.timeout == 60_000
  end
end
