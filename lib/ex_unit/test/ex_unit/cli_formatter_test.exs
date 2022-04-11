defmodule ExUnit.CLIFormatterTest do
  use ExUnit.Case, async: true

  describe "test_timings" do
    test "ignored when slowest is unused" do
      {:ok, config} = ExUnit.CLIFormatter.init(slowest: 0, include: [], exclude: [], colors: [])
      message = {:test_finished, %ExUnit.Test{state: nil, tags: %{test_type: :test}}}

      {:noreply, new_config} = ExUnit.CLIFormatter.handle_cast(message, config)

      assert new_config.test_timings == []
    end

    test "recorded when slowest is used" do
      {:ok, config} = ExUnit.CLIFormatter.init(slowest: 1, include: [], exclude: [], colors: [])
      test = %ExUnit.Test{state: nil, tags: %{test_type: :test}}
      message = {:test_finished, test}

      {:noreply, new_config} = ExUnit.CLIFormatter.handle_cast(message, config)

      assert new_config.test_timings == [test]
    end
  end
end
