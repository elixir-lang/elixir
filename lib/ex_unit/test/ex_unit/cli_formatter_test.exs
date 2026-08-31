# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.CLIFormatterTest do
  use ExUnit.Case

  import ExUnit.CaptureIO

  @opts [
    seed: 0,
    max_cases: 1,
    repeat_until_failure: 0,
    dry_run: false,
    trace: false,
    colors: [enabled: false],
    slowest: 0,
    slowest_modules: 0,
    include: [],
    exclude: []
  ]

  defp failed_test(message, logs) do
    exception =
      try do
        flunk(message)
      rescue
        e -> e
      end

    %ExUnit.Test{
      name: :"test poisoned",
      description: "poisoned",
      module: Hello,
      state: {:failed, [{:error, exception, []}]},
      logs: logs,
      time: 0,
      tags: %{file: "file.ex", line: 1, test_type: :test}
    }
  end

  defp run_formatter(events) do
    capture_io(fn ->
      {:ok, formatter} = GenServer.start(ExUnit.CLIFormatter, @opts)

      for event <- events do
        GenServer.cast(formatter, event)
      end

      # Casts are processed in order before this call, so a reply
      # proves none of the events crashed the formatter
      assert is_map(:sys.get_state(formatter))
      GenServer.stop(formatter)
    end)
  end

  test "survives a failure message with invalid UTF-8 and keeps reporting" do
    poisoned = failed_test("frame bytes: " <> <<0xC3, 0x28, 0xFF>>, "")
    clean = failed_test("clean failure", "")

    output =
      run_formatter([
        {:test_finished, poisoned},
        {:test_finished, clean}
      ])

    assert String.valid?(output)
    assert output =~ "frame bytes:"
    assert output =~ "clean failure"
  end

  test "survives captured logs with invalid UTF-8" do
    poisoned = failed_test("oops", "log line: " <> <<0xFF>>)

    output = run_formatter([{:test_finished, poisoned}])

    assert String.valid?(output)
    assert output =~ "The following output was logged:"
    assert output =~ "log line:"
  end
end
