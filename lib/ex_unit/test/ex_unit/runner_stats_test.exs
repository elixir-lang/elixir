Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.RunnerStatsTest do
  use ExUnit.Case, async: false

  alias ExUnit.{FailuresManifest, RunnerStats}
  import ExUnit.TestHelpers, only: [in_tmp: 2]

  @failures_manifest_file "ex_unit_failures_manifest.elixir"

  describe "stats tracking" do
    test "counts total, failures, skipped, and excluded tests" do
      stats =
        simulate_suite([], fn formatter ->
          simulate_test(formatter, :test_1, :passed)
          simulate_test(formatter, :test_2, :skipped)
          simulate_test(formatter, :test_3, :failed)
          simulate_test(formatter, :test_4, :invalid)
          simulate_test(formatter, :test_5, :passed)
          simulate_test(formatter, :test_6, :passed)
          simulate_test(formatter, :test_7, :excluded)
          simulate_test(formatter, :test_8, :excluded)
          simulate_test(formatter, :test_9, :excluded)
          simulate_test(formatter, :test_10, :excluded)
        end)

      assert stats == %{total: 10, failures: 2, skipped: 1, excluded: 4, not_executed: 0}
    end
  end

  describe "when no failures manifest path option is provided" do
    test "does not write a failures manifest", context do
      in_tmp(context.test, fn ->
        simulate_suite([], fn formatter ->
          simulate_test(formatter, :test_1, :passed)
          simulate_test(formatter, :test_2, :failed)
        end)

        assert File.ls(".") == {:ok, []}
      end)
    end
  end

  describe "when a failures manifest path option is provided" do
    test "records the test failures in the failures manifest file", context do
      in_tmp(context.test, fn ->
        simulate_suite(fn formatter ->
          simulate_test(formatter, :test_1, :passed)
          simulate_test(formatter, :test_2, :failed)
        end)

        assert read_failures_manifest() == %{{TestModule, :test_2} => __ENV__.file}
      end)
    end

    test "merges the results with the results from the prior run", context do
      in_tmp(context.test, fn ->
        simulate_suite(&simulate_test(&1, :test_1, :failed))
        simulate_suite(&simulate_test(&1, :test_2, :failed))

        assert read_failures_manifest() == %{
                 {TestModule, :test_1} => __ENV__.file,
                 {TestModule, :test_2} => __ENV__.file
               }
      end)
    end
  end

  defp simulate_suite(opts \\ [failures_manifest_file: @failures_manifest_file], fun) do
    {:ok, pid} = GenServer.start_link(RunnerStats, opts)
    GenServer.cast(pid, {:suite_started, opts})

    fun.(pid)

    GenServer.cast(pid, {:suite_finished, 0, 0})
    RunnerStats.stats(pid)
  end

  defp simulate_test(formatter, test_name, status) do
    GenServer.cast(
      formatter,
      {:test_finished,
       %ExUnit.Test{
         module: TestModule,
         name: test_name,
         tags: %{file: __ENV__.file},
         state: state_for(status)
       }}
    )
  end

  defp state_for(:passed), do: nil
  defp state_for(:failed), do: {:failed, []}
  defp state_for(:invalid), do: {:invalid, TestModule}
  defp state_for(:skipped), do: {:skipped, "reason"}
  defp state_for(:excluded), do: {:excluded, "reason"}

  defp read_failures_manifest do
    FailuresManifest.read(@failures_manifest_file)
  end
end
