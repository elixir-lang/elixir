Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.RunnerStatsTest do
  use ExUnit.Case, async: false

  alias ExUnit.{Manifest, RunnerStats}
  import Manifest, only: [entry: 1]
  import ExUnit.TestHelpers, only: [in_tmp: 2]

  @manifest_path "manifests"
  @manifest_file "#{@manifest_path}/.ex_unit_results.elixir"

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

      assert stats == %{total: 10, failures: 2, skipped: 1, excluded: 4}
    end
  end

  describe "when no manifest path option is provided" do
    test "does not write a manifest", context do
      in_tmp(context.test, fn ->
        simulate_suite([], fn formatter ->
          simulate_test(formatter, :test_1, :passed)
          simulate_test(formatter, :test_2, :failed)
        end)

        assert File.ls(".") == {:ok, []}
      end)
    end
  end

  describe "when a manifest path option is provided" do
    test "records the test results in the manifest file", context do
      in_tmp(context.test, fn ->
        simulate_suite(fn formatter ->
          simulate_test(formatter, :test_1, :passed)
          simulate_test(formatter, :test_2, :failed)
        end)

        assert read_manifest() == %{
                 {TestModule, :test_1} => entry(file: __ENV__.file, last_run_status: :passed),
                 {TestModule, :test_2} => entry(file: __ENV__.file, last_run_status: :failed)
               }
      end)
    end

    test "merges the results with the results from the prior run", context do
      in_tmp(context.test, fn ->
        simulate_suite(&simulate_test(&1, :test_1, :passed))
        simulate_suite(&simulate_test(&1, :test_2, :failed))

        assert read_manifest() == %{
                 {TestModule, :test_1} => entry(file: __ENV__.file, last_run_status: :passed),
                 {TestModule, :test_2} => entry(file: __ENV__.file, last_run_status: :failed)
               }
      end)
    end
  end

  defp simulate_suite(opts \\ [manifest_path: @manifest_path], fun) do
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

  defp read_manifest do
    Manifest.read(@manifest_file)
  end
end
