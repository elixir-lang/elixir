Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.RunnerStatsTest do
  use ExUnit.Case, async: false

  alias ExUnit.{FailuresManifest, RunnerStats}

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

      assert stats == %{total: 10, failures: 2, skipped: 1, excluded: 4}
    end

    test "invalidates successful tests when a module fails" do
      module_tests_a = [
        {:test_a1, :passed},
        {:test_a2, :skipped},
        {:test_a3, :failed},
        {:test_a4, :invalid},
        {:test_a5, :excluded}
      ]

      module_tests_b = [
        {:test_b1, :passed},
        {:test_b2, :failed},
        {:test_b3, :failed},
        {:test_b4, :passed},
        {:test_b5, :passed}
      ]

      tests = module_tests_a ++ module_tests_b

      stats =
        simulate_suite([], fn formatter ->
          for {test_name, status} <- tests do
            simulate_test(formatter, test_name, status)
          end

          simulate_module_finished(
            formatter,
            :module_a,
            :passed,
            module_tests_a
          )

          simulate_module_finished(
            formatter,
            :module_b,
            :failed,
            module_tests_b
          )
        end)

      assert stats == %{total: 10, failures: 7, skipped: 1, excluded: 1}
    end
  end

  describe "when no failures manifest path option is provided" do
    @tag :tmp_dir
    test "does not write a failures manifest", %{tmp_dir: tmp_dir} do
      File.cd!(tmp_dir, fn ->
        simulate_suite([], fn formatter ->
          simulate_test(formatter, :test_1, :passed)
          simulate_test(formatter, :test_2, :failed)
        end)

        assert File.ls(".") == {:ok, []}
      end)
    end
  end

  describe "when a failures manifest path option is provided" do
    @tag :tmp_dir
    test "records the test failures in the failures manifest file", %{tmp_dir: tmp_dir} do
      File.cd!(tmp_dir, fn ->
        simulate_suite(fn formatter ->
          simulate_test(formatter, :test_1, :passed)
          simulate_test(formatter, :test_2, :failed)
        end)

        assert read_failures_manifest() == %{{TestModule, :test_2} => __ENV__.file}
      end)
    end

    @tag :tmp_dir
    test "merges the results with the results from the prior run", %{tmp_dir: tmp_dir} do
      File.cd!(tmp_dir, fn ->
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

    GenServer.cast(pid, {:suite_finished, %{}})
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

  defp simulate_module_finished(formatter, module_name, status, tests) do
    tests =
      for {test_name, status} <- tests do
        %ExUnit.Test{
          module: TestModule,
          name: test_name,
          tags: %{file: __ENV__.file},
          state: state_for(status)
        }
      end

    GenServer.cast(
      formatter,
      {:module_finished,
       %ExUnit.TestModule{
         file: __ENV__.file,
         name: module_name,
         state: state_for(status),
         tests: tests
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
