Code.require_file "../../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Test.StaleTest do
  use MixTest.Case

  test "--stale: runs all tests for first run, then none on second" do
    in_fixture "test_stale", fn ->
      assert_stale_run_output "2 tests, 0 failures"

      assert_stale_run_output """
      mix test --stale did not find any tests to run, add --force if you want to run all tests
      """
    end
  end

  test "--stale: runs tests that depend on modified modules" do
    in_fixture "test_stale", fn ->
      assert_stale_run_output "2 tests, 0 failures"

      # These sleeps are here because the file is not detected as modified otherwise
      # due to the resolution of mtimes, and if we manually change the times on any files
      # to the future mix corrects it
      :timer.sleep(1_000)

      File.touch!("lib/b.ex")
      assert_stale_run_output "1 test, 0 failures"

      :timer.sleep(1_000)

      File.touch!("lib/a.ex")
      assert_stale_run_output "2 tests, 0 failures"
    end
  end

  test "--stale: runs tests that have changed" do
    in_fixture "test_stale", fn ->
      assert_stale_run_output "2 tests, 0 failures"

      :timer.sleep(1_000)

      File.touch!("test/a_test_stale.exs")
      assert_stale_run_output "1 test, 0 failures"
    end
  end

  test "--stale: runs tests that have changed test_helpers" do
    in_fixture "test_stale", fn ->
      assert_stale_run_output "2 tests, 0 failures"

      :timer.sleep(1_000)

      File.touch!("test/test_helper.exs")
      assert_stale_run_output "2 tests, 0 failures"
    end
  end

  test "--stale: runs all tests no matter what with --force" do
    in_fixture "test_stale", fn ->
      assert_stale_run_output "2 tests, 0 failures"

      assert_stale_run_output ~w[--force], "2 tests, 0 failures"
    end
  end

  defp assert_stale_run_output(opts \\ [], expected) do
    assert mix(~w[test --stale] ++ opts) =~ expected
  end

  defp mix(args, envs \\ []) when is_list(args) do
    System.cmd(elixir_executable,
               ["-r", mix_executable, "--" | args],
               stderr_to_stdout: true,
               env: envs) |> elem(0)
  end

  defp mix_executable do
    Path.expand("../../../../../../bin/mix", __DIR__)
  end

  defp elixir_executable do
    Path.expand("../../../../../../bin/elixir", __DIR__)
  end
end
