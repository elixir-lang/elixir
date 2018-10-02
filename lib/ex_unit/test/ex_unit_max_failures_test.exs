Code.require_file("test_helper.exs", __DIR__)

defmodule ExUnitMaxFailuresTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO
  import IEx.Helpers, only: [pid: 3]

  describe "max failures" do
    test "default value to :infinity" do
      on_exit_reload_config()
      ExUnit.start(autorun: false)
      config = ExUnit.configuration()
      assert config[:max_failures] == :infinity
    end

    test "sets value of max failures" do
      on_exit_reload_config()
      ExUnit.start(max_failures: 5, autorun: false)
      config = ExUnit.configuration()
      assert config[:max_failures] == 5
    end

    # TODO: move this test to test/ex_unit/on_exit_handler_test.exs
    test "get_registered_pids" do
      manager1 = {pid(0, 100, 1), pid(0, 100, 2)}
      manager2 = {pid(0, 200, 1), pid(0, 200, 2)}

      ExUnit.OnExitHandler.register(pid(0, 1, 1), manager1)
      ExUnit.OnExitHandler.register(pid(0, 1, 2), manager1)
      ExUnit.OnExitHandler.register(pid(0, 1, 3), manager1)
      ExUnit.OnExitHandler.register(pid(0, 1, 4), manager1)
      ExUnit.OnExitHandler.register(pid(0, 1, 5), manager2)
      ExUnit.OnExitHandler.register(pid(0, 1, 6), manager2)

      assert ExUnit.OnExitHandler.get_registered_pids(manager2) |> Enum.sort() == [
               pid(0, 1, 5),
               pid(0, 1, 6)
             ]
    end

    test "max failures are reached, async: false, max_cases: 1" do
      defmodule TestMaxFailuresReached do
        use ExUnit.Case, async: false

        test __ENV__.line, do: assert(false)
        test __ENV__.line, do: assert(false)
      end

      ExUnit.Server.modules_loaded()
      on_exit_reload_config()

      output =
        capture_io(fn ->
          ex_unit_start(trace: true, max_failures: 1)

          assert ExUnit.run() == %{
                   total: 2,
                   failures: 1,
                   skipped: 0,
                   excluded: 0,
                   not_executed: 1
                 }
        end)

      assert strip(output) =~ "2 tests, 1 failure, 1 not executed"
    end

    test "max failures is right bellow the limit" do
      defmodule TestMaxFailures do
        use ExUnit.Case, async: false

        test "pass #{__ENV__.line}", do: assert(true)
        test "fail #{__ENV__.line}", do: assert(false)
        test "pass #{__ENV__.line}", do: assert(true)
        test "fail #{__ENV__.line}", do: assert(false)
        test "pass #{__ENV__.line}", do: assert(true)
      end

      ExUnit.Server.modules_loaded()
      on_exit_reload_config()

      output =
        capture_io(fn ->
          ex_unit_start(max_failures: 3, max_cases: 5)

          assert ExUnit.run() == %{
                   total: 5,
                   excluded: 0,
                   failures: 2,
                   not_executed: 0,
                   skipped: 0
                 }
        end)

      assert strip(output) =~ "5 tests, 2 failures"
    end

    test "max failures are reached" do
      defmodule TestMaxFailuresReached2 do
        use ExUnit.Case, async: false

        defp sleep(time) do
          Process.sleep(time)
          :ok
        end

        test "pass 1" do
          assert sleep(1000) == :ok
        end

        test "fail 1" do
          assert sleep(1000) == false
        end

        test "fail 2" do
          assert sleep(1000) == false
        end

        test "fail 3" do
          assert sleep(1000) == false
        end

        test "fail 4" do
          assert sleep(1000) == false
        end

        test __ENV__.line, do: assert(false)
        test __ENV__.line, do: assert(false)
        test __ENV__.line, do: assert(false)
        test __ENV__.line, do: assert(false)
        test __ENV__.line, do: assert(false)
        test __ENV__.line, do: assert(false)
        test __ENV__.line, do: assert(false)
        test __ENV__.line, do: assert(false)
        test __ENV__.line, do: assert(false)
        test __ENV__.line, do: assert(false)
      end

      ExUnit.Server.modules_loaded()
      on_exit_reload_config()

      output =
        capture_io(fn ->
          ex_unit_start(max_failures: 2)

          assert ExUnit.run() == %{
                   total: 15,
                   excluded: 0,
                   failures: 2,
                   not_executed: 12,
                   skipped: 0
                 }
        end)

      assert strip(output) =~ "15 tests, 2 failures, 12 not executed"
    end
  end

  ##  Helpers

  defp on_exit_reload_config(extra \\ []) do
    old_config = ExUnit.configuration()
    on_exit(fn -> ExUnit.configure(extra ++ old_config) end)
  end

  # Strips the colors from IO.ANSI strings
  defp strip(string) do
    String.replace(string, ~r/\e\[(\d+;?)+m/, "")
  end

  # Runs ExUnit.start/1 with common options needed for predictability
  def ex_unit_start(options) do
    ExUnit.start(options ++ [autorun: false, seed: 0])
  end
end
