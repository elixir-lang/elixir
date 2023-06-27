Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.CaptureLogTest do
  use ExUnit.Case

  require Logger
  import ExUnit.CaptureLog

  test "no output" do
    assert capture_log(fn -> nil end) == ""
  end

  test "assert inside" do
    group_leader = Process.group_leader()

    try do
      capture_log(fn ->
        assert false
      end)
    rescue
      error in [ExUnit.AssertionError] ->
        assert error.message == "Expected truthy, got false"
    end

    # Ensure no leakage on failures
    assert group_leader == Process.group_leader()
    refute_received {:gen_event_EXIT, _, _}
  end

  test "level aware" do
    assert capture_log([level: :warning], fn ->
             Logger.info("here")
           end) == ""
  end

  @tag timeout: 2000
  test "capture removal on exit" do
    {_pid, ref} =
      spawn_monitor(fn ->
        capture_log(fn ->
          spawn_link(Kernel, :exit, [:shutdown])
          Process.sleep(:infinity)
        end)
      end)

    assert_receive {:DOWN, ^ref, _, _, :shutdown}
    wait_capture_removal()
  end

  test "log tracking" do
    logged =
      capture_log(fn ->
        Logger.info("one")

        logged = capture_log(fn -> Logger.error("one") end)
        send(test = self(), {:nested, logged})

        Logger.warning("two")

        spawn(fn ->
          Logger.debug("three")
          send(test, :done)
        end)

        receive do: (:done -> :ok)
      end)

    assert logged
    assert logged =~ "[info] one\n"
    assert logged =~ "[warning] two\n"
    assert logged =~ "[debug] three\n"
    assert logged =~ "[error] one\n"

    receive do
      {:nested, logged} ->
        assert logged =~ "[error] one\n"
        refute logged =~ "[warning] two\n"
    end
  end

  test "deprecated log level" do
    ExUnit.CaptureIO.capture_io(:stderr, fn ->
      output =
        capture_log([level: :warn], fn ->
          Logger.log(:warn, "ABC")
          Logger.log(:warning, "DEF")
        end)

      assert output =~ "ABC"
      assert output =~ "DEF"
    end)
  end

  describe "with_log/2" do
    test "returns the result and the log" do
      {result, log} =
        with_log(fn ->
          Logger.error("calculating...")
          2 + 2
        end)

      assert result == 4
      assert log =~ "calculating..."
    end

    test "respects the :format, :metadata, and :colors options" do
      options = [format: "$metadata| $message", metadata: [:id], colors: [enabled: false]]

      assert {4, log} =
               with_log(options, fn ->
                 Logger.info("hello", id: 123)
                 2 + 2
               end)

      assert log == "id=123 | hello"
    end

    @tag capture_log: true
    test "respect options with capture_log: true" do
      options = [format: "$metadata| $message", metadata: [:id], colors: [enabled: false]]

      assert {4, log} =
               with_log(options, fn ->
                 Logger.info("hello", id: 123)
                 2 + 2
               end)

      assert log == "id=123 | hello"
    end
  end

  defp wait_capture_removal() do
    if ExUnit.CaptureServer in Enum.map(:logger.get_handler_config(), & &1.id) do
      Process.sleep(20)
      wait_capture_removal()
    else
      :ok
    end
  end
end
