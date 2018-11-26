Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.CaptureLogTest do
  use ExUnit.Case

  require Logger

  import ExUnit.CaptureLog

  setup_all do
    :ok = Logger.remove_backend(:console)
    on_exit(fn -> Logger.add_backend(:console, flush: true) end)
  end

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
    assert capture_log([level: :warn], fn ->
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

        Logger.warn("two")

        spawn(fn ->
          Logger.debug("three")
          send(test, :done)
        end)

        receive do: (:done -> :ok)
      end)

    assert logged
    assert logged =~ "[info]  one\n"
    assert logged =~ "[warn]  two\n"
    assert logged =~ "[debug] three\n"
    assert logged =~ "[error] one\n"

    receive do
      {:nested, logged} ->
        assert logged =~ "[error] one\n"
        refute logged =~ "[warn]  two\n"
    end
  end

  defp wait_capture_removal() do
    case :gen_event.which_handlers(Logger) do
      [Logger.Config] ->
        :ok

      _otherwise ->
        Process.sleep(20)
        wait_capture_removal()
    end
  end
end
