defmodule Logger.BackendsTest do
  use Logger.Case
  require Logger

  defmodule MyBackend do
    @behaviour :gen_event

    def init({MyBackend, pid}) when is_pid(pid) do
      {:ok, pid}
    end

    def handle_event(event, state) do
      send(state, {:event, event})
      {:ok, state}
    end

    def handle_call(:error, _) do
      raise "oops"
    end

    def handle_info(_msg, state) do
      {:ok, state}
    end

    def code_change(_old_vsn, state, _extra) do
      {:ok, state}
    end

    def terminate(_reason, _state) do
      :ok
    end
  end

  test "add_backend/1 and remove_backend/1" do
    assert {:ok, _pid} = Logger.add_backend(Logger.Backends.Console)
    assert Logger.add_backend(Logger.Backends.Console) == {:error, :already_present}
    assert :ok = Logger.remove_backend(Logger.Backends.Console)
    assert Logger.remove_backend(Logger.Backends.Console) == {:error, :not_found}
  end

  test "add_backend/1 with {module, id}" do
    assert {:ok, _} = Logger.add_backend({MyBackend, self()})
    assert {:error, :already_present} = Logger.add_backend({MyBackend, self()})
    assert :ok = Logger.remove_backend({MyBackend, self()})
  end

  test "add_backend/1 with unknown backend" do
    assert {:error, {{:EXIT, {:undef, [_ | _]}}, _}} =
             Logger.add_backend({UnknownBackend, self()})
  end

  test "logs or writes to stderr on failed call on async mode" do
    assert {:ok, _} = Logger.add_backend({MyBackend, self()})

    assert capture_log(fn ->
             ExUnit.CaptureIO.capture_io(:stderr, fn ->
               :gen_event.call(Logger, {MyBackend, self()}, :error)
               wait_for_handler(Logger, {MyBackend, self()})
             end)
           end) =~
             ~r":gen_event handler {Logger.BackendsTest.MyBackend, #PID<.*>} installed in Logger terminating"

    Logger.flush()
  after
    Logger.remove_backend({MyBackend, self()})
  end

  test "logs or writes to stderr on failed call on sync mode" do
    Logger.configure(sync_threshold: 0)
    assert {:ok, _} = Logger.add_backend({MyBackend, self()})

    assert capture_log(fn ->
             ExUnit.CaptureIO.capture_io(:stderr, fn ->
               :gen_event.call(Logger, {MyBackend, self()}, :error)
               wait_for_handler(Logger, {MyBackend, self()})
             end)
           end) =~
             ~r":gen_event handler {Logger.BackendsTest.MyBackend, #PID<.*>} installed in Logger terminating"

    Logger.flush()
  after
    Logger.configure(sync_threshold: 20)
    Logger.remove_backend({MyBackend, :hello})
  end

  test "logs when discarding messages" do
    assert :ok = Logger.configure(discard_threshold: 5)
    Logger.add_backend({MyBackend, self()})

    capture_log(fn ->
      :sys.suspend(Logger)
      for _ <- 1..10, do: Logger.warning("warning!")
      :sys.resume(Logger)
      Logger.flush()
      send(Logger, {Logger.Backends.Config, :update_counter})
    end)

    assert_receive {:event,
                    {:warning, _,
                     {Logger, "Attempted to log 0 messages, which is below :discard_threshold",
                      _time, _metadata}}}
  after
    :sys.resume(Logger)
    Logger.remove_backend({MyBackend, self()})
    assert :ok = Logger.configure(discard_threshold: 500)
  end

  test "restarts Logger.Backends.Config on Logger exits" do
    Logger.Backends.Internal.configure([])

    capture_log(fn ->
      Process.whereis(Logger) |> Process.exit(:kill)
      wait_for_logger()
      wait_for_handler(Logger, Logger.Backends.Config)
    end)
  end
end
