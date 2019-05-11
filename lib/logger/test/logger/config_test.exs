defmodule Logger.ConfigTest do
  use Logger.Case
  require Logger

  test "log/2 relies on sync_threshold" do
    Logger.remove_backend(:console)
    Logger.configure(sync_threshold: 0)
    for _ <- 1..1000, do: Logger.log(:info, "some message")
  after
    Logger.configure(sync_threshold: 20)
    Logger.add_backend(:console)
  end

  test "log/2 relies on discard_threshold" do
    Logger.configure(discard_threshold: 0)
    for _ <- 1..1000, do: Logger.log(:info, "some message")
  after
    Logger.configure(discard_threshold: 10000)
  end

  test "log/2 recovers from discard_threshold" do
    Logger.remove_backend(:console)
    Logger.configure(discard_threshold: 1, discard_threshold_periodic_check: 0)

    :sys.suspend(Logger)
    assert Logger.log(:info, "BEGIN") == :ok
    send(Logger, :garbage)
    :sys.resume(Logger)

    # Simulate a sync operation to make sure we are in :discard mode
    Logger.add_translator({Unknown, :translate})
    Logger.remove_translator({Unknown, :translate})

    # It should eventually heal
    wait_for_mode(:async)
  after
    Logger.configure(discard_threshold: 10000, discard_threshold_periodic_check: 30000)
    Logger.add_backend(:console)
  end

  test "restarts Logger.Config on Logger exits" do
    Process.whereis(Logger) |> Process.exit(:kill)
    wait_for_logger()
    wait_for_handler(Logger, Logger.Config)
  end

  test "Logger.Config updates config on config_change/3" do
    :ok = Logger.configure(level: :debug)

    try do
      Application.put_env(:logger, :level, :error)
      assert Logger.App.config_change([level: :error], [], []) === :ok
      assert Logger.level() === :error
    after
      Logger.configure(level: :debug)
    end
  end

  def wait_for_mode(mode) do
    unless Logger.Config.__data__().mode == mode do
      Process.sleep(10)
      wait_for_mode(mode)
    end
  end
end
