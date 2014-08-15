Code.require_file "test_helper.exs", __DIR__

defmodule GenEventTest do
  use ExUnit.Case, async: true

  defmodule LoggerHandler do
    use GenEvent

    def handle_event({:log, x}, messages) do
      {:ok, [x|messages]}
    end

    def handle_call(:messages, messages) do
      {:ok, Enum.reverse(messages), []}
    end

    def handle_call(call, state) do
      super(call, state)
    end
  end

  defmodule SlowHandler do
    use GenEvent

    def handle_event(_event, _state) do
      :timer.sleep(100)
      :remove_handler
    end
  end

  @receive_timeout 1000

  test "start_link/2 and handler workflow" do
    {:ok, pid} = GenEvent.start_link()

    {:links, links} = Process.info(self, :links)
    assert pid in links

    assert GenEvent.notify(pid, {:log, 0}) == :ok
    assert GenEvent.add_handler(pid, LoggerHandler, []) == :ok
    assert GenEvent.notify(pid, {:log, 1}) == :ok
    assert GenEvent.notify(pid, {:log, 2}) == :ok

    assert GenEvent.call(pid, LoggerHandler, :messages) == [1, 2]
    assert GenEvent.call(pid, LoggerHandler, :messages) == []

    assert GenEvent.remove_handler(pid, LoggerHandler, []) == :ok
    assert GenEvent.stop(pid) == :ok
  end

  test "start/2 with linked handler" do
    {:ok, pid} = GenEvent.start()

    {:links, links} = Process.info(self, :links)
    refute pid in links

    assert GenEvent.add_handler(pid, LoggerHandler, [], link: true) == :ok

    {:links, links} = Process.info(self, :links)
    assert pid in links

    assert GenEvent.notify(pid, {:log, 1}) == :ok
    assert GenEvent.sync_notify(pid, {:log, 2}) == :ok

    assert GenEvent.call(pid, LoggerHandler, :messages) == [1, 2]
    assert GenEvent.stop(pid) == :ok
  end

  test "start/2 with linked swap" do
    {:ok, pid} = GenEvent.start()

    assert GenEvent.add_handler(pid, LoggerHandler, []) == :ok

    {:links, links} = Process.info(self, :links)
    refute pid in links

    assert GenEvent.swap_handler(pid, LoggerHandler, [], LoggerHandler, [], link: true) == :ok

    {:links, links} = Process.info(self, :links)
    assert pid in links

    assert GenEvent.stop(pid) == :ok
  end

  test "start/2 with registered name" do
    {:ok, _} = GenEvent.start(name: :gen_event_logger)
    assert GenEvent.stop(:gen_event_logger) == :ok
  end

  test "bad calls" do
    Logger.remove_backend(:console)
    {:ok, pid} = GenEvent.start_link()
    assert GenEvent.add_handler(pid, LoggerHandler, []) == :ok
    assert GenEvent.call(pid, UnknownHandler, :messages) ==
             {:error, :module_not_found}
    assert GenEvent.call(pid, LoggerHandler, :whatever) ==
             {:error, {:bad_call, :whatever}}
    assert GenEvent.which_handlers(pid) == []
    assert GenEvent.stop(pid) == :ok
  after
    Logger.add_backend(:console, flush: true)
  end

  test "bad notify" do
    assert GenEvent.notify({:global, :foo}, :bar) == :ok
    assert GenEvent.notify({:foo, :bar}, :bar) == :ok
    assert GenEvent.notify(:foo, :bar) == :ok
  end
end
