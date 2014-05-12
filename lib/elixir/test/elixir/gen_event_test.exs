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

    def handle_event(_event, state) do
      :timer.sleep(300)
      {:ok, state}
    end
  end

  @receive_timeout 1000

  teardown _ do
    Process.flag(:trap_exit, false)
    :ok
  end

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

    assert GenEvent.call(pid, LoggerHandler, :whatever)  == {:error, :bad_call}
    assert GenEvent.call(pid, UnknownHandler, :messages) == {:error, :bad_module}

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
    {:ok, _} = GenEvent.start(name: :logger)
    assert GenEvent.stop(:logger) == :ok
  end

  test "stream/2 is enumerable" do
    # Start a manager
    {:ok, pid} = GenEvent.start_link()

    # Also start multiple subscribers
    parent = self()
    spawn_link fn -> send parent, Enum.take(GenEvent.stream(pid), 5) end
    spawn_link fn -> send parent, Enum.take(GenEvent.stream(pid), 3) end
    wait_for_handlers(pid, 2)

    # Notify the events
    for i <- 1..3 do
      GenEvent.sync_notify(pid, i)
    end

    # Receive one of the results
    assert_receive  [1, 2, 3], @receive_timeout
    refute_received [1, 2, 3, 4, 5]

    # Push the remaining events
    for i <- 4..10 do
      GenEvent.sync_notify(pid, i)
    end

    assert_receive [1, 2, 3, 4, 5], @receive_timeout

    # Both subscriptions are gone
    wait_for_handlers(pid, 0)
    GenEvent.stop(pid)
  end

  test "stream/2 with timeout" do
    # Start a manager
    {:ok, pid} = GenEvent.start_link()
    Process.flag(:trap_exit, true)

    pid = spawn_link fn ->
      Enum.take(GenEvent.stream(pid, timeout: 50), 5)
    end

    assert_receive {:EXIT, ^pid,
                     {:timeout, {Enumerable.GenEvent, :next, [_, _]}}}, @receive_timeout
  end

  test "stream/2 with error/timeout on subscription" do
    # Start a manager
    {:ok, pid} = GenEvent.start_link()

    # Start a subscriber with timeout
    child = spawn fn -> Enum.to_list(GenEvent.stream(pid)) end
    wait_for_handlers(pid, 1)

    # Kill and wait until we have 0 handlers
    Process.exit(child, :kill)
    wait_for_handlers(pid, 0)
    GenEvent.stop(pid)
  end

  test "stream/2 with manager stop" do
    # Start a manager and subscribers
    {:ok, pid} = GenEvent.start_link()

    parent = self()
    stream_pid = spawn_link fn ->
      send parent, Enum.take(GenEvent.stream(pid), 5)
    end
    wait_for_handlers(pid, 1)

    # Notify the events
    for i <- 1..3 do
      GenEvent.sync_notify(pid, i)
    end

    Process.flag(:trap_exit, true)
    GenEvent.stop(pid)
    assert_receive {:EXIT, ^stream_pid,
                     {:shutdown, {Enumerable.GenEvent, :next, [_, _]}}}, @receive_timeout
  end

  test "stream/2 with cancel streams" do
    # Start a manager and subscribers
    {:ok, pid} = GenEvent.start_link()
    stream = GenEvent.stream(pid, id: make_ref())

    parent = self()
    spawn_link fn -> send parent, Enum.take(stream, 5) end
    wait_for_handlers(pid, 1)

    # Notify the events
    for i <- 1..3 do
      GenEvent.sync_notify(pid, i)
    end

    GenEvent.cancel_streams(stream)
    assert_receive [1, 2, 3], @receive_timeout
    GenEvent.stop(pid)
  end

  test "stream/2 with swap_handler" do
    # Start a manager and subscribers
    {:ok, pid} = GenEvent.start_link()
    stream = GenEvent.stream(pid, id: make_ref())

    parent = self()
    stream_pid = spawn_link fn -> send parent, Enum.take(stream, 5) end
    wait_for_handlers(pid, 1)

    # Notify the events
    for i <- 1..3 do
      GenEvent.sync_notify(pid, i)
    end

    [handler] = GenEvent.which_handlers(pid)
    Process.flag(:trap_exit, true)
    GenEvent.swap_handler(pid, handler, :swap_handler, LogHandler, [])
    assert_receive {:EXIT, ^stream_pid,
                     {{:swapped, LogHandler, _}, {Enumerable.GenEvent, :next, [_, _]}}}, @receive_timeout
  end

  test "stream/2 with duration" do
    # Start a manager and subscribers
    {:ok, pid} = GenEvent.start_link()
    stream = GenEvent.stream(pid, duration: 200)

    parent = self()
    spawn_link fn -> send parent, {:duration, Enum.take(stream, 10)} end
    wait_for_handlers(pid, 1)

    # Notify the events
    for i <- 1..5 do
      GenEvent.sync_notify(pid, i)
    end

    # Wait until the handler is gone
    wait_for_handlers(pid, 0)

    # The stream is not complete but terminated anyway due to duration
    assert_receive {:duration, [1, 2, 3, 4, 5]}, @receive_timeout

    GenEvent.stop(pid)
  end

  test "stream/2 with parallel use and first finishing first" do
    # Start a manager and subscribers
    {:ok, pid} = GenEvent.start_link()
    stream = GenEvent.stream(pid, duration: 200)

    parent = self()
    spawn_link fn -> send parent, {:take, Enum.take(stream, 3)} end
    wait_for_handlers(pid, 1)
    spawn_link fn -> send parent, {:to_list, Enum.to_list(stream)} end
    wait_for_handlers(pid, 2)

    # Notify the events for both handlers
    for i <- 1..3 do
      GenEvent.sync_notify(pid, i)
    end
    assert_receive {:take, [1, 2, 3]}, @receive_timeout

    # Notify the events for to_list stream handler
    for i <- 4..5 do
      GenEvent.sync_notify(pid, i)
    end

    assert_receive {:to_list, [1, 2, 3, 4, 5]}, @receive_timeout
  end

  test "stream/2 with manager killed and trap_exit" do
    # Start a manager and subscribers
    {:ok, pid} = GenEvent.start_link()
    stream = GenEvent.stream(pid)

    parent = self()
    stream_pid = spawn_link fn ->
      send parent, Enum.to_list(stream)
    end
    wait_for_handlers(pid, 1)

    Process.flag(:trap_exit, true)
    Process.exit(pid, :kill)
    assert_receive {:EXIT, ^pid, :killed}, @receive_timeout
    assert_receive {:EXIT, ^stream_pid,
                     {:killed, {Enumerable.GenEvent, :next, [_,_]}}}, @receive_timeout
  end

  test "stream/2 with manager not alive" do
    # Start a manager and subscribers
    stream = GenEvent.stream(:does_not_exit)

    parent = self()
    stream_pid = spawn_link fn ->
      send parent, Enum.to_list(stream)
    end

    Process.flag(:trap_exit, true)
    assert_receive {:EXIT, ^stream_pid,
                     {:noproc, {Enumerable.GenEvent, :start, [_]}}}, @receive_timeout
  end

  test "stream/2 with manager unregistered" do
    # Start a manager and subscribers
    {:ok, pid} = GenEvent.start_link(name: :unreg)
    stream = GenEvent.stream(:unreg)

    parent = self()
    spawn_link fn ->
      send parent, Enum.take(stream, 5)
    end
    wait_for_handlers(pid, 1)

    # Notify the events
    for i <- 1..3 do
      GenEvent.sync_notify(pid, i)
    end

    # Unregister the process
    Process.unregister(:unreg)

    # Notify the remaining events
    for i <- 4..5 do
      GenEvent.sync_notify(pid, i)
    end

    # We should have gotten the message and all handlers were removed
    assert_receive [1, 2, 3, 4, 5], @receive_timeout
    wait_for_handlers(pid, 0)
  end

  test "stream/2 with slow handler" do
    # Start a manager and subscribers
    {:ok, pid} = GenEvent.start_link()
    stream = GenEvent.stream(pid, duration: 200)

    spawn_link fn ->
      # Wait for stream to start
      wait_for_handlers(pid, 1)
      GenEvent.notify(pid, 1)

      # Add slow handler so that the second or
      # third event arrives after duration of 200.
      GenEvent.add_handler(pid, SlowHandler, [], link: true)
      GenEvent.notify(pid, 2)
      GenEvent.notify(pid, 3)
    end

    # Evaluate stream.
    _ = Enum.to_list(stream)

    # Wait for the slow handler to be removed so all events have been handled.
    wait_for_handlers(pid, 0)

    # Check no messages leaked.
    refute_received _any
  end

  defp wait_for_handlers(pid, count) do
    unless length(GenEvent.which_handlers(pid)) == count do
      wait_for_handlers(pid, count)
    end
  end
end
