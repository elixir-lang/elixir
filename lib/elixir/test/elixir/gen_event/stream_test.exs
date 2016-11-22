Code.require_file "../test_helper.exs", __DIR__

defmodule GenEvent.StreamTest do
  use ExUnit.Case, async: true

  defmodule SlowHandler do
    use GenEvent

    def handle_event(_event, _state) do
      Process.sleep(100)
      :remove_handler
    end
  end

  defp notify(:async, manager, event), do: GenEvent.notify(manager, event)
  defp notify(:sync, manager, event),  do: GenEvent.sync_notify(manager, event)
  defp notify(:ack, manager, event),   do: GenEvent.ack_notify(manager, event)

  @receive_timeout 1000

  test "ack stream/2" do
    {:ok, pid} = GenEvent.start_link()
    parent = self()

    spawn_link fn ->
      send parent, Enum.take(GenEvent.stream(pid), 3)
    end

    wait_for_handlers(pid, 1)

    for i <- 1..3 do
      GenEvent.ack_notify(pid, i)
    end

    assert_receive [1, 2, 3], @receive_timeout
    wait_for_handlers(pid, 0)

    spawn_link fn ->
      Enum.each GenEvent.stream(pid), fn _ ->
        Process.sleep(:infinity)
      end
    end

    wait_for_handlers(pid, 1)

    for i <- 1..6 do
      spawn_link fn ->
        GenEvent.ack_notify(pid, i)
      end
    end

    # Note the length is 4 compared to 5 n the sync case.
    wait_for_queue_length(pid, 4)
  end

  test "sync stream/2" do
    {:ok, pid} = GenEvent.start_link()
    parent = self()

    spawn_link fn ->
      send parent, Enum.take(GenEvent.stream(pid), 3)
    end

    wait_for_handlers(pid, 1)

    for i <- 1..3 do
      GenEvent.sync_notify(pid, i)
    end

    assert_receive [1, 2, 3], @receive_timeout
    wait_for_handlers(pid, 0)

    spawn_link fn ->
      Enum.each GenEvent.stream(pid), fn _ ->
        Process.sleep(:infinity)
      end
    end

    wait_for_handlers(pid, 1)

    for i <- 1..6 do
      spawn_link fn ->
        GenEvent.sync_notify(pid, i)
      end
    end

    wait_for_queue_length(pid, 5)
  end

  test "async stream/2" do
    {:ok, pid} = GenEvent.start_link()
    parent = self()

    spawn_link fn ->
      Enum.each(GenEvent.stream(pid), fn _ ->
        Process.sleep(:infinity)
      end)
    end

    spawn_link fn ->
      send parent, Enum.take(GenEvent.stream(pid), 3)
    end

    wait_for_handlers(pid, 2)

    for i <- 1..3 do
      GenEvent.notify(pid, i)
    end

    assert_receive [1, 2, 3], @receive_timeout
    wait_for_handlers(pid, 1)
  end

  test "stream/2 with timeout" do
    {:ok, pid} = GenEvent.start_link()
    Process.flag(:trap_exit, true)

    child = spawn_link fn ->
      Enum.take(GenEvent.stream(pid, timeout: 50), 5)
    end

    assert_receive {:EXIT, ^child,
                     {:timeout, {Enumerable.GenEvent.Stream, :next, [_, _]}}}, @receive_timeout
  end

  test "stream/2 with error/timeout on subscription" do
    {:ok, pid} = GenEvent.start_link()

    child = spawn fn -> Enum.to_list(GenEvent.stream(pid)) end
    wait_for_handlers(pid, 1)

    Process.exit(child, :kill)
    wait_for_handlers(pid, 0)
    GenEvent.stop(pid)
  end

  test "stream/2 with manager killed and trap_exit" do
    {:ok, pid} = GenEvent.start_link()
    stream = GenEvent.stream(pid)

    parent = self()
    child = spawn_link fn ->
      send parent, Enum.to_list(stream)
    end
    wait_for_handlers(pid, 1)

    Process.flag(:trap_exit, true)
    Process.exit(pid, :kill)
    assert_receive {:EXIT, ^pid, :killed}, @receive_timeout
    assert_receive ({:EXIT, ^child, {stat, {Enumerable.GenEvent.Stream, :stop, [_, _]}}}
                     when stat in [:killed, :noproc]), @receive_timeout
  end

  test "stream/2 with manager not alive" do
    stream = GenEvent.stream(:does_not_exit)

    parent = self()
    Process.flag(:trap_exit, true)
    child = spawn_link fn ->
      send parent, Enum.to_list(stream)
    end

    assert_receive {:EXIT, ^child,
                     {:noproc, {Enumerable.GenEvent.Stream, :start, [_]}}}, @receive_timeout
  end

  Enum.each [:sync, :async, :ack], fn mode ->
    @mode mode

    test "#{mode} stream/2 with parallel use (and first finishing first)" do
      {:ok, pid} = GenEvent.start_link()
      stream = GenEvent.stream(pid)

      parent = self()
      spawn_link fn -> send parent, {:take3, Enum.take(stream, 3)} end
      wait_for_handlers(pid, 1)
      spawn_link fn -> send parent, {:take5, Enum.take(stream, 5)} end
      wait_for_handlers(pid, 2)

      # Notify the events for both handlers
      for i <- 1..3 do
        notify(@mode, pid, i)
      end
      assert_receive {:take3, [1, 2, 3]}, @receive_timeout

      # Notify the events for to_list stream handler
      for i <- 4..5 do
        notify(@mode, pid, i)
      end

      assert_receive {:take5, [1, 2, 3, 4, 5]}, @receive_timeout
    end

    test "#{mode} stream/2 with manager stop" do
      {:ok, pid} = GenEvent.start_link()

      parent = self()
      child = spawn_link fn ->
        send parent, Enum.take(GenEvent.stream(pid), 5)
      end
      wait_for_handlers(pid, 1)

      for i <- 1..3 do
        notify(@mode, pid, i)
      end

      Process.flag(:trap_exit, true)
      GenEvent.stop(pid)
      assert_receive {:EXIT, ^child, :normal}, @receive_timeout
    end

    test "#{mode} stream/2 with swap_handler" do
      {:ok, pid} = GenEvent.start_link()
      stream = GenEvent.stream(pid, id: make_ref())

      parent = self()
      spawn_link(fn -> send parent, Enum.take(stream, 5) end)
      wait_for_handlers(pid, 1)

      for i <- 1..3 do
        notify(@mode, pid, i)
      end

      [handler] = GenEvent.which_handlers(pid)
      GenEvent.swap_handler(pid, handler, :swap_handler, UnknownHandler, [])
      assert_receive [1, 2, 3], @receive_timeout
    end

    test "#{mode} stream/2 with manager unregistered" do
      {:ok, pid} = GenEvent.start_link(name: :unreg)
      stream = GenEvent.stream(:unreg)

      parent = self()
      spawn_link fn ->
        send parent, Enum.take(stream, 5)
      end
      wait_for_handlers(pid, 1)

      # Notify the events
      for i <- 1..3 do
        notify(@mode, pid, i)
      end

      # Unregister the process
      Process.unregister(:unreg)

      # Notify the remaining events
      for i <- 4..5 do
        notify(@mode, pid, i)
      end

      # We should have gotten the message and all handlers were removed
      assert_receive [1, 2, 3, 4, 5], @receive_timeout
      wait_for_handlers(pid, 0)
    end

    test "#{mode} stream/2 flushes events on abort" do
      {:ok, pid} = GenEvent.start_link()

      spawn_link fn ->
        wait_for_handlers(pid, 2)
        notify(@mode, pid, 1)
        notify(@mode, pid, 2)
        notify(@mode, pid, 3)
      end

      GenEvent.add_handler(pid, SlowHandler, [])
      stream = GenEvent.stream(pid)

      try do
        Enum.each stream, fn _ -> throw :done end
      catch
        :done -> :ok
      end

      # Wait for the slow handler to be removed
      # so all events have been handled
      wait_for_handlers(pid, 0)

      # Check no messages leaked.
      refute_received _any
    end
  end

  defp wait_for_handlers(pid, count) do
    unless length(GenEvent.which_handlers(pid)) == count do
      wait_for_handlers(pid, count)
    end
  end

  defp wait_for_queue_length(pid, count) do
    {:message_queue_len, n} = Process.info(pid, :message_queue_len)
    unless n == count do
      wait_for_queue_length(pid, count)
    end
  end
end
