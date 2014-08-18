Code.require_file "test_helper.exs", __DIR__

defmodule GenServerTest do
  use ExUnit.Case, async: true

  defmodule Stack do
    use GenServer

    def handle_call(:stop, from, state) do
      GenServer.reply(from, :ok)
      {:stop, :normal, state}
    end

    def handle_call(:pop, _from, [h|t]) do
      {:reply, h, t}
    end

    def handle_call(request, from, state) do
      super(request, from, state)
    end

    def handle_cast({:push, item}, state) do
      {:noreply, [item|state]}
    end

    def handle_cast(request, state) do
      super(request, state)
    end

    def terminate(_reason, _state) do
      # There is a race condition if the agent is
      # restarted too fast and it is registered.
      try do
        self |> Process.info(:registered_name) |> elem(1) |> Process.unregister
      rescue
        _ -> :ok
      end
      :ok
    end
  end

  test "start_link/2, call/2 and cast/2" do
    {:ok, pid} = GenServer.start_link(Stack, [:hello])

    {:links, links} = Process.info(self, :links)
    assert pid in links

    assert GenServer.call(pid, :pop) == :hello
    assert GenServer.cast(pid, {:push, :world}) == :ok
    assert GenServer.call(pid, :pop) == :world
    assert GenServer.call(pid, :stop) == :ok

    assert GenServer.cast({:global, :foo}, {:push, :world}) == :ok
    assert GenServer.cast({:via, :foo, :bar}, {:push, :world}) == :ok
    assert_raise ArgumentError, fn ->
      GenServer.cast(:foo, {:push, :world})
    end
  end

  test "nil name" do
    {:ok, pid} = GenServer.start_link(Stack, [:hello], name: nil)
    assert Process.info(pid, :registered_name) == {:registered_name, []}
  end

  test "start/2" do
    {:ok, pid} = GenServer.start(Stack, [:hello])
    {:links, links} = Process.info(self, :links)
    refute pid in links
    GenServer.call(pid, :stop)
  end

  test "abcast/3" do
    {:ok, _} = GenServer.start_link(Stack, [], name: :stack)

    assert GenServer.abcast(:stack, {:push, :hello}) == :abcast
    assert GenServer.call({:stack, node()}, :pop) == :hello

    assert GenServer.abcast([node, :foo@bar], :stack, {:push, :world}) == :abcast
    assert GenServer.call(:stack, :pop) == :world

    GenServer.call(:stack, :stop)
  end

  test "multi_call/4" do
    {:ok, _} = GenServer.start_link(Stack, [:hello, :world], name: :stack)

    assert GenServer.multi_call(:stack, :pop) ==
           {[{node(), :hello}], []}
    assert GenServer.multi_call([node, :foo@bar], :stack, :pop) ==
           {[{node, :world}], [:foo@bar]}

    GenServer.call(:stack, :stop)
  end
end
