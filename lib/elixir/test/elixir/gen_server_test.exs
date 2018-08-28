Code.require_file("test_helper.exs", __DIR__)

defmodule GenServerTest do
  use ExUnit.Case, async: true

  defmodule Stack do
    use GenServer

    def init(args) do
      {:ok, args}
    end

    def handle_call(:pop, _from, [h | t]) do
      {:reply, h, t}
    end

    def handle_call(:noreply, _from, h) do
      {:noreply, h}
    end

    def handle_call(:stop_self, _from, state) do
      reason = catch_exit(GenServer.stop(self()))
      {:reply, reason, state}
    end

    def handle_cast({:push, item}, state) do
      {:noreply, [item | state]}
    end

    def terminate(_reason, _state) do
      # There is a race condition if the agent is
      # restarted too fast and it is registered.
      try do
        self() |> Process.info(:registered_name) |> elem(1) |> Process.unregister()
      rescue
        _ -> :ok
      end

      :ok
    end
  end

  test "generates child_spec/1" do
    assert Stack.child_spec([:hello]) == %{
             id: Stack,
             start: {Stack, :start_link, [[:hello]]}
           }

    defmodule CustomStack do
      use GenServer, id: :id, restart: :temporary, shutdown: :infinity, start: {:foo, :bar, []}

      def init(args) do
        {:ok, args}
      end
    end

    assert CustomStack.child_spec([:hello]) == %{
             id: :id,
             restart: :temporary,
             shutdown: :infinity,
             start: {:foo, :bar, []}
           }
  end

  test "start_link/3" do
    assert_raise ArgumentError, ~r"expected :name option to be one of the following:", fn ->
      GenServer.start_link(Stack, [:hello], name: "my_gen_server_name")
    end

    assert_raise ArgumentError, ~r"expected :name option to be one of the following:", fn ->
      GenServer.start_link(Stack, [:hello], name: {:invalid_tuple, "my_gen_server_name"})
    end

    assert_raise ArgumentError, ~r"expected :name option to be one of the following:", fn ->
      GenServer.start_link(Stack, [:hello], name: {:via, "Via", "my_gen_server_name"})
    end

    assert_raise ArgumentError, ~r/Got: "my_gen_server_name"/, fn ->
      GenServer.start_link(Stack, [:hello], name: "my_gen_server_name")
    end
  end

  test "start_link/3 with via" do
    GenServer.start_link(Stack, [:hello], name: {:via, :global, :via_stack})
    assert GenServer.call({:via, :global, :via_stack}, :pop) == :hello
  end

  test "start_link/3 with global" do
    GenServer.start_link(Stack, [:hello], name: {:global, :global_stack})
    assert GenServer.call({:global, :global_stack}, :pop) == :hello
  end

  test "start_link/3 with local" do
    GenServer.start_link(Stack, [:hello], name: :stack)
    assert GenServer.call(:stack, :pop) == :hello
  end

  test "start_link/2, call/2 and cast/2" do
    {:ok, pid} = GenServer.start_link(Stack, [:hello])

    {:links, links} = Process.info(self(), :links)
    assert pid in links

    assert GenServer.call(pid, :pop) == :hello
    assert GenServer.cast(pid, {:push, :world}) == :ok
    assert GenServer.call(pid, :pop) == :world
    assert GenServer.stop(pid) == :ok

    assert GenServer.cast({:global, :foo}, {:push, :world}) == :ok
    assert GenServer.cast({:via, :foo, :bar}, {:push, :world}) == :ok
    assert GenServer.cast(:foo, {:push, :world}) == :ok
  end

  @tag capture_log: true
  test "call/3 exit messages" do
    name = :self
    Process.register(self(), name)
    :global.register_name(name, self())
    {:ok, pid} = GenServer.start_link(Stack, [:hello])
    {:ok, stopped_pid} = GenServer.start(Stack, [:hello])
    GenServer.stop(stopped_pid)

    assert catch_exit(GenServer.call(name, :pop, 5000)) ==
             {:calling_self, {GenServer, :call, [name, :pop, 5000]}}

    assert catch_exit(GenServer.call({:global, name}, :pop, 5000)) ==
             {:calling_self, {GenServer, :call, [{:global, name}, :pop, 5000]}}

    assert catch_exit(GenServer.call({:via, :global, name}, :pop, 5000)) ==
             {:calling_self, {GenServer, :call, [{:via, :global, name}, :pop, 5000]}}

    assert catch_exit(GenServer.call(self(), :pop, 5000)) ==
             {:calling_self, {GenServer, :call, [self(), :pop, 5000]}}

    assert catch_exit(GenServer.call(pid, :noreply, 1)) ==
             {:timeout, {GenServer, :call, [pid, :noreply, 1]}}

    assert catch_exit(GenServer.call(nil, :pop, 5000)) ==
             {:noproc, {GenServer, :call, [nil, :pop, 5000]}}

    assert catch_exit(GenServer.call(stopped_pid, :pop, 5000)) ==
             {:noproc, {GenServer, :call, [stopped_pid, :pop, 5000]}}

    assert catch_exit(GenServer.call({:stack, :bogus_node}, :pop, 5000)) ==
             {{:nodedown, :bogus_node}, {GenServer, :call, [{:stack, :bogus_node}, :pop, 5000]}}
  end

  test "nil name" do
    {:ok, pid} = GenServer.start_link(Stack, [:hello], name: nil)
    assert Process.info(pid, :registered_name) == {:registered_name, []}
  end

  test "start/2" do
    {:ok, pid} = GenServer.start(Stack, [:hello])
    {:links, links} = Process.info(self(), :links)
    refute pid in links
    GenServer.stop(pid)
  end

  test "abcast/3", %{test: name} do
    {:ok, _} = GenServer.start_link(Stack, [], name: name)

    assert GenServer.abcast(name, {:push, :hello}) == :abcast
    assert GenServer.call({name, node()}, :pop) == :hello

    assert GenServer.abcast([node(), :foo@bar], name, {:push, :world}) == :abcast
    assert GenServer.call(name, :pop) == :world
  end

  test "multi_call/4", %{test: name} do
    {:ok, _} = GenServer.start_link(Stack, [:hello, :world], name: name)

    assert GenServer.multi_call(name, :pop) == {[{node(), :hello}], []}

    assert GenServer.multi_call([node(), :foo@bar], name, :pop) ==
             {[{node(), :world}], [:foo@bar]}
  end

  test "whereis/1" do
    name = :whereis_server

    {:ok, pid} = GenServer.start_link(Stack, [], name: name)
    assert GenServer.whereis(name) == pid
    assert GenServer.whereis({name, node()}) == pid
    assert GenServer.whereis({name, :another_node}) == {name, :another_node}
    assert GenServer.whereis(pid) == pid
    assert GenServer.whereis(:whereis_bad_server) == nil

    {:ok, pid} = GenServer.start_link(Stack, [], name: {:global, name})
    assert GenServer.whereis({:global, name}) == pid
    assert GenServer.whereis({:global, :whereis_bad_server}) == nil
    assert GenServer.whereis({:via, :global, name}) == pid
    assert GenServer.whereis({:via, :global, :whereis_bad_server}) == nil
  end

  test "stop/3", %{test: name} do
    {:ok, pid} = GenServer.start(Stack, [])
    assert GenServer.stop(pid, :normal) == :ok

    stopped_pid = pid

    assert catch_exit(GenServer.stop(stopped_pid)) ==
             {:noproc, {GenServer, :stop, [stopped_pid, :normal, :infinity]}}

    assert catch_exit(GenServer.stop(nil)) ==
             {:noproc, {GenServer, :stop, [nil, :normal, :infinity]}}

    {:ok, pid} = GenServer.start(Stack, [])

    assert GenServer.call(pid, :stop_self) ==
             {:calling_self, {GenServer, :stop, [pid, :normal, :infinity]}}

    {:ok, _} = GenServer.start(Stack, [], name: name)
    assert GenServer.stop(name, :normal) == :ok
  end
end
