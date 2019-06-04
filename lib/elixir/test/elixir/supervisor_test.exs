Code.require_file("test_helper.exs", __DIR__)

defmodule SupervisorTest do
  use ExUnit.Case, async: true

  defmodule Stack do
    use GenServer

    def start_link({state, opts}) do
      GenServer.start_link(__MODULE__, state, opts)
    end

    def init(args) do
      {:ok, args}
    end

    def handle_call(:pop, _from, [h | t]) do
      {:reply, h, t}
    end

    def handle_call(:stop, _from, stack) do
      # There is a race condition between genserver terminations.
      # So we will explicitly unregister it here.
      try do
        self() |> Process.info(:registered_name) |> elem(1) |> Process.unregister()
      rescue
        _ -> :ok
      end

      {:stop, :normal, :ok, stack}
    end

    def handle_cast({:push, h}, t) do
      {:noreply, [h | t]}
    end
  end

  defmodule Stack.Sup do
    use Supervisor

    def init(pair) do
      Supervisor.init([{Stack, pair}], strategy: :one_for_one)
    end
  end

  test "generates child_spec/1" do
    assert Stack.Sup.child_spec([:hello]) == %{
             id: Stack.Sup,
             start: {Stack.Sup, :start_link, [[:hello]]},
             type: :supervisor
           }

    defmodule CustomSup do
      use Supervisor,
        id: :id,
        restart: :temporary,
        start: {:foo, :bar, []}

      def init(arg) do
        arg
      end
    end

    assert CustomSup.child_spec([:hello]) == %{
             id: :id,
             restart: :temporary,
             start: {:foo, :bar, []},
             type: :supervisor
           }
  end

  test "child_spec/2" do
    assert Supervisor.child_spec(Task, []) ==
             %{id: Task, restart: :temporary, start: {Task, :start_link, [[]]}}

    assert Supervisor.child_spec({Task, :foo}, []) ==
             %{id: Task, restart: :temporary, start: {Task, :start_link, [:foo]}}

    assert Supervisor.child_spec(%{id: Task}, []) == %{id: Task}

    assert Supervisor.child_spec(
             Task,
             id: :foo,
             start: {:foo, :bar, []},
             restart: :permanent,
             shutdown: :infinity
           ) == %{id: :foo, start: {:foo, :bar, []}, restart: :permanent, shutdown: :infinity}

    message = ~r"The module SupervisorTest was given as a child.*\nbut it does not implement"m

    assert_raise ArgumentError, message, fn ->
      Supervisor.child_spec(SupervisorTest, [])
    end

    message = ~r"The module Unknown was given as a child.*but it does not exist"m

    assert_raise ArgumentError, message, fn ->
      Supervisor.child_spec(Unknown, [])
    end

    message = ~r"supervisors expect each child to be one of"

    assert_raise ArgumentError, message, fn ->
      Supervisor.child_spec("other", [])
    end
  end

  test "init/2" do
    flags = %{intensity: 3, period: 5, strategy: :one_for_one}
    children = [%{id: Task, restart: :temporary, start: {Task, :start_link, [[]]}}]
    assert Supervisor.init([Task], strategy: :one_for_one) == {:ok, {flags, children}}

    flags = %{intensity: 1, period: 2, strategy: :one_for_all}
    children = [%{id: Task, restart: :temporary, start: {Task, :start_link, [:foo]}}]

    assert Supervisor.init(
             [{Task, :foo}],
             strategy: :one_for_all,
             max_restarts: 1,
             max_seconds: 2
           ) == {:ok, {flags, children}}

    assert_raise ArgumentError, "expected :strategy option to be given", fn ->
      Supervisor.init([], [])
    end
  end

  test "init/2 with old and new child specs" do
    flags = %{intensity: 3, period: 5, strategy: :one_for_one}

    children = [
      %{id: Task, restart: :temporary, start: {Task, :start_link, [[]]}},
      old_spec = {Task, {Task, :start_link, []}, :permanent, 5000, :worker, [Task]}
    ]

    assert Supervisor.init([Task, old_spec], strategy: :one_for_one) ==
             {:ok, {flags, children}}
  end

  test "start_link/2 with via" do
    Supervisor.start_link([], strategy: :one_for_one, name: {:via, :global, :via_sup})
    assert Supervisor.which_children({:via, :global, :via_sup}) == []
  end

  test "start_link/3 with global" do
    Supervisor.start_link([], strategy: :one_for_one, name: {:global, :global_sup})
    assert Supervisor.which_children({:global, :global_sup}) == []
  end

  test "start_link/3 with local" do
    Supervisor.start_link([], strategy: :one_for_one, name: :my_sup)
    assert Supervisor.which_children(:my_sup) == []
  end

  test "start_link/2" do
    children = [{Stack, {[:hello], [name: :dyn_stack]}}]
    {:ok, pid} = Supervisor.start_link(children, strategy: :one_for_one)

    wait_until_registered(:dyn_stack)
    assert GenServer.call(:dyn_stack, :pop) == :hello
    assert GenServer.call(:dyn_stack, :stop) == :ok

    wait_until_registered(:dyn_stack)
    assert GenServer.call(:dyn_stack, :pop) == :hello
    Supervisor.stop(pid)

    assert_raise ArgumentError, ~r"expected :name option to be one of the following:", fn ->
      name = "my_gen_server_name"
      Supervisor.start_link(children, name: name, strategy: :one_for_one)
    end

    assert_raise ArgumentError, ~r"expected :name option to be one of the following:", fn ->
      name = {:invalid_tuple, "my_gen_server_name"}
      Supervisor.start_link(children, name: name, strategy: :one_for_one)
    end

    assert_raise ArgumentError, ~r"expected :name option to be one of the following:", fn ->
      name = {:via, "Via", "my_gen_server_name"}
      Supervisor.start_link(children, name: name, strategy: :one_for_one)
    end
  end

  test "start_link/2 with old and new specs" do
    children = [
      {Stack, {[:hello], []}},
      {:old_stack, {SupervisorTest.Stack, :start_link, [{[:hello], []}]}, :permanent, 5000,
       :worker, [SupervisorTest.Stack]}
    ]

    {:ok, _} = Supervisor.start_link(children, strategy: :one_for_one)
  end

  test "start_link/3" do
    {:ok, pid} = Supervisor.start_link(Stack.Sup, {[:hello], [name: :stat_stack]})
    wait_until_registered(:stat_stack)
    assert GenServer.call(:stat_stack, :pop) == :hello
    Supervisor.stop(pid)
  end

  describe "start_child/2" do
    test "supports old child spec" do
      {:ok, pid} = Supervisor.start_link([], strategy: :one_for_one)
      child = {Task, {Task, :start_link, [fn -> :ok end]}, :temporary, 5000, :worker, [Task]}
      assert {:ok, pid} = Supervisor.start_child(pid, child)
      assert is_pid(pid)
    end

    test "supports new child spec as tuple" do
      {:ok, pid} = Supervisor.start_link([], strategy: :one_for_one)
      child = %{id: Task, restart: :temporary, start: {Task, :start_link, [fn -> :ok end]}}
      assert {:ok, pid} = Supervisor.start_child(pid, child)
      assert is_pid(pid)
    end

    test "supports new child spec" do
      {:ok, pid} = Supervisor.start_link([], strategy: :one_for_one)
      child = {Task, fn -> :timer.sleep(:infinity) end}
      assert {:ok, pid} = Supervisor.start_child(pid, child)
      assert is_pid(pid)
    end

    test "with invalid child spec" do
      {:ok, pid} = Supervisor.start_link([], strategy: :one_for_one)

      assert Supervisor.start_child(pid, %{}) == {:error, :missing_id}
      assert Supervisor.start_child(pid, {1, 2, 3, 4, 5, 6}) == {:error, {:invalid_mfa, 2}}

      assert Supervisor.start_child(pid, %{id: 1, start: {Task, :foo, :bar}}) ==
               {:error, {:invalid_mfa, {Task, :foo, :bar}}}
    end
  end

  test "child life-cycle" do
    {:ok, pid} = Supervisor.start_link([], strategy: :one_for_one)

    assert Supervisor.which_children(pid) == []
    assert Supervisor.count_children(pid) == %{specs: 0, active: 0, supervisors: 0, workers: 0}

    child_spec = Supervisor.child_spec({Stack, {[:hello], []}}, [])
    {:ok, stack} = Supervisor.start_child(pid, child_spec)
    assert GenServer.call(stack, :pop) == :hello

    assert Supervisor.which_children(pid) ==
             [{SupervisorTest.Stack, stack, :worker, [SupervisorTest.Stack]}]

    assert Supervisor.count_children(pid) == %{specs: 1, active: 1, supervisors: 0, workers: 1}

    assert Supervisor.delete_child(pid, Stack) == {:error, :running}
    assert Supervisor.terminate_child(pid, Stack) == :ok

    {:ok, stack} = Supervisor.restart_child(pid, Stack)
    assert GenServer.call(stack, :pop) == :hello

    assert Supervisor.terminate_child(pid, Stack) == :ok
    assert Supervisor.delete_child(pid, Stack) == :ok
    Supervisor.stop(pid)
  end

  defp wait_until_registered(name) do
    unless Process.whereis(name) do
      wait_until_registered(name)
    end
  end
end
