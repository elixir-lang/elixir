Code.require_file "test_helper.exs", __DIR__

defmodule SupervisorTest do
  use ExUnit.Case, async: true

  defmodule Stack do
    use GenServer

    def start_link(state) do
      GenServer.start_link(__MODULE__, state, [name: :sup_stack])
    end

    def handle_call(:pop, _from, [h|t]) do
      {:reply, h, t}
    end

    def handle_call(:stop, _from, stack) do
      try do
        # There is a race condition in between genserver terminations.
        # So we will explicitly unregister it here.
        Process.unregister(:sup_stack)
      catch
        _, _ -> :ok
      end
      {:stop, :normal, :ok, stack}
    end

    def handle_cast({:push, h}, _from, t) do
      {:noreply, [h|t]}
    end
  end

  defmodule Stack.Sup do
    use Supervisor

    def init(arg) do
      children = [worker(Stack, [arg])]
      supervise(children, strategy: :one_for_one)
    end
  end

  import Supervisor.Spec

  test "start_link/2" do
    children = [worker(Stack, [[:hello]])]
    {:ok, pid} = Supervisor.start_link(children, strategy: :one_for_one)

    wait_until_registered(:sup_stack)
    assert GenServer.call(:sup_stack, :pop) == :hello
    assert GenServer.call(:sup_stack, :stop) == :ok

    wait_until_registered(:sup_stack)
    assert GenServer.call(:sup_stack, :pop) == :hello

    Process.exit(pid, :normal)
  end

  test "start_link/3" do
    {:ok, pid} = Supervisor.start_link(Stack.Sup, [:hello], name: :stack_sup)
    wait_until_registered(:stack_sup)

    assert GenServer.call(:sup_stack, :pop) == :hello
    Process.exit(pid, :normal)
  end

  test "*_child functions" do
    {:ok, pid} = Supervisor.start_link([], strategy: :one_for_one)

    assert Supervisor.which_children(pid) == []
    assert Supervisor.count_children(pid) ==
           %{specs: 0, active: 0, supervisors: 0, workers: 0}

    {:ok, stack} = Supervisor.start_child(pid, worker(Stack, [[:hello]]))
    assert GenServer.call(stack, :pop) == :hello

    assert Supervisor.which_children(pid) ==
           [{SupervisorTest.Stack, stack, :worker, [SupervisorTest.Stack]}]
    assert Supervisor.count_children(pid) ==
           %{specs: 1, active: 1, supervisors: 0, workers: 1}

    assert Supervisor.delete_child(pid, Stack) == {:error, :running}
    assert Supervisor.terminate_child(pid, Stack) == :ok

    {:ok, stack} = Supervisor.restart_child(pid, Stack)
    assert GenServer.call(stack, :pop) == :hello

    assert Supervisor.terminate_child(pid, Stack) == :ok
    assert Supervisor.delete_child(pid, Stack) == :ok

    Process.exit(pid, :normal)
  end

  defp wait_until_registered(name) do
    unless Process.whereis(name) do
      wait_until_registered(name)
    end
  end
end
