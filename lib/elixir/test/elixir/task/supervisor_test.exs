Code.require_file "../test_helper.exs", __DIR__

defmodule Task.SupervisorTest do
  use ExUnit.Case, async: true

  setup do
    {:ok, pid} = Task.Supervisor.start_link()
    {:ok, supervisor: pid}
  end

  teardown config do
    Process.exit(config[:supervisor], :shutdown)
    :ok
  end

  def wait_and_send(caller, atom) do
    receive do: (true -> true)
    send caller, atom
  end

  test "async/1", config do
    task = Task.Supervisor.async config[:supervisor], fn ->
      receive do: (true -> true)
      :done
    end

    assert Task.Supervisor.children(config[:supervisor]) == [task.pid]

    # Assert the struct
    assert task.__struct__ == Task
    assert is_pid task.pid
    assert is_reference task.ref

    # Assert the link
    {:links, links} = Process.info(self, :links)
    assert task.pid in links

    # Run the task
    send task.pid, true

    # Assert response and monitoring messages
    ref = task.ref
    assert_receive {^ref, :done}
    assert_receive {:DOWN, ^ref, _, _, :normal}
  end

  test "async/3", config do
    task = Task.Supervisor.async(config[:supervisor], __MODULE__, :wait_and_send, [self(), :done])
    assert Task.Supervisor.children(config[:supervisor]) == [task.pid]

    send task.pid, true
    assert task.__struct__ == Task
    assert Task.await(task) == :done
  end

  test "start_child/1", config do
    parent = self()
    {:ok, pid} = Task.Supervisor.start_child(config[:supervisor], fn -> wait_and_send(parent, :done) end)
    assert Task.Supervisor.children(config[:supervisor]) == [pid]

    {:links, links} = Process.info(self, :links)
    refute pid in links

    send pid, true
    assert_receive :done
  end

  test "start_child/3", config do
    {:ok, pid} = Task.Supervisor.start_child(config[:supervisor], __MODULE__, :wait_and_send, [self(), :done])
    assert Task.Supervisor.children(config[:supervisor]) == [pid]

    {:links, links} = Process.info(self, :links)
    refute pid in links

    send pid, true
    assert_receive :done
  end

  test "terminate_child/2", config do
    {:ok, pid} = Task.Supervisor.start_child(config[:supervisor], __MODULE__, :wait_and_send, [self(), :done])
    assert Task.Supervisor.children(config[:supervisor]) == [pid]
    assert Task.Supervisor.terminate_child(config[:supervisor], pid) == :ok
    assert Task.Supervisor.children(config[:supervisor]) == []
    assert Task.Supervisor.terminate_child(config[:supervisor], pid) == :ok
  end

  test "await/1 exits on task throw", config do
    task = Task.Supervisor.async(config[:supervisor], fn -> throw :unknown end)
    assert {{{:nocatch, :unknown}, _}, {Task, :await, [^task, 5000]}} =
           catch_exit(Task.await(task))
  end

  test "await/1 exits on task error", config do
    task = Task.Supervisor.async(config[:supervisor], fn -> raise "oops" end)
    assert {{%RuntimeError{}, _}, {Task, :await, [^task, 5000]}} =
           catch_exit(Task.await(task))
  end

  test "await/1 exits on task exit", config do
    task = Task.Supervisor.async(config[:supervisor], fn -> exit :unknown end)
    assert {:unknown, {Task, :await, [^task, 5000]}} =
           catch_exit(Task.await(task))
  end
end
