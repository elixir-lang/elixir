Code.require_file "test_helper.exs", __DIR__

defmodule TaskTest do
  use ExUnit.Case, async: true

  setup do
    :error_logger.tty(false)
    on_exit fn -> :error_logger.tty(true) end
    :ok
  end

  def wait_and_send(caller, atom) do
    receive do: (true -> true)
    send caller, atom
  end

  test "async/1" do
    task = Task.async fn ->
      receive do: (true -> true)
      :done
    end

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

  test "async/3" do
    task = Task.async(List, :flatten, [[1, [2], 3]])
    assert task.__struct__ == Task
    assert Task.await(task) == [1, 2, 3]
  end

  test "start_link/1" do
    parent = self()
    {:ok, pid} = Task.start_link(fn -> wait_and_send(parent, :done) end)

    {:links, links} = Process.info(self, :links)
    assert pid in links

    send pid, true
    assert_receive :done
  end

  test "start_link/3" do
    {:ok, pid} = Task.start_link(__MODULE__, :wait_and_send, [self(), :done])

    {:links, links} = Process.info(self, :links)
    assert pid in links

    send pid, true
    assert_receive :done
  end

  test "await/1 exits on timeout" do
    task = %Task{ref: make_ref()}
    assert catch_exit(Task.await(task, 0)) == {:timeout, {Task, :await, [task, 0]}}
  end

  test "await/1 exits on normal exit" do
    task = Task.async(fn -> exit :normal end)
    assert catch_exit(Task.await(task)) == {:normal, {Task, :await, [task, 5000]}}
  end

  test "await/1 exits on task throw" do
    Process.flag(:trap_exit, true)
    task = Task.async(fn -> throw :unknown end)
    assert {{{:nocatch, :unknown}, _}, {Task, :await, [^task, 5000]}} =
           catch_exit(Task.await(task))
  end

  test "await/1 exits on task error" do
    Process.flag(:trap_exit, true)
    task = Task.async(fn -> raise "oops" end)
    assert {{%RuntimeError{}, _}, {Task, :await, [^task, 5000]}} =
           catch_exit(Task.await(task))
  end

  test "await/1 exits on task exit" do
    Process.flag(:trap_exit, true)
    task = Task.async(fn -> exit :unknown end)
    assert {:unknown, {Task, :await, [^task, 5000]}} =
           catch_exit(Task.await(task))
  end

  test "await/1 exits on :noconnection" do
    ref  = make_ref()
    task = %Task{ref: ref, pid: self()}
    send self(), {:DOWN, ref, self(), self(), :noconnection}
    assert catch_exit(Task.await(task)) |> elem(0) == {:nodedown, :nonode@nohost}
  end

  test "find/2" do
    task = %Task{ref: make_ref}
    assert Task.find([task], {make_ref, :ok}) == nil
    assert Task.find([task], {task.ref, :ok}) == {:ok, task}

    assert Task.find([task], {:DOWN, make_ref, :process, self, :kill}) == nil
    msg = {:DOWN, task.ref, :process, self, :kill}
    assert catch_exit(Task.find([task], msg)) ==
           {:kill, {Task, :find, [[task], msg]}}
  end
end
