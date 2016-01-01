Code.require_file "test_helper.exs", __DIR__

defmodule TaskTest do
  use ExUnit.Case

  setup do
    Logger.remove_backend(:console)
    on_exit fn -> Logger.add_backend(:console, flush: true) end
    :ok
  end

  def wait_and_send(caller, atom) do
    send caller, :ready
    receive do: (true -> true)
    send caller, atom
  end

  defp create_task_in_other_process do
    caller = self()
    spawn fn -> send caller, Task.async(fn -> nil end) end
    receive do: (task -> task)
  end

  defp create_dummy_task do
    %Task{ref: make_ref, pid: spawn(fn() -> :ok end), owner: self()}
  end

  test "async/1" do
    parent = self()
    fun = fn -> wait_and_send(parent, :done) end
    task = Task.async(fun)

    # Assert the struct
    assert task.__struct__ == Task
    assert is_pid task.pid
    assert is_reference task.ref

    # Assert the link
    {:links, links} = Process.info(self, :links)
    assert task.pid in links

    receive do: (:ready -> :ok)

    # Assert the initial call
    {:name, fun_name} = :erlang.fun_info(fun, :name)
    assert {__MODULE__, fun_name, 0} === :proc_lib.translate_initial_call(task.pid)

    # Run the task
    send task.pid, true

    # Assert response and monitoring messages
    ref = task.ref
    assert_receive {^ref, :done}
    assert_receive {:DOWN, ^ref, _, _, :normal}
  end

  test "async/3" do
    task = Task.async(__MODULE__, :wait_and_send, [self(), :done])
    assert task.__struct__ == Task

    {:links, links} = Process.info(self, :links)
    assert task.pid in links

    receive do: (:ready -> :ok)

    assert {__MODULE__, :wait_and_send, 2} === :proc_lib.translate_initial_call(task.pid)

    send(task.pid, true)

    assert Task.await(task) === :done
    assert_receive :done
  end

  test "start/1" do
    parent = self()
    fun = fn -> wait_and_send(parent, :done) end
    {:ok, pid} = Task.start(fun)

    {:links, links} = Process.info(self, :links)
    refute pid in links

    receive do: (:ready -> :ok)

    {:name, fun_name} = :erlang.fun_info(fun, :name)
    assert {__MODULE__, fun_name, 0} === :proc_lib.translate_initial_call(pid)

    send pid, true
    assert_receive :done
  end

  test "start/3" do
    {:ok, pid} = Task.start(__MODULE__, :wait_and_send, [self(), :done])

    {:links, links} = Process.info(self, :links)
    refute pid in links

    receive do: (:ready -> :ok)

    assert {__MODULE__, :wait_and_send, 2} === :proc_lib.translate_initial_call(pid)

    send pid, true
    assert_receive :done
  end

  test "start_link/1" do
    parent = self()
    fun = fn -> wait_and_send(parent, :done) end
    {:ok, pid} = Task.start_link(fun)

    {:links, links} = Process.info(self, :links)
    assert pid in links

    receive do: (:ready -> :ok)

    {:name, fun_name} = :erlang.fun_info(fun, :name)
    assert {__MODULE__, fun_name, 0} === :proc_lib.translate_initial_call(pid)

    send pid, true
    assert_receive :done
  end

  test "start_link/3" do
    {:ok, pid} = Task.start_link(__MODULE__, :wait_and_send, [self(), :done])

    {:links, links} = Process.info(self, :links)
    assert pid in links

    receive do: (:ready -> :ok)

    assert {__MODULE__, :wait_and_send, 2} === :proc_lib.translate_initial_call(pid)

    send pid, true
    assert_receive :done
  end

  test "await/2 exits on timeout" do
    task = %Task{ref: make_ref(), owner: self()}
    assert catch_exit(Task.await(task, 0)) == {:timeout, {Task, :await, [task, 0]}}
  end

  test "await/2 exits on normal exit" do
    task = Task.async(fn -> exit :normal end)
    assert catch_exit(Task.await(task)) == {:normal, {Task, :await, [task, 5000]}}
  end

  test "await/2 exits on task throw" do
    Process.flag(:trap_exit, true)
    task = Task.async(fn -> throw :unknown end)
    assert {{{:nocatch, :unknown}, _}, {Task, :await, [^task, 5000]}} =
           catch_exit(Task.await(task))
  end

  test "await/2 exits on task error" do
    Process.flag(:trap_exit, true)
    task = Task.async(fn -> raise "oops" end)
    assert {{%RuntimeError{}, _}, {Task, :await, [^task, 5000]}} =
           catch_exit(Task.await(task))
  end

  test "await/2 exits on task undef module error" do
    Process.flag(:trap_exit, true)
    task = Task.async(&:module_does_not_exist.undef/0)
    assert {{:undef, [{:module_does_not_exist, :undef, _, _} | _]},
            {Task, :await, [^task, 5000]}} =
           catch_exit(Task.await(task))
  end

  test "await/2 exits on task undef function error" do
    Process.flag(:trap_exit, true)
    task = Task.async(&TaskTest.undef/0)
    assert {{:undef, [{TaskTest, :undef, _, _} | _]},
            {Task, :await, [^task, 5000]}} =
           catch_exit(Task.await(task))
  end

  test "await/2 exits on task exit" do
    Process.flag(:trap_exit, true)
    task = Task.async(fn -> exit :unknown end)
    assert {:unknown, {Task, :await, [^task, 5000]}} =
           catch_exit(Task.await(task))
  end

  test "await/2 exits on :noconnection" do
    ref  = make_ref()
    task = %Task{ref: ref, pid: self(), owner: self()}
    send self(), {:DOWN, ref, :process, self(), :noconnection}
    assert catch_exit(Task.await(task)) |> elem(0) == {:nodedown, :nonode@nohost}
  end

  test "await/2 exits on :noconnection from named monitor" do
    ref  = make_ref()
    task = %Task{ref: ref, pid: nil, owner: self()}
    send self(), {:DOWN, ref, :process, {:name, :node}, :noconnection}
    assert catch_exit(Task.await(task)) |> elem(0) == {:nodedown, :node}
  end

  test "await/2 raises when invoked from a non-owner process" do
    task = create_task_in_other_process()
    message = "task #{inspect task} must be queried from the owner but was queried from #{inspect self()}"
    assert_raise ArgumentError, message, fn -> Task.await(task, 1) end
  end

  test "yield/2 returns {:ok, result} when reply and :DOWN in message queue" do
    task = %Task{ref: make_ref, owner: self()}
    send(self(), {task.ref, :result})
    send(self(), {:DOWN, task.ref, :process, self, :abnormal})
    assert Task.yield(task, 0) == {:ok, :result}
    refute_received {:DOWN, _, _, _, _}
  end

  test "yield/2 returns nil on timeout" do
    task = %Task{ref: make_ref(), owner: self()}
    assert Task.yield(task, 0) == nil
  end

  test "yield/2 return exit on normal exit" do
    task = Task.async(fn -> exit :normal end)
    assert Task.yield(task) == {:exit, :normal}
  end

  test "yield/2 exits on :noconnection" do
    ref  = make_ref()
    task = %Task{ref: ref, pid: self(), owner: self()}
    send self(), {:DOWN, ref, self(), self(), :noconnection}
    assert catch_exit(Task.yield(task)) |> elem(0) == {:nodedown, :nonode@nohost}
  end

  test "yield/2 raises when invoked from a non-owner process" do
    task = create_task_in_other_process()
    message = "task #{inspect task} must be queried from the owner but was queried from #{inspect self()}"
    assert_raise ArgumentError, message, fn -> Task.yield(task, 1) end
  end

  test "yield_many/2 returns {:ok, result} when reply and :DOWN in message queue" do
    task = %Task{ref: make_ref, owner: self()}
    send(self(), {task.ref, :result})
    send(self(), {:DOWN, task.ref, :process, self, :abnormal})
    assert Task.yield_many([task], 0) == [{task, {:ok, :result}}]
    refute_received {:DOWN, _, _, _, _}
  end

  test "yield_many/2 returns nil on timeout" do
    task = %Task{ref: make_ref(), owner: self()}
    assert Task.yield_many([task], 0) == [{task, nil}]
  end

  test "yield_many/2 return exit on normal exit" do
    task = Task.async(fn -> exit :normal end)
    assert Task.yield_many([task]) == [{task, {:exit, :normal}}]
  end

  test "yield_many/2 exits on :noconnection" do
    ref  = make_ref()
    task = %Task{ref: ref, pid: self(), owner: self()}
    send self(), {:DOWN, ref, self(), self(), :noconnection}
    assert catch_exit(Task.yield_many([task])) |> elem(0) == {:nodedown, :nonode@nohost}
  end

  test "yield_many/2 raises when invoked from a non-owner process" do
    task = create_task_in_other_process()
    message = "task #{inspect task} must be queried from the owner but was queried from #{inspect self()}"
    assert_raise ArgumentError, message, fn -> Task.yield_many([task], 1) end
  end

  test "yield_many/2 returns results from multiple tasks" do
    task1 = %Task{ref: make_ref(), owner: self()}
    task2 = %Task{ref: make_ref(), owner: self()}
    task3 = Task.async(fn -> exit :normal end)

    send(self(), {task1.ref, :result})
    ref = Process.monitor(task3.pid)
    assert_receive {:DOWN, ^ref, _, _, :normal}

    assert Task.yield_many([task1, task2, task3], 0) ==
           [{task1, {:ok, :result}}, {task2, nil}, {task3, {:exit, :normal}}]
  end

  test "shutdown/2 returns {:ok, result} when reply and abnormal :DOWN in message queue" do
    task = create_dummy_task()
    send(self(), {task.ref, :result})
    send(self(), {:DOWN, task.ref, :process, task.pid, :abnormal})
    assert Task.shutdown(task) == {:ok, :result}
    refute_received {:DOWN, _, _, _, _}
  end

  test "shutdown/2 returns {:ok, result} when reply and normal :DOWN in message queue" do
    task = create_dummy_task()
    send(self(), {task.ref, :result})
    send(self(), {:DOWN, task.ref, :process, task.pid, :normal})
    assert Task.shutdown(task) == {:ok, :result}
    refute_received {:DOWN, _, _, _, _}
  end

  test "shutdown/2 returns {:ok, result} when reply and shutdown :DOWN in message queue" do
    task = create_dummy_task()
    send(self(), {task.ref, :result})
    send(self(), {:DOWN, task.ref, :process, task.pid, :shutdown})
    assert Task.shutdown(task) == {:ok, :result}
    refute_received {:DOWN, _, _, _, _}
  end

  test "shutdown/2 returns nil on shutting down task" do
    task = Task.async(:timer, :sleep, [:infinity])
    assert Task.shutdown(task) == nil
  end

  test "shutdown/2 return exit on abnormal :DOWN in message queue" do
    task = create_dummy_task()
    send(self(), {:DOWN, task.ref, :process, task.pid, :abnormal})
    assert Task.shutdown(task) == {:exit, :abnormal}
  end

  test "shutdown/2 return exit on normal :DOWN in message queue" do
    task = create_dummy_task()
    send(self(), {:DOWN, task.ref, :process, task.pid, :normal})
    assert Task.shutdown(task) == {:exit, :normal}
  end

  test "shutdown/2 returns nil on shutdown :DOWN in message queue" do
    task = create_dummy_task()
    send(self(), {:DOWN, task.ref, :process, task.pid, :shutdown})
    assert Task.shutdown(task) == nil
  end

  test "shutdown/2 return exit on killed :DOWN in message queue" do
    task = create_dummy_task()
    send(self(), {:DOWN, task.ref, :process, task.pid, :killed})
    assert Task.shutdown(task) == {:exit, :killed}
  end

  test "shutdown/2 exits on noconnection :DOWN in message queue" do
    task = create_dummy_task()
    send(self(), {:DOWN, task.ref, :process, task.pid, :noconnection})
    assert catch_exit(Task.shutdown(task)) ==
      {{:nodedown, node()}, {Task, :shutdown, [task, 5000]}}
  end

  test "shutdown/2 raises if task pid is nil" do
    task = %Task{ref: make_ref, pid: nil}
    assert_raise ArgumentError, "task #{inspect task} does not have an associated task process",
      fn -> Task.shutdown(task) end
  end

  test "shutdown/2 raises when invoked from a non-owner process" do
    task = create_task_in_other_process()
    message = "task #{inspect task} must be queried from the owner but was queried from #{inspect self()}"
    assert_raise ArgumentError, message, fn -> Task.shutdown(task) end
  end

  test "shutdown/2 brutal_ kill returns {:ok, result} when reply and abnormal :DOWN in message queue" do
    task = create_dummy_task()
    send(self(), {task.ref, :result})
    send(self(), {:DOWN, task.ref, :process, task.pid, :abnormal})
    assert Task.shutdown(task, :brutal_kill) == {:ok, :result}
    refute_received {:DOWN, _, _, _, _}
  end

  test "shutdown/2 brutal kill returns {:ok, result} when reply and normal :DOWN in message queue" do
    task = create_dummy_task()
    send(self(), {task.ref, :result})
    send(self(), {:DOWN, task.ref, :process, task.pid, :normal})
    assert Task.shutdown(task, :brutal_kill) == {:ok, :result}
    refute_received {:DOWN, _, _, _, _}
  end

  test "shutdown/2 brutal kill returns {:ok, result} when reply and shutdown :DOWN in message queue" do
    task = create_dummy_task()
    send(self(), {task.ref, :result})
    send(self(), {:DOWN, task.ref, :process, task.pid, :shutdown})
    assert Task.shutdown(task, :brutal_kill) == {:ok, :result}
    refute_received {:DOWN, _, _, _, _}
  end

  test "shutdown/2 brutal kill returns exit on abnormal :DOWN in message queue" do
    task = create_dummy_task()
    send(self(), {:DOWN, task.ref, :process, task.pid, :abnormal})
    assert Task.shutdown(task, :brutal_kill) == {:exit, :abnormal}
  end

  test "shutdown/2 brutal kill returns exit on normal :DOWN in message queue" do
    task = create_dummy_task()
    send(self(), {:DOWN, task.ref, :process, task.pid, :normal})
    assert Task.shutdown(task, :brutal_kill) == {:exit, :normal}
  end

  test "shutdown/2 brutal kill returns exit on shutdown :DOWN in message queue" do
    task = create_dummy_task()
    send(self(), {:DOWN, task.ref, :process, task.pid, :shutdown})
    assert Task.shutdown(task, :brutal_kill) == {:exit, :shutdown}
  end

  test "shutdown/2 brutal kill exits on noconnection :DOWN in message queue" do
    task = create_dummy_task()
    send(self(), {:DOWN, task.ref, :process, task.pid, :noconnection})
    assert catch_exit(Task.shutdown(task, :brutal_kill)) ==
      {{:nodedown, node()}, {Task, :shutdown, [task, :brutal_kill]}}
  end

  test "shutdown/2 returns exit on killing task after shutdown timeout" do
    caller = self()

    task = Task.async(fn() ->
      Process.flag(:trap_exit, true)
      wait_and_send(caller, :ready)
      :timer.sleep(:infinity)
    end)

    receive do: (:ready -> :ok)
    assert Task.shutdown(task, 1) == {:exit, :killed}
  end

  test "shutdown/2 returns nil on killing task" do
    caller = self()

    task = Task.async(fn() ->
      Process.flag(:trap_exit, true)
      wait_and_send(caller, :ready)
      :timer.sleep(:infinity)
    end)

    receive do: (:ready -> :ok)

    assert Task.shutdown(task, :brutal_kill) == nil
    refute_received {:DOWN, _, _, _, _}
  end
end
