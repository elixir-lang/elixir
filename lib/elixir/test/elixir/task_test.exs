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

  test "await/1 exits on task undef module error" do
    Process.flag(:trap_exit, true)
    task = Task.async(&:module_does_not_exist.undef/0)
    assert {{:undef, [{:module_does_not_exist, :undef, _, _} | _]},
            {Task, :await, [^task, 5000]}} =
           catch_exit(Task.await(task))
  end

  test "await/1 exits on task undef function error" do
    Process.flag(:trap_exit, true)
    task = Task.async(&TaskTest.undef/0)
    assert {{:undef, [{TaskTest, :undef, _, _} | _]},
            {Task, :await, [^task, 5000]}} =
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
