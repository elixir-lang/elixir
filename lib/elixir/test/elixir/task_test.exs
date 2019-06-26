Code.require_file("test_helper.exs", __DIR__)

defmodule TaskTest do
  use ExUnit.Case
  doctest Task
  @moduletag :capture_log

  def wait_and_send(caller, atom) do
    send(caller, :ready)
    receive do: (true -> true)
    send(caller, atom)
  end

  defp create_task_in_other_process do
    caller = self()
    spawn(fn -> send(caller, Task.async(fn -> nil end)) end)
    receive do: (task -> task)
  end

  defp create_dummy_task(reason) do
    {pid, ref} = spawn_monitor(Kernel, :exit, [reason])

    receive do
      {:DOWN, ^ref, _, _, _} ->
        %Task{ref: ref, pid: pid, owner: self()}
    end
  end

  def sleep(number) do
    Process.sleep(number)
    number
  end

  test "can be supervised directly" do
    assert {:ok, _} = Supervisor.start_link([{Task, fn -> :ok end}], strategy: :one_for_one)
  end

  @compile {:no_warn_undefined, [TaskTest.MyTask, TaskTest.CustomTask]}

  test "generates child_spec/1" do
    defmodule MyTask do
      use Task
    end

    defmodule CustomTask do
      use Task, id: :id, restart: :permanent, shutdown: :infinity, start: {:foo, :bar, []}
    end

    assert MyTask.child_spec([:hello]) == %{
             id: MyTask,
             restart: :temporary,
             start: {MyTask, :start_link, [[:hello]]}
           }

    assert CustomTask.child_spec([:hello]) == %{
             id: :id,
             restart: :permanent,
             shutdown: :infinity,
             start: {:foo, :bar, []}
           }
  end

  test "async/1" do
    parent = self()
    fun = fn -> wait_and_send(parent, :done) end
    task = Task.async(fun)

    # Assert the struct
    assert task.__struct__ == Task
    assert is_pid(task.pid)
    assert is_reference(task.ref)

    # Assert the link
    {:links, links} = Process.info(self(), :links)
    assert task.pid in links

    receive do: (:ready -> :ok)

    # Assert the initial call
    {:name, fun_name} = Function.info(fun, :name)
    assert {__MODULE__, fun_name, 0} === :proc_lib.translate_initial_call(task.pid)

    # Run the task
    send(task.pid, true)

    # Assert response and monitoring messages
    ref = task.ref
    assert_receive {^ref, :done}
    assert_receive {:DOWN, ^ref, _, _, :normal}
  end

  test "async/3" do
    task = Task.async(__MODULE__, :wait_and_send, [self(), :done])
    assert task.__struct__ == Task

    {:links, links} = Process.info(self(), :links)
    assert task.pid in links

    receive do: (:ready -> :ok)
    assert {__MODULE__, :wait_and_send, 2} === :proc_lib.translate_initial_call(task.pid)

    send(task.pid, true)
    assert Task.await(task) === :done
    assert_receive :done
  end

  test "async with $callers" do
    grandparent = self()

    Task.async(fn ->
      parent = self()
      assert Process.get(:"$callers") == [grandparent]

      Task.async(fn ->
        assert Process.get(:"$callers") == [parent, grandparent]
      end)
      |> Task.await()
    end)
    |> Task.await()
  end

  test "start/1" do
    parent = self()
    fun = fn -> wait_and_send(parent, :done) end
    {:ok, pid} = Task.start(fun)

    {:links, links} = Process.info(self(), :links)
    refute pid in links

    receive do: (:ready -> :ok)

    {:name, fun_name} = Function.info(fun, :name)
    assert {__MODULE__, fun_name, 0} === :proc_lib.translate_initial_call(pid)

    send(pid, true)
    assert_receive :done
  end

  test "start/3" do
    {:ok, pid} = Task.start(__MODULE__, :wait_and_send, [self(), :done])

    {:links, links} = Process.info(self(), :links)
    refute pid in links

    receive do: (:ready -> :ok)

    assert {__MODULE__, :wait_and_send, 2} === :proc_lib.translate_initial_call(pid)

    send(pid, true)
    assert_receive :done
  end

  test "start_link/1" do
    parent = self()
    fun = fn -> wait_and_send(parent, :done) end
    {:ok, pid} = Task.start_link(fun)

    {:links, links} = Process.info(self(), :links)
    assert pid in links

    receive do: (:ready -> :ok)

    {:name, fun_name} = Function.info(fun, :name)
    assert {__MODULE__, fun_name, 0} === :proc_lib.translate_initial_call(pid)

    send(pid, true)
    assert_receive :done
  end

  test "start_link/3" do
    {:ok, pid} = Task.start_link(__MODULE__, :wait_and_send, [self(), :done])

    {:links, links} = Process.info(self(), :links)
    assert pid in links

    receive do: (:ready -> :ok)

    assert {__MODULE__, :wait_and_send, 2} === :proc_lib.translate_initial_call(pid)

    send(pid, true)
    assert_receive :done
  end

  test "start_link with $callers" do
    grandparent = self()

    Task.start_link(fn ->
      parent = self()
      assert Process.get(:"$callers") == [grandparent]

      Task.start_link(fn ->
        assert Process.get(:"$callers") == [parent, grandparent]
        send(grandparent, :done)
      end)
    end)

    assert_receive :done
  end

  describe "await/2" do
    test "exits on timeout" do
      task = %Task{ref: make_ref(), owner: self(), pid: nil}
      assert catch_exit(Task.await(task, 0)) == {:timeout, {Task, :await, [task, 0]}}
    end

    test "exits on normal exit" do
      task = Task.async(fn -> exit(:normal) end)
      assert catch_exit(Task.await(task)) == {:normal, {Task, :await, [task, 5000]}}
    end

    test "exits on task throw" do
      Process.flag(:trap_exit, true)
      task = Task.async(fn -> throw(:unknown) end)

      assert {{{:nocatch, :unknown}, _}, {Task, :await, [^task, 5000]}} =
               catch_exit(Task.await(task))
    end

    test "exits on task error" do
      Process.flag(:trap_exit, true)
      task = Task.async(fn -> raise "oops" end)
      assert {{%RuntimeError{}, _}, {Task, :await, [^task, 5000]}} = catch_exit(Task.await(task))
    end

    test "exits on task undef module error" do
      Process.flag(:trap_exit, true)
      task = Task.async(&:module_does_not_exist.undef/0)

      assert {exit_status, mfa} = catch_exit(Task.await(task))
      assert {:undef, [{:module_does_not_exist, :undef, _, _} | _]} = exit_status
      assert {Task, :await, [^task, 5000]} = mfa
    end

    test "exits on task undef function error" do
      Process.flag(:trap_exit, true)
      task = Task.async(&TaskTest.undef/0)

      assert {{:undef, [{TaskTest, :undef, _, _} | _]}, {Task, :await, [^task, 5000]}} =
               catch_exit(Task.await(task))
    end

    test "exits on task exit" do
      Process.flag(:trap_exit, true)
      task = Task.async(fn -> exit(:unknown) end)
      assert {:unknown, {Task, :await, [^task, 5000]}} = catch_exit(Task.await(task))
    end

    test "exits on :noconnection" do
      ref = make_ref()
      task = %Task{ref: ref, pid: self(), owner: self()}
      send(self(), {:DOWN, ref, :process, self(), :noconnection})
      assert catch_exit(Task.await(task)) |> elem(0) == {:nodedown, :nonode@nohost}
    end

    test "exits on :noconnection from named monitor" do
      ref = make_ref()
      task = %Task{ref: ref, owner: self(), pid: nil}
      send(self(), {:DOWN, ref, :process, {:name, :node}, :noconnection})
      assert catch_exit(Task.await(task)) |> elem(0) == {:nodedown, :node}
    end

    test "raises when invoked from a non-owner process" do
      task = create_task_in_other_process()

      message =
        "task #{inspect(task)} must be queried from the owner " <>
          "but was queried from #{inspect(self())}"

      assert_raise ArgumentError, message, fn -> Task.await(task, 1) end
    end
  end

  describe "yield/2" do
    test "returns {:ok, result} when reply and :DOWN in message queue" do
      task = %Task{ref: make_ref(), owner: self(), pid: nil}
      send(self(), {task.ref, :result})
      send(self(), {:DOWN, task.ref, :process, self(), :abnormal})
      assert Task.yield(task, 0) == {:ok, :result}
      refute_received {:DOWN, _, _, _, _}
    end

    test "returns nil on timeout" do
      task = %Task{ref: make_ref(), pid: nil, owner: self()}
      assert Task.yield(task, 0) == nil
    end

    test "return exit on normal exit" do
      task = Task.async(fn -> exit(:normal) end)
      assert Task.yield(task) == {:exit, :normal}
    end

    test "exits on :noconnection" do
      ref = make_ref()
      task = %Task{ref: ref, pid: self(), owner: self()}
      send(self(), {:DOWN, ref, self(), self(), :noconnection})
      assert catch_exit(Task.yield(task)) |> elem(0) == {:nodedown, :nonode@nohost}
    end

    test "raises when invoked from a non-owner process" do
      task = create_task_in_other_process()

      message =
        "task #{inspect(task)} must be queried from the owner " <>
          "but was queried from #{inspect(self())}"

      assert_raise ArgumentError, message, fn -> Task.yield(task, 1) end
    end
  end

  describe "yield_many/2" do
    test "returns {:ok, result} when reply and :DOWN in message queue" do
      task = %Task{ref: make_ref(), owner: self(), pid: nil}
      send(self(), {task.ref, :result})
      send(self(), {:DOWN, task.ref, :process, self(), :abnormal})
      assert Task.yield_many([task], 0) == [{task, {:ok, :result}}]
      refute_received {:DOWN, _, _, _, _}
    end

    test "returns nil on timeout" do
      task = %Task{ref: make_ref(), owner: self(), pid: nil}
      assert Task.yield_many([task], 0) == [{task, nil}]
    end

    test "return exit on normal exit" do
      task = Task.async(fn -> exit(:normal) end)
      assert Task.yield_many([task]) == [{task, {:exit, :normal}}]
    end

    test "exits on :noconnection" do
      ref = make_ref()
      task = %Task{ref: ref, pid: self(), owner: self()}
      send(self(), {:DOWN, ref, :process, self(), :noconnection})
      assert catch_exit(Task.yield_many([task])) |> elem(0) == {:nodedown, :nonode@nohost}
    end

    test "raises when invoked from a non-owner process" do
      task = create_task_in_other_process()

      message =
        "task #{inspect(task)} must be queried from the owner " <>
          "but was queried from #{inspect(self())}"

      assert_raise ArgumentError, message, fn -> Task.yield_many([task], 1) end
    end

    test "returns results from multiple tasks" do
      task1 = %Task{ref: make_ref(), owner: self(), pid: nil}
      task2 = %Task{ref: make_ref(), owner: self(), pid: nil}
      task3 = %Task{ref: make_ref(), owner: self(), pid: nil}

      send(self(), {task1.ref, :result})
      send(self(), {:DOWN, task3.ref, :process, self(), :normal})

      assert Task.yield_many([task1, task2, task3], 0) ==
               [{task1, {:ok, :result}}, {task2, nil}, {task3, {:exit, :normal}}]
    end

    test "returns results on infinity timeout" do
      task1 = %Task{ref: make_ref(), owner: self(), pid: nil}
      task2 = %Task{ref: make_ref(), owner: self(), pid: nil}
      task3 = %Task{ref: make_ref(), owner: self(), pid: nil}

      send(self(), {task1.ref, :result})
      send(self(), {task2.ref, :result})
      send(self(), {:DOWN, task3.ref, :process, self(), :normal})

      assert Task.yield_many([task1, task2, task3], :infinity) ==
               [{task1, {:ok, :result}}, {task2, {:ok, :result}}, {task3, {:exit, :normal}}]
    end
  end

  describe "shutdown/2" do
    test "returns {:ok, result} when reply and abnormal :DOWN in message queue" do
      task = create_dummy_task(:abnormal)
      send(self(), {task.ref, :result})
      send(self(), {:DOWN, task.ref, :process, task.pid, :abnormal})
      assert Task.shutdown(task) == {:ok, :result}
      refute_received {:DOWN, _, _, _, _}
    end

    test "returns {:ok, result} when reply and normal :DOWN in message queue" do
      task = create_dummy_task(:normal)
      send(self(), {task.ref, :result})
      send(self(), {:DOWN, task.ref, :process, task.pid, :normal})
      assert Task.shutdown(task) == {:ok, :result}
      refute_received {:DOWN, _, _, _, _}
    end

    test "returns {:ok, result} when reply and shut down :DOWN in message queue" do
      task = create_dummy_task(:shutdown)
      send(self(), {task.ref, :result})
      send(self(), {:DOWN, task.ref, :process, task.pid, :shutdown})
      assert Task.shutdown(task) == {:ok, :result}
      refute_received {:DOWN, _, _, _, _}
    end

    test "returns nil on shutting down task" do
      task = Task.async(:timer, :sleep, [:infinity])
      assert Task.shutdown(task) == nil
    end

    test "returns exit on abnormal :DOWN in message queue" do
      task = create_dummy_task(:abnormal)
      send(self(), {:DOWN, task.ref, :process, task.pid, :abnormal})
      assert Task.shutdown(task) == {:exit, :abnormal}
    end

    test "returns exit on normal :DOWN in message queue" do
      task = create_dummy_task(:normal)
      send(self(), {:DOWN, task.ref, :process, task.pid, :normal})
      assert Task.shutdown(task) == {:exit, :normal}
    end

    test "returns nil on shutdown :DOWN in message queue" do
      task = create_dummy_task(:shutdown)
      send(self(), {:DOWN, task.ref, :process, task.pid, :shutdown})
      assert Task.shutdown(task) == nil
    end

    test "returns exit on killed :DOWN in message queue" do
      task = create_dummy_task(:killed)
      send(self(), {:DOWN, task.ref, :process, task.pid, :killed})
      assert Task.shutdown(task) == {:exit, :killed}
    end

    test "exits on noconnection :DOWN in message queue" do
      task = create_dummy_task(:noconnection)
      send(self(), {:DOWN, task.ref, :process, task.pid, :noconnection})

      assert catch_exit(Task.shutdown(task)) ==
               {{:nodedown, node()}, {Task, :shutdown, [task, 5000]}}
    end

    test "raises if task PID is nil" do
      task = %Task{ref: make_ref(), owner: nil, pid: nil}
      message = "task #{inspect(task)} does not have an associated task process"
      assert_raise ArgumentError, message, fn -> Task.shutdown(task) end
    end

    test "raises when invoked from a non-owner process" do
      task = create_task_in_other_process()

      message =
        "task #{inspect(task)} must be queried from the owner " <>
          "but was queried from #{inspect(self())}"

      assert_raise ArgumentError, message, fn -> Task.shutdown(task) end
    end

    test "returns nil on killing task" do
      caller = self()

      task =
        Task.async(fn ->
          Process.flag(:trap_exit, true)
          wait_and_send(caller, :ready)
          Process.sleep(:infinity)
        end)

      receive do: (:ready -> :ok)

      assert Task.shutdown(task, :brutal_kill) == nil
      refute_received {:DOWN, _, _, _, _}
    end

    test "returns {:exit, :noproc} if task handled" do
      task = create_dummy_task(:noproc)
      assert Task.shutdown(task) == {:exit, :noproc}
    end
  end

  describe "shutdown/2 with :brutal_kill" do
    test "returns {:ok, result} when reply and abnormal :DOWN in message queue" do
      task = create_dummy_task(:abnormal)
      send(self(), {task.ref, :result})
      send(self(), {:DOWN, task.ref, :process, task.pid, :abnormal})
      assert Task.shutdown(task, :brutal_kill) == {:ok, :result}
      refute_received {:DOWN, _, _, _, _}
    end

    test "returns {:ok, result} when reply and normal :DOWN in message queue" do
      task = create_dummy_task(:normal)
      send(self(), {task.ref, :result})
      send(self(), {:DOWN, task.ref, :process, task.pid, :normal})
      assert Task.shutdown(task, :brutal_kill) == {:ok, :result}
      refute_received {:DOWN, _, _, _, _}
    end

    test "returns {:ok, result} when reply and shut down :DOWN in message queue" do
      task = create_dummy_task(:shutdown)
      send(self(), {task.ref, :result})
      send(self(), {:DOWN, task.ref, :process, task.pid, :shutdown})
      assert Task.shutdown(task, :brutal_kill) == {:ok, :result}
      refute_received {:DOWN, _, _, _, _}
    end

    test "returns nil on killed :DOWN in message queue" do
      task = create_dummy_task(:killed)
      send(self(), {:DOWN, task.ref, :process, task.pid, :killed})
      assert Task.shutdown(task, :brutal_kill) == nil
    end

    test "returns exit on abnormal :DOWN in message queue" do
      task = create_dummy_task(:abnormal)
      send(self(), {:DOWN, task.ref, :process, task.pid, :abnormal})
      assert Task.shutdown(task, :brutal_kill) == {:exit, :abnormal}
    end

    test "returns exit on normal :DOWN in message queue" do
      task = create_dummy_task(:normal)
      send(self(), {:DOWN, task.ref, :process, task.pid, :normal})
      assert Task.shutdown(task, :brutal_kill) == {:exit, :normal}
    end

    test "returns exit on shutdown :DOWN in message queue" do
      task = create_dummy_task(:shutdown)
      send(self(), {:DOWN, task.ref, :process, task.pid, :shutdown})
      assert Task.shutdown(task, :brutal_kill) == {:exit, :shutdown}
    end

    test "exits on noconnection :DOWN in message queue" do
      task = create_dummy_task(:noconnection)
      send(self(), {:DOWN, task.ref, :process, task.pid, :noconnection})

      assert catch_exit(Task.shutdown(task, :brutal_kill)) ==
               {{:nodedown, node()}, {Task, :shutdown, [task, :brutal_kill]}}
    end

    test "returns exit on killing task after shutdown timeout" do
      caller = self()

      task =
        Task.async(fn ->
          Process.flag(:trap_exit, true)
          wait_and_send(caller, :ready)
          Process.sleep(:infinity)
        end)

      receive do: (:ready -> :ok)
      assert Task.shutdown(task, 1) == {:exit, :killed}
    end

    test "returns {:exit, :noproc} if task handled" do
      task = create_dummy_task(:noproc)
      assert Task.shutdown(task, :brutal_kill) == {:exit, :noproc}
    end
  end

  describe "async_stream/2" do
    test "timeout" do
      assert catch_exit([:infinity] |> Task.async_stream(&sleep/1, timeout: 0) |> Enum.to_list()) ==
               {:timeout, {Task.Supervised, :stream, [0]}}

      refute_received _
    end

    test "streams an enumerable with ordered: false" do
      opts = [max_concurrency: 1, ordered: false]

      assert 4..1
             |> Task.async_stream(&sleep(&1 * 100), opts)
             |> Enum.to_list() == [ok: 400, ok: 300, ok: 200, ok: 100]

      opts = [max_concurrency: 4, ordered: false]

      assert 4..1
             |> Task.async_stream(&sleep(&1 * 100), opts)
             |> Enum.to_list() == [ok: 100, ok: 200, ok: 300, ok: 400]
    end

    test "streams an enumerable with ordered: false, on_timeout: :kill_task" do
      opts = [max_concurrency: 4, ordered: false, on_timeout: :kill_task, timeout: 50]

      assert [100, 1, 100, 1]
             |> Task.async_stream(&sleep/1, opts)
             |> Enum.to_list() == [ok: 1, ok: 1, exit: :timeout, exit: :timeout]

      refute_received _
    end

    test "streams an enumerable with infinite timeout" do
      [ok: :ok] = Task.async_stream([1], fn _ -> :ok end, timeout: :infinity) |> Enum.to_list()
    end

    test "with $callers" do
      grandparent = self()

      Task.async_stream([1], fn 1 ->
        parent = self()
        assert Process.get(:"$callers") == [grandparent]

        Task.async_stream([1], fn 1 ->
          assert Process.get(:"$callers") == [parent, grandparent]
          send(grandparent, :done)
        end)
        |> Stream.run()
      end)
      |> Stream.run()

      assert_receive :done
    end
  end

  for {desc, concurrency} <- [==: 4, <: 2, >: 8] do
    describe "async_stream with max_concurrency #{desc} tasks" do
      @opts [max_concurrency: concurrency]

      test "streams an enumerable with fun" do
        assert 1..4
               |> Task.async_stream(&sleep/1, @opts)
               |> Enum.to_list() == [ok: 1, ok: 2, ok: 3, ok: 4]
      end

      test "streams an enumerable with mfa" do
        assert 1..4
               |> Task.async_stream(__MODULE__, :sleep, [], @opts)
               |> Enum.to_list() == [ok: 1, ok: 2, ok: 3, ok: 4]
      end

      test "streams an enumerable without leaking tasks" do
        assert 1..4
               |> Task.async_stream(&sleep/1, @opts)
               |> Enum.to_list() == [ok: 1, ok: 2, ok: 3, ok: 4]

        refute_received _
      end

      test "streams an enumerable with slowest first" do
        Process.flag(:trap_exit, true)

        assert 4..1
               |> Task.async_stream(&sleep/1, @opts)
               |> Enum.to_list() == [ok: 4, ok: 3, ok: 2, ok: 1]
      end

      test "streams an enumerable with exits" do
        Process.flag(:trap_exit, true)

        assert 1..4
               |> Task.async_stream(&exit/1, @opts)
               |> Enum.to_list() == [exit: 1, exit: 2, exit: 3, exit: 4]

        refute_received {:EXIT, _, _}
      end

      test "shuts down unused tasks" do
        assert [0, :infinity, :infinity, :infinity]
               |> Task.async_stream(&sleep/1, @opts)
               |> Enum.take(1) == [ok: 0]

        assert Process.info(self(), :links) == {:links, []}
      end

      test "shuts down unused tasks without leaking messages" do
        assert [0, :infinity, :infinity, :infinity]
               |> Task.async_stream(&sleep/1, @opts)
               |> Enum.take(1) == [ok: 0]

        refute_received _
      end

      test "is zippable on success" do
        task = 1..4 |> Task.async_stream(&sleep/1, @opts) |> Stream.map(&elem(&1, 1))
        assert Enum.zip(task, task) == [{1, 1}, {2, 2}, {3, 3}, {4, 4}]
      end

      test "is zippable on failure" do
        Process.flag(:trap_exit, true)
        task = 1..4 |> Task.async_stream(&exit/1, @opts) |> Stream.map(&elem(&1, 1))
        assert Enum.zip(task, task) == [{1, 1}, {2, 2}, {3, 3}, {4, 4}]
      end

      test "is zippable with slowest first" do
        task = 4..1 |> Task.async_stream(&sleep/1, @opts) |> Stream.map(&elem(&1, 1))
        assert Enum.zip(task, task) == [{4, 4}, {3, 3}, {2, 2}, {1, 1}]
      end

      test "with inner halt on success" do
        assert 1..8
               |> Stream.take(4)
               |> Task.async_stream(&sleep/1, @opts)
               |> Enum.to_list() == [ok: 1, ok: 2, ok: 3, ok: 4]
      end

      test "with inner halt on failure" do
        Process.flag(:trap_exit, true)

        assert 1..8
               |> Stream.take(4)
               |> Task.async_stream(&exit/1, @opts)
               |> Enum.to_list() == [exit: 1, exit: 2, exit: 3, exit: 4]
      end

      test "with inner halt and slowest first" do
        assert 8..1
               |> Stream.take(4)
               |> Task.async_stream(&sleep/1, @opts)
               |> Enum.to_list() == [ok: 8, ok: 7, ok: 6, ok: 5]
      end

      test "with outer halt on success" do
        assert 1..8
               |> Task.async_stream(&sleep/1, @opts)
               |> Enum.take(4) == [ok: 1, ok: 2, ok: 3, ok: 4]
      end

      test "with outer halt on failure" do
        Process.flag(:trap_exit, true)

        assert 1..8
               |> Task.async_stream(&exit/1, @opts)
               |> Enum.take(4) == [exit: 1, exit: 2, exit: 3, exit: 4]
      end

      test "with outer halt and slowest first" do
        assert 8..1
               |> Task.async_stream(&sleep/1, @opts)
               |> Enum.take(4) == [ok: 8, ok: 7, ok: 6, ok: 5]
      end

      test "terminates inner effect" do
        stream =
          1..4
          |> Task.async_stream(&sleep/1, @opts)
          |> Stream.transform(fn -> :ok end, fn x, acc -> {[x], acc} end, fn _ ->
            Process.put(:stream_transform, true)
          end)

        Process.put(:stream_transform, false)
        assert Enum.to_list(stream) == [ok: 1, ok: 2, ok: 3, ok: 4]
        assert Process.get(:stream_transform)
      end

      test "terminates outer effect" do
        stream =
          1..4
          |> Stream.transform(fn -> :ok end, fn x, acc -> {[x], acc} end, fn _ ->
            Process.put(:stream_transform, true)
          end)
          |> Task.async_stream(&sleep/1, @opts)

        Process.put(:stream_transform, false)
        assert Enum.to_list(stream) == [ok: 1, ok: 2, ok: 3, ok: 4]
        assert Process.get(:stream_transform)
      end

      test "with :on_timeout set to :kill_task" do
        opts = Keyword.merge(@opts, on_timeout: :kill_task, timeout: 50)

        assert [100, 1, 100, 1]
               |> Task.async_stream(&sleep/1, opts)
               |> Enum.to_list() == [exit: :timeout, ok: 1, exit: :timeout, ok: 1]

        refute_received _
      end
    end
  end
end
