Code.require_file("../test_helper.exs", __DIR__)

defmodule Task.SupervisorTest do
  use ExUnit.Case

  @moduletag :capture_log

  setup do
    {:ok, pid} = Task.Supervisor.start_link()
    {:ok, supervisor: pid}
  end

  def wait_and_send(caller, atom) do
    send(caller, :ready)
    receive do: (true -> true)
    send(caller, atom)
  end

  def sleep(number) do
    Process.sleep(number)
    number
  end

  test "can be supervised directly", config do
    modules = [{Task.Supervisor, name: config.test}]
    assert {:ok, _} = Supervisor.start_link(modules, strategy: :one_for_one)
    assert Process.whereis(config.test)
  end

  test "multiple supervisors can be supervised and identified with simple child spec" do
    {:ok, _} = Registry.start_link(keys: :unique, name: TaskSup.Registry)

    children = [
      {Task.Supervisor, strategy: :one_for_one, name: :simple_name},
      {Task.Supervisor, strategy: :one_for_one, name: {:global, :global_name}},
      {Task.Supervisor,
       strategy: :one_for_one, name: {:via, Registry, {TaskSup.Registry, "via_name"}}}
    ]

    assert {:ok, supsup} = Supervisor.start_link(children, strategy: :one_for_one)

    assert {:ok, no_name_dynsup} =
             Supervisor.start_child(supsup, {Task.Supervisor, strategy: :one_for_one})

    assert Task.Supervisor.children(:simple_name) == []
    assert Task.Supervisor.children({:global, :global_name}) == []
    assert Task.Supervisor.children({:via, Registry, {TaskSup.Registry, "via_name"}}) == []
    assert Task.Supervisor.children(no_name_dynsup) == []

    assert Supervisor.start_child(supsup, {Task.Supervisor, strategy: :one_for_one}) ==
             {:error, {:already_started, no_name_dynsup}}
  end

  test "counts and returns children", config do
    assert Task.Supervisor.children(config[:supervisor]) == []

    assert Supervisor.count_children(config[:supervisor]) ==
             %{active: 0, specs: 0, supervisors: 0, workers: 0}

    assert DynamicSupervisor.count_children(config[:supervisor]) ==
             %{active: 0, specs: 0, supervisors: 0, workers: 0}
  end

  describe "async/1" do
    test "spawns tasks under the supervisor", config do
      parent = self()
      fun = fn -> wait_and_send(parent, :done) end
      task = Task.Supervisor.async(config[:supervisor], fun)
      assert Task.Supervisor.children(config[:supervisor]) == [task.pid]

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

    test "with custom shutdown", config do
      Process.flag(:trap_exit, true)
      parent = self()

      fun = fn -> wait_and_send(parent, :done) end
      %{pid: pid} = Task.Supervisor.async(config[:supervisor], fun, shutdown: :brutal_kill)

      Process.exit(config[:supervisor], :shutdown)
      assert_receive {:DOWN, _, _, ^pid, :killed}
    end

    test "raises when :max_children is reached" do
      {:ok, sup} = Task.Supervisor.start_link(max_children: 1)
      Task.Supervisor.async(sup, fn -> Process.sleep(:infinity) end)

      assert_raise RuntimeError, ~r/reached the maximum number of tasks/, fn ->
        Task.Supervisor.async(sup, fn -> :ok end)
      end
    end

    test "with $callers", config do
      sup = config[:supervisor]
      grandparent = self()

      Task.Supervisor.async(sup, fn ->
        parent = self()
        assert Process.get(:"$callers") == [grandparent]
        assert Process.get(:"$ancestors") == [sup, grandparent]

        Task.Supervisor.async(sup, fn ->
          assert Process.get(:"$callers") == [parent, grandparent]
          assert Process.get(:"$ancestors") == [sup, grandparent]
        end)
        |> Task.await()
      end)
      |> Task.await()
    end
  end

  test "async/3", config do
    args = [self(), :done]
    task = Task.Supervisor.async(config[:supervisor], __MODULE__, :wait_and_send, args)
    assert Task.Supervisor.children(config[:supervisor]) == [task.pid]

    receive do: (:ready -> :ok)
    assert {__MODULE__, :wait_and_send, 2} === :proc_lib.translate_initial_call(task.pid)

    send(task.pid, true)
    assert task.__struct__ == Task
    assert Task.await(task) == :done
  end

  describe "async_nolink/1" do
    test "spawns a task under the supervisor without linking to the caller", config do
      parent = self()
      fun = fn -> wait_and_send(parent, :done) end
      task = Task.Supervisor.async_nolink(config[:supervisor], fun)
      assert Task.Supervisor.children(config[:supervisor]) == [task.pid]

      # Assert the struct
      assert task.__struct__ == Task
      assert is_pid(task.pid)
      assert is_reference(task.ref)

      # Refute the link
      {:links, links} = Process.info(self(), :links)
      refute task.pid in links

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

    test "with custom shutdown", config do
      Process.flag(:trap_exit, true)
      parent = self()

      fun = fn -> wait_and_send(parent, :done) end
      %{pid: pid} = Task.Supervisor.async_nolink(config[:supervisor], fun, shutdown: :brutal_kill)

      Process.exit(config[:supervisor], :shutdown)
      assert_receive {:DOWN, _, _, ^pid, :killed}
    end

    test "raises when :max_children is reached" do
      {:ok, sup} = Task.Supervisor.start_link(max_children: 1)

      Task.Supervisor.async_nolink(sup, fn -> Process.sleep(:infinity) end)

      assert_raise RuntimeError, ~r/reached the maximum number of tasks/, fn ->
        Task.Supervisor.async_nolink(sup, fn -> :ok end)
      end
    end
  end

  test "async_nolink/3", config do
    args = [self(), :done]
    task = Task.Supervisor.async_nolink(config[:supervisor], __MODULE__, :wait_and_send, args)
    assert Task.Supervisor.children(config[:supervisor]) == [task.pid]

    receive do: (:ready -> :ok)
    assert {__MODULE__, :wait_and_send, 2} === :proc_lib.translate_initial_call(task.pid)

    send(task.pid, true)
    assert task.__struct__ == Task
    assert Task.await(task) == :done
  end

  test "start_child/1", config do
    parent = self()
    fun = fn -> wait_and_send(parent, :done) end
    {:ok, pid} = Task.Supervisor.start_child(config[:supervisor], fun)
    assert Task.Supervisor.children(config[:supervisor]) == [pid]

    {:links, links} = Process.info(self(), :links)
    refute pid in links

    receive do: (:ready -> :ok)
    {:name, fun_name} = Function.info(fun, :name)
    assert {__MODULE__, fun_name, 0} === :proc_lib.translate_initial_call(pid)

    send(pid, true)
    assert_receive :done
  end

  test "start_child/3", config do
    args = [self(), :done]

    {:ok, pid} =
      Task.Supervisor.start_child(config[:supervisor], __MODULE__, :wait_and_send, args)

    assert Task.Supervisor.children(config[:supervisor]) == [pid]

    {:links, links} = Process.info(self(), :links)
    refute pid in links

    receive do: (:ready -> :ok)
    assert {__MODULE__, :wait_and_send, 2} === :proc_lib.translate_initial_call(pid)

    send(pid, true)
    assert_receive :done

    assert_raise FunctionClauseError, fn ->
      Task.Supervisor.start_child(config[:supervisor], __MODULE__, :wait_and_send, :illegal_arg)
    end

    assert_raise FunctionClauseError, fn ->
      args = [self(), :done]
      Task.Supervisor.start_child(config[:supervisor], __MODULE__, "wait_and_send", args)
    end
  end

  test "start_child/1 with custom shutdown", config do
    Process.flag(:trap_exit, true)
    parent = self()

    fun = fn -> wait_and_send(parent, :done) end
    {:ok, pid} = Task.Supervisor.start_child(config[:supervisor], fun, shutdown: :brutal_kill)

    Process.monitor(pid)
    Process.exit(config[:supervisor], :shutdown)
    assert_receive {:DOWN, _, _, ^pid, :killed}
  end

  test "start_child/1 with custom restart", config do
    parent = self()

    fun = fn -> wait_and_send(parent, :done) end
    {:ok, pid} = Task.Supervisor.start_child(config[:supervisor], fun, restart: :permanent)

    assert_receive :ready
    Process.monitor(pid)
    Process.exit(pid, :shutdown)
    assert_receive {:DOWN, _, _, ^pid, :shutdown}
    assert_receive :ready
  end

  test "start_child/1 with $callers", config do
    sup = config[:supervisor]
    grandparent = self()

    Task.Supervisor.start_child(sup, fn ->
      parent = self()
      assert Process.get(:"$callers") == [grandparent]
      assert Process.get(:"$ancestors") == [sup, grandparent]

      Task.Supervisor.start_child(sup, fn ->
        assert Process.get(:"$callers") == [parent, grandparent]
        assert Process.get(:"$ancestors") == [sup, grandparent]
        send(grandparent, :done)
      end)
    end)

    assert_receive :done
  end

  test "terminate_child/2", config do
    args = [self(), :done]

    {:ok, pid} =
      Task.Supervisor.start_child(config[:supervisor], __MODULE__, :wait_and_send, args)

    assert Task.Supervisor.children(config[:supervisor]) == [pid]
    assert Task.Supervisor.terminate_child(config[:supervisor], pid) == :ok
    assert Task.Supervisor.children(config[:supervisor]) == []
    assert Task.Supervisor.terminate_child(config[:supervisor], pid) == {:error, :not_found}
  end

  describe "await/1" do
    test "exits on task throw", config do
      Process.flag(:trap_exit, true)
      task = Task.Supervisor.async(config[:supervisor], fn -> throw(:unknown) end)

      assert {{{:nocatch, :unknown}, _}, {Task, :await, [^task, 5000]}} =
               catch_exit(Task.await(task))
    end

    test "exits on task error", config do
      Process.flag(:trap_exit, true)
      task = Task.Supervisor.async(config[:supervisor], fn -> raise "oops" end)
      assert {{%RuntimeError{}, _}, {Task, :await, [^task, 5000]}} = catch_exit(Task.await(task))
    end

    test "exits on task exit", config do
      Process.flag(:trap_exit, true)
      task = Task.Supervisor.async(config[:supervisor], fn -> exit(:unknown) end)
      assert {:unknown, {Task, :await, [^task, 5000]}} = catch_exit(Task.await(task))
    end
  end

  describe "async_stream" do
    @opts []
    test "streams an enumerable with fun", %{supervisor: supervisor} do
      assert supervisor
             |> Task.Supervisor.async_stream(1..4, &sleep/1, @opts)
             |> Enum.to_list() == [ok: 1, ok: 2, ok: 3, ok: 4]
    end

    test "streams an enumerable with mfa", %{supervisor: supervisor} do
      assert supervisor
             |> Task.Supervisor.async_stream(1..4, __MODULE__, :sleep, [], @opts)
             |> Enum.to_list() == [ok: 1, ok: 2, ok: 3, ok: 4]
    end

    test "streams an enumerable without leaking tasks", %{supervisor: supervisor} do
      assert supervisor
             |> Task.Supervisor.async_stream(1..4, &sleep/1, @opts)
             |> Enum.to_list() == [ok: 1, ok: 2, ok: 3, ok: 4]

      refute_received _
    end

    test "streams an enumerable with slowest first", %{supervisor: supervisor} do
      Process.flag(:trap_exit, true)

      assert supervisor
             |> Task.Supervisor.async_stream(4..1, &sleep/1, @opts)
             |> Enum.to_list() == [ok: 4, ok: 3, ok: 2, ok: 1]
    end

    test "streams an enumerable with exits", %{supervisor: supervisor} do
      Process.flag(:trap_exit, true)

      assert supervisor
             |> Task.Supervisor.async_stream(1..4, &yield_and_exit(Integer.to_string(&1)), @opts)
             |> Enum.to_list() == [exit: "1", exit: "2", exit: "3", exit: "4"]
    end

    test "shuts down unused tasks", %{supervisor: supervisor} do
      collection = [0, :infinity, :infinity, :infinity]

      assert supervisor
             |> Task.Supervisor.async_stream(collection, &sleep/1, @opts)
             |> Enum.take(1) == [ok: 0]

      assert Process.info(self(), :links) == {:links, [supervisor]}
    end

    test "shuts down unused tasks without leaking messages", %{supervisor: supervisor} do
      collection = [0, :infinity, :infinity, :infinity]

      assert supervisor
             |> Task.Supervisor.async_stream(collection, &sleep/1, @opts)
             |> Enum.take(1) == [ok: 0]

      refute_received _
    end

    test "raises an error if :max_children is reached with clean stream shutdown",
         %{supervisor: unused_supervisor} do
      {:ok, supervisor} = Task.Supervisor.start_link(max_children: 1)
      collection = [:infinity, :infinity, :infinity]

      assert_raise RuntimeError, ~r/reached the maximum number of tasks/, fn ->
        supervisor
        |> Task.Supervisor.async_stream(collection, &sleep/1, max_concurrency: 2)
        |> Enum.to_list()
      end

      {:links, links} = Process.info(self(), :links)
      assert MapSet.new(links) == MapSet.new([unused_supervisor, supervisor])
      refute_received _
    end

    test "with $callers", config do
      sup = config[:supervisor]
      grandparent = self()

      Task.Supervisor.async_stream(sup, [1], fn 1 ->
        parent = self()
        assert Process.get(:"$callers") == [grandparent]
        assert Process.get(:"$ancestors") == [sup, grandparent]

        Task.Supervisor.async_stream(sup, [1], fn 1 ->
          assert Process.get(:"$callers") == [parent, grandparent]
          assert Process.get(:"$ancestors") == [sup, grandparent]
          send(grandparent, :done)
        end)
        |> Stream.run()
      end)
      |> Stream.run()

      assert_receive :done
    end
  end

  describe "async_stream_nolink" do
    @opts [max_concurrency: 4]

    test "streams an enumerable with fun", %{supervisor: supervisor} do
      assert supervisor
             |> Task.Supervisor.async_stream_nolink(1..4, &sleep/1, @opts)
             |> Enum.to_list() == [ok: 1, ok: 2, ok: 3, ok: 4]
    end

    test "streams an enumerable with mfa", %{supervisor: supervisor} do
      assert supervisor
             |> Task.Supervisor.async_stream_nolink(1..4, __MODULE__, :sleep, [], @opts)
             |> Enum.to_list() == [ok: 1, ok: 2, ok: 3, ok: 4]
    end

    test "streams an enumerable without leaking tasks", %{supervisor: supervisor} do
      assert supervisor
             |> Task.Supervisor.async_stream_nolink(1..4, &sleep/1, @opts)
             |> Enum.to_list() == [ok: 1, ok: 2, ok: 3, ok: 4]

      refute_received _
    end

    test "streams an enumerable with slowest first", %{supervisor: supervisor} do
      assert supervisor
             |> Task.Supervisor.async_stream_nolink(4..1, &sleep/1, @opts)
             |> Enum.to_list() == [ok: 4, ok: 3, ok: 2, ok: 1]
    end

    test "streams an enumerable with exits", %{supervisor: supervisor} do
      assert supervisor
             |> Task.Supervisor.async_stream_nolink(1..4, &yield_and_exit/1, @opts)
             |> Enum.to_list() == [exit: 1, exit: 2, exit: 3, exit: 4]
    end

    test "shuts down unused tasks", %{supervisor: supervisor} do
      collection = [0, :infinity, :infinity, :infinity]

      assert supervisor
             |> Task.Supervisor.async_stream_nolink(collection, &sleep/1, @opts)
             |> Enum.take(1) == [ok: 0]

      assert Process.info(self(), :links) == {:links, [supervisor]}
    end

    test "shuts down unused tasks without leaking messages", %{supervisor: supervisor} do
      collection = [0, :infinity, :infinity, :infinity]

      assert supervisor
             |> Task.Supervisor.async_stream_nolink(collection, &sleep/1, @opts)
             |> Enum.take(1) == [ok: 0]

      refute_received _
    end

    test "raises an error if :max_children is reached with clean stream shutdown",
         %{supervisor: unused_supervisor} do
      {:ok, supervisor} = Task.Supervisor.start_link(max_children: 1)
      collection = [:infinity, :infinity, :infinity]

      assert_raise RuntimeError, ~r/reached the maximum number of tasks/, fn ->
        supervisor
        |> Task.Supervisor.async_stream_nolink(collection, &sleep/1, max_concurrency: 2)
        |> Enum.to_list()
      end

      {:links, links} = Process.info(self(), :links)
      assert MapSet.new(links) == MapSet.new([unused_supervisor, supervisor])
      refute_received _
    end
  end

  def yield_and_exit(value) do
    # We call yield first so we give the parent a chance to monitor
    :erlang.yield()
    :erlang.exit(value)
  end
end
