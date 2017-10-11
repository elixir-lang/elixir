Code.require_file("../test_helper.exs", __DIR__)

defmodule Task.SupervisorTest do
  use ExUnit.Case

  @moduletag report: [:supervisor]
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

  def sleep_and_return_ancestor(number, :another_arg) do
    sleep_and_return_ancestor(number)
  end

  def sleep_and_return_ancestor(number) do
    Process.sleep(number)
    {:dictionary, dictionary} = Process.info(self(), :dictionary)

    dictionary
    |> Keyword.get(:"$ancestors")
    |> List.first()
  end

  test "can be supervised directly", config do
    modules = [{Task.Supervisor, name: config.test}]
    assert {:ok, _} = Supervisor.start_link(modules, strategy: :one_for_one)
    assert Process.whereis(config.test)
  end

  test "async/1", config do
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
    {:name, fun_name} = :erlang.fun_info(fun, :name)
    assert {__MODULE__, fun_name, 0} === :proc_lib.translate_initial_call(task.pid)

    # Run the task
    send(task.pid, true)

    # Assert response and monitoring messages
    ref = task.ref
    assert_receive {^ref, :done}
    assert_receive {:DOWN, ^ref, _, _, :normal}
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

  test "async_nolink/1", config do
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
    {:name, fun_name} = :erlang.fun_info(fun, :name)
    assert {__MODULE__, fun_name, 0} === :proc_lib.translate_initial_call(task.pid)

    # Run the task
    send(task.pid, true)

    # Assert response and monitoring messages
    ref = task.ref
    assert_receive {^ref, :done}
    assert_receive {:DOWN, ^ref, _, _, :normal}
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
    {:name, fun_name} = :erlang.fun_info(fun, :name)
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

  test "terminate_child/2", config do
    args = [self(), :done]

    {:ok, pid} =
      Task.Supervisor.start_child(config[:supervisor], __MODULE__, :wait_and_send, args)

    assert Task.Supervisor.children(config[:supervisor]) == [pid]
    assert Task.Supervisor.terminate_child(config[:supervisor], pid) == :ok
    assert Task.Supervisor.children(config[:supervisor]) == []
    assert Task.Supervisor.terminate_child(config[:supervisor], pid) == :ok
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
             |> Task.Supervisor.async_stream(1..4, &exit(Integer.to_string(&1)), @opts)
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

    test "streams an enumerable with fun and supervisor fun", %{supervisor: supervisor} do
      {:ok, other_supervisor} = Task.Supervisor.start_link()

      assert fn i -> if rem(i, 2) == 0, do: supervisor, else: other_supervisor end
             |> Task.Supervisor.async_stream(1..4, &sleep_and_return_ancestor/1, @opts)
             |> Enum.to_list() ==
               [ok: other_supervisor, ok: supervisor, ok: other_supervisor, ok: supervisor]
    end

    test "streams an enumerable with mfa and supervisor fun", %{supervisor: supervisor} do
      {:ok, other_supervisor} = Task.Supervisor.start_link()
      fun = :sleep_and_return_ancestor

      assert fn i -> if rem(i, 2) == 0, do: supervisor, else: other_supervisor end
             |> Task.Supervisor.async_stream(1..4, __MODULE__, fun, [], @opts)
             |> Enum.to_list() ==
               [ok: other_supervisor, ok: supervisor, ok: other_supervisor, ok: supervisor]
    end

    test "streams an enumerable with mfa with args and supervisor fun", %{supervisor: supervisor} do
      {:ok, other_supervisor} = Task.Supervisor.start_link()
      fun = :sleep_and_return_ancestor

      assert fn i -> if rem(i, 2) == 0, do: supervisor, else: other_supervisor end
             |> Task.Supervisor.async_stream(1..4, __MODULE__, fun, [:another_arg], @opts)
             |> Enum.to_list() ==
               [ok: other_supervisor, ok: supervisor, ok: other_supervisor, ok: supervisor]
    end

    test "streams an enumerable with fun and executes supervisor fun in monitor process", context do
      %{supervisor: supervisor} = context
      parent = self()

      supervisor_fun =
        fn _i ->
          {:links, links} = Process.info(self(), :links)
          assert parent in links
          send(parent, {parent, self()})
          supervisor
        end

      assert supervisor_fun
             |> Task.Supervisor.async_stream(1..4, &sleep_and_return_ancestor/1, @opts)
             |> Enum.to_list() == [ok: supervisor, ok: supervisor, ok: supervisor, ok: supervisor]

      receive do
        {^parent, linked} ->
          for _ <- 1..3, do: assert_received({^parent, ^linked})
      after
        0 ->
          flunk("Did not receive any message from monitor process.")
      end
    end

    test "streams an enumerable with fun and bad supervisor fun" do
      Process.flag(:trap_exit, true)

      stream =
        fn _i -> raise "bad" end
        |> Task.Supervisor.async_stream(1..4, &sleep_and_return_ancestor/1, @opts)

      assert {{%RuntimeError{message: "bad"}, _stacktrace}, _mfa} = catch_exit(Stream.run(stream))

      refute_received _

      stream =
        fn _i -> :not_a_supervisor end
        |> Task.Supervisor.async_stream(1..4, &sleep_and_return_ancestor/1, @opts)

      assert {{:noproc, _stacktrace}, _mfa} = catch_exit(Stream.run(stream))

      refute_received _
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
             |> Task.Supervisor.async_stream_nolink(1..4, &exit/1, @opts)
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
  end
end
