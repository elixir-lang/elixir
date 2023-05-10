defmodule ExUnit.ProcessAssertionsTest.Registered do
  use GenServer

  @impl true
  def init(value) do
    {:ok, value}
  end

  def start_processes(registry, names) do
    Enum.map(names, fn name -> start_link(registry, name) end)
  end

  def stop_processes(registry, names) do
    Enum.map(names, fn name -> stop_process(registry, name) end)
  end

  def start_link(registry, name) do
    GenServer.start(__MODULE__, nil, name: {:via, Registry, {registry, name}})
  end

  def stop_process(registry, name) do
    case Registry.lookup(registry, name) do
      [{pid, nil}] ->
        Process.exit(pid, :shutdown)

      _ ->
        nil
    end
  end
end

defmodule ExUnit.ProcessAssertionsTest.Named do
  use GenServer

  @impl true
  def init(stack) do
    {:ok, stack}
  end

  def start_processes(names) do
    Enum.map(names, fn name -> start_process(name) end)
  end

  def stop_processes(names) do
    Enum.map(names, fn name -> stop_process(name) end)
  end

  def start_process(name) do
    GenServer.start(__MODULE__, [], name: name)
  end

  def stop_process(name) do
    case Process.whereis(name) do
      nil ->
        nil

      pid ->
        Process.exit(pid, :shutdown)
    end
  end
end

defmodule ExUnit.ProcessAssertionsTest.TestRegistry do
  def wait_for_registry(name) do
    case Registry.start_link(keys: :unique, name: name) do
      {:ok, pid} when is_pid(pid) -> pid
      _ -> wait_for_registry(name)
    end
  end
end

alias ExUnit.ProcessAssertionsTest.{Registered, Named, TestRegistry}

defmodule ExUnit.ProcessAssertionsTest do
  use ExUnit.Case, async: true

  setup_all do
    TestRegistry.wait_for_registry(:test_registry_one)
    TestRegistry.wait_for_registry(:test_registry_two)
    :ok
  end

  setup do
    {:ok,
     start_fun: fn ->
       Registered.start_processes(:test_registry_one, [:agent_foo])

       Registered.start_processes(:test_registry_two, [
         :agent_bar,
         :agent_baz
       ])

       Named.start_processes([:foo, :bar, :baz])
       :processes_started
     end,
     stop_fun: fn ->
       Registered.stop_processes(:test_registry_one, [:agent_foo])

       Registered.stop_processes(:test_registry_two, [
         :agent_bar,
         :agent_baz
       ])

       Named.stop_processes([:foo, :bar, :baz])
       :processes_stopped
     end,
     processes_spec: %{
       registry: %{
         test_registry_one: :agent_foo,
         test_registry_two: [:agent_bar, :agent_baz]
       },
       names: [:foo, :bar, :baz]
     }}
  end

  @message_before_and_after "Some processes in incorrect state:\n [before] Process :foo expected to not be started, \n[before] Process :bar expected to not be started, \n[before] Process :baz expected to not be started, \n[before] Process :test_registry_one with id agent_foo expected to not be started, \n[before] Process :test_registry_two with id agent_bar expected to not be started, \n[before] Process :test_registry_two with id agent_baz expected to not be started, \n[after] Process :foo expected to not be started, \n[after] Process :bar expected to not be started, \n[after] Process :baz expected to not be started, \n[after] Process :test_registry_one with id agent_foo expected to not be started, \n[after] Process :test_registry_two with id agent_bar expected to not be started, \n[after] Process :test_registry_two with id agent_baz expected to not be started"

  @message_before "Some processes in incorrect state:\n [before] Process :foo expected to not be started, \n[before] Process :bar expected to not be started, \n[before] Process :baz expected to not be started, \n[before] Process :test_registry_one with id agent_foo expected to not be started, \n[before] Process :test_registry_two with id agent_bar expected to not be started, \n[before] Process :test_registry_two with id agent_baz expected to not be started"

  @message_after "Some processes in incorrect state:\n [after] Process :foo expected to be started, but not found, \n[after] Process :bar expected to be started, but not found, \n[after] Process :baz expected to be started, but not found, \n[after] Process :test_registry_one with id agent_foo expected to be started, but not found, \n[after] Process :test_registry_two with id agent_bar expected to be started, but not found, \n[after] Process :test_registry_two with id agent_baz expected to be started, but not found"

  @message_alive "Some processes in incorrect state:\n [after] Process :foo expected to be stopped but still alive, \n[after] Process :bar expected to be stopped but still alive, \n[after] Process :baz expected to be stopped but still alive, \n[after] Process :test_registry_one: agent_foo expected to be stopped but still alive, \n[after] Process :test_registry_two: agent_bar expected to be stopped but still alive, \n[after] Process :test_registry_two: agent_baz expected to be stopped but still alive"

  test "assert_processes_started success", %{
    start_fun: start_fun,
    stop_fun: stop_fun,
    processes_spec: %{registry: registry, names: names}
  } do
    assert :processes_started ==
             assert_processes_started(
               start_fun,
               registry: registry,
               names: names
             )

    stop_fun.()
  end

  test "assert_processes_started failed", %{
    start_fun: start_fun,
    stop_fun: stop_fun,
    processes_spec: %{registry: registry, names: names}
  } do
    start_fun.()

    assert_raise(
      RuntimeError,
      @message_before,
      fn ->
        assert_processes_started(
          start_fun,
          registry: registry,
          names: names
        )
      end
    )

    stop_fun.()
  end

  test "refute_processes_started success", %{
    processes_spec: %{registry: registry, names: names}
  } do
    assert :processes_not_started ==
             refute_processes_started(
               fn -> :processes_not_started end,
               registry: registry,
               names: names
             )
  end

  test "refute_processes_started failed", %{
    start_fun: start_fun,
    stop_fun: stop_fun,
    processes_spec: %{registry: registry, names: names}
  } do
    start_fun.()

    assert_raise(
      RuntimeError,
      @message_before_and_after,
      fn ->
        refute_processes_started(
          start_fun,
          registry: registry,
          names: names
        )
      end
    )

    stop_fun.()
  end

  test "assert_processes_stopped success", %{
    start_fun: start_fun,
    stop_fun: stop_fun,
    processes_spec: %{registry: registry, names: names}
  } do
    start_fun.()

    assert :processes_stopped ==
             assert_processes_stopped(
               stop_fun,
               registry: registry,
               names: names
             )
  end

  test "assert_processes_stopped failed", %{
    start_fun: start_fun,
    stop_fun: stop_fun,
    processes_spec: %{registry: registry, names: names}
  } do
    start_fun.()

    assert_raise(
      RuntimeError,
      @message_alive,
      fn ->
        assert_processes_stopped(
          fn -> :processes_not_stopped end,
          registry: registry,
          names: names
        )
      end
    )

    stop_fun.()
  end

  test "refute_processes_stopped success", %{
    start_fun: start_fun,
    stop_fun: stop_fun,
    processes_spec: %{registry: registry, names: names}
  } do
    start_fun.()

    assert :processes_not_stopped ==
             refute_processes_stopped(
               fn -> :processes_not_stopped end,
               registry: registry,
               names: names
             )

    stop_fun.()
  end

  test "refute_processes_stopped failed", %{
    start_fun: start_fun,
    stop_fun: stop_fun,
    processes_spec: %{registry: registry, names: names}
  } do
    start_fun.()

    assert_raise(
      RuntimeError,
      @message_after,
      fn ->
        refute_processes_stopped(
          stop_fun,
          registry: registry,
          names: names
        )
      end
    )
  end

  test "assert_processes_survived success", %{
    start_fun: start_fun,
    stop_fun: stop_fun,
    processes_spec: %{registry: registry, names: names}
  } do
    start_fun.()

    assert :processes_not_stopped ==
             assert_processes_survived(
               fn -> :processes_not_stopped end,
               registry: registry,
               names: names
             )

    stop_fun.()
  end

  test "assert_processes_survived failed", %{
    start_fun: start_fun,
    stop_fun: stop_fun,
    processes_spec: %{registry: registry, names: names}
  } do
    start_fun.()

    assert_raise(
      RuntimeError,
      @message_after,
      fn ->
        assert_processes_survived(
          stop_fun,
          registry: registry,
          names: names
        )
      end
    )
  end

  test "refute_processes_survived success", %{
    start_fun: start_fun,
    stop_fun: stop_fun,
    processes_spec: %{registry: registry, names: names}
  } do
    start_fun.()

    assert :processes_stopped ==
             refute_processes_survived(
               stop_fun,
               registry: registry,
               names: names
             )
  end

  test "refute_processes_survived failed", %{
    start_fun: start_fun,
    stop_fun: stop_fun,
    processes_spec: %{registry: registry, names: names}
  } do
    start_fun.()

    assert_raise(
      RuntimeError,
      @message_alive,
      fn ->
        refute_processes_survived(
          fn -> :processes_not_stopped end,
          registry: registry,
          names: names
        )
      end
    )

    stop_fun.()
  end
end
