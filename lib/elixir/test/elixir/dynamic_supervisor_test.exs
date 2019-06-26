Code.require_file("test_helper.exs", __DIR__)

defmodule DynamicSupervisorTest do
  use ExUnit.Case, async: true

  defmodule Simple do
    use DynamicSupervisor

    def init(args), do: args
  end

  test "can be supervised directly" do
    children = [{DynamicSupervisor, strategy: :one_for_one, name: :dyn_sup_spec_test}]
    assert {:ok, _} = Supervisor.start_link(children, strategy: :one_for_one)
    assert DynamicSupervisor.which_children(:dyn_sup_spec_test) == []
  end

  test "multiple supervisors can be supervised and identified with simple child spec" do
    {:ok, _} = Registry.start_link(keys: :unique, name: DynSup.Registry)

    children = [
      {DynamicSupervisor, strategy: :one_for_one, name: :simple_name},
      {DynamicSupervisor, strategy: :one_for_one, name: {:global, :global_name}},
      {DynamicSupervisor,
       strategy: :one_for_one, name: {:via, Registry, {DynSup.Registry, "via_name"}}}
    ]

    assert {:ok, supsup} = Supervisor.start_link(children, strategy: :one_for_one)

    assert {:ok, no_name_dynsup} =
             Supervisor.start_child(supsup, {DynamicSupervisor, strategy: :one_for_one})

    assert DynamicSupervisor.which_children(:simple_name) == []
    assert DynamicSupervisor.which_children({:global, :global_name}) == []
    assert DynamicSupervisor.which_children({:via, Registry, {DynSup.Registry, "via_name"}}) == []
    assert DynamicSupervisor.which_children(no_name_dynsup) == []

    assert Supervisor.start_child(supsup, {DynamicSupervisor, strategy: :one_for_one}) ==
             {:error, {:already_started, no_name_dynsup}}
  end

  describe "use/2" do
    @compile {:no_warn_undefined, DynamicSupervisorTest.Custom}

    test "generates child_spec/1" do
      assert Simple.child_spec([:hello]) == %{
               id: Simple,
               start: {Simple, :start_link, [[:hello]]},
               type: :supervisor
             }

      defmodule Custom do
        use DynamicSupervisor,
          id: :id,
          restart: :temporary,
          shutdown: :infinity,
          start: {:foo, :bar, []}

        def init(arg), do: {:producer, arg}
      end

      assert Custom.child_spec([:hello]) == %{
               id: :id,
               restart: :temporary,
               shutdown: :infinity,
               start: {:foo, :bar, []},
               type: :supervisor
             }
    end
  end

  describe "init/1" do
    test "set default options" do
      assert DynamicSupervisor.init(strategy: :one_for_one) ==
               {:ok,
                %{
                  strategy: :one_for_one,
                  intensity: 3,
                  period: 5,
                  max_children: :infinity,
                  extra_arguments: []
                }}
    end
  end

  describe "start_link/3" do
    test "with non-ok init" do
      Process.flag(:trap_exit, true)

      assert DynamicSupervisor.start_link(Simple, {:ok, %{strategy: :unknown}}) ==
               {:error, {:supervisor_data, {:invalid_strategy, :unknown}}}

      assert DynamicSupervisor.start_link(Simple, {:ok, %{intensity: -1}}) ==
               {:error, {:supervisor_data, {:invalid_intensity, -1}}}

      assert DynamicSupervisor.start_link(Simple, {:ok, %{period: 0}}) ==
               {:error, {:supervisor_data, {:invalid_period, 0}}}

      assert DynamicSupervisor.start_link(Simple, {:ok, %{max_children: -1}}) ==
               {:error, {:supervisor_data, {:invalid_max_children, -1}}}

      assert DynamicSupervisor.start_link(Simple, {:ok, %{extra_arguments: -1}}) ==
               {:error, {:supervisor_data, {:invalid_extra_arguments, -1}}}

      assert DynamicSupervisor.start_link(Simple, :unknown) ==
               {:error, {:bad_return, {Simple, :init, :unknown}}}

      assert DynamicSupervisor.start_link(Simple, :ignore) == :ignore
    end

    test "with registered process" do
      {:ok, pid} = DynamicSupervisor.start_link(Simple, {:ok, %{}}, name: __MODULE__)

      # Sets up a link
      {:links, links} = Process.info(self(), :links)
      assert pid in links

      # A name
      assert Process.whereis(__MODULE__) == pid

      # And the initial call
      assert {:supervisor, DynamicSupervisorTest.Simple, 1} =
               :proc_lib.translate_initial_call(pid)

      # And shuts down
      assert DynamicSupervisor.stop(__MODULE__) == :ok
    end

    test "sets initial call to the same as a regular supervisor" do
      {:ok, pid} = Supervisor.start_link([], strategy: :one_for_one)
      assert :proc_lib.initial_call(pid) == {:supervisor, Supervisor.Default, [:Argument__1]}

      {:ok, pid} = DynamicSupervisor.start_link(strategy: :one_for_one)
      assert :proc_lib.initial_call(pid) == {:supervisor, Supervisor.Default, [:Argument__1]}
    end

    test "returns the callback module" do
      {:ok, pid} = Supervisor.start_link([], strategy: :one_for_one)
      assert :supervisor.get_callback_module(pid) == Supervisor.Default

      {:ok, pid} = DynamicSupervisor.start_link(strategy: :one_for_one)
      assert :supervisor.get_callback_module(pid) == Supervisor.Default
    end
  end

  ## Code change

  describe "code_change/3" do
    test "with non-ok init" do
      {:ok, pid} = DynamicSupervisor.start_link(Simple, {:ok, %{}})

      assert fake_upgrade(pid, {:ok, %{strategy: :unknown}}) ==
               {:error, {:error, {:supervisor_data, {:invalid_strategy, :unknown}}}}

      assert fake_upgrade(pid, {:ok, %{intensity: -1}}) ==
               {:error, {:error, {:supervisor_data, {:invalid_intensity, -1}}}}

      assert fake_upgrade(pid, {:ok, %{period: 0}}) ==
               {:error, {:error, {:supervisor_data, {:invalid_period, 0}}}}

      assert fake_upgrade(pid, {:ok, %{max_children: -1}}) ==
               {:error, {:error, {:supervisor_data, {:invalid_max_children, -1}}}}

      assert fake_upgrade(pid, :unknown) == {:error, :unknown}
      assert fake_upgrade(pid, :ignore) == :ok
    end

    test "with ok init" do
      {:ok, pid} = DynamicSupervisor.start_link(Simple, {:ok, %{}})
      {:ok, _} = DynamicSupervisor.start_child(pid, sleepy_worker())
      assert %{active: 1} = DynamicSupervisor.count_children(pid)

      assert fake_upgrade(pid, {:ok, %{max_children: 1}}) == :ok
      assert %{active: 1} = DynamicSupervisor.count_children(pid)
      assert DynamicSupervisor.start_child(pid, {Task, fn -> :ok end}) == {:error, :max_children}
    end

    defp fake_upgrade(pid, init_arg) do
      :ok = :sys.suspend(pid)
      :sys.replace_state(pid, fn state -> %{state | args: init_arg} end)
      res = :sys.change_code(pid, :gen_server, 123, :extra)
      :ok = :sys.resume(pid)
      res
    end
  end

  describe "start_child/2" do
    test "supports old child spec" do
      {:ok, pid} = DynamicSupervisor.start_link(strategy: :one_for_one)
      child = {Task, {Task, :start_link, [fn -> :ok end]}, :temporary, 5000, :worker, [Task]}
      assert {:ok, pid} = DynamicSupervisor.start_child(pid, child)
      assert is_pid(pid)
    end

    test "supports new child spec as tuple" do
      {:ok, pid} = DynamicSupervisor.start_link(strategy: :one_for_one)
      child = %{id: Task, restart: :temporary, start: {Task, :start_link, [fn -> :ok end]}}
      assert {:ok, pid} = DynamicSupervisor.start_child(pid, child)
      assert is_pid(pid)
    end

    test "supports new child spec" do
      {:ok, pid} = DynamicSupervisor.start_link(strategy: :one_for_one)
      child = {Task, fn -> :timer.sleep(:infinity) end}
      assert {:ok, pid} = DynamicSupervisor.start_child(pid, child)
      assert is_pid(pid)
    end

    test "supports extra arguments" do
      parent = self()
      fun = fn -> send(parent, :from_child) end

      {:ok, pid} = DynamicSupervisor.start_link(strategy: :one_for_one, extra_arguments: [fun])
      child = %{id: Task, restart: :temporary, start: {Task, :start_link, []}}
      assert {:ok, pid} = DynamicSupervisor.start_child(pid, child)
      assert is_pid(pid)
      assert_receive :from_child
    end

    test "with invalid child spec" do
      assert DynamicSupervisor.start_child(:not_used, %{}) == {:error, {:invalid_child_spec, %{}}}

      assert DynamicSupervisor.start_child(:not_used, {1, 2, 3, 4, 5, 6}) ==
               {:error, {:invalid_mfa, 2}}

      assert DynamicSupervisor.start_child(:not_used, %{id: 1, start: {Task, :foo, :bar}}) ==
               {:error, {:invalid_mfa, {Task, :foo, :bar}}}
    end

    test "with different returns" do
      {:ok, pid} = DynamicSupervisor.start_link(strategy: :one_for_one)

      assert {:ok, _, :extra} = DynamicSupervisor.start_child(pid, current_module_worker([:ok3]))
      assert {:ok, _} = DynamicSupervisor.start_child(pid, current_module_worker([:ok2]))
      assert :ignore = DynamicSupervisor.start_child(pid, current_module_worker([:ignore]))

      assert {:error, :found} =
               DynamicSupervisor.start_child(pid, current_module_worker([:error]))

      assert {:error, :unknown} =
               DynamicSupervisor.start_child(pid, current_module_worker([:unknown]))
    end

    test "with throw/error/exit" do
      {:ok, pid} = DynamicSupervisor.start_link(strategy: :one_for_one)

      assert {:error, {{:nocatch, :oops}, [_ | _]}} =
               DynamicSupervisor.start_child(pid, current_module_worker([:non_local, :throw]))

      assert {:error, {%RuntimeError{}, [_ | _]}} =
               DynamicSupervisor.start_child(pid, current_module_worker([:non_local, :error]))

      assert {:error, :oops} =
               DynamicSupervisor.start_child(pid, current_module_worker([:non_local, :exit]))
    end

    test "with max_children" do
      {:ok, pid} = DynamicSupervisor.start_link(strategy: :one_for_one, max_children: 0)

      assert {:error, :max_children} =
               DynamicSupervisor.start_child(pid, current_module_worker([:ok2]))
    end

    test "temporary child is not restarted regardless of reason" do
      child = current_module_worker([:ok2], restart: :temporary)
      {:ok, pid} = DynamicSupervisor.start_link(strategy: :one_for_one)

      assert {:ok, child_pid} = DynamicSupervisor.start_child(pid, child)
      assert_kill(child_pid, :shutdown)
      assert %{workers: 0, active: 0} = DynamicSupervisor.count_children(pid)

      assert {:ok, child_pid} = DynamicSupervisor.start_child(pid, child)
      assert_kill(child_pid, :whatever)
      assert %{workers: 0, active: 0} = DynamicSupervisor.count_children(pid)
    end

    test "transient child is restarted unless normal/shutdown/{shutdown, _}" do
      child = current_module_worker([:ok2], restart: :transient)
      {:ok, pid} = DynamicSupervisor.start_link(strategy: :one_for_one)

      assert {:ok, child_pid} = DynamicSupervisor.start_child(pid, child)
      assert_kill(child_pid, :shutdown)
      assert %{workers: 0, active: 0} = DynamicSupervisor.count_children(pid)

      assert {:ok, child_pid} = DynamicSupervisor.start_child(pid, child)
      assert_kill(child_pid, {:shutdown, :signal})
      assert %{workers: 0, active: 0} = DynamicSupervisor.count_children(pid)

      assert {:ok, child_pid} = DynamicSupervisor.start_child(pid, child)
      assert_kill(child_pid, :whatever)
      assert %{workers: 1, active: 1} = DynamicSupervisor.count_children(pid)
    end

    test "permanent child is restarted regardless of reason" do
      child = current_module_worker([:ok2], restart: :permanent)
      {:ok, pid} = DynamicSupervisor.start_link(strategy: :one_for_one, max_restarts: 100_000)

      assert {:ok, child_pid} = DynamicSupervisor.start_child(pid, child)
      assert_kill(child_pid, :shutdown)
      assert %{workers: 1, active: 1} = DynamicSupervisor.count_children(pid)

      assert {:ok, child_pid} = DynamicSupervisor.start_child(pid, child)
      assert_kill(child_pid, {:shutdown, :signal})
      assert %{workers: 2, active: 2} = DynamicSupervisor.count_children(pid)

      assert {:ok, child_pid} = DynamicSupervisor.start_child(pid, child)
      assert_kill(child_pid, :whatever)
      assert %{workers: 3, active: 3} = DynamicSupervisor.count_children(pid)
    end

    test "child is restarted with different values" do
      {:ok, pid} = DynamicSupervisor.start_link(strategy: :one_for_one, max_restarts: 100_000)

      assert {:ok, child1} =
               DynamicSupervisor.start_child(pid, current_module_worker([:restart, :ok2]))

      assert [{:undefined, ^child1, :worker, [DynamicSupervisorTest]}] =
               DynamicSupervisor.which_children(pid)

      assert_kill(child1, :shutdown)
      assert %{workers: 1, active: 1} = DynamicSupervisor.count_children(pid)

      assert {:ok, child2} =
               DynamicSupervisor.start_child(pid, current_module_worker([:restart, :ok3]))

      assert [
               {:undefined, _, :worker, [DynamicSupervisorTest]},
               {:undefined, ^child2, :worker, [DynamicSupervisorTest]}
             ] = DynamicSupervisor.which_children(pid)

      assert_kill(child2, :shutdown)
      assert %{workers: 2, active: 2} = DynamicSupervisor.count_children(pid)

      assert {:ok, child3} =
               DynamicSupervisor.start_child(pid, current_module_worker([:restart, :ignore]))

      assert [
               {:undefined, _, :worker, [DynamicSupervisorTest]},
               {:undefined, _, :worker, [DynamicSupervisorTest]},
               {:undefined, _, :worker, [DynamicSupervisorTest]}
             ] = DynamicSupervisor.which_children(pid)

      assert_kill(child3, :shutdown)
      assert %{workers: 2, active: 2} = DynamicSupervisor.count_children(pid)

      assert {:ok, child4} =
               DynamicSupervisor.start_child(pid, current_module_worker([:restart, :error]))

      assert [
               {:undefined, _, :worker, [DynamicSupervisorTest]},
               {:undefined, _, :worker, [DynamicSupervisorTest]},
               {:undefined, _, :worker, [DynamicSupervisorTest]}
             ] = DynamicSupervisor.which_children(pid)

      assert_kill(child4, :shutdown)
      assert %{workers: 3, active: 2} = DynamicSupervisor.count_children(pid)

      assert {:ok, child5} =
               DynamicSupervisor.start_child(pid, current_module_worker([:restart, :unknown]))

      assert [
               {:undefined, _, :worker, [DynamicSupervisorTest]},
               {:undefined, _, :worker, [DynamicSupervisorTest]},
               {:undefined, :restarting, :worker, [DynamicSupervisorTest]},
               {:undefined, _, :worker, [DynamicSupervisorTest]}
             ] = DynamicSupervisor.which_children(pid)

      assert_kill(child5, :shutdown)
      assert %{workers: 4, active: 2} = DynamicSupervisor.count_children(pid)
    end

    test "restarting on init children counted in max_children" do
      child = current_module_worker([:restart, :error], restart: :permanent)
      opts = [strategy: :one_for_one, max_children: 1, max_restarts: 100_000]
      {:ok, pid} = DynamicSupervisor.start_link(opts)

      assert {:ok, child_pid} = DynamicSupervisor.start_child(pid, child)
      assert_kill(child_pid, :shutdown)
      assert %{workers: 1, active: 0} = DynamicSupervisor.count_children(pid)

      child = current_module_worker([:restart, :ok2], restart: :permanent)
      assert {:error, :max_children} = DynamicSupervisor.start_child(pid, child)
    end

    test "restarting on exit children counted in max_children" do
      child = current_module_worker([:ok2], restart: :permanent)
      opts = [strategy: :one_for_one, max_children: 1, max_restarts: 100_000]
      {:ok, pid} = DynamicSupervisor.start_link(opts)

      assert {:ok, child_pid} = DynamicSupervisor.start_child(pid, child)
      assert_kill(child_pid, :shutdown)
      assert %{workers: 1, active: 1} = DynamicSupervisor.count_children(pid)

      child = current_module_worker([:ok2], restart: :permanent)
      assert {:error, :max_children} = DynamicSupervisor.start_child(pid, child)
    end

    test "restarting a child with extra_arguments successfully restarts child" do
      parent = self()

      fun = fn ->
        send(parent, :from_child)
        :timer.sleep(:infinity)
      end

      {:ok, sup} = DynamicSupervisor.start_link(strategy: :one_for_one, extra_arguments: [fun])
      child = %{id: Task, restart: :transient, start: {Task, :start_link, []}}

      assert {:ok, child} = DynamicSupervisor.start_child(sup, child)
      assert is_pid(child)
      assert_receive :from_child
      assert %{active: 1, workers: 1} = DynamicSupervisor.count_children(sup)
      assert_kill(child, :oops)
      assert_receive :from_child
      assert %{workers: 1, active: 1} = DynamicSupervisor.count_children(sup)
    end

    test "child is restarted when trying again" do
      child = current_module_worker([:try_again, self()], restart: :permanent)
      {:ok, pid} = DynamicSupervisor.start_link(strategy: :one_for_one, max_restarts: 2)

      assert {:ok, child_pid} = DynamicSupervisor.start_child(pid, child)
      assert_received {:try_again, true}
      assert_kill(child_pid, :shutdown)
      assert_receive {:try_again, false}
      assert_receive {:try_again, true}
      assert %{workers: 1, active: 1} = DynamicSupervisor.count_children(pid)
    end

    test "child triggers maximum restarts" do
      Process.flag(:trap_exit, true)
      child = current_module_worker([:restart, :error], restart: :permanent)
      {:ok, pid} = DynamicSupervisor.start_link(strategy: :one_for_one, max_restarts: 1)

      assert {:ok, child_pid} = DynamicSupervisor.start_child(pid, child)
      assert_kill(child_pid, :shutdown)
      assert_receive {:EXIT, ^pid, :shutdown}
    end

    test "child triggers maximum intensity when trying again" do
      Process.flag(:trap_exit, true)
      child = current_module_worker([:restart, :error], restart: :permanent)
      {:ok, pid} = DynamicSupervisor.start_link(strategy: :one_for_one, max_restarts: 10)

      assert {:ok, child_pid} = DynamicSupervisor.start_child(pid, child)
      assert_kill(child_pid, :shutdown)
      assert_receive {:EXIT, ^pid, :shutdown}
    end

    def start_link(:ok3), do: {:ok, spawn_link(fn -> :timer.sleep(:infinity) end), :extra}
    def start_link(:ok2), do: {:ok, spawn_link(fn -> :timer.sleep(:infinity) end)}
    def start_link(:error), do: {:error, :found}
    def start_link(:ignore), do: :ignore
    def start_link(:unknown), do: :unknown

    def start_link(:non_local, :throw), do: throw(:oops)
    def start_link(:non_local, :error), do: raise("oops")
    def start_link(:non_local, :exit), do: exit(:oops)

    def start_link(:try_again, notify) do
      if Process.get(:try_again) do
        Process.put(:try_again, false)
        send(notify, {:try_again, false})
        {:error, :try_again}
      else
        Process.put(:try_again, true)
        send(notify, {:try_again, true})
        start_link(:ok2)
      end
    end

    def start_link(:restart, value) do
      if Process.get({:restart, value}) do
        start_link(value)
      else
        Process.put({:restart, value}, true)
        start_link(:ok2)
      end
    end
  end

  describe "terminate/2" do
    test "terminates children with brutal kill" do
      Process.flag(:trap_exit, true)
      {:ok, sup} = DynamicSupervisor.start_link(strategy: :one_for_one)

      child = sleepy_worker(shutdown: :brutal_kill)
      assert {:ok, child1} = DynamicSupervisor.start_child(sup, child)
      assert {:ok, child2} = DynamicSupervisor.start_child(sup, child)
      assert {:ok, child3} = DynamicSupervisor.start_child(sup, child)

      Process.monitor(child1)
      Process.monitor(child2)
      Process.monitor(child3)
      assert_kill(sup, :shutdown)
      assert_receive {:DOWN, _, :process, ^child1, :killed}
      assert_receive {:DOWN, _, :process, ^child2, :killed}
      assert_receive {:DOWN, _, :process, ^child3, :killed}
    end

    test "terminates children with infinity shutdown" do
      Process.flag(:trap_exit, true)
      {:ok, sup} = DynamicSupervisor.start_link(strategy: :one_for_one)

      child = sleepy_worker(shutdown: :infinity)
      assert {:ok, child1} = DynamicSupervisor.start_child(sup, child)
      assert {:ok, child2} = DynamicSupervisor.start_child(sup, child)
      assert {:ok, child3} = DynamicSupervisor.start_child(sup, child)

      Process.monitor(child1)
      Process.monitor(child2)
      Process.monitor(child3)
      assert_kill(sup, :shutdown)
      assert_receive {:DOWN, _, :process, ^child1, :shutdown}
      assert_receive {:DOWN, _, :process, ^child2, :shutdown}
      assert_receive {:DOWN, _, :process, ^child3, :shutdown}
    end

    test "terminates children with infinity shutdown and abnormal reason" do
      Process.flag(:trap_exit, true)
      {:ok, sup} = DynamicSupervisor.start_link(strategy: :one_for_one)

      fun = fn ->
        Process.flag(:trap_exit, true)
        receive(do: (_ -> exit({:shutdown, :oops})))
      end

      child = Supervisor.child_spec({Task, fun}, shutdown: :infinity)
      assert {:ok, child1} = DynamicSupervisor.start_child(sup, child)
      assert {:ok, child2} = DynamicSupervisor.start_child(sup, child)
      assert {:ok, child3} = DynamicSupervisor.start_child(sup, child)

      Process.monitor(child1)
      Process.monitor(child2)
      Process.monitor(child3)
      assert_kill(sup, :shutdown)
      assert_receive {:DOWN, _, :process, ^child1, {:shutdown, :oops}}
      assert_receive {:DOWN, _, :process, ^child2, {:shutdown, :oops}}
      assert_receive {:DOWN, _, :process, ^child3, {:shutdown, :oops}}
    end

    test "terminates children with integer shutdown" do
      Process.flag(:trap_exit, true)
      {:ok, sup} = DynamicSupervisor.start_link(strategy: :one_for_one)

      child = sleepy_worker(shutdown: 1000)
      assert {:ok, child1} = DynamicSupervisor.start_child(sup, child)
      assert {:ok, child2} = DynamicSupervisor.start_child(sup, child)
      assert {:ok, child3} = DynamicSupervisor.start_child(sup, child)

      Process.monitor(child1)
      Process.monitor(child2)
      Process.monitor(child3)
      assert_kill(sup, :shutdown)
      assert_receive {:DOWN, _, :process, ^child1, :shutdown}
      assert_receive {:DOWN, _, :process, ^child2, :shutdown}
      assert_receive {:DOWN, _, :process, ^child3, :shutdown}
    end

    test "terminates children with integer shutdown and abnormal reason" do
      Process.flag(:trap_exit, true)
      {:ok, sup} = DynamicSupervisor.start_link(strategy: :one_for_one)

      fun = fn ->
        Process.flag(:trap_exit, true)
        receive(do: (_ -> exit({:shutdown, :oops})))
      end

      child = Supervisor.child_spec({Task, fun}, shutdown: 1000)
      assert {:ok, child1} = DynamicSupervisor.start_child(sup, child)
      assert {:ok, child2} = DynamicSupervisor.start_child(sup, child)
      assert {:ok, child3} = DynamicSupervisor.start_child(sup, child)

      Process.monitor(child1)
      Process.monitor(child2)
      Process.monitor(child3)
      assert_kill(sup, :shutdown)
      assert_receive {:DOWN, _, :process, ^child1, {:shutdown, :oops}}
      assert_receive {:DOWN, _, :process, ^child2, {:shutdown, :oops}}
      assert_receive {:DOWN, _, :process, ^child3, {:shutdown, :oops}}
    end

    test "terminates children with expired integer shutdown" do
      Process.flag(:trap_exit, true)
      {:ok, sup} = DynamicSupervisor.start_link(strategy: :one_for_one)

      fun = fn ->
        :timer.sleep(:infinity)
      end

      tmt = fn ->
        Process.flag(:trap_exit, true)
        :timer.sleep(:infinity)
      end

      child_fun = Supervisor.child_spec({Task, fun}, shutdown: 1)
      child_tmt = Supervisor.child_spec({Task, tmt}, shutdown: 1)
      assert {:ok, child1} = DynamicSupervisor.start_child(sup, child_fun)
      assert {:ok, child2} = DynamicSupervisor.start_child(sup, child_tmt)
      assert {:ok, child3} = DynamicSupervisor.start_child(sup, child_fun)

      Process.monitor(child1)
      Process.monitor(child2)
      Process.monitor(child3)
      assert_kill(sup, :shutdown)
      assert_receive {:DOWN, _, :process, ^child1, :shutdown}
      assert_receive {:DOWN, _, :process, ^child2, :killed}
      assert_receive {:DOWN, _, :process, ^child3, :shutdown}
    end

    test "terminates children with permanent restart and normal reason" do
      Process.flag(:trap_exit, true)
      {:ok, sup} = DynamicSupervisor.start_link(strategy: :one_for_one)

      fun = fn ->
        Process.flag(:trap_exit, true)
        receive(do: (_ -> exit(:normal)))
      end

      child = Supervisor.child_spec({Task, fun}, shutdown: :infinity, restart: :permanent)
      assert {:ok, child1} = DynamicSupervisor.start_child(sup, child)
      assert {:ok, child2} = DynamicSupervisor.start_child(sup, child)
      assert {:ok, child3} = DynamicSupervisor.start_child(sup, child)

      Process.monitor(child1)
      Process.monitor(child2)
      Process.monitor(child3)
      assert_kill(sup, :shutdown)
      assert_receive {:DOWN, _, :process, ^child1, :normal}
      assert_receive {:DOWN, _, :process, ^child2, :normal}
      assert_receive {:DOWN, _, :process, ^child3, :normal}
    end

    test "terminates with mixed children" do
      Process.flag(:trap_exit, true)
      {:ok, sup} = DynamicSupervisor.start_link(strategy: :one_for_one)

      assert {:ok, child1} =
               DynamicSupervisor.start_child(sup, sleepy_worker(shutdown: :infinity))

      assert {:ok, child2} =
               DynamicSupervisor.start_child(sup, sleepy_worker(shutdown: :brutal_kill))

      Process.monitor(child1)
      Process.monitor(child2)
      assert_kill(sup, :shutdown)
      assert_receive {:DOWN, _, :process, ^child1, :shutdown}
      assert_receive {:DOWN, _, :process, ^child2, :killed}
    end
  end

  describe "terminate_child/2" do
    test "terminates child with brutal kill" do
      {:ok, sup} = DynamicSupervisor.start_link(strategy: :one_for_one)

      child = sleepy_worker(shutdown: :brutal_kill)
      assert {:ok, child_pid} = DynamicSupervisor.start_child(sup, child)

      Process.monitor(child_pid)
      assert :ok = DynamicSupervisor.terminate_child(sup, child_pid)
      assert_receive {:DOWN, _, :process, ^child_pid, :killed}

      assert {:error, :not_found} = DynamicSupervisor.terminate_child(sup, child_pid)
      assert %{workers: 0, active: 0} = DynamicSupervisor.count_children(sup)
    end

    test "terminates child with integer shutdown" do
      {:ok, sup} = DynamicSupervisor.start_link(strategy: :one_for_one)

      child = sleepy_worker(shutdown: 1000)
      assert {:ok, child_pid} = DynamicSupervisor.start_child(sup, child)

      Process.monitor(child_pid)
      assert :ok = DynamicSupervisor.terminate_child(sup, child_pid)
      assert_receive {:DOWN, _, :process, ^child_pid, :shutdown}

      assert {:error, :not_found} = DynamicSupervisor.terminate_child(sup, child_pid)
      assert %{workers: 0, active: 0} = DynamicSupervisor.count_children(sup)
    end

    test "terminates restarting child" do
      {:ok, sup} = DynamicSupervisor.start_link(strategy: :one_for_one, max_restarts: 100_000)

      child = current_module_worker([:restart, :error], restart: :permanent)
      assert {:ok, child_pid} = DynamicSupervisor.start_child(sup, child)
      assert_kill(child_pid, :shutdown)
      assert :ok = DynamicSupervisor.terminate_child(sup, child_pid)

      assert {:error, :not_found} = DynamicSupervisor.terminate_child(sup, child_pid)
      assert %{workers: 0, active: 0} = DynamicSupervisor.count_children(sup)
    end
  end

  defp sleepy_worker(opts \\ []) do
    mfa = {Task, :start_link, [:timer, :sleep, [:infinity]]}
    Supervisor.child_spec(%{id: Task, start: mfa}, opts)
  end

  defp current_module_worker(args, opts \\ []) do
    Supervisor.child_spec(%{id: __MODULE__, start: {__MODULE__, :start_link, args}}, opts)
  end

  defp assert_kill(pid, reason) do
    ref = Process.monitor(pid)
    Process.exit(pid, reason)
    assert_receive {:DOWN, ^ref, _, _, _}
  end
end
