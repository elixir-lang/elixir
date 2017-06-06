defmodule Logger.TranslatorTest do
  use Logger.Case

  defmodule MyGenServer do
    use GenServer

    def handle_cast(:error, _) do
      raise "oops"
    end

    def handle_call(:error, _, _) do
      raise "oops"
    end
    def handle_call(:error_on_down, {pid, _}, _) do
      mon = Process.monitor(pid)
      assert_receive {:DOWN, ^mon, _, _, _}
      raise "oops"
    end
  end

  defmodule MyGenEvent do
    @behaviour :gen_event

    def init(args) do
      {:ok, args}
    end

    def handle_event(_event, state) do
      {:ok, state}
    end

    def handle_call(:error, _) do
      raise "oops"
    end

    def handle_info(_msg, state) do
      {:ok, state}
    end

    def code_change(_old_vsn, state, _extra) do
      {:ok, state}
    end

    def terminate(_reason, _state) do
      :ok
    end
  end

  defmodule MyBridge do
    @behaviour :supervisor_bridge

    def init(reason) do
      {:ok, pid} = Task.start_link(Kernel, :exit, [reason])
      {:ok, pid, pid}
    end

    def terminate(_reason, pid) do
      Process.exit(pid, :shutdown)
    end
  end

  setup_all do
    sasl_reports? = Application.get_env(:logger, :handle_sasl_reports, false)
    Application.put_env(:logger, :handle_sasl_reports, true)

    # Restart the app but change the level before to avoid warnings
    level = Logger.level()
    Logger.configure(level: :error)
    Logger.App.stop()
    Application.start(:logger)
    Logger.configure(level: level)

    on_exit(fn() ->
      Application.put_env(:logger, :handle_sasl_reports, sasl_reports?)
      Logger.App.stop()
      Application.start(:logger)
    end)
  end

  test "translates GenServer crashes" do
    {:ok, pid} = GenServer.start(MyGenServer, :ok)

    assert capture_log(:info, fn ->
      catch_exit(GenServer.call(pid, :error))
    end) =~ """
    [error] GenServer #{inspect pid} terminating
    ** (RuntimeError) oops
    """
  end

  test "translates GenServer crashes with custom inspect options" do
    {:ok, pid} = GenServer.start(MyGenServer, List.duplicate(:ok, 1000))
    Application.put_env(:logger, :translator_inspect_opts, [limit: 3])

    assert capture_log(:debug, fn ->
      catch_exit(GenServer.call(pid, :error))
    end) =~ """
    [:ok, :ok, :ok, ...]
    """
  after
    Application.put_env(:logger, :translator_inspect_opts, [])
  end

  # TODO: Remove this check once we depend only on 20
  if :erlang.system_info(:otp_release) >= '20' do
    test "translates GenServer crashes on debug" do
      {:ok, pid} = GenServer.start(MyGenServer, :ok)

      assert capture_log(:debug, fn ->
        catch_exit(GenServer.call(pid, :error))
      end) =~ ~r"""
      \[error\] GenServer #PID<\d+\.\d+\.\d+> terminating
      \*\* \(RuntimeError\) oops
      .*
      Last message \(from #PID<\d+\.\d+\.\d+>\): :error
      State: :ok
      Client #PID<\d+\.\d+\.\d+> is alive
      .*
      """s
    end

    test "translates GenServer crashes with named client on debug" do
      {:ok, pid} = GenServer.start(MyGenServer, :ok)

      assert capture_log(:debug, fn ->
        Process.register(self(), :named_client)
        catch_exit(GenServer.call(pid, :error))
      end) =~ ~r"""
      \[error\] GenServer #PID<\d+\.\d+\.\d+> terminating
      \*\* \(RuntimeError\) oops
      .*
      Last message \(from :named_client\): :error
      State: :ok
      Client :named_client is alive
      .*
      """s
    end

    test "translates GenServer crashes with dead client on debug" do
      {:ok, pid} = GenServer.start(MyGenServer, :ok)

      assert capture_log(:debug, fn ->
        mon = Process.monitor(pid)
        spawn_link(fn() ->
          catch_exit(GenServer.call(pid, :error_on_down, 0))
        end)
        assert_receive {:DOWN, ^mon, _, _, _}
      end) =~ ~r"""
      \[error\] GenServer #PID<\d+\.\d+\.\d+> terminating
      \*\* \(RuntimeError\) oops
      .*
      Last message \(from #PID<\d+\.\d+\.\d+>\): :error_on_down
      State: :ok
      Client #PID<\d+\.\d+\.\d+> is dead
      """s
    end
  else
    test "translates GenServer crashes on debug" do
      {:ok, pid} = GenServer.start(MyGenServer, :ok)

      assert capture_log(:debug, fn ->
        catch_exit(GenServer.call(pid, :error))
      end) =~ ~r"""
      \[error\] GenServer #PID<\d+\.\d+\.\d+> terminating
      \*\* \(RuntimeError\) oops
      .*
      Last message: :error
      State: :ok
      """s
    end
  end

  test "translates GenServer crashes with no client" do
    {:ok, pid} = GenServer.start(MyGenServer, :ok)

    assert capture_log(:debug, fn ->
      mon = Process.monitor(pid)
      GenServer.cast(pid, :error)
      assert_receive {:DOWN, ^mon, _, _, _}
    end) =~ ~r"""
    \[error\] GenServer #PID<\d+\.\d+\.\d+> terminating
    \*\* \(RuntimeError\) oops
    .*
    Last message: {:"\$gen_cast", :error}
    State: :ok
    """s
  end

  test "translates GenServer crashes with no client on debug" do
    {:ok, pid} = GenServer.start(MyGenServer, :ok)

    refute capture_log(:debug, fn ->
      mon = Process.monitor(pid)
      GenServer.cast(pid, :error)
      assert_receive {:DOWN, ^mon, _, _, _}
    end) =~ "Client"
  end

  test "translates :gen_event crashes" do
    {:ok, pid} = :gen_event.start()
    :ok = :gen_event.add_handler(pid, MyGenEvent, :ok)

    assert capture_log(:info, fn ->
      :gen_event.call(pid, MyGenEvent, :error)
    end) =~ """
    [error] GenEvent handler Logger.TranslatorTest.MyGenEvent installed in #{inspect pid} terminating
    ** (RuntimeError) oops
    """
  end

  test "translates :gen_event crashes on debug" do
    {:ok, pid} = :gen_event.start()
    :ok = :gen_event.add_handler(pid, MyGenEvent, :ok)

    assert capture_log(:debug, fn ->
      :gen_event.call(pid, MyGenEvent, :error)
    end) =~ ~r"""
    \[error\] GenEvent handler Logger.TranslatorTest.MyGenEvent installed in #PID<\d+\.\d+\.\d+> terminating
    \*\* \(RuntimeError\) oops
    .*
    Last message: :error
    State: :ok
    """s
  end

  test "translates Task crashes" do
    {:ok, pid} = Task.start_link(__MODULE__, :task, [self()])

    assert capture_log(fn ->
      ref = Process.monitor(pid)
      send(pid, :go)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ ~r"""
    \[error\] Task #PID<\d+\.\d+\.\d+> started from #PID<\d+\.\d+\.\d+> terminating
    \*\* \(RuntimeError\) oops
    .*
    Function: &Logger.TranslatorTest.task\/1
        Args: \[#PID<\d+\.\d+\.\d+>\]
    """s
  end

  test "translates Task async_stream crashes with neighbour" do
    fun = fn -> Task.async_stream([:oops], :erlang, :error, []) |> Enum.to_list() end
    {:ok, pid} = Task.start(__MODULE__, :task, [self(), fun])

    assert capture_log(:debug, fn ->
      ref = Process.monitor(pid)
      send(pid, :go)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ ~r"""
    Neighbours:
        #{inspect pid}
            Initial Call: Logger\.TranslatorTest\.task/2
    """
  end

  test "translates Task undef module crash" do
    assert capture_log(fn ->
      {:ok, pid} = Task.start(:module_does_not_exist, :undef, [])
      ref = Process.monitor(pid)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ ~r"""
    \[error\] Task #PID<\d+\.\d+\.\d+> started from #PID<\d+\.\d+\.\d+> terminating
    \*\* \(UndefinedFunctionError\) function :module_does_not_exist.undef/0 is undefined \(module :module_does_not_exist is not available\)
    .*
    Function: &:module_does_not_exist.undef/0
        Args: \[\]
    """s
  end

  test "translates Task undef function crash" do
    assert capture_log(fn ->
      {:ok, pid} = Task.start(__MODULE__, :undef, [])
      ref = Process.monitor(pid)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ ~r"""
    \[error\] Task #PID<\d+\.\d+\.\d+> started from #PID<\d+\.\d+\.\d+> terminating
    \*\* \(UndefinedFunctionError\) function Logger.TranslatorTest.undef/0 is undefined or private
    .*
    Function: &Logger.TranslatorTest.undef/0
        Args: \[\]
    """s
  end

  test "translates Task raising ErlangError" do
    assert capture_log(fn ->
      exception =
        try do
          :erlang.error(:foo)
        rescue
         x ->
           x
        end
      {:ok, pid} = Task.start(:erlang, :error, [exception])
      ref = Process.monitor(pid)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ ~r"""
    \[error\] Task #PID<\d+\.\d+\.\d+> started from #PID<\d+\.\d+\.\d+> terminating
    \*\* \(ErlangError\) Erlang error: :foo
    .*
    Function: &:erlang\.error/1
        Args: \[%ErlangError{.*}\]
    """s
  end

  test "translates Task raising Erlang badarg error" do
    assert capture_log(fn ->
      {:ok, pid} = Task.start(:erlang, :error, [:badarg])
      ref = Process.monitor(pid)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ ~r"""
    \[error\] Task #PID<\d+\.\d+\.\d+> started from #PID<\d+\.\d+\.\d+> terminating
    \*\* \(ArgumentError\) argument error
    .*
    Function: &:erlang\.error/1
        Args: \[:badarg\]
    """s
  end

  test "translates Task exiting abnormally" do
    assert capture_log(fn ->
      {:ok, pid} = Task.start(:erlang, :exit, [:abnormal])
      ref = Process.monitor(pid)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ ~r"""
    \[error\] Task #PID<\d+\.\d+\.\d+> started from #PID<\d+\.\d+\.\d+> terminating
    \*\* \(stop\) :abnormal
    .*
    Function: &:erlang\.exit/1
        Args: \[:abnormal\]
    """s
  end

  test "translates application stop" do
    assert capture_log(fn ->
      :ok = Application.start(:eex)
      Application.stop(:eex)
    end) =~ """
    Application eex exited: :stopped
    """
  end

  test "translates application start" do
    assert capture_log(fn ->
      Application.start(:eex)
      Application.stop(:eex)
    end) =~ """
    Application eex started at #{inspect(node())}
    """
  end

  test "translates Process crashes" do
    assert capture_log(:info, fn ->
      {_, ref} = spawn_monitor(fn() -> raise "oops" end)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
      # Even though the monitor has been received the emulator may not have
      # sent the message to the error logger
      Process.sleep(200)
    end) =~ ~r"""
    \[error\] Process #PID<\d+\.\d+\.\d+>\ raised an exception
    \*\* \(RuntimeError\) oops
    """
  end

  test "translates :proc_lib crashes" do
    {:ok, pid} = Task.start_link(__MODULE__, :task, [self()])

    assert capture_log(:info, fn ->
      ref = Process.monitor(pid)

      send(pid, :go)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ ~r"""
    \[error\] Process #PID<\d+\.\d+\.\d+> terminating
    \*\* \(exit\) an exception was raised:
        \*\* \(RuntimeError\) oops
    .*
    Initial Call: Logger.TranslatorTest.task/1
    Ancestors: \[#PID<\d+\.\d+\.\d+>\]
    """s
  end

  test "translates :proc_lib crashes with name" do
    {:ok, pid} = Task.start_link(__MODULE__, :task,
                                 [self(), fn() ->
                                            Process.register(self(), __MODULE__)
                                            raise "oops"
                                          end])

    assert capture_log(:info, fn ->
      ref = Process.monitor(pid)
      send(pid, :message)
      send(pid, :go)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ ~r"""
    \[error\] Process Logger.TranslatorTest \(#PID<\d+\.\d+\.\d+>\) terminating
    \*\* \(exit\) an exception was raised:
        \*\* \(RuntimeError\) oops
    .*
    Initial Call: Logger.TranslatorTest.task/2
    Ancestors: \[#PID<\d+\.\d+\.\d+>\]
    """s
  end

  test "translates :proc_lib crashes without initial call" do
    {:ok, pid} = Task.start_link(__MODULE__, :task,
                                 [self(), fn() ->
                                            Process.delete(:"$initial_call")
                                            raise "oops"
                                          end])

    assert capture_log(:info, fn ->
      ref = Process.monitor(pid)
      send(pid, :message)
      send(pid, :go)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ ~r"""
    \[error\] Process #PID<\d+\.\d+\.\d+> terminating
    \*\* \(exit\) an exception was raised:
        \*\* \(RuntimeError\) oops
    .*
    Ancestors: \[#PID<\d+\.\d+\.\d+>\]
    """s
  end

  test "translates :proc_lib crashes with neighbour" do
    {:ok, pid} = Task.start_link(__MODULE__, :sub_task, [self()])

    assert capture_log(:info, fn ->
      ref = Process.monitor(pid)
      send(pid, :message)
      send(pid, :go)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ ~r"""
    Ancestors: \[#PID<\d+\.\d+\.\d+>\]
    Neighbours:
        #PID<\d+\.\d+\.\d+>
            Initial Call: Logger.TranslatorTest.sleep/1
            Current Call: Logger.TranslatorTest.sleep/1
            Ancestors: \[#PID<\d+\.\d+\.\d+>, #PID<\d+\.\d+\.\d+>\]
    """
  end

  test "translates :proc_lib crashes with neighbour with name" do
    {:ok, pid} = Task.start_link(__MODULE__, :sub_task,
                                 [self(), fn(pid2) ->
                                            Process.register(pid2, __MODULE__)
                                            raise "oops"
                                          end])

    assert capture_log(:info, fn ->
      ref = Process.monitor(pid)
      send(pid, :message)
      send(pid, :go)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ ~r"""
    Ancestors: \[#PID<\d+\.\d+\.\d+>\]
    Neighbours:
        Logger.TranslatorTest \(#PID<\d+\.\d+\.\d+>\)
            Initial Call: Logger.TranslatorTest.sleep/1
            Current Call: Logger.TranslatorTest.sleep/1
            Ancestors: \[#PID<\d+\.\d+\.\d+>, #PID<\d+\.\d+\.\d+>\]
    """
  end

  test "translates :proc_lib crashes on debug" do
    {:ok, pid} = Task.start_link(__MODULE__, :task, [self()])

    assert capture_log(:debug, fn ->
      ref = Process.monitor(pid)
      send(pid, :message)
      send(pid, :go)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ ~r"""
    Ancestors: \[#PID<\d+\.\d+\.\d+>\](?:
    Message Queue Length: 1(?#TODO: Require once depend on 20)|)
    Messages: \[:message\]
    Links: \[\]
    Dictionary: \[\]
    Trapping Exits: false
    Status: :running
    Heap Size: \d+
    Stack Size: \d+
    Reductions: \d+
    """
  end

  test "translates :proc_lib crashes with neighbour on debug" do
    {:ok, pid} = Task.start_link(__MODULE__, :sub_task, [self()])

    assert capture_log(:debug, fn ->
      ref = Process.monitor(pid)
      send(pid, :message)
      send(pid, :go)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ ~r"""
            Ancestors: \[#PID<\d+\.\d+\.\d+>, #PID<\d+\.\d+\.\d+>\](?:
            Message Queue Length: 0|
            Messages: \[\](?# TODO: Remove once depend on 20))
            Links: \[#PID<\d+\.\d+\.\d+>\](?:|
            Dictionary: \[\](?# TODO: Remove once depend on 20))
            Trapping Exits: false
            Status: :waiting
            Heap Size: \d+
            Stack Size: \d+
            Reductions: \d+(?:
            Current Stacktrace:
                test/logger/translator_test.exs:\d+: Logger.TranslatorTest.sleep/1(?#TODO: Require once depend on 20)|)
    """
  end

  test "translates Supervisor progress" do
    {:ok, pid} = Supervisor.start_link([], [strategy: :one_for_one])
    assert capture_log(:info, fn ->
      ref = Process.monitor(pid)
      Supervisor.start_child(pid, worker(Task, [__MODULE__, :sleep, [self()]]))
      Process.exit(pid, :normal)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ ~r"""
    \[info\]  Child Task of Supervisor #PID<\d+\.\d+\.\d+> \(Supervisor\.Default\) started
    Pid: #PID<\d+\.\d+\.\d+>
    Start Call: Task.start_link\(Logger.TranslatorTest, :sleep, \[#PID<\d+\.\d+\.\d+>\]\)
    """
  end

  test "translates Supervisor progress with name" do
    {:ok, pid} = Supervisor.start_link([],
      [strategy: :one_for_one, name: __MODULE__])
    assert capture_log(:info, fn ->
      ref = Process.monitor(pid)
      Supervisor.start_child(pid, worker(Task, [__MODULE__, :sleep, [self()]]))
      Process.exit(pid, :normal)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ ~r"""
    \[info\]  Child Task of Supervisor Logger.TranslatorTest started
    """

    {:ok, pid} = Supervisor.start_link([],
      [strategy: :one_for_one, name: {:global, __MODULE__}])
    assert capture_log(:info, fn ->
      ref = Process.monitor(pid)
      Supervisor.start_child(pid, worker(Task, [__MODULE__, :sleep, [self()]]))
      Process.exit(pid, :normal)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ ~r"""
    \[info\]  Child Task of Supervisor Logger.TranslatorTest started
    """

    {:ok, pid} = Supervisor.start_link([],
      [strategy: :one_for_one, name: {:via, :global, __MODULE__}])
    assert capture_log(:info, fn ->
      ref = Process.monitor(pid)
      Supervisor.start_child(pid, worker(Task, [__MODULE__, :sleep, [self()]]))
      Process.exit(pid, :normal)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ ~r"""
    \[info\]  Child Task of Supervisor Logger.TranslatorTest started
    """
  end

  test "translates Supervisor progress on debug" do
    {:ok, pid} = Supervisor.start_link([], [strategy: :one_for_one])
    assert capture_log(:debug, fn ->
      ref = Process.monitor(pid)
      Supervisor.start_child(pid, worker(Task, [__MODULE__, :sleep, [self()]]))
      Process.exit(pid, :normal)
      receive do: ({:DOWN, ^ref, _, _, _} -> :ok)
    end) =~ ~r"""
    Start Call: Task.start_link\(Logger.TranslatorTest, :sleep, \[#PID<\d+\.\d+\.\d+>\]\)
    Restart: :permanent
    Shutdown: 5000
    Type: :worker
    """
  end

  test "translates Supervisor reports start error" do
    assert capture_log(:info, fn ->
      trap = Process.flag(:trap_exit, true)
      children = [worker(__MODULE__, [], function: :error)]
      Supervisor.start_link(children, strategy: :one_for_one)
      receive do: ({:EXIT, _, {:shutdown, {:failed_to_start_child, _, _}}} -> :ok)
      Process.flag(:trap_exit, trap)
    end) =~ ~r"""
    \[error\] Child Logger.TranslatorTest of Supervisor #PID<\d+\.\d+\.\d+> \(Supervisor\.Default\) failed to start
    \*\* \(exit\) :stop
    Start Call: Logger.TranslatorTest.error\(\)
    """
  end

  test "translates Supervisor reports start error with raise" do
    assert capture_log(:info, fn ->
      trap = Process.flag(:trap_exit, true)
      children = [worker(__MODULE__, [], function: :undef)]
      Supervisor.start_link(children, strategy: :one_for_one)
      receive do: ({:EXIT, _, {:shutdown, {:failed_to_start_child, _, _}}} -> :ok)
      Process.flag(:trap_exit, trap)
    end) =~ ~r"""
    \[error\] Child Logger.TranslatorTest of Supervisor #PID<\d+\.\d+\.\d+> \(Supervisor\.Default\) failed to start
    \*\* \(exit\) an exception was raised:
        \*\* \(UndefinedFunctionError\) function Logger.TranslatorTest.undef/0 is undefined or private
        .*
    Start Call: Logger.TranslatorTest.undef\(\)
    """s
  end

  test "translates Supervisor reports terminated" do
    assert capture_log(:info, fn ->
      trap = Process.flag(:trap_exit, true)
      children = [worker(Task, [Kernel, :exit, [:stop]])]
      {:ok, pid} = Supervisor.start_link(children, strategy: :one_for_one, max_restarts: 0)
      receive do: ({:EXIT, ^pid, _} -> :ok)
      Process.flag(:trap_exit, trap)
    end) =~ ~r"""
    \[error\] Child Task of Supervisor #PID<\d+\.\d+\.\d+> \(Supervisor\.Default\) terminated
    \*\* \(exit\) :stop
    Pid: #PID<\d+\.\d+\.\d+>
    Start Call: Task.start_link\(Kernel, :exit, \[:stop\]\)
    """
  end

  test "translates Supervisor reports max restarts shutdown" do
    assert capture_log(:info, fn ->
      trap = Process.flag(:trap_exit, true)
      children = [worker(Task, [Kernel, :exit, [:stop]])]
      {:ok, pid} = Supervisor.start_link(children, strategy: :one_for_one, max_restarts: 0)
      receive do: ({:EXIT, ^pid, _} -> :ok)
      Process.flag(:trap_exit, trap)
    end) =~ ~r"""
    \[error\] Child Task of Supervisor #PID<\d+\.\d+\.\d+> \(Supervisor\.Default\) caused shutdown
    \*\* \(exit\) :reached_max_restart_intensity
    Start Call: Task.start_link\(Kernel, :exit, \[:stop\]\)
    """
  end

  test "translates Supervisor reports abnormal shutdown" do
    assert capture_log(:info, fn ->
      children = [worker(__MODULE__, [], function: :abnormal)]
      {:ok, pid} = Supervisor.start_link(children, strategy: :one_for_one)
      :ok = Supervisor.terminate_child(pid, __MODULE__)
    end) =~ ~r"""
    \[error\] Child Logger.TranslatorTest of Supervisor #PID<\d+\.\d+\.\d+> \(Supervisor\.Default\) shutdown abnormally
    \*\* \(exit\) :stop
    Pid: #PID<\d+\.\d+\.\d+>
    Start Call: Logger.TranslatorTest.abnormal\(\)
    """
  end

  test "translates Supervisor reports abnormal shutdown on debug" do
    assert capture_log(:debug, fn ->
      children = [worker(__MODULE__, [], function: :abnormal, restart: :permanent, shutdown: 5000)]
      {:ok, pid} = Supervisor.start_link(children, strategy: :one_for_one)
      :ok = Supervisor.terminate_child(pid, __MODULE__)
    end) =~ ~r"""
    \*\* \(exit\) :stop
    Pid: #PID<\d+\.\d+\.\d+>
    Start Call: Logger.TranslatorTest.abnormal\(\)
    Restart: :permanent
    Shutdown: 5000
    Type: :worker
    """
  end

  test "translates Supervisor reports abnormal shutdown in simple_one_for_one" do
    assert capture_log(:info, fn ->
      trap = Process.flag(:trap_exit, true)
      children = [worker(__MODULE__, [], function: :abnormal)]
      {:ok, pid} = Supervisor.start_link(children, strategy: :simple_one_for_one)
      {:ok, _pid2} = Supervisor.start_child(pid, [])
      Process.exit(pid, :normal)
      receive do: ({:EXIT, ^pid, _} -> :ok)
      Process.flag(:trap_exit, trap)
    end) =~ ~r"""
    \[error\] Children Logger.TranslatorTest of Supervisor #PID<\d+\.\d+\.\d+> \(Supervisor\.Default\) shutdown abnormally
    \*\* \(exit\) :stop
    Number: 1
    Start Call: Logger.TranslatorTest.abnormal\(\)
    """
  end

  test "translates :supervisor_bridge progress" do
    assert capture_log(:info, fn ->
      trap = Process.flag(:trap_exit, true)
      {:ok, pid} = :supervisor_bridge.start_link(MyBridge, :normal)
      receive do: ({:EXIT, ^pid, _} -> :ok)
      Process.flag(:trap_exit, trap)
    end) =~ ~r"""
    \[info\]  Child of Supervisor #PID<\d+\.\d+\.\d+> \(Logger\.TranslatorTest\.MyBridge\) started
    Pid: #PID<\d+\.\d+\.\d+>
    Start Call: Logger.TranslatorTest.MyBridge.init\(:normal\)
    """
  end

  test "translates :supervisor_bridge reports" do
    assert capture_log(:info, fn ->
      trap = Process.flag(:trap_exit, true)
      {:ok, pid} = :supervisor_bridge.start_link(MyBridge, :stop)
      receive do: ({:EXIT, ^pid, _} -> :ok)
      Process.flag(:trap_exit, trap)
    end) =~ ~r"""
    \[error\] Child of Supervisor #PID<\d+\.\d+\.\d+> \(Logger\.TranslatorTest\.MyBridge\) terminated
    \*\* \(exit\) :stop
    Pid: #PID<\d+\.\d+\.\d+>
    Start Module: Logger.TranslatorTest.MyBridge
    """
  end

  test "handles :undefined MFA properly" do
    defmodule WeirdFunctionNamesGenServer do
      use GenServer
      def unquote(:"start link")(), do: GenServer.start_link(__MODULE__, [])
      def handle_call(_call, _from, _state), do: raise("oops")
    end

    child_opts = [restart: :temporary, function: :"start link"]
    children = [worker(WeirdFunctionNamesGenServer, [], child_opts)]
    {:ok, sup} = Supervisor.start_link(children, strategy: :simple_one_for_one)

    log = capture_log(:info, fn ->
      {:ok, pid} = Supervisor.start_child(sup, [])
      catch_exit(GenServer.call(pid, :error))
      [] = Supervisor.which_children(sup)
    end)

    assert log =~ ~s(Start Call: Logger.TranslatorTest.WeirdFunctionNamesGenServer."start link"/?)
  after
    :code.purge(WeirdFunctionNamesGenServer)
    :code.delete(WeirdFunctionNamesGenServer)
  end

  def task(parent, fun \\ (fn() -> raise "oops" end)) do
    mon = Process.monitor(parent)
    Process.unlink(parent)
    receive do
      :go ->
        fun.()
      {:DOWN, ^mon, _, _, _} ->
        exit(:shutdown)
    end
  end

  def sub_task(parent, fun \\ (fn(_) -> raise "oops" end)) do
    mon = Process.monitor(parent)
    Process.unlink(parent)
    {:ok, pid} = Task.start_link(__MODULE__, :sleep, [self()])
    receive do: (:sleeping -> :ok)
    receive do
      :go ->
        fun.(pid)
      {:DOWN, ^mon, _, _, _} ->
        exit(:shutdown)
    end
  end

  def sleep(pid) do
    mon = Process.monitor(pid)
    send(pid, :sleeping)
    receive do: ({:DOWN, ^mon, _, _, _} -> exit(:shutdown))
  end

  def error(), do: {:error, :stop}

  def abnormal() do
    :proc_lib.start_link(__MODULE__, :abnormal_init, [])
  end

  def abnormal_init() do
    Process.flag(:trap_exit, true)
    :proc_lib.init_ack({:ok, self()})
    receive do: ({:EXIT, _, _} -> exit(:stop))
  end

  defp worker(name, args, opts \\ []) do
    Enum.into(opts, %{id: name, start: {name, opts[:function] || :start_link, args}})
  end
end
