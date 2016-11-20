Code.require_file "test_helper.exs", __DIR__

defmodule GenEventTest do
  use ExUnit.Case

  defmodule ReplyHandler do
    use GenEvent

    def init(:raise) do
      raise "oops"
    end

    def init({:throw, process}) do
      {:ok, process}
    end

    def init({:raise, _}) do
      raise "oops"
    end

    def init({:swap, {:error, :not_found}}) do
      {:error, :not_found_on_swap}
    end

    def init({:swap, parent}) when is_pid(parent) do
      send parent, :swapped
      {:ok, parent}
    end

    def init({:custom, return}) do
      return
    end

    def init({parent, :hibernate}) do
      {:ok, parent, :hibernate}
    end

    def init({parent, trap}) when is_pid(parent) and is_boolean(trap) do
      Process.flag(:trap_exit, trap)
      {:ok, parent}
    end

    def handle_event(:raise, _parent) do
      raise "oops"
    end

    def handle_event(:hibernate, parent) do
      {:ok, parent, :hibernate}
    end

    def handle_event({:custom, reply}, _parent) do
      reply
    end

    def handle_event(event, parent) do
      send parent, {:event, event}
      {:ok, parent}
    end

    def handle_call(:raise, _parent) do
      raise "oops"
    end

    def handle_call(:hibernate, parent) do
      {:ok, :ok, parent, :hibernate}
    end

    def handle_call({:custom, reply}, _parent) do
      reply
    end

    def handle_call(event, parent) do
      send parent, {:call, event}
      {:ok, :ok, parent}
    end

    def handle_info(:hibernate, parent) do
      {:ok, parent, :hibernate}
    end

    def handle_info(event, parent) do
      send parent, {:info, event}
      {:ok, parent}
    end

    def terminate(:raise, _parent) do
      raise "oops"
    end

    def terminate(:swapped, parent) do
      send parent, {:terminate, :swapped}
      parent
    end

    def terminate(arg, parent) do
      send parent, {:terminate, arg}
    end
  end

  defmodule DefaultHandler do
    use GenEvent
  end

  defmodule Via do
    def register_name(name, pid) do
      Process.register(pid, name)
      :yes
    end

    def whereis_name(name) do
      Process.whereis(name) || :undefined
    end
  end

  test "start/1" do
    assert {:ok, pid} = GenEvent.start()
    assert GenEvent.which_handlers(pid) == []
    assert GenEvent.stop(pid) == :ok

    assert {:ok, pid} = GenEvent.start(name: :my_gen_event_name)
    assert GenEvent.which_handlers(:my_gen_event_name) == []
    assert GenEvent.which_handlers(pid) == []
    assert GenEvent.stop(:my_gen_event_name) == :ok
  end

  test "start_link/1" do
    assert {:ok, pid} = GenEvent.start_link()
    assert GenEvent.which_handlers(pid) == []
    assert GenEvent.stop(pid) == :ok

    assert {:ok, pid} = GenEvent.start_link(name: :my_gen_event_name)
    assert GenEvent.which_handlers(:my_gen_event_name) == []
    assert GenEvent.which_handlers(pid) == []
    assert GenEvent.stop(:my_gen_event_name) == :ok

    assert {:ok, pid} = GenEvent.start_link(name: {:global, :my_gen_event_name})
    assert GenEvent.which_handlers({:global, :my_gen_event_name}) == []
    assert GenEvent.which_handlers(pid) == []
    assert GenEvent.stop({:global, :my_gen_event_name}) == :ok

    assert {:ok, pid} = GenEvent.start_link(name: {:via, Via, :my_gen_event_name})
    assert GenEvent.which_handlers({:via, Via, :my_gen_event_name}) == []
    assert GenEvent.which_handlers(pid) == []
    assert GenEvent.stop({:via, Via, :my_gen_event_name}) == :ok

    assert {:ok, pid} = GenEvent.start_link(name: :my_gen_event_name)
    assert GenEvent.start_link(name: :my_gen_event_name) ==
           {:error, {:already_started, pid}}
    assert GenEvent.stop(pid) == :ok

    assert_raise ArgumentError, ~r"expected :name option to be one of:", fn ->
      GenEvent.start_link(name: "my_gen_event_name")
    end

    assert_raise ArgumentError, ~r"expected :name option to be one of:", fn ->
      GenEvent.start_link(name: {:invalid_tuple, "my_gen_event_name"})
    end

    assert_raise ArgumentError, ~r"expected :name option to be one of:", fn ->
      GenEvent.start_link(name: {:via, "Via", "my_gen_event_name"})
    end
  end

  test "handles exit signals" do
    Process.flag(:trap_exit, true)

    # Terminates on signal from parent when not trapping exits
    {:ok, pid} = GenEvent.start_link()
    :ok = GenEvent.add_handler(pid, ReplyHandler, {self(), false})
    Process.exit(pid, :shutdown)
    assert_receive {:EXIT, ^pid, :shutdown}
    refute_received {:terminate, _}

    # Terminates on signal from parent when trapping exits
    {:ok, pid} = GenEvent.start_link()
    :ok = GenEvent.add_handler(pid, ReplyHandler, {self(), true})
    Process.exit(pid, :shutdown)
    assert_receive {:EXIT, ^pid, :shutdown}
    assert_receive {:terminate, :stop}

    # Terminates on signal not from parent when not trapping exits
    {:ok, pid} = GenEvent.start_link()
    :ok = GenEvent.add_handler(pid, ReplyHandler, {self(), false})
    spawn fn -> Process.exit(pid, :shutdown) end
    assert_receive {:EXIT, ^pid, :shutdown}
    refute_received {:terminate, _}

    # Does not terminate on signal not from parent when trapping exits
    {:ok, pid} = GenEvent.start_link()
    :ok = GenEvent.add_handler(pid, ReplyHandler, {self(), true})
    terminator = spawn fn -> Process.exit(pid, :shutdown) end
    assert_receive {:info, {:EXIT, ^terminator, :shutdown}}
    refute_received {:terminate, _}
  end

  test "hibernates" do
    {:ok, pid} = GenEvent.start()
    :ok = GenEvent.add_handler(pid, ReplyHandler, {self(), :hibernate})
    wait_until fn -> hibernating?(pid) end

    wake_up(pid)
    refute hibernating?(pid)

    :ok = GenEvent.call(pid, ReplyHandler, :hibernate)
    wait_until fn -> hibernating?(pid) end

    wake_up(pid)
    :ok = GenEvent.sync_notify(pid, :hibernate)
    wait_until fn -> hibernating?(pid) end

    GenEvent.stop(pid)
  end

  test "add_handler/3" do
    {:ok, pid} = GenEvent.start()

    assert GenEvent.add_handler(pid, ReplyHandler, {:custom, {:error, :my_error}}) ==
           {:error, :my_error}
    assert GenEvent.add_handler(pid, ReplyHandler, {:custom, :oops}) ==
           {:error, {:bad_return_value, :oops}}

    assert {:error, {%RuntimeError{}, _}} =
           GenEvent.add_handler(pid, ReplyHandler, :raise)

    assert GenEvent.add_handler(pid, ReplyHandler, {:throw, self()}) == :ok
    assert GenEvent.which_handlers(pid) == [ReplyHandler]
    assert GenEvent.add_handler(pid, ReplyHandler, {:throw, self()}) == {:error, :already_present}

    assert GenEvent.add_handler(pid, {ReplyHandler, self()}, {self(), false}) == :ok
    assert GenEvent.which_handlers(pid) == [{ReplyHandler, self()}, ReplyHandler]
  end

  test "add_mon_handler/3" do
    {:ok, pid} = GenEvent.start()
    parent = self()

    {mon_pid, mon_ref} = spawn_monitor(fn ->
      assert GenEvent.add_mon_handler(pid, ReplyHandler, {self(), false}) == :ok
      send parent, :ok
      receive after: (:infinity -> :ok)
    end)

    assert_receive :ok
    assert GenEvent.add_handler(pid, {ReplyHandler, self()}, {self(), false}) == :ok
    assert GenEvent.which_handlers(pid) == [{ReplyHandler, self()}, ReplyHandler]

    # A regular monitor message is passed forward
    send pid, {:DOWN, make_ref(), :process, self(), :oops}
    assert_receive {:info, {:DOWN, _, :process, _, :oops}}

    # Killing the monitor though is not passed forward
    Process.exit(mon_pid, :oops)
    assert_receive {:DOWN, ^mon_ref, :process, ^mon_pid, :oops}
    refute_received {:info, {:DOWN, _, :process, _, :oops}}
    assert GenEvent.which_handlers(pid) == [{ReplyHandler, self()}]
  end

  test "add_mon_handler/3 with notifications" do
    {:ok, pid} = GenEvent.start()
    self = self()

    GenEvent.add_mon_handler(pid, ReplyHandler, {self(), false})
    GenEvent.remove_handler(pid, ReplyHandler, :ok)
    assert_receive {:gen_event_EXIT, ReplyHandler, :normal}

    :ok = GenEvent.add_mon_handler(pid, ReplyHandler, {self(), false})
    :ok = GenEvent.swap_handler(pid, ReplyHandler, :swapped, ReplyHandler, :swap)
    assert_receive {:gen_event_EXIT, ReplyHandler, {:swapped, ReplyHandler, nil}}

    :ok = GenEvent.swap_mon_handler(pid, ReplyHandler, :swapped, ReplyHandler, :swap)
    :ok = GenEvent.swap_mon_handler(pid, ReplyHandler, :swapped, ReplyHandler, :swap)
    assert_receive {:gen_event_EXIT, ReplyHandler, {:swapped, ReplyHandler, ^self}}

    GenEvent.stop(pid)
    assert_receive {:gen_event_EXIT, ReplyHandler, :shutdown}
  end

  test "remove_handler/3" do
    {:ok, pid} = GenEvent.start()

    GenEvent.add_mon_handler(pid, ReplyHandler, {self(), false})

    assert GenEvent.remove_handler(pid, {ReplyHandler, self()}, :ok) ==
           {:error, :not_found}
    assert GenEvent.remove_handler(pid, ReplyHandler, :ok) ==
           {:terminate, :ok}
    assert_receive {:terminate, :ok}

    GenEvent.add_mon_handler(pid, {ReplyHandler, self()}, {self(), false})

    assert GenEvent.remove_handler(pid, ReplyHandler, :ok) ==
           {:error, :not_found}
    assert {:error, {%RuntimeError{}, _}} =
           GenEvent.remove_handler(pid, {ReplyHandler, self()}, :raise)

    assert GenEvent.which_handlers(pid) == []
  end

  test "swap_handler/5" do
    {:ok, pid} = GenEvent.start()

    GenEvent.add_handler(pid, ReplyHandler, {self(), false})
    assert GenEvent.swap_handler(pid, ReplyHandler, :swapped,
                                 {ReplyHandler, self()}, :swap) == :ok
    assert_receive {:terminate, :swapped}
    assert_receive :swapped

    assert GenEvent.add_handler(pid, ReplyHandler, {self(), false}) == :ok
    assert GenEvent.swap_handler(pid, ReplyHandler, :swapped,
                                 {ReplyHandler, self()}, :swap) == {:error, :already_present}
    assert GenEvent.which_handlers(pid) == [{ReplyHandler, self()}]

    assert GenEvent.remove_handler(pid, {ReplyHandler, self()}, :remove_handler) ==
           {:terminate, :remove_handler}

    # The handler is initialized even when the module does not exist
    # on swap. However, in this case, we are returning an error on init.
    assert GenEvent.swap_handler(pid, ReplyHandler, :swapped, ReplyHandler, :swap) ==
           {:error, :not_found_on_swap}
  end

  test "notify/2" do
    {:ok, pid} = GenEvent.start()
    GenEvent.add_handler(pid, ReplyHandler, {self(), false})

    assert GenEvent.notify(pid, :hello) == :ok
    assert_receive {:event, :hello}

    assert GenEvent.notify(pid, {:custom, :remove_handler}) == :ok
    assert_receive {:terminate, :remove_handler}
    assert GenEvent.which_handlers(pid) == []

    Logger.remove_backend(:console)

    GenEvent.add_handler(pid, ReplyHandler, {self(), false})
    assert GenEvent.notify(pid, {:custom, :oops}) == :ok
    assert_receive {:terminate, {:error, {:bad_return_value, :oops}}}

    GenEvent.add_handler(pid, ReplyHandler, {self(), false})
    assert GenEvent.notify(pid, :raise) == :ok
    assert_receive {:terminate, {:error, {%RuntimeError{}, _}}}
    assert GenEvent.which_handlers(pid) == []
  after
    Logger.add_backend(:console, flush: true)
  end

  test "notify/2 with bad args" do
    assert GenEvent.notify({:global, :foo}, :bar) == :ok
    assert GenEvent.notify({:foo, :bar}, :bar) == :ok
    assert GenEvent.notify(self(), :bar) == :ok

    assert_raise ArgumentError, fn ->
      GenEvent.notify(:foo, :bar)
    end
  end

  test "ack_notify/2" do
    {:ok, pid} = GenEvent.start()
    GenEvent.add_handler(pid, ReplyHandler, {self(), false})

    assert GenEvent.ack_notify(pid, :hello) == :ok
    assert_receive {:event, :hello}

    assert GenEvent.ack_notify(pid, {:custom, :remove_handler}) == :ok
    assert_receive {:terminate, :remove_handler}
    assert GenEvent.which_handlers(pid) == []

    Logger.remove_backend(:console)

    GenEvent.add_handler(pid, ReplyHandler, {self(), false})
    assert GenEvent.ack_notify(pid, {:custom, :oops}) == :ok
    assert_receive {:terminate, {:error, {:bad_return_value, :oops}}}

    GenEvent.add_handler(pid, ReplyHandler, {self(), false})
    assert GenEvent.ack_notify(pid, :raise) == :ok
    assert_receive {:terminate, {:error, {%RuntimeError{}, _}}}
    assert GenEvent.which_handlers(pid) == []
  after
    Logger.add_backend(:console, flush: true)
  end

  test "sync_notify/2" do
    {:ok, pid} = GenEvent.start()
    GenEvent.add_handler(pid, ReplyHandler, {self(), false})

    assert GenEvent.sync_notify(pid, :hello) == :ok
    assert_received {:event, :hello}

    assert GenEvent.sync_notify(pid, {:custom, :remove_handler}) == :ok
    assert_received {:terminate, :remove_handler}
    assert GenEvent.which_handlers(pid) == []

    Logger.remove_backend(:console)

    GenEvent.add_handler(pid, ReplyHandler, {self(), false})
    assert GenEvent.sync_notify(pid, {:custom, :oops}) == :ok
    assert_received {:terminate, {:error, {:bad_return_value, :oops}}}

    GenEvent.add_handler(pid, ReplyHandler, {self(), false})
    assert GenEvent.sync_notify(pid, :raise) == :ok
    assert_received {:terminate, {:error, {%RuntimeError{}, _}}}
  after
    Logger.add_backend(:console, flush: true)
  end

  test "call/3" do
    {:ok, pid} = GenEvent.start()
    GenEvent.add_handler(pid, ReplyHandler, {self(), false})

    assert GenEvent.call(pid, ReplyHandler, :hello) == :ok
    assert_receive {:call, :hello}

    assert GenEvent.call(pid, ReplyHandler, {:custom, {:remove_handler, :ok}}) == :ok
    assert_receive {:terminate, :remove_handler}
    assert GenEvent.which_handlers(pid) == []

    Logger.remove_backend(:console)
    GenEvent.add_handler(pid, ReplyHandler, {self(), false})

    assert {:error, {:bad_return_value, :oops}} =
           GenEvent.call(pid, ReplyHandler, {:custom, :oops})
    assert_receive {:terminate, {:error, {:bad_return_value, :oops}}}
    assert GenEvent.which_handlers(pid) == []

    GenEvent.add_handler(pid, ReplyHandler, {self(), false})
    assert {:error, {%RuntimeError{}, _}} = GenEvent.call(pid, ReplyHandler, :raise)
    assert_receive {:terminate, {:error, {%RuntimeError{}, _}}}
    assert GenEvent.which_handlers(pid) == []
  after
    Logger.add_backend(:console, flush: true)
  end

  test "call/2 with bad args" do
    Logger.remove_backend(:console)
    {:ok, pid} = GenEvent.start_link()

    assert GenEvent.add_handler(pid, DefaultHandler, []) == :ok
    assert {:error, :not_found} =
           GenEvent.call(pid, UnknownHandler, :messages)
    assert {:error, {%RuntimeError{}, [_ | _]}} =
           GenEvent.call(pid, DefaultHandler, :whatever)
    assert GenEvent.which_handlers(pid) == []
    assert GenEvent.stop(pid) == :ok
  after
    Logger.add_backend(:console, flush: true)
  end

  test "add_process_handler/2 with GenEvent" do
    {:ok, snd} = GenEvent.start_link()
    GenEvent.add_handler(snd, ReplyHandler, {self(), false})

    {:ok, fst} = GenEvent.start_link()
    :gen.call(fst, self(), {:add_process_handler, snd, snd})

    assert GenEvent.notify(fst, :hello) == :ok
    assert_receive {:event, :hello}

    assert GenEvent.ack_notify(fst, :hello) == :ok
    assert_receive {:event, :hello}

    assert GenEvent.sync_notify(fst, :hello) == :ok
    assert_received {:event, :hello}
  end

  test ":sys.get_status/2" do
    {:ok, pid} = GenEvent.start()
    :ok = GenEvent.add_handler(pid, {ReplyHandler, :ok}, {self(), true})

    self = self()
    status = :sys.get_status(pid)
    GenEvent.stop(pid)

    assert {:status, ^pid, {:module, GenEvent},
             [pdict, _, ^pid, [], data]} = status
    assert pdict[:"$ancestors"] == [self()]
    assert pdict[:"$initial_call"] == {GenEvent, :init_it, 6}
    assert {'Installed handlers', [
            {:handler, ReplyHandler, {ReplyHandler, :ok}, ^self, nil, nil}]} = data[:items]
  end

  test ":sys.get_state/1 and :sys.replace_state/2" do
    {:ok, pid} = GenEvent.start_link()
    self = self()

    assert [] = :sys.get_state(pid)

    :ok = GenEvent.add_handler(pid, ReplyHandler, {self, true})
    assert [{ReplyHandler, ReplyHandler, ^self}] = :sys.get_state(pid)

    replacer = fn {ReplyHandler, ReplyHandler, _} -> {ReplyHandler, ReplyHandler, :unknown} end
    :sys.replace_state(pid, replacer)
    assert [{ReplyHandler, ReplyHandler, :unknown}] = :sys.get_state(pid)

    # Fail while replacing does not cause a crash
    :sys.replace_state(pid, fn _ -> exit(:fail) end)
    assert [{ReplyHandler, ReplyHandler, :unknown}] = :sys.get_state(pid)

    :ok = GenEvent.add_handler(pid, {ReplyHandler, :ok}, {self, true})
    assert [{ReplyHandler, {ReplyHandler, :ok}, ^self},
            {ReplyHandler, ReplyHandler, :unknown}] = :sys.get_state(pid)

    :ok = :sys.suspend(pid)
    assert [{ReplyHandler, {ReplyHandler, :ok}, ^self},
            {ReplyHandler, ReplyHandler, :unknown}] = :sys.get_state(pid)
    :ok = :sys.resume(pid)
  end

  test "stop/3" do
    {:ok, pid} = GenEvent.start()
    :ok = GenEvent.add_handler(pid, ReplyHandler, {self(), true})
    assert GenEvent.stop(pid, :normal) == :ok
    assert_receive {:terminate, :stop}

    {:ok, _} = GenEvent.start(name: :my_gen_event_name)
    :ok = GenEvent.add_handler(:my_gen_event_name, ReplyHandler, {self(), true})
    assert GenEvent.stop(:my_gen_event_name, {:error, "some reason"}) == :ok
    assert_receive {:terminate, :stop}
  end

  defp hibernating?(pid) do
    Process.info(pid, :current_function) ==
      {:current_function, {:erlang, :hibernate, 3}}
  end

  defp wait_until(fun, counter \\ 0) do
    cond do
      counter > 100 ->
        flunk "Waited for 1s, but #{inspect fun} never returned true"
      fun.() ->
        true
      true ->
        receive after: (10 -> wait_until(fun, counter + 1))
    end
  end

  defp wake_up(pid) do
    send pid, :wake
    assert_receive {:info, :wake}
  end
end
