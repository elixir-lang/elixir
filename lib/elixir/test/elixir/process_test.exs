Code.require_file("test_helper.exs", __DIR__)

defmodule ProcessTest do
  use ExUnit.Case, async: true

  doctest Process

  test "dictionary" do
    assert Process.put(:foo, :bar) == nil
    assert Process.put(:foo, :baz) == :bar

    assert Enum.member?(Process.get_keys(), :foo)
    refute Enum.member?(Process.get_keys(), :bar)
    refute Enum.member?(Process.get_keys(), :baz)
    assert Process.get_keys(:bar) == []
    assert Process.get_keys(:baz) == [:foo]

    assert Process.get(:foo) == :baz
    assert Process.delete(:foo) == :baz
    assert Process.get(:foo) == nil
  end

  test "group_leader/2 and group_leader/0" do
    another = spawn_link(fn -> Process.sleep(1000) end)
    assert Process.group_leader(self(), another)
    assert Process.group_leader() == another
  end

  # In contrast with other inlined functions,
  # it is important to test that monitor/1,2 are inlined,
  # this way we gain the monitor receive optimisation.
  test "monitor/1 and monitor/2 are inlined" do
    assert expand(quote(do: Process.monitor(pid())), __ENV__) ==
             quote(do: :erlang.monitor(:process, pid()))

    assert expand(quote(do: Process.monitor(pid(), alias: :demonitor)), __ENV__) ==
             quote(do: :erlang.monitor(:process, pid(), alias: :demonitor))
  end

  test "monitor/2 with monitor options" do
    pid =
      spawn(fn ->
        receive do
          {:ping, source_alias} -> send(source_alias, :pong)
        end
      end)

    ref_and_alias = Process.monitor(pid, alias: :explicit_unalias)

    send(pid, {:ping, ref_and_alias})

    assert_receive :pong
    assert_receive {:DOWN, ^ref_and_alias, _, _, _}
  end

  test "sleep/1" do
    assert Process.sleep(0) == :ok
  end

  test "sleep/1 with 2^32" do
    {pid, monitor_ref} = spawn_monitor(fn -> Process.sleep(2 ** 32) end)
    refute_receive {:DOWN, ^monitor_ref, :process, ^pid, {:timeout_value, _trace}}
    Process.exit(pid, :kill)
  end

  test "info/2" do
    pid = spawn(fn -> Process.sleep(1000) end)
    assert Process.info(pid, :priority) == {:priority, :normal}
    assert Process.info(pid, [:priority]) == [priority: :normal]

    Process.exit(pid, :kill)
    assert Process.info(pid, :backtrace) == nil
    assert Process.info(pid, [:backtrace, :status]) == nil
  end

  test "info/2 with registered name" do
    pid = spawn(fn -> nil end)
    Process.exit(pid, :kill)
    assert Process.info(pid, :registered_name) == nil
    assert Process.info(pid, [:registered_name]) == nil

    assert Process.info(self(), :registered_name) == {:registered_name, []}
    assert Process.info(self(), [:registered_name]) == [registered_name: []]

    Process.register(self(), __MODULE__)
    assert Process.info(self(), :registered_name) == {:registered_name, __MODULE__}
    assert Process.info(self(), [:registered_name]) == [registered_name: __MODULE__]
  end

  test "send_after/3 sends messages once expired" do
    Process.send_after(self(), :hello, 10)
    assert_receive :hello
  end

  test "send_after/4 with absolute time sends message once expired" do
    time = System.monotonic_time(:millisecond) + 10
    Process.send_after(self(), :hello, time, abs: true)
    assert_receive :hello
  end

  test "send_after/3 returns a timer reference that can be read or cancelled" do
    timer = Process.send_after(self(), :hello, 100_000)
    refute_received :hello
    assert is_integer(Process.read_timer(timer))
    assert is_integer(Process.cancel_timer(timer))

    timer = Process.send_after(self(), :hello, 0)
    assert_receive :hello
    assert Process.read_timer(timer) == false
    assert Process.cancel_timer(timer) == false

    timer = Process.send_after(self(), :hello, 100_000)
    assert Process.cancel_timer(timer, async: true)
    assert_receive {:cancel_timer, ^timer, result}
    assert is_integer(result)
  end

  test "exit(pid, :normal) does not cause the target process to exit" do
    Process.flag(:trap_exit, true)

    pid =
      spawn_link(fn ->
        receive do
          :done -> nil
        end
      end)

    true = Process.exit(pid, :normal)
    refute_receive {:EXIT, ^pid, :normal}
    assert Process.alive?(pid)

    # now exit the process for real so it doesn't hang around
    true = Process.exit(pid, :abnormal)
    assert_receive {:EXIT, ^pid, :abnormal}
    refute Process.alive?(pid)
  end

  test "exit(self(), :normal) causes the calling process to exit" do
    Process.flag(:trap_exit, true)
    pid = spawn_link(fn -> Process.exit(self(), :normal) end)
    assert_receive {:EXIT, ^pid, :normal}
    refute Process.alive?(pid)
  end

  describe "alias/0, alias/1, and unalias/1" do
    test "simple alias + unalias flow" do
      server =
        spawn(fn ->
          receive do
            {:ping, alias} -> send(alias, :pong)
          end
        end)

      alias = Process.alias()
      Process.unalias(alias)

      send(server, {:ping, alias})
      refute_receive :pong, 20
    end

    test "with :reply option when aliasing" do
      server =
        spawn(fn ->
          receive do
            {:ping, alias} ->
              send(alias, :pong)
              send(alias, :extra_pong)
          end
        end)

      alias = Process.alias([:reply])

      send(server, {:ping, alias})
      assert_receive :pong
      refute_receive :extra_pong, 20
    end
  end

  describe "set_label/1" do
    @compile {:no_warn_undefined, :proc_lib}

    test "sets a process label, compatible with OTP 27+ `:proc_lib.get_label/1`" do
      label = {:some_label, :rand.uniform(99999)}
      assert :ok = Process.set_label(label)

      # TODO: Remove this when we require Erlang/OTP 27+
      if System.otp_release() >= "27" do
        assert :proc_lib.get_label(self()) == label
      end
    end
  end

  defp expand(expr, env) do
    {expr, _, _} = :elixir_expand.expand(expr, :elixir_env.env_to_ex(env), env)
    expr
  end
end
