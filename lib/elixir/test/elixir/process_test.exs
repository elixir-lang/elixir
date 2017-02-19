Code.require_file "test_helper.exs", __DIR__

defmodule ProcessTest do
  use ExUnit.Case, async: true

  doctest Process

  test "dictionary" do
    assert Process.put(:foo, :bar) == nil
    assert Process.put(:foo, :baz) == :bar

    assert Process.get_keys() == [:foo]
    assert Process.get_keys(:bar) == []
    assert Process.get_keys(:baz) == [:foo]

    assert Process.get(:foo) == :baz
    assert Process.delete(:foo) == :baz
    assert Process.get(:foo) == nil
  end

  test "group_leader/2 and group_leader/0" do
    another = spawn_link(fn -> Process.sleep(1000) end)
    assert Process.group_leader(self(), another)
    assert Process.group_leader == another
  end

  test "monitoring functions are inlined by the compiler" do
    assert expand(quote(do: Process.monitor(pid())), __ENV__) ==
           quote(do: :erlang.monitor(:process, pid()))
  end

  test "sleep/1" do
    assert Process.sleep(0) == :ok
  end

  test "info/2" do
    pid = spawn fn -> Process.sleep(1000) end
    assert Process.info(pid, :priority) == {:priority, :normal}
    assert Process.info(pid, [:priority]) == [priority: :normal]

    Process.exit(pid, :kill)
    assert Process.info(pid, :backtrace) == nil
    assert Process.info(pid, [:backtrace, :status]) == nil
  end

  test "info/2 with registered name" do
    pid = spawn fn -> nil end
    Process.exit(pid, :kill)
    assert Process.info(pid, :registered_name) ==
           nil
    assert Process.info(pid, [:registered_name]) ==
           nil

    assert Process.info(self(), :registered_name) ==
           {:registered_name, []}
    assert Process.info(self(), [:registered_name]) ==
           [registered_name: []]

    Process.register(self(), __MODULE__)
    assert Process.info(self(), :registered_name) ==
           {:registered_name, __MODULE__}
    assert Process.info(self(), [:registered_name]) ==
           [registered_name: __MODULE__]
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
    assert is_integer Process.read_timer(timer)
    assert is_integer Process.cancel_timer(timer)

    timer = Process.send_after(self(), :hello, 0)
    assert_receive :hello
    assert Process.read_timer(timer) == false
    assert Process.cancel_timer(timer) == false
  end

  test "exit(pid, :normal) does not cause the target process to exit" do
    pid = spawn_link fn ->
      receive do
        :done -> nil
      end
    end

    trap = Process.flag(:trap_exit, true)
    true = Process.exit(pid, :normal)
    refute_receive {:EXIT, ^pid, :normal}
    assert Process.alive?(pid)

    # now exit the process for real so it doesn't hang around
    true = Process.exit(pid, :abnormal)
    assert_receive {:EXIT, ^pid, :abnormal}
    refute Process.alive?(pid)

    Process.flag(:trap_exit, trap)
  end

  test "exit(pid, :normal) makes the process receive a message if it traps exits" do
    parent = self()
    pid = spawn_link fn ->
      Process.flag(:trap_exit, true)
      receive do
        {:EXIT, ^parent, :normal} -> send(parent, {:ok, self()})
      end
    end

    refute_receive _
    Process.exit(pid, :normal)
    assert_receive {:ok, ^pid}
    refute Process.alive?(pid)
  end

  test "exit(self(), :normal) causes the calling process to exit" do
    trap = Process.flag(:trap_exit, true)

    pid = spawn_link fn -> Process.exit(self(), :normal) end

    assert_receive {:EXIT, ^pid, :normal}
    refute Process.alive?(pid)

    Process.flag(:trap_exit, trap)
  end

  defp expand(expr, env) do
    {expr, _env} = :elixir_expand.expand(expr, env)
    expr
  end
end
