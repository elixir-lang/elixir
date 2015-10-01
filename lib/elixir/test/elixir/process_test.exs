Code.require_file "../test_helper.exs", __DIR__

defmodule ProcessTest do
  use ExUnit.Case, async: true

  test "group_leader/2 and group_leader/0" do
    another = spawn_link(fn -> :timer.sleep(1000) end)
    assert Process.group_leader(self, another)
    assert Process.group_leader == another
  end

  test "monitoring functions are inlined by the compiler" do
    assert expand(quote(do: Process.monitor(pid())), __ENV__) ==
           quote(do: :erlang.monitor(:process, pid()))
  end

  test "info/2" do
    pid = spawn fn -> :timer.sleep(1000) end
    assert Process.info(pid, :priority) == {:priority, :normal}
    assert Process.info(pid, [:priority]) == [priority: :normal]

    Process.exit(pid, :kill)
    assert Process.info(pid, :backtrace) == nil
    assert Process.info(pid, [:backtrace, :status]) == nil
  end

  test "info/2 with registered name" do
    pid = spawn fn -> end
    Process.exit(pid, :kill)
    assert Process.info(pid, :registered_name) ==
           nil
    assert Process.info(pid, [:registered_name]) ==
           nil

    assert Process.info(self, :registered_name) ==
           {:registered_name, []}
    assert Process.info(self, [:registered_name]) ==
           [registered_name: []]

    Process.register(self, __MODULE__)
    assert Process.info(self, :registered_name) ==
           {:registered_name, __MODULE__}
    assert Process.info(self, [:registered_name]) ==
           [registered_name: __MODULE__]
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
    {expr, _env} = :elixir_exp.expand(expr, env)
    expr
  end
end
