# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Code.require_file("test_helper.exs", __DIR__)

defmodule Registry.CommonTest do
  use ExUnit.Case, async: true
  doctest Registry, except: [:moduledoc]
end

defmodule Registry.Test do
  use ExUnit.Case,
    async: true,
    parameterize:
      for(
        keys <- [:unique, :duplicate, {:duplicate, :pid}, {:duplicate, :key}],
        partitions <- [1, 8],
        do: %{keys: keys, partitions: partitions}
      )

  setup config do
    keys = config.keys || :unique
    partitions = config.partitions

    listeners =
      List.wrap(config[:base_listener]) |> Enum.map(&:"#{&1}_#{partitions}_#{inspect(keys)}")

    name = :"#{config.test}_#{partitions}_#{inspect(keys)}"
    opts = [keys: keys, name: name, partitions: partitions, listeners: listeners]
    {:ok, _} = start_supervised({Registry, opts})
    %{registry: name, listeners: listeners}
  end

  # Note: those tests relies on internals
  test "clean up registry on process crash",
       %{registry: registry, partitions: partitions} do
    {_, task1} = register_task(registry, "hello", :value)
    {_, task2} = register_task(registry, "world", :value)

    kill_and_assert_down(task1)
    kill_and_assert_down(task2)

    # pid might be in different partition to key so need to sync with all
    # partitions before checking ETS tables are empty.
    if partitions > 1 do
      for i <- 0..(partitions - 1) do
        [{_, _, {partition, _}}] = :ets.lookup(registry, i)
        GenServer.call(partition, :sync)
      end

      for i <- 0..(partitions - 1) do
        [{_, key, {_, pid}}] = :ets.lookup(registry, i)
        assert :ets.tab2list(key) == []
        assert :ets.tab2list(pid) == []
      end
    else
      [{-1, {_, _, key, {partition, pid}, _}}] = :ets.lookup(registry, -1)
      GenServer.call(partition, :sync)
      assert :ets.tab2list(key) == []
      assert :ets.tab2list(pid) == []
    end
  end

  defp register_task(registry, key, value) do
    parent = self()

    {:ok, task} =
      Task.start(fn ->
        send(parent, Registry.register(registry, key, value))
        Process.sleep(:infinity)
      end)

    assert_receive {:ok, owner}
    {owner, task}
  end

  defp kill_and_assert_down(pid) do
    ref = Process.monitor(pid)
    Process.exit(pid, :kill)
    assert_receive {:DOWN, ^ref, _, _, _}
  end
end

defmodule Registry.LockTest do
  use ExUnit.Case,
    async: true,
    parameterize: [
      %{keys: :unique, partitions: 1},
      %{keys: :unique, partitions: 8},
      %{keys: :duplicate, partitions: 1},
      %{keys: :duplicate, partitions: 8}
    ]

  setup config do
    keys = config.keys
    partitions = config.partitions
    name = :"#{config.test}_#{keys}_#{partitions}"
    opts = [keys: keys, name: name, partitions: partitions]
    {:ok, _} = start_supervised({Registry, opts})
    %{registry: name}
  end

  test "does not lock when using different keys", config do
    parent = self()

    task1 =
      Task.async(fn ->
        Registry.lock(config.registry, 1, fn ->
          send(parent, :locked1)
          assert_receive :unlock
          :done
        end)
      end)

    assert_receive :locked1

    task2 =
      Task.async(fn ->
        Registry.lock(config.registry, 2, fn ->
          send(parent, :locked2)
          assert_receive :unlock
          :done
        end)
      end)

    assert_receive :locked2

    send(task1.pid, :unlock)
    send(task2.pid, :unlock)
    assert Task.await(task1) == :done
    assert Task.await(task2) == :done
    assert Registry.lock(config.registry, 1, fn -> :done end) == :done
    assert Registry.lock(config.registry, 2, fn -> :done end) == :done
  end

  test "locks when using the same key", config do
    parent = self()

    task1 =
      Task.async(fn ->
        Registry.lock(config.registry, :ok, fn ->
          send(parent, :locked1)
          assert_receive :unlock
          :done
        end)
      end)

    assert_receive :locked1

    task2 =
      Task.async(fn ->
        Registry.lock(config.registry, :ok, fn ->
          send(parent, :locked2)
          :done
        end)
      end)

    refute_receive :locked2, 100

    send(task1.pid, :unlock)
    assert Task.await(task1) == :done
    assert_receive :locked2
    assert Task.await(task2) == :done
    assert Registry.lock(config.registry, :ok, fn -> :done end) == :done
  end

  @tag :capture_log
  test "locks when the one holding the lock raises", config do
    parent = self()

    task1 =
      Task.async(fn ->
        Registry.lock(config.registry, :ok, fn ->
          send(parent, :locked)
          assert_receive :unlock
          raise "oops"
        end)
      end)

    Process.unlink(task1.pid)
    assert_receive :locked

    task2 =
      Task.async(fn ->
        Registry.lock(config.registry, :ok, fn ->
          :done
        end)
      end)

    send(task1.pid, :unlock)
    assert {:exit, {%RuntimeError{message: "oops"}, [_ | _]}} = Task.yield(task1)
    assert Task.await(task2) == :done
    assert Registry.lock(config.registry, :ok, fn -> :done end) == :done
  end

  test "locks when the one holding the lock terminates", config do
    parent = self()

    task1 =
      Task.async(fn ->
        Registry.lock(config.registry, :ok, fn ->
          send(parent, :locked)
          assert_receive :unlock
          :done
        end)
      end)

    assert_receive :locked

    task2 =
      Task.async(fn ->
        Registry.lock(config.registry, :ok, fn ->
          :done
        end)
      end)

    assert Task.shutdown(task1, :brutal_kill) == nil
    assert Task.await(task2) == :done
    assert Registry.lock(config.registry, :ok, fn -> :done end) == :done
  end

  test "locks when the one waiting for the lock terminates", config do
    parent = self()

    task1 =
      Task.async(fn ->
        Registry.lock(config.registry, :ok, fn ->
          send(parent, :locked)
          assert_receive :unlock
          :done
        end)
      end)

    assert_receive :locked

    task2 =
      Task.async(fn ->
        Registry.lock(config.registry, :ok, fn ->
          :done
        end)
      end)

    :erlang.yield()
    assert Task.shutdown(task2, :brutal_kill) == nil

    send(task1.pid, :unlock)
    assert Task.await(task1) == :done
    assert Registry.lock(config.registry, :ok, fn -> :done end) == :done
  end
end
