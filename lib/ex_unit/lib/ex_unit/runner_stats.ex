defmodule ExUnit.RunnerStats do
  @moduledoc false

  use GenServer
  alias ExUnit.{Test, TestModule}

  def init(_opts) do
    {:ok, %{total: 0, failures: 0, skipped: 0}}
  end

  def stats(pid) do
    GenServer.call(pid, :stats, :infinity)
  end

  def handle_call(:stats, _from, map) do
    {:reply, map, map}
  end

  def handle_cast({:test_finished, %ExUnit.Test{state: {tag, _}}}, map)
      when tag in [:failed, :invalid] do
    %{total: total, failures: failures} = map
    {:noreply, %{map | total: total + 1, failures: failures + 1}}
  end

  def handle_cast({:test_finished, %Test{state: {:skip, _}}}, map) do
    %{total: total, skipped: skipped} = map
    {:noreply, %{map | total: total + 1, skipped: skipped + 1}}
  end

  def handle_cast({:test_finished, _}, %{total: total} = map) do
    {:noreply, %{map | total: total + 1}}
  end

  def handle_cast({:module_finished, %TestModule{state: {:failed, _}} = test_module}, map) do
    %{failures: failures, total: total} = map
    test_count = length(test_module.tests)
    {:noreply, %{map | failures: failures + test_count, total: total + test_count}}
  end

  def handle_cast(_, map) do
    {:noreply, map}
  end
end
