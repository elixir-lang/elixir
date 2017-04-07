defmodule ExUnit.RunnerStats do
  @moduledoc false

  use GenServer

  def init(_opts) do
    {:ok, %{total: 0, failures: 0, skipped: 0}}
  end

  def stats(pid) do
    GenServer.call(pid, :stats, :infinity)
  end

  def handle_call(:stats, _from, map) do
    {:reply, map, map}
  end

  def handle_cast({:test_finished, %ExUnit.Test{state: {tag, _}}},
                   %{total: total, failures: failures} = map) when tag in [:failed, :invalid] do
    {:noreply, %{map | total: total + 1, failures: failures + 1}}
  end

  def handle_cast({:test_finished, %ExUnit.Test{state: {:skip, _}}},
                   %{total: total, skipped: skipped} = map) do
    {:noreply, %{map | total: total + 1, skipped: skipped + 1}}
  end

  def handle_cast({:test_finished, _}, %{total: total} = map) do
    {:noreply, %{map | total: total + 1}}
  end

  def handle_cast(_, map) do
    {:noreply, map}
  end
end
