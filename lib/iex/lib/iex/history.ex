 defmodule IEx.History do
  @moduledoc false

  use GenServer

  alias IEx.History.Array

  @doc """
  Initializes IEx process variables. All history
  information is kept in the process dictionary.
  """
  def init([]) do
    {:ok, Array.new}
  end

  @doc """
  Appends one entry to the history with the given counter.
  """
  def handle_cast({:append, entry, _counter, limit}, state) do
    {should_collect, state} = state 
    |> Array.append(entry)
    |> Array.limit(limit)
    if should_collect do
      collect_garbage
    end
    {:noreply, state}
  end

  @doc """
  Removes all entries from the history and forces a garbage collection cycle.
  """
  def handle_call(:reset, _from, _state) do
    collect_garbage
    {:reply, true, Array.new}
  end

  @doc """
  Enumerates over all items in the history starting from the oldest one and
  applies `fun` to each one in turn.
  """
  def handle_call({:each, fun}, _from, state) do
    history = state 
    |> Array.to_list
    |> Enum.each(fun)
    {:reply, history, state}
  end
  
  @doc """
  Gets the nth item from the history.

  If `n` < 0, the count starts from the most recent item and goes back in time.
  """
  def handle_call({:nth, n}, _from, state) do
    entry = Array.nth(state, n)
    if is_nil(entry) do
      {:reply, :error, state}
    else
      {:reply, entry, state}
    end
  end

  # Based on https://github.com/erlang/otp/blob/7dcccee4371477e983f026db9e243cb66900b1ef/lib/stdlib/src/shell.erl#L1401
  defp collect_garbage do
    :erlang.garbage_collect(self())
    collect_garbage Process.whereis(:user)
    collect_garbage Process.group_leader()
    :erlang.garbage_collect()
  end

  defp collect_garbage(process) do
    try do
      :erlang.garbage_collect(process)
    catch
      _, _ -> nil
    end
  end
end
