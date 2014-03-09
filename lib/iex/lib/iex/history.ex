 defmodule IEx.History do
  @moduledoc false

  @doc """
  Initializes IEx process variables. All history
  information is kept in the process dictionary.
  """
  def init do
    Process.put(:iex_history_start_counter, 1)
    Process.put(:iex_history_counter, 1)
  end

  @doc """
  Appends one entry to the history with the given counter.
  """
  def append(entry, counter, limit) do
    Process.put({:iex_history, counter}, entry)
    Process.put(:iex_history_counter, counter+1)

    start_counter = Process.get(:iex_history_start_counter)
    should_collect = limit_history(start_counter, counter, limit, false)
    if should_collect do
      collect_garbage()
    end
  end

  defp limit_history(_, _, limit, _) when limit < 0 do
    false
  end

  defp limit_history(counter, max_counter, limit, should_collect) when max_counter - counter < limit do
    Process.put(:iex_history_start_counter, counter)
    should_collect
  end

  defp limit_history(counter, max_counter, limit, should_collect) do
    if not should_collect do
      entry = Process.delete({:iex_history, counter})
      should_collect = has_binary(entry)
    else
      Process.delete({:iex_history, counter})
    end
    limit_history(counter+1, max_counter, limit, should_collect)
  end

  @doc """
  Removes all entries from the history and forces a garbage collection cycle.
  """
  def reset() do
    each_pair(fn(key, _) ->
      Process.delete(key)
    end)

    counter = Process.get(:iex_history_counter)
    Process.put(:iex_history_start_counter, counter)

    collect_garbage()
  end

  # Checks val and each of its elements (if it is a list or a tuple)
  # recursively to see if it has any binaries
  defp has_binary(val) do
    try do
      has_bin(val)
    catch
      :throw, true -> true
    end
  end

  # Worker function used by has_binary. Throws when the first binary of the
  # minimum specified size is found
  defp has_bin(val) when is_tuple(val) do
    has_bin(val, tuple_size(val)-1)
  end

  defp has_bin([h|t]) do
    has_bin(h)
    has_bin(t)
  end

  defp has_bin(val) when byte_size(val) > 64 do
    throw true
  end

  defp has_bin(_) do
    false
  end

  defp has_bin(_, -1) do
    false
  end

  defp has_bin(tuple, index) do
    has_bin(elem(tuple, index))
    has_bin(tuple, index-1)
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

  @doc """
  Enumerates over all items in the history starting from the oldest one and
  applies `fun` to each one in turn.
  """
  def each(fun) do
    each_pair(fn _, item -> fun.(item) end)
  end

  # Private helper that invokes fun with both key and item.
  defp each_pair(fun) do
    each_pair(Process.get(:iex_history_start_counter),
              Process.get(:iex_history_counter),
              fun)
  end

  defp each_pair(counter, max_counter, fun) when counter < max_counter do
    key = {:iex_history, counter}
    entry = Process.get(key)
    fun.(key, entry)
    each_pair(counter+1, max_counter, fun)
  end

  defp each_pair(_, _, _) do
    :ok
  end

  @doc """
  Gets the nth item from the history.

  If `n` < 0, the count starts from the most recent item and goes back in time.
  """
  def nth(n) do
    entry = case n do
      x when x >= 0 ->
        Process.get({:iex_history, n})
      x when x < 0 ->
        counter = Process.get(:iex_history_counter)
        Process.get({:iex_history, counter + n})
    end
    if nil?(entry) do
      raise "v(#{n}) is out of bounds"
    end
    entry
  end
end
