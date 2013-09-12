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
  def append(entry, counter) do
    Process.put({:iex_history, counter}, entry)
    Process.put(:iex_history_counter, counter+1)

    limit = IEx.Options.get(:history_size)
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
      should_collect = has_binary(entry.result)
    else
      Process.delete({:iex_history, counter})
    end
    limit_history(counter+1, max_counter, limit, should_collect)
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
    try do
      :erlang.garbage_collect(Process.whereis(:user))
    catch
      _, _ -> nil
    end
    try do
      :erlang.garbage_collect(Process.group_leader())
    catch
      _, _ -> nil
    end
    :erlang.garbage_collect()
  end

  @doc """
  Enumerates each item in the history.
  """
  def each(fun) do
    each(Process.get(:iex_history_start_counter),
         Process.get(:iex_history_counter),
         fun)
  end

  defp each(counter, max_counter, fun) when counter < max_counter do
    entry = Process.get({:iex_history, counter})
    fun.(entry)
    each(counter+1, max_counter, fun)
  end

  defp each(_, _, _) do
    :ok
  end

  @doc """
  Gets the nth item in the history.
  """
  def nth(n) do
    entry = case n do
      n when n >= 0 ->
        Process.get({:iex_history, n})
      n when n < 0 ->
        counter = Process.get(:iex_history_counter)
        Process.get({:iex_history, counter + n})
    end
    if nil?(entry) do
      raise "v(#{n}) is out of bounds"
    end
    entry
  end
end
