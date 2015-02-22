 defmodule IEx.History do
  @moduledoc false

  alias IEx.History.Array

  @doc """
  Initializes IEx process variables. All history
  information is kept in the process dictionary.
  """
  def init do
    Agent.start_link(fn -> Array.new end)
  end

  @doc """
  Appends one entry to the history with the given counter.
  """
  def append(pid, entry, _counter, limit) do
    should_collect = Agent.get_and_update(pid, fn entries ->
      entries |> Array.append(entry) |> Array.limit(limit)
    end)
    if should_collect do
      Agent.cast(pid, &collect_garbage/1)
    end
  end

  @doc """
  Removes all entries from the history and forces a garbage collection cycle.
  """
  def reset(pid) do
    pid
    |> Agent.update(fn _ -> Array.new end)
    |> Agent.cast(&collect_garbage/1)
    true
  end

  @doc """
  Enumerates over all items in the history starting from the oldest one and
  applies `fun` to each one in turn.
  """
  def each(pid, fun) do
    Agent.get(pid, &Array.to_list/1)
    |> Enum.each(fun)
  end

  @doc """
  Gets the nth item from the history.

  If `n` < 0, the count starts from the most recent item and goes back in time.
  """
  def nth(pid, n) do
    entry = Agent.get(pid, &Array.nth(&1, n))
    if is_nil(entry) do
      raise "v(#{n}) is out of bounds"
    end
    entry
  end

  # Based on https://github.com/erlang/otp/blob/7dcccee4371477e983f026db9e243cb66900b1ef/lib/stdlib/src/shell.erl#L1401
  defp collect_garbage(state) do
    :erlang.garbage_collect(self())
    collect_proc_garbage Process.whereis(:user)
    collect_proc_garbage Process.group_leader()
    :erlang.garbage_collect()
    state
  end

  defp collect_proc_garbage(process) do
    try do
      :erlang.garbage_collect(process)
    catch
      _, _ -> nil
    end
  end
end

defmodule IEx.History.Array do
  @moduledoc false

  # Array abstraction for the history server's state

  def new, do: {:queue.new, 0, 1}

  def append({q, size, start}, item),
    do: {:queue.in(item, q), size+1, start}

  def to_list({q, _, _}),
    do: :queue.to_list(q)

  # if the index is positive, we traverse the queue front-to-back
  def nth({q, size, start}, n)
    when n-start >= 0 and n-start < size,
    do: get_nth_head(q, n-start)

  # if the index is negative, we traverse the queue back-to-front
  def nth({q, size, start}, n)
    when n < 0 and size+n >= start-1,
    do: get_nth_tail(q, abs(n)-1)

  def nth(_, _), do: nil

  defp get_nth_head(q, 0), do: :queue.head(q)
  defp get_nth_head(q, n) when n > 0,
    do: get_nth_head(:queue.tail(q), n-1)

  defp get_nth_tail(q, 0), do: :queue.last(q)
  defp get_nth_tail(q, n) when n > 0,
    do: get_nth_tail(:queue.drop_r(q), n-1)


  # here we traverse the queue front-to-back, dropping items as we go
  # until its size is within the specified limit
  #
  # the `start` value contains the index of the expression at the head
  # of the queue

  def limit({_, _, start}=q, limit),
    do: limit(q, start, limit, false)

  defp limit(q, _, limit, _) when limit < 0,
    do: {false, q}

  defp limit({q, size, _}, counter, limit, collect?)
    when size - counter < limit,
    do: {collect?, {q, size, counter}}

  defp limit({q, size, start}, counter, limit, collect?) do
    unless collect? do
      entry = :queue.head(q)
      collect? = has_binary(entry)
    end
    updated_q = {:queue.drop(q), size, start}
    limit(updated_q, counter+1, limit, collect?)
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
  defp has_bin(val) when is_tuple(val),
    do: has_bin(val, tuple_size(val)-1)

  defp has_bin([h|t]) do
    has_bin(h)
    has_bin(t)
  end

  defp has_bin(val) when byte_size(val) > 64,
    do: throw true

  defp has_bin(_), do: false

  defp has_bin(_, -1), do: false

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
    if is_nil(entry) do
      raise "v(#{n}) is out of bounds"
    end
    entry
  end
end
