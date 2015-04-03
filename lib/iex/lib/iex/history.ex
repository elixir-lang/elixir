defmodule IEx.History.State do
  @moduledoc false

  def new(), do: {:queue.new, 0, 1}

  def append({q, size, start}, item),
    do: {:queue.in(item, q), size + 1, start}

  def to_list({q, _, _}),
    do: :queue.to_list(q)

  # Traverses the queue front-to-back if the index is positive.
  def nth({q, size, start}, n)
    when n - start >= 0 and n - start < size,
    do: get_nth(q, n - start)

  # Traverses the queue back-to-front if the index is negative.
  def nth({q, size, start}, n)
    when n < 0 and size + n >= start - 1,
    do: get_nth(:queue.reverse(q), abs(n) - 1)

  def nth(_, _), do: nil

  defp get_nth(q, 0), do: :queue.head(q)
  defp get_nth(q, n) when n > 0,
    do: get_nth(:queue.tail(q), n - 1)

  # Traverses the queue front-to-back, dropping items as we go
  # until its size is within the specified limit.
  #
  # The `start` value contains the index of the expression at the head
  # of the queue.
  def prune({_, _, start} = state, limit),
    do: prune(state, start, limit, false)

  defp prune(state, _, limit, _) when limit < 0,
    do: {false, state}

  defp prune({q, size, _}, counter, limit, collect?)
    when size - counter < limit,
    do: {collect?, {q, size, counter}}

  defp prune({q, size, start}, counter, limit, collect?) do
    {{:value, entry}, q} = :queue.out(q)
    unless collect? do
      collect? = has_binary(entry)
    end
    prune({q, size, start}, counter + 1, limit, collect?)
  end

  # Checks val and each of its elements (if it is a list or a tuple)
  # recursively to see if it has any large binaries (outside of the heap).
  defp has_binary(val) do
    try do
      has_bin(val)
    catch
      :throw, :found -> true
    end
  end

  defp has_bin(val) when is_tuple(val),
    do: has_bin(val, tuple_size(val) - 1)

  defp has_bin([head | tail]) do
    has_bin(head)
    has_bin(tail)
  end

  defp has_bin(val) when byte_size(val) > 64,
    do: throw(:found)

  defp has_bin(_), do: false

  defp has_bin(_, -1), do: false

  defp has_bin(tuple, index) do
    has_bin(elem(tuple, index))
    has_bin(tuple, index - 1)
  end
end

defmodule IEx.History do
  @moduledoc false

  alias IEx.History.State

  @doc """
  Starts IEx process which stores all history information.
  """
  def start_link() do
    Agent.start_link(fn -> State.new end)
  end

  @doc """
  Appends one entry to the history.
  """
  def append(pid, entry, limit) do
    should_collect = Agent.get_and_update(pid, fn entries ->
      State.append(entries, entry) |> State.prune(limit)
    end)
    if should_collect do
      collect_garbage(pid)
    end
  end

  @doc """
  Removes all entries from the history and forces a garbage collection cycle.
  """
  def reset(pid) do
    Agent.update(pid, fn _ -> State.new end)
    collect_garbage(pid)
  end

  @doc """
  Enumerates over all items in the history starting from the oldest one and
  applies `fun` to each one in turn.
  """
  def each(pid, fun) do
    Agent.get(pid, &State.to_list/1)
    |> Enum.each(fun)
  end

  @doc """
  Gets the nth item from the history.

  If `n` < 0, the count starts from the most recent item and goes back in time.
  """
  def nth(pid, n) do
    entry = Agent.get(pid, &State.nth(&1, n))
    if is_nil(entry) do
      raise "v(#{n}) is out of bounds"
    end
    entry
  end

  # Based on https://github.com/erlang/otp/blob/7dcccee4371477e983f026db9e243cb66900b1ef/lib/stdlib/src/shell.erl#L1401
  defp collect_garbage(pid) do
    :erlang.garbage_collect(pid)
    collect_proc_garbage Process.whereis(:user)
    collect_proc_garbage Process.group_leader()
    :erlang.garbage_collect()
  end

  defp collect_proc_garbage(process) do
    try do
      :erlang.garbage_collect(process)
    catch
      _, _ -> nil
    end
  end
end
