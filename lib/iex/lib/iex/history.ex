defmodule IEx.History.State do
  @moduledoc false

  defstruct queue: :queue.new, size: 0, start: 1

  def append(%{queue: q, size: size} = state, item) do
    %{state | queue: :queue.in(item, q), size: size + 1}
  end

  def to_list(%{queue: q}),
    do: :queue.to_list(q)

  # Traverses the queue front-to-back if the index is positive.
  def nth(%{queue: q, size: size, start: start}, n)
    when n - start >= 0 and n - start < size,
    do: get_nth(q, n - start)

  # Traverses the queue back-to-front if the index is negative.
  def nth(%{queue: q, size: size, start: start}, n)
    when n < 0 and size + n >= start - 1,
    do: get_nth(:queue.reverse(q), abs(n) - 1)

  def nth(_, _), do: nil

  defp get_nth(q, 0), do: :queue.head(q)
  defp get_nth(q, n) when n > 0,
    do: get_nth(:queue.tail(q), n - 1)

  # Traverses the queue front-to-back, dropping items as we go
  # until its size is within the specified limit.
  #
  # The "start" value contains the index of the expression at the head
  # of the queue.
  def prune(%{start: start} = state, limit),
    do: prune(state, start, limit, false)

  defp prune(state, _, limit, _) when limit < 0,
    do: {false, state}

  defp prune(%{size: size} = state, counter, limit, collect?)
    when size - counter < limit,
    do: {collect?, %{state | start: counter}}

  defp prune(%{queue: q} = state, counter, limit, collect?) do
    {{:value, entry}, q} = :queue.out(q)
    collect? = collect? || has_binary(entry)
    prune(%{state | queue: q}, counter + 1, limit, collect?)
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
  Initializes IEx history state.
  """
  def init(), do: %State{}

  @doc """
  Appends one entry to the history.
  """
  def append(%State{} = state, entry, limit) do
    {collect?, state} =
      State.append(state, entry)
      |> State.prune(limit)

    if collect?, do: collect_garbage()
    state
  end

  @doc """
  Enumerates over all items in the history starting from the oldest one and
  applies `fun` to each one in turn.
  """
  def each(%State{} = state, fun) do
    State.to_list(state)
    |> Enum.each(fun)
  end

  @doc """
  Gets the nth item from the history.

  If `n` < 0, the count starts from the most recent item and goes back in time.
  """
  def nth(%State{} = state, n) do
    case State.nth(state, n) do
      nil -> raise "v(#{n}) is out of bounds"
      entry -> entry
    end
  end

  # Based on https://github.com/erlang/otp/blob/7dcccee4371477e983f026db9e243cb66900b1ef/lib/stdlib/src/shell.erl#L1401
  defp collect_garbage() do
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
