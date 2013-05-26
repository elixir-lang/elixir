defmodule IEx.History do

  def init do
    Process.put :iex_history, :queue.new
  end

  ### append ###

  def append(entry) do
    history = Process.get(:iex_history)
    len = :queue.len(history)
    limit = IEx.Options.get(:history_size)

    new_history =
      :queue.in(entry, history)
      |> limit_history(len + 1, limit)
    Process.put(:iex_history, new_history)
  end

  defp limit_history(_, _, 0) do
    :queue.new
  end

  defp limit_history(queue, _, limit) when limit < 0 do
    queue
  end

  defp limit_history(queue, len, limit) when len > limit do
    # FIXME: check if the result we're removing had any binaries in it and
    # garbage collect them
    limit_history(:queue.drop(queue), len-1, limit)
  end

  defp limit_history(queue, _, _) do
    queue
  end

  ### each ###

  def each(fun) do
    history = Process.get(:iex_history)
    each(history, fun)
  end

  defp each(queue, fun) do
    case :queue.out(queue) do
      { { :value, val }, tail } ->
        fun.(val)
        each(tail, fun)

      { :empty, _ } -> :ok
    end
  end

  ### nth ###

  def nth(n) do
    history = Process.get(:iex_history)
    len = :queue.len(history)
    case n do
      n when n > 0 ->
        queue_nth(history, n - 1, len)
      n when n < 0 ->
        queue_nth_r(history, -n - 1, len)
      0 ->
        raise ArgumentError[]
    end
  end

  defp queue_nth(_, _, 0) do
    raise_bounds
  end

  defp queue_nth(queue, 0, _) do
    { :value, value } = :queue.peek(queue)
    value
  end

  defp queue_nth(queue, n, len) when n > 0 do
    queue_nth(:queue.drop(queue), n-1, len-1)
  end

  defp queue_nth_r(_, _, 0) do
    raise_bounds
  end

  defp queue_nth_r(queue, 0, _) do
    { :value, value } = :queue.peek_r(queue)
    value
  end

  defp queue_nth_r(queue, n, len) when n > 0 do
    queue_nth_r(:queue.drop_r(queue), n-1, len-1)
  end

  defp raise_bounds do
    raise "Out of bounds"
  end
end
