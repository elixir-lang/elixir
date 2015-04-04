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
end
