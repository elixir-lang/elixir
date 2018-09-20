defmodule Stream.Reducers do
  # Collection of reducers and utilities shared by Enum and Stream.
  @moduledoc false

  def chunk_every(chunk_by, enumerable, count, step, leftover) do
    limit = :erlang.max(count, step)

    chunk_fun = fn entry, {acc_buffer, acc_count} ->
      acc_buffer = [entry | acc_buffer]
      acc_count = acc_count + 1

      new_state =
        if acc_count >= limit do
          remaining = acc_count - step
          {Enum.take(acc_buffer, remaining), remaining}
        else
          {acc_buffer, acc_count}
        end

      if acc_count == count do
        {:cont, :lists.reverse(acc_buffer), new_state}
      else
        {:cont, new_state}
      end
    end

    after_fun = fn {acc_buffer, acc_count} ->
      if leftover == :discard or acc_count == 0 or acc_count >= count do
        {:cont, []}
      else
        {:cont, :lists.reverse(acc_buffer, Enum.take(leftover, count - acc_count)), []}
      end
    end

    chunk_by.(enumerable, {[], 0}, chunk_fun, after_fun)
  end

  def chunk_by(chunk_by, enumerable, fun) do
    chunk_fun = fn
      entry, nil ->
        {:cont, {[entry], fun.(entry)}}

      entry, {acc, value} ->
        case fun.(entry) do
          ^value -> {:cont, {[entry | acc], value}}
          new_value -> {:cont, :lists.reverse(acc), {[entry], new_value}}
        end
    end

    after_fun = fn
      nil -> {:cont, :done}
      {acc, _value} -> {:cont, :lists.reverse(acc), :done}
    end

    chunk_by.(enumerable, nil, chunk_fun, after_fun)
  end

  defmacro dedup(callback, fun \\ nil) do
    quote do
      fn entry, acc(head, prev, tail) = acc ->
        value = unquote(callback).(entry)

        case prev do
          {:value, ^value} -> skip(acc)
          _ -> next_with_acc(unquote(fun), entry, head, {:value, value}, tail)
        end
      end
    end
  end

  defmacro drop(fun \\ nil) do
    quote do
      fn
        _entry, acc(head, amount, tail) when amount > 0 ->
          skip(acc(head, amount - 1, tail))

        entry, acc(head, amount, tail) ->
          next_with_acc(unquote(fun), entry, head, amount, tail)
      end
    end
  end

  defmacro drop_every(nth, fun \\ nil) do
    quote do
      fn
        entry, acc(head, curr, tail) when curr in [unquote(nth), :first] ->
          skip(acc(head, 1, tail))

        entry, acc(head, curr, tail) ->
          next_with_acc(unquote(fun), entry, head, curr + 1, tail)
      end
    end
  end

  defmacro drop_while(callback, fun \\ nil) do
    quote do
      fn entry, acc(head, bool, tail) = original ->
        if bool and unquote(callback).(entry) do
          skip(original)
        else
          next_with_acc(unquote(fun), entry, head, false, tail)
        end
      end
    end
  end

  defmacro filter(callback, fun \\ nil) do
    quote do
      fn entry, acc ->
        if unquote(callback).(entry) do
          next(unquote(fun), entry, acc)
        else
          skip(acc)
        end
      end
    end
  end

  defmacro filter_map(filter, mapper, fun \\ nil) do
    quote do
      fn entry, acc ->
        if unquote(filter).(entry) do
          next(unquote(fun), unquote(mapper).(entry), acc)
        else
          skip(acc)
        end
      end
    end
  end

  defmacro map(callback, fun \\ nil) do
    quote do
      fn entry, acc ->
        next(unquote(fun), unquote(callback).(entry), acc)
      end
    end
  end

  defmacro map_every(nth, mapper, fun \\ nil) do
    quote do
      fn
        entry, acc(head, curr, tail) when curr in [unquote(nth), :first] ->
          next_with_acc(unquote(fun), unquote(mapper).(entry), head, 1, tail)

        entry, acc(head, curr, tail) ->
          next_with_acc(unquote(fun), entry, head, curr + 1, tail)
      end
    end
  end

  defmacro reject(callback, fun \\ nil) do
    quote do
      fn entry, acc ->
        unless unquote(callback).(entry) do
          next(unquote(fun), entry, acc)
        else
          skip(acc)
        end
      end
    end
  end

  defmacro scan2(callback, fun \\ nil) do
    quote do
      fn
        entry, acc(head, :first, tail) ->
          next_with_acc(unquote(fun), entry, head, {:ok, entry}, tail)

        entry, acc(head, {:ok, acc}, tail) ->
          value = unquote(callback).(entry, acc)
          next_with_acc(unquote(fun), value, head, {:ok, value}, tail)
      end
    end
  end

  defmacro scan3(callback, fun \\ nil) do
    quote do
      fn entry, acc(head, acc, tail) ->
        value = unquote(callback).(entry, acc)
        next_with_acc(unquote(fun), value, head, value, tail)
      end
    end
  end

  defmacro take(fun \\ nil) do
    quote do
      fn entry, acc(head, curr, tail) = original ->
        case curr do
          0 ->
            {:halt, original}

          1 ->
            {_, acc} = next_with_acc(unquote(fun), entry, head, 0, tail)
            {:halt, acc}

          _ ->
            next_with_acc(unquote(fun), entry, head, curr - 1, tail)
        end
      end
    end
  end

  defmacro take_every(nth, fun \\ nil) do
    quote do
      fn
        entry, acc(head, curr, tail) when curr in [unquote(nth), :first] ->
          next_with_acc(unquote(fun), entry, head, 1, tail)

        entry, acc(head, curr, tail) ->
          skip(acc(head, curr + 1, tail))
      end
    end
  end

  defmacro take_while(callback, fun \\ nil) do
    quote do
      fn entry, acc ->
        if unquote(callback).(entry) do
          next(unquote(fun), entry, acc)
        else
          {:halt, acc}
        end
      end
    end
  end

  defmacro uniq_by(callback, fun \\ nil) do
    quote do
      fn entry, acc(head, prev, tail) = original ->
        value = unquote(callback).(entry)

        if Map.has_key?(prev, value) do
          skip(original)
        else
          next_with_acc(unquote(fun), entry, head, Map.put(prev, value, true), tail)
        end
      end
    end
  end

  defmacro with_index(fun \\ nil) do
    quote do
      fn entry, acc(head, counter, tail) ->
        next_with_acc(unquote(fun), {entry, counter}, head, counter + 1, tail)
      end
    end
  end
end
