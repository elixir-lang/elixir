defmodule Stream.Reducers do
  # Collection of reducers shared by Enum and Stream.
  @moduledoc false

  defmacro chunk(n, step, limit, f \\ nil) do
    quote do
      fn entry, acc(h, {buffer, count}, t) ->
        buffer = [entry|buffer]
        count  = count + 1

        new =
          if count >= unquote(limit) do
            left = count - unquote(step)
            {Enum.take(buffer, left), left}
          else
            {buffer, count}
          end

        if count == unquote(n) do
          next_with_acc(unquote(f), :lists.reverse(buffer), h, new, t)
        else
          skip(acc(h, new, t))
        end
      end
    end
  end

  defmacro chunk_by(callback, f \\ nil) do
    quote do
      fn
        entry, acc(h, {buffer, value}, t) ->
          new_value = unquote(callback).(entry)
          if new_value == value do
            skip(acc(h, {[entry|buffer], value}, t))
          else
            next_with_acc(unquote(f), :lists.reverse(buffer), h, {[entry], new_value}, t)
          end
        entry, acc(h, nil, t) ->
          skip(acc(h, {[entry], unquote(callback).(entry)}, t))
      end
    end
  end

  defmacro dedup(callback, f \\ nil) do
    quote do
      fn(entry, acc(h, prev, t) = acc) ->
        value = unquote(callback).(entry)
        case prev do
          {:value, ^value}  -> skip(acc)
          _ -> next_with_acc(unquote(f), entry, h, {:value, value}, t)
        end
      end
    end
  end

  defmacro drop(f \\ nil) do
    quote do
      fn
        _entry, acc(h, n, t) when n > 0 ->
          skip(acc(h, n-1, t))
        entry, acc(h, n, t) ->
          next_with_acc(unquote(f), entry, h, n, t)
      end
    end
  end

  defmacro drop_while(callback, f \\ nil) do
    quote do
      fn entry, acc(h, bool, t) = orig ->
        if bool and unquote(callback).(entry) do
          skip(orig)
        else
          next_with_acc(unquote(f), entry, h, false, t)
        end
      end
    end
  end

  defmacro filter(callback, f \\ nil) do
    quote do
      fn(entry, acc) ->
        if unquote(callback).(entry) do
          next(unquote(f), entry, acc)
        else
          skip(acc)
        end
      end
    end
  end

  defmacro filter_map(filter, mapper, f \\ nil) do
    quote do
      fn(entry, acc) ->
        if unquote(filter).(entry) do
          next(unquote(f), unquote(mapper).(entry), acc)
        else
          skip(acc)
        end
      end
    end
  end

  defmacro map(callback, f \\ nil) do
    quote do
      fn(entry, acc) ->
        next(unquote(f), unquote(callback).(entry), acc)
      end
    end
  end

  defmacro reject(callback, f \\ nil) do
    quote do
      fn(entry, acc) ->
        unless unquote(callback).(entry) do
          next(unquote(f), entry, acc)
        else
          skip(acc)
        end
      end
    end
  end

  defmacro scan_2(callback, f \\ nil) do
    quote do
      fn
        entry, acc(h, :first, t) ->
          next_with_acc(unquote(f), entry, h, {:ok, entry}, t)
        entry, acc(h, {:ok, acc}, t) ->
          value = unquote(callback).(entry, acc)
          next_with_acc(unquote(f), value, h, {:ok, value}, t)
      end
    end
  end

  defmacro scan_3(callback, f \\ nil) do
    quote do
      fn(entry, acc(h, acc, t)) ->
        value = unquote(callback).(entry, acc)
        next_with_acc(unquote(f), value, h, value, t)
      end
    end
  end

  defmacro take(f \\ nil) do
    quote do
      fn(entry, acc(h, n, t) = orig) ->
        case n do
          0 ->
            {:halt, orig}
          1 ->
            case next_with_acc(unquote(f), entry, h, n-1, t) do
              {:cont, acc} -> {:halt, acc}
              reason -> reason
            end
          _ ->
            next_with_acc(unquote(f), entry, h, n-1, t)
        end
      end
    end
  end

  defmacro take_every(nth, f \\ nil) do
    quote do
      fn
        entry, acc(h, n, t) when n === :first
                            when n === unquote(nth) ->
          next_with_acc(unquote(f), entry, h, 1, t)
        entry, acc(h, n, t) ->
          skip(acc(h, n+1, t))
      end
    end
  end

  defmacro take_while(callback, f \\ nil) do
    quote do
      fn(entry, acc) ->
        if unquote(callback).(entry) do
          next(unquote(f), entry, acc)
        else
          {:halt, acc}
        end
      end
    end
  end

  defmacro uniq(callback, f \\ nil) do
    quote do
      fn(entry, acc(h, prev, t) = acc) ->
        value = unquote(callback).(entry)
        if Map.has_key?(prev, value) do
          skip(acc)
        else
          next_with_acc(unquote(f), entry, h, Map.put(prev, value, true), t)
        end
      end
    end
  end

  defmacro with_index(f \\ nil) do
    quote do
      fn(entry, acc(h, counter, t)) ->
        next_with_acc(unquote(f), {entry, counter}, h, counter + 1, t)
      end
    end
  end
end
