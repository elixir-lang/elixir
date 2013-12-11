defmodule Stream.Reducers do
  # Collection of reducers shared by Enum and Stream.
  @moduledoc false

  defmacro chunks_by(fun, f // nil) do
    quote do
      fn
        entry, acc(h, { buffer, value }, t) ->
          new_value = unquote(fun).(entry)
          if new_value == value do
            { :cont, acc(h, { [entry|buffer], value }, t) }
          else
            cont_with_acc(unquote(f), :lists.reverse(buffer), h, { [entry], new_value }, t)
          end
        entry, acc(h, nil, t) ->
          { :cont, acc(h, { [entry], unquote(fun).(entry) }, t) }
      end
    end
  end

  defmacro drop(f // nil) do
    quote do
      fn
        _entry, acc(h, n, t) when n > 0 ->
          { :cont, acc(h, n-1, t) }
        entry, acc(h, n, t) ->
          cont_with_acc(unquote(f), entry, h, n, t)
      end
    end
  end

  defmacro drop_while(callback, f // nil) do
    quote do
      fn entry, acc(h, bool, t) = orig ->
        if bool and unquote(callback).(entry) do
          { :cont, orig }
        else
          cont_with_acc(unquote(f), entry, h, false, t)
        end
      end
    end
  end

  defmacro filter(callback, f // nil) do
    quote do
      fn(entry, acc) ->
        if unquote(callback).(entry) do
          cont(unquote(f), entry, acc)
        else
          { :cont, acc }
        end
      end
    end
  end

  defmacro filter_map(filter, mapper, f // nil) do
    quote do
      fn(entry, acc) ->
        if unquote(filter).(entry) do
          cont(unquote(f), unquote(mapper).(entry), acc)
        else
          { :cont, acc }
        end
      end
    end
  end

  defmacro map(callback, f // nil) do
    quote do
      fn(entry, acc) ->
        cont(unquote(f), unquote(callback).(entry), acc)
      end
    end
  end

  defmacro reject(callback, f // nil) do
    quote do
      fn(entry, acc) ->
        unless unquote(callback).(entry) do
          cont(unquote(f), entry, acc)
        else
          { :cont, acc }
        end
      end
    end
  end

  defmacro take(f // nil) do
    quote do
      fn(entry, acc(h, n, t) = orig) ->
        if n >= 1 do
          cont_with_acc(unquote(f), entry, h, n-1, t)
        else
          { :halt, orig }
        end
      end
    end
  end

  defmacro take_every(nth, f // nil) do
    quote do
      fn
        entry, acc(h, n, t) when n === :first
                            when n === unquote(nth) ->
          cont_with_acc(unquote(f), entry, h, 1, t)
        entry, acc(h, n, t) ->
          { :cont, acc(h, n+1, t) }
      end
    end
  end

  defmacro take_while(callback, f // nil) do
    quote do
      fn(entry, acc) ->
        if unquote(callback).(entry) do
          cont(unquote(f), entry, acc)
        else
          { :halt, acc }
        end
      end
    end
  end

  defmacro with_index(f // nil) do
    quote do
      fn(entry, acc(h, counter, t)) ->
        cont_with_acc(unquote(f), { entry, counter }, h, counter + 1, t)
      end
    end
  end
end
