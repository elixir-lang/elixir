# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Stream.Reducers do
  # Collection of reducers and utilities shared by Enum and Stream.
  @moduledoc false

  def zip_with(enumerables, zip_fun) when is_function(zip_fun, 1) do
    if is_list(enumerables) and :lists.all(&is_list/1, enumerables) do
      &zip_list(enumerables, &1, &2, zip_fun)
    else
      &zip_enum(enumerables, &1, &2, zip_fun)
    end
  end

  defp zip_list(_enumerables, {:halt, acc}, _fun, _zip_fun) do
    {:halted, acc}
  end

  defp zip_list(enumerables, {:suspend, acc}, fun, zip_fun) do
    {:suspended, acc, &zip_list(enumerables, &1, fun, zip_fun)}
  end

  defp zip_list([], {:cont, acc}, _fun, _zip_fun), do: {:done, acc}

  defp zip_list(enumerables, {:cont, acc}, fun, zip_fun) do
    case zip_list_heads_tails(enumerables, [], []) do
      {heads, tails} -> zip_list(tails, fun.(zip_fun.(heads), acc), fun, zip_fun)
      :error -> {:done, acc}
    end
  end

  defp zip_list_heads_tails([[head | tail] | rest], heads, tails) do
    zip_list_heads_tails(rest, [head | heads], [tail | tails])
  end

  defp zip_list_heads_tails([[] | _rest], _heads, _tails) do
    :error
  end

  defp zip_list_heads_tails([], heads, tails) do
    {:lists.reverse(heads), :lists.reverse(tails)}
  end

  defp zip_enum(enumerables, acc, fun, zip_fun) do
    step = fn x, acc ->
      {:suspend, :lists.reverse([x | acc])}
    end

    enum_funs =
      Enum.map(enumerables, fn enum ->
        {&Enumerable.reduce(enum, &1, step), [], :cont}
      end)

    do_zip_enum(enum_funs, acc, fun, zip_fun)
  end

  # This implementation of do_zip_enum/4 works for any number of streams to zip
  defp do_zip_enum(zips, {:halt, acc}, _fun, _zip_fun) do
    do_zip_close(zips)
    {:halted, acc}
  end

  defp do_zip_enum(zips, {:suspend, acc}, fun, zip_fun) do
    {:suspended, acc, &do_zip_enum(zips, &1, fun, zip_fun)}
  end

  defp do_zip_enum([], {:cont, acc}, _callback, _zip_fun) do
    {:done, acc}
  end

  defp do_zip_enum(zips, {:cont, acc}, callback, zip_fun) do
    try do
      do_zip_next(zips, acc, callback, [], [], zip_fun)
    catch
      kind, reason ->
        do_zip_close(zips)
        :erlang.raise(kind, reason, __STACKTRACE__)
    else
      {:next, buffer, acc} ->
        do_zip_enum(buffer, acc, callback, zip_fun)

      {:done, _acc} = other ->
        other
    end
  end

  # do_zip_next/6 computes the next tuple formed by
  # the next element of each zipped stream.
  defp do_zip_next(
         [{_, [], :halt} | zips],
         acc,
         _callback,
         _yielded_elems,
         buffer,
         _zip_fun
       ) do
    do_zip_close(:lists.reverse(buffer, zips))
    {:done, acc}
  end

  defp do_zip_next([{fun, [], :cont} | zips], acc, callback, yielded_elems, buffer, zip_fun) do
    case fun.({:cont, []}) do
      {:suspended, [elem | next_acc], fun} ->
        next_buffer = [{fun, next_acc, :cont} | buffer]
        do_zip_next(zips, acc, callback, [elem | yielded_elems], next_buffer, zip_fun)

      {_, [elem | next_acc]} ->
        next_buffer = [{fun, next_acc, :halt} | buffer]
        do_zip_next(zips, acc, callback, [elem | yielded_elems], next_buffer, zip_fun)

      {_, []} ->
        # The current zipped stream terminated, so we close all the streams
        # and return {:halted, acc} (which is returned as is by do_zip/3).
        do_zip_close(:lists.reverse(buffer, zips))
        {:done, acc}
    end
  end

  defp do_zip_next(
         [{fun, zip_acc, zip_op} | zips],
         acc,
         callback,
         yielded_elems,
         buffer,
         zip_fun
       ) do
    [elem | rest] = zip_acc
    next_buffer = [{fun, rest, zip_op} | buffer]
    do_zip_next(zips, acc, callback, [elem | yielded_elems], next_buffer, zip_fun)
  end

  defp do_zip_next([] = _zips, acc, callback, yielded_elems, buffer, zip_fun) do
    # "yielded_elems" is a reversed list of results for the current iteration of
    # zipping. That is to say, the nth element from each of the enums being zipped.
    # It needs to be reversed and passed to the zipping function so it can do it's thing.
    {:next, :lists.reverse(buffer), callback.(zip_fun.(:lists.reverse(yielded_elems)), acc)}
  end

  defp do_zip_close(zips) do
    :lists.foreach(fn {fun, _, _} -> fun.({:halt, []}) end, zips)
  end

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
        if unquote(callback).(entry) do
          skip(acc)
        else
          next(unquote(fun), entry, acc)
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

  defmacro with_index(fun) do
    quote do
      fn entry, acc(head, counter, tail) ->
        next_with_acc(unquote(fun), {entry, counter}, head, counter + 1, tail)
      end
    end
  end

  defmacro with_index(callback, fun) do
    quote do
      fn entry, acc(head, counter, tail) ->
        next_with_acc(unquote(fun), unquote(callback).(entry, counter), head, counter + 1, tail)
      end
    end
  end
end
