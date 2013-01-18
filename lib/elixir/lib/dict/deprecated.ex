defmodule Dict.Common do
  @moduledoc false

  defmacro __using__(_) do
    quote do
      @behavior Dict

      @doc """
      Creates a new empty dict.
      """
      def new do
        IO.puts "#{inspect __MODULE__} is deprecated, please use HashDict instead"
        Exception.print_stacktrace
        empty(nil)
      end

      @doc """
      Creates a new dict from a list of pairs.

      ## Examples

          #{inspect(__MODULE__)}.new [{:b,1},{:a,2}]
          #=> [a: 1, b: 2]

      """
      def new(pairs) do
        IO.puts "#{inspect __MODULE__} is deprecated, please use HashDict instead"
        Exception.print_stacktrace

        Enum.reduce pairs, new, fn { k, v }, dict ->
          put(dict, k, v)
        end
      end

      @doc """
      Creates a new dict from a list of elements with the
      help of the transformation function.

      ## Examples

          #{inspect(__MODULE__)}.new ["a", "b"], fn x -> {x, x} end
          #=> ["a": "a", "b": "b"]
      """
      def new(list, transform) when is_function(transform) do
        IO.puts "#{inspect __MODULE__} is deprecated, please use HashDict instead"
        Exception.print_stacktrace

        Enum.reduce list, new(), fn i, dict ->
          { k, v } = transform.(i)
          put(dict, k, v)
        end
      end
    end
  end
end

defmodule OrdDict do
  @moduledoc """
  This module implements a dictionary type that stores items
  as a list of tuples. It is a simple wrapper around
  [Erlang's orddict module](http://www.erlang.org/doc/man/orddict.html)
  and exposed via the `Dict` module.

  Check the `Dict` module for examples and documentation.
  """

  use Dict.Common

  defmacrop dict(data) do
    quote do
      { OrdDict, unquote(data) }
    end
  end

  @doc false
  def keys(dict(data)) do
    lc { k, _ } inlist data, do: k
  end

  @doc false
  def values(dict(data)) do
    lc { _, v } inlist data, do: v
  end

  @doc false
  def size(dict(data)) do
    length(data)
  end

  @doc false
  def has_key?(dict(data), key) do
    :orddict.is_key key, data
  end

  @doc false
  def get(dict(data), key, default // nil) do
    case :orddict.find(key, data) do
      {:ok, value} -> value
      :error       -> default
    end
  end

  @doc false
  def get!(dict(data), key) do
    case :orddict.find(key, data) do
      {:ok, value} -> value
      :error       -> raise(KeyError, key: key)
    end
  end

  @doc false
  def put(dict(data), key, value) do
    dict(:orddict.store key, value, data)
  end

  @doc false
  def put_new(dict(data), key, value) do
    dict(:orddict.update key, fn(x) -> x end, value, data)
  end

  @doc false
  def delete(dict(data), key) do
    dict(:orddict.erase key, data)
  end

  @doc false
  def merge(d1, d2, fun // fn(_k, _v1, v2) -> v2 end)

  def merge(dict(d1), dict(d2), fun) do
    dict(:orddict.merge fun, d1, d2)
  end

  def merge(dict(_) = d1, d2, fun) do
    merge(d1, new(d2), fun)
  end

  @doc false
  def update(dict(data), key, fun) do
    dict(:orddict.update key, fun, data)
  end

  @doc false
  def update(dict(data), key, initial, fun) do
    dict(:orddict.update key, fun, initial, data)
  end

  @doc false
  def empty(_) do
    dict([])
  end

  @doc false
  def to_list(dict(data)) do
    data
  end
end

defimpl Enum.Iterator, for: OrdDict do
  def iterator({ OrdDict, data }), do: data
  def count({ OrdDict, data }),    do: length(data)
end

defimpl Access, for: OrdDict do
  def access(dict, key), do: OrdDict.get(dict, key, nil)
end

defmodule Binary.Dict do
  @moduledoc """
  This module implements a dictionary that forces the keys to be
  converted to binaries on insertion. Currently it is implemented
  using an `OrdDict`, but this may change in the future.

  Check the `Dict` module for examples and documentation.
  """

  use Dict.Common

  defmacrop dict(data) do
    quote do
      { Binary.Dict, unquote(data) }
    end
  end

  @doc false
  def keys(dict(data)) do
    lc { k, _ } inlist data, do: k
  end

  @doc false
  def values(dict(data)) do
    lc { _, v } inlist data, do: v
  end

  @doc false
  def size(dict(data)) do
    length(data)
  end

  @doc false
  def has_key?(dict(data), key) do
    :orddict.is_key to_binary(key), data
  end

  @doc false
  def get(dict(data), key, default // nil) do
    case :orddict.find(to_binary(key), data) do
      {:ok, value} -> value
      :error       -> default
    end
  end

  @doc false
  def get!(dict(data), key) do
    case :orddict.find(to_binary(key), data) do
      {:ok, value} -> value
      :error       -> raise(KeyError, key: key)
    end
  end

  @doc false
  def put(dict(data), key, value) do
    dict(:orddict.store to_binary(key), value, data)
  end

  @doc false
  def put_new(dict(data), key, value) do
    dict(:orddict.update to_binary(key), fn(x) -> x end, value, data)
  end

  @doc false
  def delete(dict(data), key) do
    dict(:orddict.erase to_binary(key), data)
  end

  @doc false
  def merge(d1, d2, fun // fn(_k, _v1, v2) -> v2 end)

  def merge(dict(d1), dict(d2), fun) do
    dict(:orddict.merge fun, d1, d2)
  end

  def merge(dict(_) = d1, d2, fun) do
    merge(d1, new(d2), fun)
  end

  @doc false
  def update(dict(data), key, fun) do
    dict(:orddict.update to_binary(key), fun, data)
  end

  @doc false
  def update(dict(data), key, initial, fun) do
    dict(:orddict.update to_binary(key), fun, initial, data)
  end

  @doc false
  def empty(_) do
    dict([])
  end

  @doc false
  def to_list(dict(data)) do
    data
  end
end

defimpl Enum.Iterator, for: Binary.Dict do
  def iterator({ Binary.Dict, data }), do: data
  def count({ Binary.Dict, data }),    do: length(data)
end

defimpl Access, for: Binary.Dict do
  def access(dict, key), do: Binary.Dict.get(dict, key, nil)
end