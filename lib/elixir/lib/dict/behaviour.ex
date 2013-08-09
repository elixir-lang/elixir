defmodule Dict.Behaviour do
  @moduledoc """
  This module makes it easier to create your own `Dict` compliant
  module, by providing default implementations for some required functions.
  
  Usage:

      defmodule MyDict do
        use Dict.Behaviour

        # implement required functions (see below)

        # override default implementations if needed
      end

  The client module must contain following functions: `size/1`, `fetch/2`, 
  `put/3`, `update/4`, `delete/2` and `reduce/3`. All of them are part of
  the Dict behaviour, so no extra functions are actually required.

  Based on these functions, `Dict.Behaviour` generates default implementations 
  for other functions such as `drop`, `take`, etc. All of the functions are
  defined as overridable, so you can provide your own implementation if
  needed.

  If you implement `new/0` and `new/1` functions, you can also test your custom 
  module via `Dict` doctests:

      defmodule MyDict do
        def new(keywords // []) do
          ...
        end
      end

      defmodule MyTests do
        use ExUnit.Case
        doctest Dict
        defp dict_impl, do: MyDict
      end

  """

  defmacro __using__(_) do
    quote do
      @behaviour Dict
      
      def get(dict, key, default // nil) do
        case fetch(dict, key) do
          { :ok, value } -> value
          :error -> default
        end
      end
      defoverridable get: 2, get: 3

      def fetch!(dict, key) do
        case fetch(dict, key) do
          { :ok, value } -> value
          :error -> raise(KeyError, key: key)
        end
      end
      defoverridable fetch!: 2
 
      def has_key?(dict, key) do
        match? { :ok, _ }, fetch(dict, key)
      end
      defoverridable has_key?: 2
 
      def put_new(dict, key, value) do
        update(dict, key, value, fn(v) -> v end)
      end
      defoverridable put_new: 3
 
      def drop(dict, []), do: dict

      def drop(dict, [key|keys]) do
        drop(delete(dict, key), keys)
      end
      defoverridable drop: 2
 
      def take(dict, keys) do
        take(dict, keys, new)
      end
      defoverridable take: 2
 
      defp take(_dict, [], acc), do: acc
      defp take(dict, [key|keys], acc) do
        case fetch(dict, key) do
          { :ok, value } -> take(dict, keys, put(acc, key, value))
          :error -> take(dict, keys, acc)
        end
      end
 
      def to_list(dict), do: reduce(dict, [], &[&1|&2]) |> Enum.reverse
      defoverridable to_list: 1
 
      def keys(dict), do: reduce(dict, [], fn({k, _}, acc) -> [k | acc] end) |> Enum.reverse
      defoverridable keys: 1
 
      def values(dict), do: reduce(dict, [], fn({_, v}, acc) -> [v | acc] end) |> Enum.reverse
      defoverridable values: 1
 
      def equal?(dict1, dict2) do
        case size(dict1) == size(dict2) do
          false -> false
          true -> 
            try do
              reduce(dict1, nil, fn({ k, v }, _acc) ->
                unless fetch(dict2, k) == { :ok, v }, do: throw(:error)
              end)
              true
            catch
              :error -> false
            end
        end
      end
      defoverridable equal?: 2
 
      def merge(dict, enumerable, callback // fn(_k, _v1, v2) -> v2 end) do
        Enum.reduce(enumerable, dict, fn({key, value}, acc) ->
          update(acc, key, value, fn(v1) -> callback.(key, v1, value) end)
        end)
      end
      defoverridable merge: 2, merge: 3
    end
  end
end