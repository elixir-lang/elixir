defmodule Dict.Behaviour do
  @moduledoc false

  defmacro __using__(_) do
    # IO.write :stderr, "use Dict.Behaviour is deprecated\n" <>
    #                   Exception.format_stacktrace(Macro.Env.stacktrace(__CALLER__))

    # Use this import to guarantee proper code expansion
    import Kernel, except: [size: 1]

    quote do
      @behaviour Dict

      def get(dict, key, default \\ nil) do
        case fetch(dict, key) do
          {:ok, value} -> value
          :error -> default
        end
      end

      def fetch!(dict, key) do
        case fetch(dict, key) do
          {:ok, value} -> value
          :error -> raise KeyError, key: key, term: dict
        end
      end

      def has_key?(dict, key) do
        match? {:ok, _}, fetch(dict, key)
      end

      def put_new(dict, key, value) do
        update(dict, key, value, fn(v) -> v end)
      end

      def drop(dict, keys) do
        Enum.reduce keys, dict, &delete(&2, &1)
      end

      def take(dict, keys) do
        Enum.reduce keys, new, fn key, acc ->
          case fetch(dict, key) do
            {:ok, value} -> put(acc, key, value)
            :error -> acc
          end
        end
      end

      def to_list(dict) do
        reduce(dict, {:cont, []}, fn
          kv, acc -> {:cont, [kv|acc]}
        end) |> elem(1) |> :lists.reverse
      end

      def keys(dict) do
        reduce(dict, {:cont, []}, fn
          {k, _}, acc -> {:cont, [k|acc]}
        end) |> elem(1) |> :lists.reverse
      end

      def values(dict) do
        reduce(dict, {:cont, []}, fn
          {_, v}, acc -> {:cont, [v|acc]}
        end) |> elem(1) |> :lists.reverse
      end

      def equal?(dict1, dict2) do
        # Use this import to avoid conflicts in the user code
        import Kernel, except: [size: 1]

        case size(dict1) == size(dict2) do
          false -> false
          true  ->
            reduce(dict1, {:cont, true}, fn({k, v}, _acc) ->
              case fetch(dict2, k) do
                {:ok, ^v} -> {:cont, true}
                _ -> {:halt, false}
              end
            end) |> elem(1)
        end
      end

      def merge(dict1, dict2, fun \\ fn(_k, _v1, v2) -> v2 end) do
        reduce(dict1, {:cont, dict2}, fn {k, v1}, acc ->
          {:cont, update(acc, k, v1, &fun.(k, v1, &1))}
        end) |> elem(1)
      end

      def update(dict, key, initial, fun) do
        case fetch(dict, key) do
          { :ok, value } ->
            put(dict, key, fun.(value))

          :error ->
            put(dict, key, initial)
        end
      end

      def update!(dict, key, fun) do
        case fetch(dict, key) do
          { :ok, value } ->
            put(dict, key, fun.(value))

          :error ->
            raise KeyError, key: key, term: dict
        end
      end

      def pop(dict, key, default \\ nil) do
        case fetch(dict, key) do
          { :ok, value } ->
            { value, delete(dict, key) }

          :error ->
            { default, dict }
        end
      end

      def split(dict, keys) do
        Enum.reduce keys, { new, dict }, fn key, { inc, exc } = acc ->
          case fetch(exc, key) do
            { :ok, value } ->
              { put(inc, key, value), delete(exc, key) }

            :error ->
              acc
          end
        end
      end

      defoverridable merge: 2, merge: 3, equal?: 2, to_list: 1, keys: 1,
                     values: 1, take: 2, drop: 2, get: 2, get: 3, fetch!: 2,
                     has_key?: 2, put_new: 3, pop: 2, pop: 3, update: 4, update!: 3,
                     split: 2
    end
  end
end
