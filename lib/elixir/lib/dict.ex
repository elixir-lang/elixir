defmodule Dict do
  @moduledoc ~S"""
  Generic API for dictionaries.

  If you need a general dictionary, use the `Map` module.
  If you need to manipulate keyword lists, use `Keyword`.

  To convert maps into keywords and vice-versa, use the
  `new` function in the respective modules.
  """

  @moduledoc deprecated: "Use Map or Keyword modules instead"

  @type key :: any
  @type value :: any
  @type t :: list | map

  message =
    "Use the Map module for working with maps or the Keyword module for working with keyword lists"

  # TODO: Remove by 2.0

  @deprecated message
  defmacro __using__(_) do
    # Use this import to guarantee proper code expansion
    import Kernel, except: [size: 1]

    quote do
      message = "Use maps and the Map module instead"

      @deprecated message
      def get(dict, key, default \\ nil) do
        case fetch(dict, key) do
          {:ok, value} -> value
          :error -> default
        end
      end

      @deprecated message
      def get_lazy(dict, key, fun) when is_function(fun, 0) do
        case fetch(dict, key) do
          {:ok, value} -> value
          :error -> fun.()
        end
      end

      @deprecated message
      def get_and_update(dict, key, fun) do
        current_value = get(dict, key)
        {get, new_value} = fun.(current_value)
        {get, put(dict, key, new_value)}
      end

      @deprecated message
      def fetch!(dict, key) do
        case fetch(dict, key) do
          {:ok, value} -> value
          :error -> raise KeyError, key: key, term: dict
        end
      end

      @deprecated message
      def has_key?(dict, key) do
        match?({:ok, _}, fetch(dict, key))
      end

      @deprecated message
      def put_new(dict, key, value) do
        case has_key?(dict, key) do
          true -> dict
          false -> put(dict, key, value)
        end
      end

      @deprecated message
      def put_new_lazy(dict, key, fun) when is_function(fun, 0) do
        case has_key?(dict, key) do
          true -> dict
          false -> put(dict, key, fun.())
        end
      end

      @deprecated message
      def drop(dict, keys) do
        Enum.reduce(keys, dict, &delete(&2, &1))
      end

      @deprecated message
      def take(dict, keys) do
        Enum.reduce(keys, new(), fn key, acc ->
          case fetch(dict, key) do
            {:ok, value} -> put(acc, key, value)
            :error -> acc
          end
        end)
      end

      @deprecated message
      def to_list(dict) do
        reduce(dict, {:cont, []}, fn kv, acc -> {:cont, [kv | acc]} end)
        |> elem(1)
        |> Enum.reverse()
      end

      @deprecated message
      def keys(dict) do
        reduce(dict, {:cont, []}, fn {k, _}, acc -> {:cont, [k | acc]} end)
        |> elem(1)
        |> Enum.reverse()
      end

      @deprecated message
      def values(dict) do
        reduce(dict, {:cont, []}, fn {_, v}, acc -> {:cont, [v | acc]} end)
        |> elem(1)
        |> Enum.reverse()
      end

      @deprecated message
      def equal?(dict1, dict2) do
        # Use this import to avoid conflicts in the user code
        import Kernel, except: [size: 1]

        case size(dict1) == size(dict2) do
          false ->
            false

          true ->
            reduce(dict1, {:cont, true}, fn {k, v}, _acc ->
              case fetch(dict2, k) do
                {:ok, ^v} -> {:cont, true}
                _ -> {:halt, false}
              end
            end)
            |> elem(1)
        end
      end

      @deprecated message
      def merge(dict1, dict2, fun \\ fn _k, _v1, v2 -> v2 end) do
        # Use this import to avoid conflicts in the user code
        import Kernel, except: [size: 1]

        if size(dict1) < size(dict2) do
          reduce(dict1, {:cont, dict2}, fn {k, v1}, acc ->
            {:cont, update(acc, k, v1, &fun.(k, v1, &1))}
          end)
        else
          reduce(dict2, {:cont, dict1}, fn {k, v2}, acc ->
            {:cont, update(acc, k, v2, &fun.(k, &1, v2))}
          end)
        end
        |> elem(1)
      end

      @deprecated message
      def update(dict, key, initial, fun) do
        case fetch(dict, key) do
          {:ok, value} ->
            put(dict, key, fun.(value))

          :error ->
            put(dict, key, initial)
        end
      end

      @deprecated message
      def update!(dict, key, fun) do
        case fetch(dict, key) do
          {:ok, value} ->
            put(dict, key, fun.(value))

          :error ->
            raise KeyError, key: key, term: dict
        end
      end

      @deprecated message
      def pop(dict, key, default \\ nil) do
        case fetch(dict, key) do
          {:ok, value} ->
            {value, delete(dict, key)}

          :error ->
            {default, dict}
        end
      end

      @deprecated message
      def pop_lazy(dict, key, fun) when is_function(fun, 0) do
        case fetch(dict, key) do
          {:ok, value} ->
            {value, delete(dict, key)}

          :error ->
            {fun.(), dict}
        end
      end

      @deprecated message
      def split(dict, keys) do
        Enum.reduce(keys, {new(), dict}, fn key, {inc, exc} = acc ->
          case fetch(exc, key) do
            {:ok, value} ->
              {put(inc, key, value), delete(exc, key)}

            :error ->
              acc
          end
        end)
      end

      defoverridable merge: 2,
                     merge: 3,
                     equal?: 2,
                     to_list: 1,
                     keys: 1,
                     values: 1,
                     take: 2,
                     drop: 2,
                     get: 2,
                     get: 3,
                     fetch!: 2,
                     has_key?: 2,
                     put_new: 3,
                     pop: 2,
                     pop: 3,
                     split: 2,
                     update: 4,
                     update!: 3,
                     get_and_update: 3,
                     get_lazy: 3,
                     pop_lazy: 3,
                     put_new_lazy: 3
    end
  end

  defmacrop target(dict) do
    quote do
      case unquote(dict) do
        %module{} -> module
        %{} -> Map
        dict when is_list(dict) -> Keyword
        dict -> unsupported_dict(dict)
      end
    end
  end

  @deprecated message
  @spec keys(t) :: [key]
  def keys(dict) do
    target(dict).keys(dict)
  end

  @deprecated message
  @spec values(t) :: [value]
  def values(dict) do
    target(dict).values(dict)
  end

  @deprecated message
  @spec size(t) :: non_neg_integer
  def size(dict) do
    target(dict).size(dict)
  end

  @deprecated message
  @spec has_key?(t, key) :: boolean
  def has_key?(dict, key) do
    target(dict).has_key?(dict, key)
  end

  @deprecated message
  @spec get(t, key, value) :: value
  def get(dict, key, default \\ nil) do
    target(dict).get(dict, key, default)
  end

  @deprecated message
  @spec get_lazy(t, key, (() -> value)) :: value
  def get_lazy(dict, key, fun) do
    target(dict).get_lazy(dict, key, fun)
  end

  @deprecated message
  @spec get_and_update(t, key, (value -> {value, value})) :: {value, t}
  def get_and_update(dict, key, fun) do
    target(dict).get_and_update(dict, key, fun)
  end

  @deprecated message
  @spec fetch(t, key) :: value
  def fetch(dict, key) do
    target(dict).fetch(dict, key)
  end

  @deprecated message
  @spec fetch!(t, key) :: value
  def fetch!(dict, key) do
    target(dict).fetch!(dict, key)
  end

  @deprecated message
  @spec put(t, key, value) :: t
  def put(dict, key, val) do
    target(dict).put(dict, key, val)
  end

  @deprecated message
  @spec put_new(t, key, value) :: t
  def put_new(dict, key, val) do
    target(dict).put_new(dict, key, val)
  end

  @deprecated message
  @spec put_new_lazy(t, key, (() -> value)) :: t
  def put_new_lazy(dict, key, fun) do
    target(dict).put_new_lazy(dict, key, fun)
  end

  @deprecated message
  @spec delete(t, key) :: t
  def delete(dict, key) do
    target(dict).delete(dict, key)
  end

  @deprecated message
  @spec merge(t, t) :: t
  def merge(dict1, dict2) do
    target1 = target(dict1)
    target2 = target(dict2)

    if target1 == target2 do
      target1.merge(dict1, dict2)
    else
      do_merge(target1, dict1, dict2, fn _k, _v1, v2 -> v2 end)
    end
  end

  @deprecated message
  @spec merge(t, t, (key, value, value -> value)) :: t
  def merge(dict1, dict2, fun) do
    target1 = target(dict1)
    target2 = target(dict2)

    if target1 == target2 do
      target1.merge(dict1, dict2, fun)
    else
      do_merge(target1, dict1, dict2, fun)
    end
  end

  defp do_merge(target1, dict1, dict2, fun) do
    Enumerable.reduce(dict2, {:cont, dict1}, fn {k, v}, acc ->
      {:cont, target1.update(acc, k, v, fn other -> fun.(k, other, v) end)}
    end)
    |> elem(1)
  end

  @deprecated message
  @spec pop(t, key, value) :: {value, t}
  def pop(dict, key, default \\ nil) do
    target(dict).pop(dict, key, default)
  end

  @deprecated message
  @spec pop_lazy(t, key, (() -> value)) :: {value, t}
  def pop_lazy(dict, key, fun) do
    target(dict).pop_lazy(dict, key, fun)
  end

  @deprecated message
  @spec update!(t, key, (value -> value)) :: t
  def update!(dict, key, fun) do
    target(dict).update!(dict, key, fun)
  end

  @deprecated message
  @spec update(t, key, value, (value -> value)) :: t
  def update(dict, key, initial, fun) do
    target(dict).update(dict, key, initial, fun)
  end

  @deprecated message
  @spec split(t, [key]) :: {t, t}
  def split(dict, keys) do
    target(dict).split(dict, keys)
  end

  @deprecated message
  @spec drop(t, [key]) :: t
  def drop(dict, keys) do
    target(dict).drop(dict, keys)
  end

  @deprecated message
  @spec take(t, [key]) :: t
  def take(dict, keys) do
    target(dict).take(dict, keys)
  end

  @deprecated message
  @spec empty(t) :: t
  def empty(dict) do
    target(dict).empty(dict)
  end

  @deprecated message
  @spec equal?(t, t) :: boolean
  def equal?(dict1, dict2) do
    target1 = target(dict1)
    target2 = target(dict2)

    cond do
      target1 == target2 ->
        target1.equal?(dict1, dict2)

      target1.size(dict1) == target2.size(dict2) ->
        Enumerable.reduce(dict2, {:cont, true}, fn {k, v}, _acc ->
          case target1.fetch(dict1, k) do
            {:ok, ^v} -> {:cont, true}
            _ -> {:halt, false}
          end
        end)
        |> elem(1)

      true ->
        false
    end
  end

  @deprecated message
  @spec to_list(t) :: list
  def to_list(dict) do
    target(dict).to_list(dict)
  end

  @spec unsupported_dict(t) :: no_return
  defp unsupported_dict(dict) do
    raise ArgumentError, "unsupported dict: #{inspect(dict)}"
  end
end
