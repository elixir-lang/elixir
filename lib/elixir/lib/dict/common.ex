defmodule Dict.Common do
  @moduledoc false

  defmacro __using__(_) do
    quote do
      @behavior Dict

      @doc """
      Creates a new empty dict.
      """
      def new do
        empty(nil)
      end

      @doc """
      Creates a new dict from a list of pairs.

      ## Examples

          #{inspect(__MODULE__)}.new [{:b,1},{:a,2}]
          #=> [a: 1, b: 2]

      """
      def new(pairs) do
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
        Enum.reduce list, new(), fn i, dict ->
          { k, v } = transform.(i)
          put(dict, k, v)
        end
      end
    end
  end
end