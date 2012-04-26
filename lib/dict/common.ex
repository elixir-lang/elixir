defmodule Dict.Common do
  defmacro __using__(_module, ref) do
    quote do
      @doc """
      Creates a new empty dict.
      """
      def new do
        unquote(ref).empty(nil)
      end

      @doc """
      Creates a new dict with one entry.
      """
      def new({key, value}) do
        unquote(ref).put new, {key, value}
      end

      @doc """
      Creates a new dict from a list of pairs.

      ## Examples

          Dict.new [{:b,1},{:a,2}]
          #=> [a: 1, b: 2]

      """
      def new(pairs) do
        Enum.reduce pairs, new, fn(pair, dict) ->
          unquote(ref).put(dict, pair)
        end
      end

      @doc """
      Creates a new dict from a list of elements with the
      help of the transformation function.

      ## Examples

          Dict.new ["a", "b"], fn(x) -> {x, x} end
          #=> ["a": "a", "b": "b"]
      """
      def new(list, transform) when is_function(transform) do
        Enum.reduce list, new(), fn(i, dict) ->
          pair = transform.(i)
          unquote(ref).put(dict, pair)
        end
      end
    end
  end
end