defmodule Tuple do
  @doc """
  Access the tuple via an integer. Negative indexes
  performs an inverted lookup, for example, -1 can be
  used to retrieve the last item in the tuple. Returns
  nil if an out of bounds access occurs.

  This implements the same API as the `Access` protocol.

  ## Examples

      tuple = { :a, :b, :c }
      Tuple.access tuple, -1 #=> :c

  """
  def access(tuple, access) when is_tuple(tuple) do
    Access.Tuple.access(tuple, access)
  end
end