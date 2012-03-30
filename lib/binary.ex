defmodule Binary do
  @doc """
  Access the binary via a predicate.

  If an integer, it does a byte lookup with the index
  starting with one. Negative indexes performs a reverse
  lookup, for example, -1 can be used to retrieve the
  last byte in the binary. Returns nil if an out of bounds
  access occurs.

  If a regular expression, it returns a binary with the
  matched contents.

  This implements the same API as the `Access` protocol.

  Notice currently Elixir does not provide functions for
  accessing utf-8 code points.

  ## Examples

      binary = "abc"
      Binary.access binary, -1 #=> 99

  """
  def access(binary, access) do
    Access.BitString.access(binary, access)
  end
end