defmodule Binary do
  @doc """
  Access the binary byte via an integer. Negative indexes
  performs an inverted lookup, for example, -1 can be
  used to retrieve the last byte in the binary.

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