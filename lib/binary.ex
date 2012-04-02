defmodule Binary do
  @doc """
  Access the binary via a predicate.

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