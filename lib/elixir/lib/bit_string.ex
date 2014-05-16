defmodule BitString do
  @doc """
  Convenience functions for working with bitstrings.
  """

  @doc """
  Converts a bitstring to list.

  If the number of bits in the binary is not divisible by 8,
  the last element of the list will be a bitstring containing
  the remaining bits (1 up to 7 bits).

  Inlined by the compiler.
  """
  @spec to_list(bitstring) :: list
  def to_list(bitstring) do
    :erlang.bitstring_to_list(bitstring)
  end
end
