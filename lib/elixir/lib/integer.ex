defmodule Integer do
  @moduledoc """
  Functions for working with integers.
  """

  import Bitwise

  @doc """
  Determines if an integer is odd.

  Returns `true` if `n` is an odd number, otherwise `false`.
  Implemented as a macro so it is allowed in guard clauses.
  """
  defmacro odd?(n) do
    quote do: (unquote(n) &&& 1) == 1
  end

  @doc """
  Determines if an integer is even.

  Returns `true` if `n` is an even number, otherwise `false`.
  Implemented as a macro so it is allowed in guard clauses.
  """
  defmacro even?(n) do
    quote do: (unquote(n) &&& 1) == 0
  end

  @doc """
  Converts a binary to an integer.

  If successful, returns a tuple of the form `{ integer, remainder_of_binary }`.
  Otherwise `:error`.

  ## Examples

      iex> Integer.parse("34")
      {34,""}

      iex> Integer.parse("34.5")
      {34,".5"}

      iex> Integer.parse("three")
      :error

  """
  @spec parse(binary) :: { integer, binary } | :error
  def parse(<< ?-, bin :: binary >>) do
    case do_parse(bin) do
      :error -> :error
      { number, remainder } -> { -number, remainder }
    end
  end

  def parse(<< ?+, bin :: binary >>) do
    do_parse(bin)
  end

  def parse(bin) when is_binary(bin) do
    do_parse(bin)
  end

  defp do_parse(<< char, bin :: binary >>) when char in ?0..?9, do: do_parse(bin, char - ?0)
  defp do_parse(_), do: :error

  defp do_parse(<< char, rest :: binary >>, acc) when char in ?0..?9 do
    do_parse rest, 10 * acc + (char - ?0)
  end

  defp do_parse(bitstring, acc) do
    { acc, bitstring }
  end
end
