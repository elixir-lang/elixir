defmodule Integer do
  @moduledoc """
  Functions for working with integers.
  """

  import Bitwise

  @doc """
  Returns true if `n` is an odd number, otherwise false.
  Implemented as a macro so it is allowed in guard clauses.
  """
  defmacro odd?(n) do
    quote do: (unquote(n) &&& 1) == 1
  end

  @doc """
  Returns true if `n` is an even number, otherwise false.
  Implemented as a macro so it is allowed in guard clauses.
  """
  defmacro even?(n) do
    quote do: (unquote(n) &&& 1) == 0
  end

  @doc """
  Converts a binary to an integer. If successful, returns a
  tuple of the form `{ integer, remainder_of_binary }`.
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
  def parse(<< ?-, char, rest :: binary >>) when char in ?0..?9 do
    case parse(<< char, rest :: binary >>, 0) do
      :error -> :error
      { number, remainder } -> { -number, remainder }
    end
  end

  def parse(<< ?+, char, rest :: binary >>) when char in ?0..?9 do
    parse(<< char, rest :: binary >>, 0)
  end

  def parse(<< char, _ :: binary >> = bin) when char in ?0..?9 do
    parse(bin, 0)
  end

  def parse(bitstring) when is_binary(bitstring), do: :error

  defp parse(<< char, rest :: binary >>, acc) when char in ?0..?9 do
    parse rest, 10 * acc + (char - ?0)
  end

  defp parse(bitstring, acc) do
    { acc, bitstring }
  end
end
