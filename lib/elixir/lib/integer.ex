defmodule Integer do
  @moduledoc """
  Functions for working with integers.
  """

  import Bitwise

  @doc """
  Determines if an integer is odd.

  Returns `true` if `n` is an odd number, otherwise `false`.

  Allowed in guard clauses.

  ## Examples

      iex> Integer.is_odd(3)
      true

      iex> Integer.is_odd(4)
      false
  """
  defmacro is_odd(n) do
    quote do: (unquote(n) &&& 1) == 1
  end

  @doc """
  Determines if an integer is even.

  Returns `true` if `n` is an even number, otherwise `false`.

  Allowed in guard clauses.

  ## Examples

      iex> Integer.is_even(10)
      true

      iex> Integer.is_even(5)
      false
  """
  defmacro is_even(n) do
    quote do: (unquote(n) &&& 1) == 0
  end

  @doc """
  Returns the ordered digits for the given non-negative integer.

  An optional base value may be provided representing the radix for the returned
  digits.

  ## Examples

      iex> Integer.digits(101)
      [1, 0, 1]

      iex> Integer.digits(58127, 2)
      [1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]

  """
  @spec digits(non_neg_integer, pos_integer) :: [non_neg_integer]
  def digits(n, base \\ 10) when is_integer(n)    and n >= 0
                            and  is_integer(base) and base >= 2 do
    do_digits(n, base, [])
  end

  defp do_digits(0, _base, []),  do: [0]
  defp do_digits(0, _base, acc), do: acc
  defp do_digits(n, base, acc)  do
    do_digits div(n, base), base, [rem(n, base) | acc]
  end

  @doc """
  Returns the integer represented by the ordered digits.

  An optional base value may be provided representing the radix for the digits.

  ## Examples

      iex> Integer.undigits([1, 0, 1])
      101

      iex> Integer.undigits([1, 4], 16)
      20
  """
  @spec undigits([integer], integer) :: integer
  def undigits(digits, base \\ 10) when is_integer(base) do
    do_undigits(digits, base, 0)
  end

  defp do_undigits([], _base, acc), do: acc
  defp do_undigits([digit | tail], base, acc) do
    do_undigits(tail, base, acc * base + digit)
  end

  @doc """
  Converts a binary from a text representation of an integer
  in an optional base `base` to the corresponding integer.

  If the base `base` is not given, base 10 will be used.

  If successful, returns a tuple in the form of `{integer, remainder_of_binary}`.
  Otherwise `:error`.

  Raises an error if `base` is less than 2 or more than 36.

  ## Examples

      iex> Integer.parse("34")
      {34, ""}

      iex> Integer.parse("34.5")
      {34, ".5"}

      iex> Integer.parse("three")
      :error

      iex> Integer.parse("34", 10)
      {34, ""}

      iex> Integer.parse("f4", 16)
      {244, ""}

      iex> Integer.parse("Awww++", 36)
      {509216, "++"}

      iex> Integer.parse("fab", 10)
      :error

      iex> Integer.parse("a2", 38)
      ** (ArgumentError) invalid base 38

  """
  @spec parse(binary, 2..36) :: {integer, binary} | :error | no_return
  def parse(binary, base \\ 10)

  def parse(binary, base) when is_integer(base) and base in 2..36 do
    parse_in_base(binary, base)
  end

  def parse(_, base) do
    raise ArgumentError, "invalid base #{base}"
  end

  defp parse_in_base("-" <> bin, base) do
    case do_parse(bin, base) do
      :error -> :error
      {number, remainder} -> {-number, remainder}
    end
  end

  defp parse_in_base("+" <> bin, base) do
    do_parse(bin, base)
  end

  defp parse_in_base(bin, base) when is_binary(bin) do
    do_parse(bin, base)
  end

  defp do_parse(<<char, rest::binary>>, base) do
    if valid_digit_in_base?(char, base) do
      do_parse(rest, base, parse_digit(char, base))
    else
      :error
    end
  end

  defp do_parse(_, _), do: :error

  defp do_parse(<<char, rest::binary>> = bin, base, acc) do
    if valid_digit_in_base?(char, base) do
      do_parse(rest, base, base * acc + parse_digit(char, base))
    else
      {acc, bin}
    end
  end

  defp do_parse(bitstring, _, acc) do
    {acc, bitstring}
  end

  defp parse_digit(char, _) do
    cond do
      char in ?0..?9 -> char - ?0
      char in ?A..?Z -> char - ?A + 10
      true           -> char - ?a + 10
    end
  end

  defp valid_digit_in_base?(char, base) do
    if base <= 10 do
      char in ?0..(?0 + base - 1)
    else
      char in ?0..?9 or char in ?A..(?A + base - 11) or char in ?a..(?a + base - 11)
    end
  end

  @doc """
  Returns a binary which corresponds to the text representation
  of `some_integer`.

  Inlined by the compiler.

  ## Examples

      iex> Integer.to_string(123)
      "123"

  """
  @spec to_string(integer) :: String.t
  def to_string(some_integer) do
    :erlang.integer_to_binary(some_integer)
  end

  @doc """
  Returns a binary which corresponds to the text representation
  of `some_integer` in base `base`.

  Inlined by the compiler.

  ## Examples

      iex> Integer.to_string(100, 16)
      "64"

  """
  @spec to_string(integer, 2..36) :: String.t
  def to_string(some_integer, base) do
    :erlang.integer_to_binary(some_integer, base)
  end

  @doc """
  Returns a char list which corresponds to the text representation of the given integer.

  Inlined by the compiler.

  ## Examples

      iex> Integer.to_char_list(7)
      '7'

  """
  @spec to_char_list(integer) :: char_list
  def to_char_list(number) do
    :erlang.integer_to_list(number)
  end

  @doc """
  Returns a char list which corresponds to the text representation of the
  given integer in the given base.

  Inlined by the compiler.

  ## Examples

      iex> Integer.to_char_list(1023, 16)
      '3FF'

  """
  @spec to_char_list(integer, 2..36) :: char_list
  def to_char_list(number, base) do
    :erlang.integer_to_list(number, base)
  end
end
