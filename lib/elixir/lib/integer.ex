defmodule Integer do
  @moduledoc """
  Functions for working with integers.
  """

  import Bitwise

  @doc """
  Determines if `integer` is odd.

  Returns `true` if the given `integer` is an odd number,
  otherwise it returns `false`.

  Allowed in guard clauses.

  ## Examples

      iex> Integer.is_odd(5)
      true

      iex> Integer.is_odd(6)
      false

      iex> Integer.is_odd(-5)
      true

      iex> Integer.is_odd(0)
      false

  """
  defmacro is_odd(integer) do
    quote do: (unquote(integer) &&& 1) == 1
  end

  @doc """
  Determines if an `integer` is even.

  Returns `true` if the given `integer` is an even number,
  otherwise it returns `false`.

  Allowed in guard clauses.

  ## Examples

      iex> Integer.is_even(10)
      true

      iex> Integer.is_even(5)
      false

      iex> Integer.is_even(-10)
      true

      iex> Integer.is_even(0)
      true

  """
  defmacro is_even(integer) do
    quote do: (unquote(integer) &&& 1) == 0
  end

  @doc """
  Returns the ordered digits for the given non-negative `integer`.

  An optional `base` value may be provided representing the radix for the returned
  digits. This one can be an integer >= 2.

  ## Examples

      iex> Integer.digits(101)
      [1, 0, 1]

      iex> Integer.digits(170, 2)
      [1, 0, 1, 0, 1, 0, 1, 0]

  """
  @spec digits(non_neg_integer, pos_integer) :: [non_neg_integer, ...]
  def digits(integer, base \\ 10)
      when is_integer(integer) and integer >= 0 and is_integer(base) and base >= 2 do
    do_digits(integer, base, [])
  end

  defp do_digits(0, _base, []), do: [0]
  defp do_digits(1, _base, []), do: [1]
  defp do_digits(base, base, []), do: [1, 0]
  defp do_digits(0, _base, acc), do: acc
  defp do_digits(integer, base, acc) do
    do_digits div(integer, base), base, [rem(integer, base) | acc]
  end

  @doc """
  Returns the integer represented by the ordered `digits`.

  An optional `base` value may be provided representing the radix for the `digits`.
  This one can be an integer >= 2.

  ## Examples

      iex> Integer.undigits([1, 2, 3])
      123

      iex> Integer.undigits([1, 4], 16)
      20

      iex> Integer.undigits([])
      0

  """
  @spec undigits([integer], integer) :: integer
  def undigits(digits, base \\ 10) when is_list(digits) and is_integer(base) and base >= 2 do
    do_undigits(digits, base, 0)
  end

  defp do_undigits([], _base, []), do: 0
  defp do_undigits([0], _base, []), do: 0
  defp do_undigits([1], _base, []), do: 1
  defp do_undigits([1, 0], base, []), do: base
  defp do_undigits([], _base, acc), do: acc
  defp do_undigits([digit | tail], base, acc) do
    do_undigits(tail, base, acc * base + digit)
  end

  @doc """
  Parses a text representation of an integer.

  An optional `base` to the corresponding integer can be provided.
  If `base` is not given, 10 will be used.

  If successful, returns a tuple in the form of `{integer, remainder_of_binary}`.
  Otherwise `:error`.

  Raises an error if `base` is less than 2 or more than 36.

  If you want to convert a string-formatted integer directly to a integer,
  `String.to_integer/1` or `String.to_integer/2` can be used instead.

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

  def parse("", base) when base in 2..36,
    do: :error

  def parse(binary, base) when is_binary(binary) and base in 2..36 do
    parse_in_base(binary, base)
  end

  def parse(binary, base) when is_binary(binary) do
    raise ArgumentError, "invalid base #{base}"
  end

  defp parse_in_base("-" <> bin, base) do
    case do_parse(bin, base) do
      {number, remainder} -> {-number, remainder}
      :error -> :error
    end
  end

  defp parse_in_base("+" <> bin, base) do
    do_parse(bin, base)
  end

  defp parse_in_base(binary, base) when is_binary(binary) do
    do_parse(binary, base)
  end

  defp do_parse(<<char, rest::binary>>, base) do
    if valid_digit_in_base?(char, base) do
      do_parse(rest, base, parse_digit(char, base))
    else
      :error
    end
  end

  defp do_parse(_, _) do
    :error
  end

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
  of `integer`.

  Inlined by the compiler.

  ## Examples

      iex> Integer.to_string(123)
      "123"

      iex> Integer.to_string(+456)
      "456"

      iex> Integer.to_string(-789)
      "-789"

      iex> Integer.to_string(0123)
      "123"

  """
  @spec to_string(integer) :: String.t
  def to_string(integer) do
    :erlang.integer_to_binary(integer)
  end

  @doc """
  Returns a binary which corresponds to the text representation
  of `integer` in the given `base`.

  `base` can be an integer between 2 and 36.

  Inlined by the compiler.

  ## Examples

      iex> Integer.to_string(100, 16)
      "64"

      iex> Integer.to_string(-100, 16)
      "-64"

      iex> Integer.to_string(882681651, 36)
      "ELIXIR"

  """
  @spec to_string(integer, 2..36) :: String.t
  def to_string(integer, base) do
    :erlang.integer_to_binary(integer, base)
  end

  @doc """
  Returns a charlist which corresponds to the text representation of the given `integer`.

  Inlined by the compiler.

  ## Examples

      iex> Integer.to_charlist(123)
      '123'

      iex> Integer.to_charlist(+456)
      '456'

      iex> Integer.to_charlist(-789)
      '-789'

      iex> Integer.to_charlist(0123)
      '123'

  """
  @spec to_charlist(integer) :: charlist
  def to_charlist(integer) do
    :erlang.integer_to_list(integer)
  end

  @doc """
  Returns a charlist which corresponds to the text representation of `integer` in the given `base`.

  `base` can be an integer between 2 and 36.

  Inlined by the compiler.

  ## Examples

      iex> Integer.to_charlist(100, 16)
      '64'

      iex> Integer.to_charlist(-100, 16)
      '-64'

      iex> Integer.to_charlist(882681651, 36)
      'ELIXIR'

  """
  @spec to_charlist(integer, 2..36) :: charlist
  def to_charlist(integer, base) do
    :erlang.integer_to_list(integer, base)
  end

  # TODO: Deprecate by v1.5
  @doc false
  @spec to_char_list(integer) :: charlist
  def to_char_list(integer), do: Integer.to_charlist(integer)
end
