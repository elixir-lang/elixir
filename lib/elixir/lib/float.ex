import Kernel, except: [round: 1]

defmodule Float do
  @moduledoc """
  Functions for working with floating point numbers.
  """

  @doc """
  Parses a binary into a float.

  If successful, returns a tuple in the form of `{float, remainder_of_binary}`;
  when the binary cannot be coerced into a valid float, the atom `:error` is
  returned.

  If the size of float exceeds the maximum size of `1.7976931348623157e+308`,
  the `ArgumentError` exception is raised.

  If a float formatted string wants to be directly converted to a float,
  `String.to_float/1` can be used instead.

  ## Examples

      iex> Float.parse("34")
      {34.0, ""}

      iex> Float.parse("34.25")
      {34.25, ""}

      iex> Float.parse("56.5xyz")
      {56.5, "xyz"}

      iex> Float.parse("pi")
      :error

  """
  @spec parse(binary) :: {float, binary} | :error
  def parse("-" <> binary) do
    case parse_unsigned(binary) do
      :error -> :error
      {number, remainder} -> {-number, remainder}
    end
  end

  def parse("+" <> binary) do
    parse_unsigned(binary)
  end

  def parse(binary) do
    parse_unsigned(binary)
  end

  defp parse_unsigned(<<digit, rest::binary>>) when digit in ?0..?9, do:
    parse_unsigned(rest, false, false, <<digit>>)

  defp parse_unsigned(binary) when is_binary(binary), do:
    :error

  defp parse_unsigned(<<digit, rest::binary>>, dot?, e?, acc) when digit in ?0..?9, do:
    parse_unsigned(rest, dot?, e?, <<acc::binary, digit>>)

  defp parse_unsigned(<<?., digit, rest::binary>>, false, false, acc) when digit in ?0..?9, do:
    parse_unsigned(rest, true, false, <<acc::binary, ?., digit>>)

  defp parse_unsigned(<<exp_marker, digit, rest::binary>>, dot?, false, acc) when exp_marker in 'eE' and  digit in ?0..?9, do:
    parse_unsigned(rest, true, true, <<add_dot(acc, dot?)::binary, ?e, digit>>)

  defp parse_unsigned(<<exp_marker, sign, digit, rest::binary>>, dot?, false, acc) when exp_marker in 'eE' and sign in '-+' and digit in ?0..?9, do:
    parse_unsigned(rest, true, true, <<add_dot(acc, dot?)::binary, ?e, sign, digit>>)

  defp parse_unsigned(rest, dot?, _e?, acc), do:
    {:erlang.binary_to_float(add_dot(acc, dot?)), rest}

  defp add_dot(acc, true),  do: acc
  defp add_dot(acc, false), do: acc <> ".0"

  @doc """
  Rounds a float to the largest integer less than or equal to `num`.

  `floor/2` also accepts a precision to round a floating point value down
  to an arbitrary number of fractional digits (between 0 and 15).

  This function always returns a float. `Kernel.trunc/1` may be used instead to
  truncate the result to an integer afterwards.

  ## Examples

      iex> Float.floor(34.25)
      34.0

      iex> Float.floor(-56.5)
      -57.0

      iex> Float.floor(34.259, 2)
      34.25

  """
  @spec floor(float, 0..15) :: float
  def floor(number, precision \\ 0) when is_float(number) and precision in 0..15 do
    power     = power_of_10(precision)
    number    = number * power
    truncated = trunc(number)
    variance  = if number - truncated < 0, do: -1.0, else: 0.0
    (truncated + variance) / power
  end

  @doc """
  Rounds a float to the smallest integer greater than or equal to `num`.

  `ceil/2` also accepts a precision to round a floating point value down
  to an arbitrary number of fractional digits (between 0 and 15).

  This function always returns floats. `Kernel.trunc/1` may be used instead to
  truncate the result to an integer afterwards.

  ## Examples

      iex> Float.ceil(34.25)
      35.0

      iex> Float.ceil(-56.5)
      -56.0

      iex> Float.ceil(34.251, 2)
      34.26

  """
  @spec ceil(float, 0..15) :: float
  def ceil(number, precision \\ 0) when is_float(number) and precision in 0..15 do
    power     = power_of_10(precision)
    number    = number * power
    truncated = trunc(number)
    variance  = if number - truncated > 0, do: 1.0, else: 0.0
    (truncated + variance) / power
  end

  @doc """
  Rounds a floating point value to an arbitrary number of fractional digits
  (between 0 and 15).

  This function only accepts floats and always returns a float. Use
  `Kernel.round/1` if you want a function that accepts both floats and integers
  and always returns an integer.

  ## Examples

      iex> Float.round(5.5674, 3)
      5.567

      iex> Float.round(5.5675, 3)
      5.568

      iex> Float.round(-5.5674, 3)
      -5.567

      iex> Float.round(-5.5675, 3)
      -5.568

  """
  @spec round(float, 0..15) :: float
  def round(number, precision \\ 0) when is_float(number) and precision in 0..15 do
    power = power_of_10(precision)
    Kernel.round(number * power) / power
  end

  Enum.reduce 0..15, 1, fn x, acc ->
    defp power_of_10(unquote(x)), do: unquote(acc)
    acc * 10
  end

  @doc """
  Returns a char list which corresponds to the text representation of the given float.

  Inlined by the compiler.

  ## Examples

      iex> Float.to_char_list(7.0)
      '7.00000000000000000000e+00'

  """
  @spec to_char_list(float) :: char_list
  def to_char_list(float) do
    :erlang.float_to_list(float)
  end

  @doc """
  Returns a list which corresponds to the text representation
  of the given float.

  ## Options

    * `:decimals`   - number of decimal points to show
    * `:scientific` - number of decimal points to show, in scientific format
    * `:compact`    - when `true`, use the most compact representation (ignored
      with the `scientific` option)

  ## Examples

      iex> Float.to_char_list 7.1, [decimals: 2, compact: true]
      '7.1'

  """
  @spec to_char_list(float, list) :: char_list
  def to_char_list(float, options) do
    :erlang.float_to_list(float, expand_compact(options))
  end

  @doc """
  Returns a binary which corresponds to the text representation
  of the given float.

  Inlined by the compiler.

  ## Examples

      iex> Float.to_string(7.0)
      "7.00000000000000000000e+00"

  """
  @spec to_string(float) :: String.t
  def to_string(float) do
    :erlang.float_to_binary(float)
  end

  @doc """
  Returns a binary which corresponds to the text representation
  of `float`.

  ## Options

    * `:decimals`   - number of decimal points to show
    * `:scientific` - number of decimal points to show, in scientific format
    * `:compact`    - when `true`, use the most compact representation (ignored
      with the `scientific` option)

  ## Examples

      iex> Float.to_string 7.1, [decimals: 2, compact: true]
      "7.1"

  """
  @spec to_string(float, list) :: String.t
  def to_string(float, options) do
    :erlang.float_to_binary(float, expand_compact(options))
  end

  defp expand_compact([{:compact, false}|t]), do: expand_compact(t)
  defp expand_compact([{:compact, true}|t]),  do: [:compact|expand_compact(t)]
  defp expand_compact([h|t]),                 do: [h|expand_compact(t)]
  defp expand_compact([]),                    do: []
end
