import Kernel, except: [round: 1]

defmodule Float do
  @moduledoc """
  Functions for working with floating point numbers.
  """

  @doc """
  Parses a binary into a float.

  If successful, returns a tuple of the form `{float, remainder_of_binary}`.
  Otherwise `:error`.

  ## Examples

      iex> Float.parse("34")
      {34.0,""}

      iex> Float.parse("34.25")
      {34.25,""}

      iex> Float.parse("56.5xyz")
      {56.5,"xyz"}

      iex> Float.parse("pi")
      :error

  """
  @spec parse(binary) :: {float, binary} | :error
  def parse("-" <> binary) do
    case parse_unsign(binary) do
      :error -> :error
      {number, remainder} -> {-number, remainder}
    end
  end

  def parse(binary) do
    parse_unsign(binary)
  end

  defp parse_unsign("-" <> _), do: :error
  defp parse_unsign(binary) when is_binary(binary) do
    case Integer.parse binary do
      :error -> :error
      {integer_part, after_integer} -> parse_unsign after_integer, integer_part
    end
  end

  # Dot followed by digit is required afterwards or we are done
  defp parse_unsign(<< ?., char, rest :: binary >>, int) when char in ?0..?9 do
    parse_unsign(rest, char - ?0, 1, int)
  end

  defp parse_unsign(rest, int) do
    {:erlang.float(int), rest}
  end

  # Handle decimal points
  defp parse_unsign(<< char, rest :: binary >>, float, decimal, int) when char in ?0..?9 do
    parse_unsign rest, 10 * float + (char - ?0), decimal + 1, int
  end

  defp parse_unsign(<< ?e, after_e :: binary >>, float, decimal, int) do
    case Integer.parse after_e do
      :error ->
        # Note we rebuild the binary here instead of breaking it apart at
        # the function clause because the current approach copies a binary
        # just on this branch. If we broke it apart in the function clause,
        # the copy would happen when calling Integer.parse/1.
        {floatify(int, float, decimal), << ?e, after_e :: binary >>}
      {exponential, after_exponential} ->
        {floatify(int, float, decimal, exponential), after_exponential}
    end
  end

  defp parse_unsign(bitstring, float, decimal, int) do
    {floatify(int, float, decimal), bitstring}
  end

  defp floatify(int, float, decimal, exponential \\ 0) do
    multiplier = if int < 0, do: -1.0, else: 1.0

    # Try to ensure the minimum amount of rounding errors
    result = multiplier * (abs(int) * :math.pow(10, decimal) + float) * :math.pow(10, exponential - decimal)

    # Try avoiding stuff like this:
    # iex(1)> 0.0001 * 75
    # 0.007500000000000001
    # Due to IEEE 754 floating point standard
    # http://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html

    final_decimal_places = decimal - exponential
    if final_decimal_places > 0 do
      decimal_power_round = :math.pow(10,  final_decimal_places)
      multiplied_result = result * decimal_power_round
      epsilon = :math.pow(10, -final_decimal_places) * 5
      closet_approximation = ceil_within_error_range(multiplied_result, epsilon)
      trunc(closet_approximation) / decimal_power_round
    else
      result
    end
  end

  defp ceil_within_error_range(result, epsilon) do
    ceiled = ceil(result)
    if ceiled - result <= epsilon do
      ceiled
    else
      result
    end
  end

  @doc """
  Rounds a float to the largest integer less than or equal to `num`.

  Floor also accepts a precision to round a floating point value down
  to an arbitrary number of fractional digits (between 0 and 15).

  This function always returns floats. One may use `Kernel.trunc/1` to
  truncate the result to an integer afterwards.

  ## Examples

      iex> Float.floor(34.25)
      34.0

      iex> Float.floor(-56.5)
      -57.0

      iex> Float.floor(34.253, 2)
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
  Rounds a float to the largest integer greater than or equal to `num`.

  Ceil also accepts a precision to round a floating point value down to
  an arbitrary number of fractional digits (between 0 and 15).

  This function always returns floats. One may use `Kernel.trunc/1` to
  truncate the result to an integer afterwards.

  ## Examples

      iex> Float.ceil(34.25)
      35.0

      iex> Float.ceil(-56.5)
      -56.0

      iex> Float.ceil(34.253, 2)
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

  This function only accepts floats and returns floats. Use `Kernel.round/1`
  if you want a function that accepts both floats and integers and always
  returns an integer.

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
  def to_char_list(number) do
    :erlang.float_to_list(number)
  end

  @doc """
  Returns a list which corresponds to the text representation
  of `float`.

  ## Options

    * `:decimals`   — number of decimal points to show
    * `:scientific` — number of decimal points to show, in scientific format
    * `:compact`    — when true, use the most compact representation (ignored
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
  of `some_float`.

  Inlined by the compiler.

  ## Examples

      iex> Float.to_string(7.0)
      "7.00000000000000000000e+00"

  """
  @spec to_string(float) :: String.t
  def to_string(some_float) do
    :erlang.float_to_binary(some_float)
  end

  @doc """
  Returns a binary which corresponds to the text representation
  of `float`.

  ## Options

    * `:decimals`   — number of decimal points to show
    * `:scientific` — number of decimal points to show, in scientific format
    * `:compact`    — when true, use the most compact representation (ignored
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
