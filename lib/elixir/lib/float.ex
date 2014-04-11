defmodule Float do
  @moduledoc """
  Functions for working with floating point numbers.
  """

  @doc """
  Parses a binary into a float.

  If successful, returns a tuple of the form `{ float, remainder_of_binary }`.
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
  @spec parse(binary) :: { float, binary } | :error
  def parse("-" <> binary) do
    case parse_unsign(binary) do
      :error -> :error
      { number, remainder } -> { -number, remainder }
    end
  end

  def parse(binary) do
    parse_unsign(binary)
  end

  defp parse_unsign("-" <> _), do: :error
  defp parse_unsign(binary) when is_binary(binary) do
    case Integer.parse binary do
      :error -> :error
      { integer_part, after_integer } -> parse_unsign after_integer, integer_part
    end
  end

  # Dot followed by digit is required afterwards or we are done
  defp parse_unsign(<< ?., char, rest :: binary >>, int) when char in ?0..?9 do
    parse_unsign(rest, char - ?0, 1, int)
  end

  defp parse_unsign(rest, int) do
    { :erlang.float(int), rest }
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
        { floatify(int, float, decimal), << ?e, after_e :: binary >> }
      { exponential, after_exponential } ->
        { floatify(int, float, decimal, exponential), after_exponential }
    end
  end

  defp parse_unsign(bitstring, float, decimal, int) do
    { floatify(int, float, decimal), bitstring }
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
      trunc(result * decimal_power_round) / decimal_power_round
    else
      result
    end
  end

  @doc """
  Rounds a float to the largest integer less than or equal to `num`.

  ## Examples

      iex> Float.floor(34)
      34

      iex> Float.floor(34.25)
      34

      iex> Float.floor(-56.5)
      -57

  """
  @spec floor(float | integer) :: integer
  def floor(num) when is_integer(num), do: num
  def floor(num) when is_float(num) do
    truncated = :erlang.trunc(num)
    case :erlang.abs(num - truncated) do
      x when x > 0 and num < 0 -> truncated - 1
      _ -> truncated
    end
  end

  @doc """
  Rounds a float to the largest integer greater than or equal to `num`.

  ## Examples

      iex> Float.ceil(34)
      34

      iex> Float.ceil(34.25)
      35

      iex> Float.ceil(-56.5)
      -56

  """
  @spec ceil(float | integer) :: integer
  def ceil(num) when is_integer(num), do: num
  def ceil(num) when is_float(num) do
    truncated = :erlang.trunc(num)
    case :erlang.abs(num - truncated) do
      x when x > 0 and num > 0 -> truncated + 1
      _ -> truncated
    end
  end

  @doc """
  Rounds a floating point value to an arbitrary number of fractional digits
  (between 0 and 15).

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
  @spec round(float, integer) :: float
  def round(number, precision) when is_float(number) and is_integer(precision) and precision in 0..15 do
    Kernel.round(number * :math.pow(10, precision)) / :math.pow(10, precision)
  end
end
