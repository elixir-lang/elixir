defmodule Float do
  @moduledoc """
  Functions for working with floats.
  """

  @doc """
  Parses a binary into a float. If successful, returns a
  tuple of the form `{ float, remainder_of_binary }`.
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
  def parse(binary) when is_binary(binary) do
    case Integer.parse binary do
      :error -> :error
      { integer_part, after_integer } -> parse after_integer, integer_part
    end
  end

  # Dot followed by digit is required afterwards or we are done
  defp parse(<< ?., char, rest :: binary >>, int) when char in ?0..?9 do
    parse(<< char, rest :: binary >>, 0, 0, int)
  end

  defp parse(rest, int) do
    { :erlang.float(int), rest }
  end

  # Handle decimal points
  defp parse(<< char :: utf8, rest :: binary >>, float, decimal, int) when char in ?0..?9 do
    parse rest, 10 * float + (char - ?0), decimal + 1, int
  end

  defp parse(<< ?e :: utf8, after_e :: binary >> = bin, float, decimal, int) do
    case Integer.parse after_e do
      :error ->
        { floatify(int, float, decimal), bin }
      { exponential, after_exponential } ->
        { floatify(int, float, decimal, exponential), after_exponential }
    end
  end

  defp parse(bitstring, float, decimal, int) do
    { floatify(int, float, decimal), bitstring }
  end

  defp floatify(int, float, decimal, exponential // 0) do
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
end
