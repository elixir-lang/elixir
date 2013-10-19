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
      { integer_part, after_integer } -> parse after_integer, 0, 0, integer_part
    end
  end

  ### parser internals
  defp parse(<< ?., rest :: binary >>, 0, 0, int) when is_integer(int), do: parse(rest, 0, 0, int)

  defp parse(<< char :: utf8, rest :: binary >>, float, decimal, int) when char in ?0..?9 do
    parse rest, 10 * float + (char - ?0), decimal + 1, int
  end

  ##Stop conditions

  #we only want to accept exponential after 1 decimal case at least, so "decimal > 0"
  defp parse(<< ?e :: utf8, after_e :: binary >> = bin, float, decimal, int) when decimal > 0 do 
    case Integer.parse after_e do 
      :error -> {floatify(int, float, decimal), bin} ##invalid exponential, treat as "string"
      {exponential, after_exponential} -> {floatify(int, float, decimal, exponential), after_exponential}
    end
  end

  defp parse(bitstring, float, decimal, int) when is_binary(bitstring), do: {floatify(int, float, decimal), bitstring} 

  defp floatify(int, float, decimal, exponential // 0) do
    multiplier = 1.0
    if int < 0, do: multiplier = -1.0

    # try to ensure the minimum amount of rounding errors
    result = multiplier * (abs(int) * :math.pow(10, decimal) + float) * :math.pow(10, exponential - decimal)

    # try avoiding stuff like this:
    # iex(1)> 0.0001 * 75
    # 0.007500000000000001
    # Due to IEEE 754 floating point standard
    # http://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html

    final_decimal_places = decimal - exponential
    if final_decimal_places > 0 do 
      decimal_power_round = :math.pow(10,  final_decimal_places)
      result = trunc(result * decimal_power_round) / decimal_power_round
    end
    
    result
  end
end
