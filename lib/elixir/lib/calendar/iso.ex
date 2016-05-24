defmodule Calendar.ISO do
  @moduledoc """
  A calendar implementation that follows to ISO8601.

  This calendar implements the proleptic Gregorian calendar and
  is therefore compatible with the calendar used in most countries
  today. The proleptic means the Gregorian rules for leap years are
  applied for all time, consequently the dates give different results
  before the year 1583 from when the Gregorian calendar was adopted.
  """

  @doc """
  Checks if the given date is valid.

  ## Examples

      iex> Calendar.ISO.valid_date?(2000, 1, 1)
      true
      iex> Calendar.ISO.valid_date?(2000, 13, 1)
      false
      iex> Calendar.ISO.valid_date?(2000, 2, 29)
      true
      iex> Calendar.ISO.valid_date?(2000, 2, 30)
      false
      iex> Calendar.ISO.valid_date?(2001, 2, 29)
      false
      iex> Calendar.ISO.valid_date?(10000, 2, 29)
      false
      iex> Calendar.ISO.valid_date?(-1, 2, 29)
      false

  """
  def valid_date?(year, month, day) when is_integer(year) and is_integer(month) and is_integer(day) do
    year in 0..9999 and month in 1..12 and day in 1..last_day_of_month(year, month)
  end

  @doc """
  Returns if the given year is a leap year.

  ## Examples

      iex> Calendar.ISO.leap_year?(2000)
      true
      iex> Calendar.ISO.leap_year?(2001)
      false
      iex> Calendar.ISO.leap_year?(2004)
      true
      iex> Calendar.ISO.leap_year?(1900)
      false

  """
  def leap_year?(year) when is_integer(year) and year >= 0 do
    rem(year, 4) === 0 and (rem(year, 100) > 0 or rem(year, 400) === 0)
  end

  @doc """
  Returns the last day of the given year and month.

  ## Examples

      iex> Calendar.ISO.last_day_of_month(2000, 1)
      31
      iex> Calendar.ISO.last_day_of_month(2000, 2)
      29
      iex> Calendar.ISO.last_day_of_month(2001, 2)
      28
      iex> Calendar.ISO.last_day_of_month(2001, 3)
      31
      iex> Calendar.ISO.last_day_of_month(2001, 4)
      30

  """
  def last_day_of_month(year, month) when is_integer(year) and year >= 0 and month in 1..12 do
    cond do
      month in [4, 6, 9, 11] -> 30
      month == 2 and leap_year?(year) -> 29
      month == 2 -> 28
      true -> 31
    end
  end

  @doc false
  def parse_microsecond("." <> rest) do
    case parse_microsecond(rest, "") do
      {microsecond, rest} when byte_size(microsecond) > 6 ->
        {String.to_integer(binary_part(microsecond, 0, 6)), rest}
      {microsecond, rest} when byte_size(microsecond) in 1..6 ->
        pad = String.duplicate("0", 6 - byte_size(microsecond))
        {String.to_integer(microsecond <> pad), rest}
      {"", _} ->
        :error
    end
  end
  def parse_microsecond(rest) do
    {0, rest}
  end

  defp parse_microsecond(<<h, t::binary>>, acc) when h in ?0..?9,
    do: parse_microsecond(t, <<acc::binary, h>>)
  defp parse_microsecond(rest, acc),
    do: {acc, rest}

  @doc false
  def parse_offset("Z"), do: {0, ""}
  def parse_offset(""),  do: {nil, ""}
  def parse_offset(_),   do: :error
end
