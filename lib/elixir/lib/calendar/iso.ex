defmodule Calendar.ISO do
  @moduledoc """
  A calendar implementation that follows to ISO8601.

  This calendar implements the proleptic Gregorian calendar and
  is therefore compatible with the calendar used in most countries
  today. The proleptic means the Gregorian rules for leap years are
  applied for all time, consequently the dates give different results
  before the year 1583 from when the Gregorian calendar was adopted.
  """

  @behaviour Calendar

  @doc """
  Builds and validates an ISO date.

  ## Examples

      iex> Calendar.ISO.date(2000, 1, 1)
      {:ok, ~D[2000-01-01]}
      iex> Calendar.ISO.date(2000, 13, 1)
      {:error, :invalid_date}
      iex> Calendar.ISO.date(2000, 2, 29)
      {:ok, ~D[2000-02-29]}

      iex> Calendar.ISO.date(2000, 2, 30)
      {:error, :invalid_date}
      iex> Calendar.ISO.date(2001, 2, 29)
      {:error, :invalid_date}

  """
  def date(year, month, day) when is_integer(year) and is_integer(month) and is_integer(day) do
    if :calendar.valid_date(year, month, day) and year <= 9999 do
      {:ok, %Date{year: year, month: month, day: day}}
    else
      {:error, :invalid_date}
    end
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
  Converts the given structure into a string.

  It uses the ISO8601 standard except for DateTime where the
  timezone information is added between brackets.
  """
  def to_string(%Date{year: year, month: month, day: day}) do
    date_to_string(year, month, day)
  end

  def to_string(%Time{hour: hour, minute: minute, second: second, microsecond: microsecond}) do
    time_to_string(hour, minute, second, microsecond)
  end

  def to_string(%NaiveDateTime{year: year, month: month, day: day,
                               hour: hour, minute: minute, second: second, microsecond: microsecond}) do
    date_to_string(year, month, day) <> " " <> time_to_string(hour, minute, second, microsecond)
  end

  def to_string(%DateTime{year: year, month: month, day: day,
                          hour: hour, minute: minute, second: second, microsecond: microsecond,
                          utc_offset: utc_offset, std_offset: std_offset, time_zone: time_zone}) do
    date_to_string(year, month, day) <> " " <>
      time_to_string(hour, minute, second, microsecond) <>
      offset_to_string(utc_offset, std_offset, time_zone) <>
      zone_to_string(utc_offset, std_offset, time_zone)
  end

  defp date_to_string(year, month, day) do
    zero_pad(year, 4) <> "-" <> zero_pad(month, 2) <> "-" <> zero_pad(day, 2)
  end

  defp time_to_string(hour, minute, second, 0) do
    zero_pad(hour, 2) <> ":" <> zero_pad(minute, 2) <> ":" <> zero_pad(second, 2)
  end
  defp time_to_string(hour, minute, second, microsecond) do
    time_to_string(hour, minute, second, 0) <> "." <>
      (microsecond |> zero_pad(6) |> String.trim_trailing("0"))
  end

  defp offset_to_string(0, 0, "Etc/UTC"), do: "Z"
  defp offset_to_string(utc, std, _zone) do
    total  = utc + std
    second = abs(total)
    minute = second |> rem(3600) |> div(60)
    hour   = second |> div(3600)
    sign(total) <> zero_pad(hour, 2) <> ":" <> zero_pad(minute, 2)
  end

  defp zone_to_string(0, 0, "Etc/UTC"), do: ""
  defp zone_to_string(_, _, zone), do: " " <> zone

  defp sign(total) when total < 0, do: "-"
  defp sign(_), do: "+"

  defp zero_pad(val, count) do
    num = Integer.to_string(val)
    :binary.copy("0", count - byte_size(num)) <> num
  end

  ## Helpers

  @doc false
  def to_iso8601(%Date{year: year, month: month, day: day}) do
    date_to_string(year, month, day)
  end

  def to_iso8601(%Time{hour: hour, minute: minute, second: second, microsecond: microsecond}) do
    time_to_string(hour, minute, second, microsecond)
  end

  def to_iso8601(%NaiveDateTime{year: year, month: month, day: day,
                                hour: hour, minute: minute, second: second, microsecond: microsecond}) do
    date_to_string(year, month, day) <> "T" <> time_to_string(hour, minute, second, microsecond)
  end

  def to_iso8601(%DateTime{year: year, month: month, day: day,
                           hour: hour, minute: minute, second: second, microsecond: microsecond,
                           utc_offset: utc_offset, std_offset: std_offset, time_zone: time_zone}) do
    date_to_string(year, month, day) <> "T" <>
      time_to_string(hour, minute, second, microsecond) <>
      offset_to_string(utc_offset, std_offset, time_zone)
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
