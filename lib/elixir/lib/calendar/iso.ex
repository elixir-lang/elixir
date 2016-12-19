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
  @unix_epoch :calendar.datetime_to_gregorian_seconds {{1970, 1, 1}, {0, 0, 0}}

  @type year  :: 0..9999
  @type month :: 1..12
  @type day   :: 1..31

  @doc """
  Returns how many days there are in the given year-month.

  ## Examples

      iex> Calendar.ISO.days_in_month(1900, 1)
      31
      iex> Calendar.ISO.days_in_month(1900, 2)
      28
      iex> Calendar.ISO.days_in_month(2000, 2)
      29
      iex> Calendar.ISO.days_in_month(2001, 2)
      28
      iex> Calendar.ISO.days_in_month(2004, 2)
      29
      iex> Calendar.ISO.days_in_month(2004, 4)
      30

  """
  @spec days_in_month(year, month) :: 28..31
  def days_in_month(year, month)

  def days_in_month(year, 2) do
    if leap_year?(year), do: 29, else: 28
  end
  def days_in_month(_, month) when month in [4, 6, 9, 11], do: 30
  def days_in_month(_, month) when month in 1..12, do: 31

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
  @spec leap_year?(year) :: boolean()
  def leap_year?(year) when is_integer(year) and year >= 0 do
    rem(year, 4) === 0 and (rem(year, 100) > 0 or rem(year, 400) === 0)
  end

  @doc """
  Calculates the day of the week from the given `year`, `month`, and `day`.

  It is an integer from 1 to 7, where 1 is Monday and 7 is Sunday.

  ## Examples

      iex> Calendar.ISO.day_of_week(2016, 10, 31)
      1
      iex> Calendar.ISO.day_of_week(2016, 11, 01)
      2
      iex> Calendar.ISO.day_of_week(2016, 11, 02)
      3
      iex> Calendar.ISO.day_of_week(2016, 11, 03)
      4
      iex> Calendar.ISO.day_of_week(2016, 11, 04)
      5
      iex> Calendar.ISO.day_of_week(2016, 11, 05)
      6
      iex> Calendar.ISO.day_of_week(2016, 11, 06)
      7
  """
  @spec day_of_week(year, month, day) :: 1..7
  def day_of_week(year, month, day)
      when is_integer(year) and is_integer(month) and is_integer(day) do
    :calendar.day_of_the_week(year, month, day)
  end

  @doc """
  Converts the given date into a string.
  """
  def date_to_string(year, month, day) do
    zero_pad(year, 4) <> "-" <> zero_pad(month, 2) <> "-" <> zero_pad(day, 2)
  end

  @doc """
  Converts the datetime (without time zone) into a string.
  """
  def naive_datetime_to_string(year, month, day, hour, minute, second, microsecond) do
    date_to_string(year, month, day) <> " " <> time_to_string(hour, minute, second, microsecond)
  end

  @doc """
  Convers the datetime (with time zone) into a string.
  """
  def datetime_to_string(year, month, day, hour, minute, second, microsecond,
                         time_zone, zone_abbr, utc_offset, std_offset) do
    date_to_string(year, month, day) <> " " <>
      time_to_string(hour, minute, second, microsecond) <>
      offset_to_string(utc_offset, std_offset, time_zone) <>
      zone_to_string(utc_offset, std_offset, zone_abbr, time_zone)
  end

  defp offset_to_string(0, 0, "Etc/UTC"), do: "Z"
  defp offset_to_string(utc, std, _zone) do
    total  = utc + std
    second = abs(total)
    minute = second |> rem(3600) |> div(60)
    hour   = second |> div(3600)
    sign(total) <> zero_pad(hour, 2) <> ":" <> zero_pad(minute, 2)
  end

  defp zone_to_string(0, 0, _abbr, "Etc/UTC"), do: ""
  defp zone_to_string(_, _, abbr, zone), do: " " <> abbr <> " " <> zone

  defp sign(total) when total < 0, do: "-"
  defp sign(_), do: "+"

  defp zero_pad(val, count) do
    num = Integer.to_string(val)
    :binary.copy("0", count - byte_size(num)) <> num
  end

  ## Helpers

  @doc false
  def time_to_string(hour, minute, second, {_, 0}) do
    time_to_string(hour, minute, second)
  end
  def time_to_string(hour, minute, second, {microsecond, precision}) do
    time_to_string(hour, minute, second) <> "." <>
      (microsecond |> zero_pad(6) |> binary_part(0, precision))
  end
  defp time_to_string(hour, minute, second) do
    zero_pad(hour, 2) <> ":" <> zero_pad(minute, 2) <> ":" <> zero_pad(second, 2)
  end

  @doc false
  def date(year, month, day) when is_integer(year) and is_integer(month) and is_integer(day) do
    if :calendar.valid_date(year, month, day) and year <= 9999 do
      {:ok, %Date{year: year, month: month, day: day}}
    else
      {:error, :invalid_date}
    end
  end

  @doc false
  def from_unix(integer, unit) when is_integer(integer) do
    total = System.convert_time_unit(integer, unit, :microsecond)
    if total < -@unix_epoch * 1_000_000 do
      {:error, :invalid_unix_time}
    else
      microsecond = rem(total, 1_000_000)
      precision = precision_for_unit(unit)
      {date, time} = :calendar.gregorian_seconds_to_datetime(@unix_epoch + div(total, 1_000_000))
      {:ok, date, time, {microsecond, precision}}
    end
  end

  defp precision_for_unit(unit) do
    subsecond = div System.convert_time_unit(1, :second, unit), 10
    precision_for_unit(subsecond, 0)
  end

  defp precision_for_unit(0, precision),
    do: precision
  defp precision_for_unit(_, 6),
    do: 6
  defp precision_for_unit(number, precision),
    do: precision_for_unit(div(number, 10), precision + 1)

  @doc false
  def date_to_iso8601(year, month, day) do
    date_to_string(year, month, day)
  end

  @doc false
  def time_to_iso8601(hour, minute, second, microsecond) do
    time_to_string(hour, minute, second, microsecond)
  end

  @doc false
  def naive_datetime_to_iso8601(year, month, day, hour, minute, second, microsecond) do
    date_to_string(year, month, day) <> "T" <> time_to_string(hour, minute, second, microsecond)
  end

  @doc false
  def datetime_to_iso8601(year, month, day, hour, minute, second, microsecond,
                          time_zone, _zone_abbr, utc_offset, std_offset) do
    date_to_string(year, month, day) <> "T" <>
      time_to_string(hour, minute, second, microsecond) <>
      offset_to_string(utc_offset, std_offset, time_zone)
  end

  @doc false
  def parse_microsecond("." <> rest) do
    case parse_microsecond(rest, 0, "") do
      {"", 0, _} ->
        :error
      {microsecond, precision, rest} when precision in 1..6 ->
        pad = String.duplicate("0", 6 - byte_size(microsecond))
        {{String.to_integer(microsecond <> pad), precision}, rest}
      {microsecond, _precision, rest} ->
        {{String.to_integer(binary_part(microsecond, 0, 6)), 6}, rest}
    end
  end
  def parse_microsecond(rest) do
    {{0, 0}, rest}
  end

  defp parse_microsecond(<<h, t::binary>>, precision, acc) when h in ?0..?9,
    do: parse_microsecond(t, precision + 1, <<acc::binary, h>>)
  defp parse_microsecond(rest, precision, acc),
    do: {acc, precision, rest}

  @doc false
  def parse_offset(""),
    do: {nil, ""}
  def parse_offset("Z"),
    do: {0, ""}
  def parse_offset("-00:00"),
    do: :error
  def parse_offset(<<?+, hour::2-bytes, ?:, min::2-bytes, rest::binary>>),
    do: parse_offset(1, hour, min, rest)
  def parse_offset(<<?-, hour::2-bytes, ?:, min::2-bytes, rest::binary>>),
    do: parse_offset(-1, hour, min, rest)
  def parse_offset(_),
    do: :error

  defp parse_offset(sign, hour, min, rest) do
    with {hour, ""} when hour < 24 <- Integer.parse(hour),
         {min, ""} when min < 60 <- Integer.parse(min) do
      {((hour * 60) + min) * 60 * sign, rest}
    else
      _ -> :error
    end
  end
end
