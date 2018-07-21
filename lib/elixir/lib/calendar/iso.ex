defmodule Calendar.ISO do
  @moduledoc """
  A calendar implementation that follows to ISO 8601.

  This calendar implements the proleptic Gregorian calendar and
  is therefore compatible with the calendar used in most countries
  today. The proleptic means the Gregorian rules for leap years are
  applied for all time, consequently the dates give different results
  before the year 1583 from when the Gregorian calendar was adopted.

  Note that while ISO 8601 allows times and datetimes to specify
  24:00:00 as the zero hour of the next day, this notation is not
  supported by Elixir.
  """

  @behaviour Calendar

  @unix_epoch 62_167_219_200
  unix_start = (315_537_897_600 + @unix_epoch) * -1_000_000
  unix_end = 315_569_519_999_999_999 - @unix_epoch * 1_000_000
  @unix_range_microseconds unix_start..unix_end

  @type year :: -9999..9999
  @type month :: 1..12
  @type day :: 1..31

  @seconds_per_minute 60
  @seconds_per_hour 60 * 60
  # Note that this does _not_ handle leap seconds.
  @seconds_per_day 24 * 60 * 60
  @microseconds_per_second 1_000_000
  @parts_per_day @seconds_per_day * @microseconds_per_second

  @days_per_nonleap_year 365
  @days_per_leap_year 366

  @months_in_year 12

  @doc false
  def __match_date__ do
    quote do
      [
        <<y1, y2, y3, y4, ?-, m1, m2, ?-, d1, d2>>,
        y1 >= ?0 and y1 <= ?9 and y2 >= ?0 and y2 <= ?9 and y3 >= ?0 and y3 <= ?9 and y4 >= ?0 and
          y4 <= ?9 and m1 >= ?0 and m1 <= ?9 and m2 >= ?0 and m2 <= ?9 and d1 >= ?0 and d1 <= ?9 and
          d2 >= ?0 and d2 <= ?9,
        {
          (y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0),
          (m1 - ?0) * 10 + (m2 - ?0),
          (d1 - ?0) * 10 + (d2 - ?0)
        }
      ]
    end
  end

  @doc false
  def __match_time__ do
    quote do
      [
        <<h1, h2, ?:, i1, i2, ?:, s1, s2>>,
        h1 >= ?0 and h1 <= ?9 and h2 >= ?0 and h2 <= ?9 and i1 >= ?0 and i1 <= ?9 and i2 >= ?0 and
          i2 <= ?9 and s1 >= ?0 and s1 <= ?9 and s2 >= ?0 and s2 <= ?9,
        {
          (h1 - ?0) * 10 + (h2 - ?0),
          (i1 - ?0) * 10 + (i2 - ?0),
          (s1 - ?0) * 10 + (s2 - ?0)
        }
      ]
    end
  end

  @doc """
  Returns the `t:Calendar.iso_days/0` format of the specified date.

  ## Examples

      iex> Calendar.ISO.naive_datetime_to_iso_days(0, 1, 1, 0, 0, 0, {0, 6})
      {0, {0, 86400000000}}
      iex> Calendar.ISO.naive_datetime_to_iso_days(2000, 1, 1, 12, 0, 0, {0, 6})
      {730485, {43200000000, 86400000000}}
      iex> Calendar.ISO.naive_datetime_to_iso_days(2000, 1, 1, 13, 0, 0, {0, 6})
      {730485, {46800000000, 86400000000}}
      iex> Calendar.ISO.naive_datetime_to_iso_days(-1, 1, 1, 0, 0, 0, {0, 6})
      {-365, {0, 86400000000}}

  """
  @doc since: "1.5.0"
  @impl true
  @spec naive_datetime_to_iso_days(
          Calendar.year(),
          Calendar.month(),
          Calendar.day(),
          Calendar.hour(),
          Calendar.minute(),
          Calendar.second(),
          Calendar.microsecond()
        ) :: Calendar.iso_days()
  def naive_datetime_to_iso_days(year, month, day, hour, minute, second, microsecond) do
    {date_to_iso_days(year, month, day), time_to_day_fraction(hour, minute, second, microsecond)}
  end

  @doc """
  Converts the `t:Calendar.iso_days/0` format to the datetime format specified by this calendar.

  ## Examples

      iex> Calendar.ISO.naive_datetime_from_iso_days({0, {0, 86400}})
      {0, 1, 1, 0, 0, 0, {0, 6}}
      iex> Calendar.ISO.naive_datetime_from_iso_days({730_485, {0, 86400}})
      {2000, 1, 1, 0, 0, 0, {0, 6}}
      iex> Calendar.ISO.naive_datetime_from_iso_days({730_485, {43200, 86400}})
      {2000, 1, 1, 12, 0, 0, {0, 6}}
      iex> Calendar.ISO.naive_datetime_from_iso_days({-365, {0, 86400000000}})
      {-1, 1, 1, 0, 0, 0, {0, 6}}

  """
  @doc since: "1.5.0"
  @spec naive_datetime_from_iso_days(Calendar.iso_days()) :: {
          Calendar.year(),
          Calendar.month(),
          Calendar.day(),
          Calendar.hour(),
          Calendar.minute(),
          Calendar.second(),
          Calendar.microsecond()
        }
  @impl true
  def naive_datetime_from_iso_days({days, day_fraction}) do
    {year, month, day} = date_from_iso_days(days)
    {hour, minute, second, microsecond} = time_from_day_fraction(day_fraction)
    {year, month, day, hour, minute, second, microsecond}
  end

  @doc """
  Returns the normalized day fraction of the specified time.

  ## Examples

      iex> Calendar.ISO.time_to_day_fraction(0, 0, 0, {0, 6})
      {0, 86400000000}
      iex> Calendar.ISO.time_to_day_fraction(12, 34, 56, {123, 6})
      {45296000123, 86400000000}

  """
  @doc since: "1.5.0"
  @impl true
  @spec time_to_day_fraction(
          Calendar.hour(),
          Calendar.minute(),
          Calendar.second(),
          Calendar.microsecond()
        ) :: Calendar.day_fraction()
  def time_to_day_fraction(0, 0, 0, {0, _}) do
    {0, @parts_per_day}
  end

  def time_to_day_fraction(hour, minute, second, {microsecond, _}) do
    combined_seconds = hour * @seconds_per_hour + minute * @seconds_per_minute + second
    {combined_seconds * @microseconds_per_second + microsecond, @parts_per_day}
  end

  @doc """
  Converts a day fraction to this Calendar's representation of time.

  ## Examples

      iex> Calendar.ISO.time_from_day_fraction({1, 2})
      {12, 0, 0, {0, 6}}
      iex> Calendar.ISO.time_from_day_fraction({13, 24})
      {13, 0, 0, {0, 6}}

  """
  @doc since: "1.5.0"
  @impl true
  @spec time_from_day_fraction(Calendar.day_fraction()) ::
          {Calendar.hour(), Calendar.minute(), Calendar.second(), Calendar.microsecond()}
  def time_from_day_fraction({parts_in_day, parts_per_day}) do
    total_microseconds = div(parts_in_day * @parts_per_day, parts_per_day)

    {hours, rest_microseconds1} =
      div_mod(total_microseconds, @seconds_per_hour * @microseconds_per_second)

    {minutes, rest_microseconds2} =
      div_mod(rest_microseconds1, @seconds_per_minute * @microseconds_per_second)

    {seconds, microseconds} = div_mod(rest_microseconds2, @microseconds_per_second)
    {hours, minutes, seconds, {microseconds, 6}}
  end

  # Converts year, month, day to count of days since 0000-01-01.
  @doc false
  @doc since: "1.5.0"
  def date_to_iso_days(0, 1, 1) do
    0
  end

  def date_to_iso_days(1970, 1, 1) do
    719_528
  end

  def date_to_iso_days(year, month, day) when year in -9999..9999 do
    true = day <= days_in_month(year, month)

    days_in_previous_years(year) + days_before_month(month) + leap_day_offset(year, month) + day -
      1
  end

  # Converts count of days since 0000-01-01 to {year, month, day} tuple.
  @doc false
  @doc since: "1.5.0"
  def date_from_iso_days(days) when days in 0..3_652_424 do
    {year, day_of_year} = days_to_year(days)
    extra_day = if leap_year?(year), do: 1, else: 0
    {month, day_in_month} = year_day_to_year_date(extra_day, day_of_year)
    {year, month, day_in_month + 1}
  end

  def date_from_iso_days(days) when days in -3_652_059..-1 do
    {year, day_of_year} = days_to_year(-days)
    previous_extra_day = if leap_year?(year), do: 1, else: 0
    extra_day = if leap_year?(year + 1), do: 1, else: 0
    day_of_year = @days_per_nonleap_year + extra_day - day_of_year
    {month, day_in_month} = year_day_to_year_date(extra_day, day_of_year)
    {-year - 1, month, day_in_month + previous_extra_day}
  end

  defp div_mod(int1, int2) do
    div = div(int1, int2)
    rem = int1 - div * int2

    if rem >= 0 do
      {div, rem}
    else
      {div - 1, rem + int2}
    end
  end

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
      iex> Calendar.ISO.days_in_month(-1, 5)
      31

  """
  @spec days_in_month(year, month) :: 28..31
  @impl true
  def days_in_month(year, month)

  def days_in_month(year, 2) do
    if leap_year?(year), do: 29, else: 28
  end

  def days_in_month(_, month) when month in [4, 6, 9, 11], do: 30
  def days_in_month(_, month) when month in 1..12, do: 31

  @doc """
  Returns how many months there are in the given year.

  ## Example

      iex> Calendar.ISO.months_in_year(2004)
      12

  """
  @doc since: "1.7.0"
  @impl true
  @spec months_in_year(year) :: 12
  def months_in_year(_year) do
    @months_in_year
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
      iex> Calendar.ISO.leap_year?(-4)
      true

  """
  @spec leap_year?(year) :: boolean()
  @impl true
  def leap_year?(year) when is_integer(year) do
    rem(year, 4) === 0 and (rem(year, 100) !== 0 or rem(year, 400) === 0)
  end

  @doc """
  Calculates the day of the week from the given `year`, `month`, and `day`.

  It is an integer from 1 to 7, where 1 is Monday and 7 is Sunday.

  ## Examples

      iex> Calendar.ISO.day_of_week(2016, 10, 31)
      1
      iex> Calendar.ISO.day_of_week(2016, 11, 1)
      2
      iex> Calendar.ISO.day_of_week(2016, 11, 2)
      3
      iex> Calendar.ISO.day_of_week(2016, 11, 3)
      4
      iex> Calendar.ISO.day_of_week(2016, 11, 4)
      5
      iex> Calendar.ISO.day_of_week(2016, 11, 5)
      6
      iex> Calendar.ISO.day_of_week(2016, 11, 6)
      7
      iex> Calendar.ISO.day_of_week(-99, 1, 31)
      4

  """
  @spec day_of_week(year, month, day) :: 1..7
  @impl true
  def day_of_week(year, month, day)
      when is_integer(year) and is_integer(month) and is_integer(day) do
    Integer.mod(date_to_iso_days(year, month, day) + 5, 7) + 1
  end

  @doc """
  Converts the given time into a string.

  ## Examples

      iex> Calendar.ISO.time_to_string(2, 2, 2, {2, 6})
      "02:02:02.000002"
      iex> Calendar.ISO.time_to_string(2, 2, 2, {2, 2})
      "02:02:02.00"
      iex> Calendar.ISO.time_to_string(2, 2, 2, {2, 0})
      "02:02:02"

  """
  @spec time_to_string(
          Calendar.hour(),
          Calendar.minute(),
          Calendar.second(),
          Calendar.microsecond()
        ) :: String.t()
  @impl true
  def time_to_string(hour, minute, second, microsecond) do
    time_to_string(hour, minute, second, microsecond, :extended)
  end

  def time_to_string(hour, minute, second, {_, 0}, format) do
    time_to_string_format(hour, minute, second, format)
  end

  def time_to_string(hour, minute, second, {microsecond, precision}, format) do
    time_to_string_format(hour, minute, second, format) <>
      "." <> (microsecond |> zero_pad(6) |> binary_part(0, precision))
  end

  defp time_to_string_format(hour, minute, second, :extended) do
    zero_pad(hour, 2) <> ":" <> zero_pad(minute, 2) <> ":" <> zero_pad(second, 2)
  end

  defp time_to_string_format(hour, minute, second, :basic) do
    zero_pad(hour, 2) <> zero_pad(minute, 2) <> zero_pad(second, 2)
  end

  @doc """
  Converts the given date into a string.

  ## Examples

      iex> Calendar.ISO.date_to_string(2015, 2, 28)
      "2015-02-28"
      iex> Calendar.ISO.date_to_string(2017, 8, 1)
      "2017-08-01"
      iex> Calendar.ISO.date_to_string(-99, 1, 31)
      "-0099-01-31"

  """
  @spec date_to_string(year, month, day) :: String.t()
  @impl true
  def date_to_string(year, month, day) do
    date_to_string(year, month, day, :extended)
  end

  defp date_to_string(year, month, day, :extended) do
    zero_pad(year, 4) <> "-" <> zero_pad(month, 2) <> "-" <> zero_pad(day, 2)
  end

  defp date_to_string(year, month, day, :basic) do
    zero_pad(year, 4) <> zero_pad(month, 2) <> zero_pad(day, 2)
  end

  @doc """
  Converts the datetime (without time zone) into a string.

  ## Examples

      iex> Calendar.ISO.naive_datetime_to_string(2015, 2, 28, 1, 2, 3, {4, 6})
      "2015-02-28 01:02:03.000004"
      iex> Calendar.ISO.naive_datetime_to_string(2017, 8, 1, 1, 2, 3, {4, 5})
      "2017-08-01 01:02:03.00000"

  """
  @impl true
  @spec naive_datetime_to_string(
          year,
          month,
          day,
          Calendar.hour(),
          Calendar.minute(),
          Calendar.second(),
          Calendar.microsecond()
        ) :: String.t()
  def naive_datetime_to_string(year, month, day, hour, minute, second, microsecond) do
    date_to_string(year, month, day) <> " " <> time_to_string(hour, minute, second, microsecond)
  end

  @doc """
  Converts the datetime (with time zone) into a string.

  ## Examples

      iex> time_zone = "Europe/Berlin"
      iex> Calendar.ISO.datetime_to_string(2017, 8, 1, 1, 2, 3, {4, 5}, time_zone, "CET", 3600, 0)
      "2017-08-01 01:02:03.00000+01:00 CET Europe/Berlin"
      iex> Calendar.ISO.datetime_to_string(2017, 8, 1, 1, 2, 3, {4, 5}, time_zone, "CDT", 3600, 3600)
      "2017-08-01 01:02:03.00000+02:00 CDT Europe/Berlin"

      iex> time_zone = "America/Los_Angeles"
      iex> Calendar.ISO.datetime_to_string(2015, 2, 28, 1, 2, 3, {4, 5}, time_zone, "PST", -28800, 0)
      "2015-02-28 01:02:03.00000-08:00 PST America/Los_Angeles"
      iex> Calendar.ISO.datetime_to_string(2015, 2, 28, 1, 2, 3, {4, 5}, time_zone, "PDT", -28800, 3600)
      "2015-02-28 01:02:03.00000-07:00 PDT America/Los_Angeles"

  """
  @impl true
  @spec datetime_to_string(
          year,
          month,
          day,
          Calendar.hour(),
          Calendar.minute(),
          Calendar.second(),
          Calendar.microsecond(),
          Calendar.time_zone(),
          Calendar.zone_abbr(),
          Calendar.utc_offset(),
          Calendar.std_offset()
        ) :: String.t()
  def datetime_to_string(
        year,
        month,
        day,
        hour,
        minute,
        second,
        microsecond,
        time_zone,
        zone_abbr,
        utc_offset,
        std_offset
      ) do
    date_to_string(year, month, day) <>
      " " <>
      time_to_string(hour, minute, second, microsecond) <>
      offset_to_string(utc_offset, std_offset, time_zone) <>
      zone_to_string(utc_offset, std_offset, zone_abbr, time_zone)
  end

  @doc """
  Determines if the date given is valid according to the proleptic Gregorian calendar.

  ## Examples

      iex> Calendar.ISO.valid_date?(2015, 2, 28)
      true
      iex> Calendar.ISO.valid_date?(2015, 2, 30)
      false
      iex> Calendar.ISO.valid_date?(-1, 12, 31)
      true
      iex> Calendar.ISO.valid_date?(-1, 12, 32)
      false

  """
  @doc since: "1.5.0"
  @impl true
  @spec valid_date?(year, month, day) :: boolean
  def valid_date?(year, month, day) do
    month in 1..12 and year in -9999..9999 and day in 1..days_in_month(year, month)
  end

  @doc """
  Determines if the date given is valid according to the proleptic Gregorian calendar.
  Note that leap seconds are considered valid, but the use of 24:00:00 as the
  zero hour of the day is considered invalid.

  ## Examples

      iex> Calendar.ISO.valid_time?(10, 50, 25, {3006, 6})
      true
      iex> Calendar.ISO.valid_time?(23, 59, 60, {0, 0})
      true
      iex> Calendar.ISO.valid_time?(24, 0, 0, {0, 0})
      false

  """
  @doc since: "1.5.0"
  @impl true
  @spec valid_time?(Calendar.hour(), Calendar.minute(), Calendar.secon(), Calendar.microsecond()) ::
          boolean
  def valid_time?(hour, minute, second, {microsecond, precision}) do
    hour in 0..23 and minute in 0..59 and second in 0..60 and microsecond in 0..999_999 and
      precision in 0..6
  end

  @doc """
  See `c:Calendar.day_rollover_relative_to_midlight_utc/0` for documentation.
  """
  @doc since: "1.5.0"
  @impl true
  @spec day_rollover_relative_to_midnight_utc() :: {0, 1}
  def day_rollover_relative_to_midnight_utc() do
    {0, 1}
  end

  defp offset_to_string(utc, std, zone, format \\ :extended)
  defp offset_to_string(0, 0, "Etc/UTC", _format), do: "Z"

  defp offset_to_string(utc, std, _zone, format) do
    total = utc + std
    second = abs(total)
    minute = second |> rem(3600) |> div(60)
    hour = div(second, 3600)
    format_offset(total, hour, minute, format)
  end

  defp format_offset(total, hour, minute, :extended) do
    sign(total) <> zero_pad(hour, 2) <> ":" <> zero_pad(minute, 2)
  end

  defp format_offset(total, hour, minute, :basic) do
    sign(total) <> zero_pad(hour, 2) <> zero_pad(minute, 2)
  end

  defp zone_to_string(0, 0, _abbr, "Etc/UTC"), do: ""
  defp zone_to_string(_, _, abbr, zone), do: " " <> abbr <> " " <> zone

  defp sign(total) when total < 0, do: "-"
  defp sign(_), do: "+"

  defp zero_pad(val, count) when val >= 0 do
    num = Integer.to_string(val)
    :binary.copy("0", count - byte_size(num)) <> num
  end

  defp zero_pad(val, count) do
    "-" <> zero_pad(-val, count)
  end

  ## Helpers

  @doc false
  def from_unix(integer, unit) when is_integer(integer) do
    total = System.convert_time_unit(integer, unit, :microsecond)

    if total in @unix_range_microseconds do
      microseconds = Integer.mod(total, @microseconds_per_second)
      seconds = @unix_epoch + Integer.floor_div(total, @microseconds_per_second)
      precision = precision_for_unit(unit)
      {date, time} = iso_seconds_to_datetime(seconds)
      {:ok, date, time, {microseconds, precision}}
    else
      {:error, :invalid_unix_time}
    end
  end

  defp precision_for_unit(unit) do
    subsecond = div(System.convert_time_unit(1, :second, unit), 10)
    precision_for_unit(subsecond, 0)
  end

  defp precision_for_unit(0, precision), do: precision
  defp precision_for_unit(_, 6), do: 6

  defp precision_for_unit(number, precision),
    do: precision_for_unit(div(number, 10), precision + 1)

  @doc false
  @doc since: "1.5.0"
  def date_to_iso8601(year, month, day, format \\ :extended) do
    date_to_string(year, month, day, format)
  end

  @doc false
  def time_to_iso8601(hour, minute, second, microsecond, format \\ :extended) do
    time_to_string(hour, minute, second, microsecond, format)
  end

  @doc false
  def naive_datetime_to_iso8601(
        year,
        month,
        day,
        hour,
        minute,
        second,
        microsecond,
        format \\ :extended
      ) do
    date_to_string(year, month, day, format) <>
      "T" <> time_to_string(hour, minute, second, microsecond, format)
  end

  @doc false
  def datetime_to_iso8601(
        year,
        month,
        day,
        hour,
        minute,
        second,
        microsecond,
        time_zone,
        _zone_abbr,
        utc_offset,
        std_offset,
        format \\ :extended
      ) do
    date_to_string(year, month, day, format) <>
      "T" <>
      time_to_string(hour, minute, second, microsecond, format) <>
      offset_to_string(utc_offset, std_offset, time_zone, format)
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

  def parse_microsecond("," <> rest) do
    parse_microsecond("." <> rest)
  end

  def parse_microsecond(rest) do
    {{0, 0}, rest}
  end

  defp parse_microsecond(<<head, tail::binary>>, precision, acc) when head in ?0..?9,
    do: parse_microsecond(tail, precision + 1, <<acc::binary, head>>)

  defp parse_microsecond(rest, precision, acc), do: {acc, precision, rest}

  @doc false
  def parse_offset(""), do: {nil, ""}
  def parse_offset("Z"), do: {0, ""}
  def parse_offset("-00:00"), do: :error

  def parse_offset(<<?+, hour::2-bytes, ?:, min::2-bytes, rest::binary>>),
    do: parse_offset(1, hour, min, rest)

  def parse_offset(<<?-, hour::2-bytes, ?:, min::2-bytes, rest::binary>>),
    do: parse_offset(-1, hour, min, rest)

  def parse_offset(<<?+, hour::2-bytes, min::2-bytes, rest::binary>>),
    do: parse_offset(1, hour, min, rest)

  def parse_offset(<<?-, hour::2-bytes, min::2-bytes, rest::binary>>),
    do: parse_offset(-1, hour, min, rest)

  def parse_offset(<<?+, hour::2-bytes, rest::binary>>), do: parse_offset(1, hour, "00", rest)
  def parse_offset(<<?-, hour::2-bytes, rest::binary>>), do: parse_offset(-1, hour, "00", rest)
  def parse_offset(_), do: :error

  defp parse_offset(sign, hour, min, rest) do
    with {hour, ""} when hour < 24 <- Integer.parse(hour),
         {min, ""} when min < 60 <- Integer.parse(min) do
      {(hour * 60 + min) * 60 * sign, rest}
    else
      _ -> :error
    end
  end

  @doc false
  @doc since: "1.5.0"
  def iso_days_to_unit({days, {parts, ppd}}, unit) do
    day_microseconds = days * @parts_per_day
    microseconds = div(parts * @parts_per_day, ppd)
    System.convert_time_unit(day_microseconds + microseconds, :microsecond, unit)
  end

  @doc false
  @doc since: "1.5.0"
  def add_day_fraction_to_iso_days({days, {parts, ppd}}, add, ppd) do
    normalize_iso_days(days, parts + add, ppd)
  end

  def add_day_fraction_to_iso_days({days, {parts, ppd}}, add, add_ppd) do
    parts = parts * add_ppd
    add = add * ppd
    gcd = Integer.gcd(ppd, add_ppd)
    result_parts = div(parts + add, gcd)
    result_ppd = div(ppd * add_ppd, gcd)
    normalize_iso_days(days, result_parts, result_ppd)
  end

  defp normalize_iso_days(days, parts, ppd) do
    days_offset = div(parts, ppd)
    parts = rem(parts, ppd)

    if parts < 0 do
      {days + days_offset - 1, {parts + ppd, ppd}}
    else
      {days + days_offset, {parts, ppd}}
    end
  end

  # Note that this function does not add the extra leap day for a leap year.
  # If you want to add that leap day when appropriate,
  # add the result of leap_day_offset/2 to the result of days_before_month/1.
  defp days_before_month(1), do: 0
  defp days_before_month(2), do: 31
  defp days_before_month(3), do: 59
  defp days_before_month(4), do: 90
  defp days_before_month(5), do: 120
  defp days_before_month(6), do: 151
  defp days_before_month(7), do: 181
  defp days_before_month(8), do: 212
  defp days_before_month(9), do: 243
  defp days_before_month(10), do: 273
  defp days_before_month(11), do: 304
  defp days_before_month(12), do: 334

  defp leap_day_offset(_year, month) when month < 3, do: 0

  defp leap_day_offset(year, _month) do
    if leap_year?(year), do: 1, else: 0
  end

  defp days_to_year(days) do
    year = Integer.floor_div(days, @days_per_nonleap_year)
    {year, days_before_year} = days_to_year(year, days, days_in_previous_years(year))
    {year, days - days_before_year}
  end

  defp days_to_year(year, days1, days2) when days1 < days2 do
    days_to_year(year - 1, days1, days_in_previous_years(year - 1))
  end

  defp days_to_year(year, _days1, days2) do
    {year, days2}
  end

  defp days_in_previous_years(0), do: 0

  defp days_in_previous_years(year) do
    previous_year = year - 1

    Integer.floor_div(previous_year, 4) - Integer.floor_div(previous_year, 100) +
      Integer.floor_div(previous_year, 400) + previous_year * @days_per_nonleap_year +
      @days_per_leap_year
  end

  # Note that 0 is the first day of the month.
  defp year_day_to_year_date(_extra_day, day_of_year) when day_of_year < 31 do
    {1, day_of_year}
  end

  defp year_day_to_year_date(extra_day, day_of_year) when day_of_year < 59 + extra_day do
    {2, day_of_year - 31}
  end

  defp year_day_to_year_date(extra_day, day_of_year) when day_of_year < 90 + extra_day do
    {3, day_of_year - (59 + extra_day)}
  end

  defp year_day_to_year_date(extra_day, day_of_year) when day_of_year < 120 + extra_day do
    {4, day_of_year - (90 + extra_day)}
  end

  defp year_day_to_year_date(extra_day, day_of_year) when day_of_year < 151 + extra_day do
    {5, day_of_year - (120 + extra_day)}
  end

  defp year_day_to_year_date(extra_day, day_of_year) when day_of_year < 181 + extra_day do
    {6, day_of_year - (151 + extra_day)}
  end

  defp year_day_to_year_date(extra_day, day_of_year) when day_of_year < 212 + extra_day do
    {7, day_of_year - (181 + extra_day)}
  end

  defp year_day_to_year_date(extra_day, day_of_year) when day_of_year < 243 + extra_day do
    {8, day_of_year - (212 + extra_day)}
  end

  defp year_day_to_year_date(extra_day, day_of_year) when day_of_year < 273 + extra_day do
    {9, day_of_year - (243 + extra_day)}
  end

  defp year_day_to_year_date(extra_day, day_of_year) when day_of_year < 304 + extra_day do
    {10, day_of_year - (273 + extra_day)}
  end

  defp year_day_to_year_date(extra_day, day_of_year) when day_of_year < 334 + extra_day do
    {11, day_of_year - (304 + extra_day)}
  end

  defp year_day_to_year_date(extra_day, day_of_year) do
    {12, day_of_year - (334 + extra_day)}
  end

  defp iso_seconds_to_datetime(seconds) do
    {days, rest_seconds} = div_mod(seconds, @seconds_per_day)

    date = date_from_iso_days(days)
    time = seconds_to_time(rest_seconds)
    {date, time}
  end

  defp seconds_to_time(seconds) when seconds in 0..(@seconds_per_day - 1) do
    {hour, rest_seconds} = div_mod(seconds, @seconds_per_hour)
    {minute, second} = div_mod(rest_seconds, @seconds_per_minute)

    {hour, minute, second}
  end
end
