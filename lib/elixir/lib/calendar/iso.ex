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
  @unix_start 1_000_000 * -@unix_epoch
  @unix_end 1_000_000 * (-@unix_epoch + :calendar.datetime_to_gregorian_seconds({{9999, 12, 31}, {23, 59, 59}}))
  @unix_range_microseconds @unix_start..@unix_end

  @type year :: 0..9999
  @type month :: 1..12
  @type day :: 1..31

  @seconds_per_minute 60
  @seconds_per_hour 60 * 60
  @seconds_per_day 24 * 60 * 60 # Note that this does _not_ handle leap seconds.
  @microseconds_per_second 1_000_000

  @doc """
  Returns the normalized Rata Die representation of the specified date.

  ## Examples

      iex> Calendar.ISO.naive_datetime_to_rata_die(1, 1, 1, 0, 0, 0, {0, 6})
      {1, {0, 86400000000}}
      iex> Calendar.ISO.naive_datetime_to_rata_die(2000, 1, 1, 12, 0, 0, {0, 6})
      {730120, {43200000000, 86400000000}}
      iex> Calendar.ISO.naive_datetime_to_rata_die(2000, 1, 1, 13, 0, 0, {0, 6})
      {730120, {46800000000, 86400000000}}

  """
  @spec naive_datetime_to_rata_die(Calendar.year, Calendar.month, Calendar.day,
                                   Calendar.hour, Calendar.minute, Calendar.second,
                                   Calendar.microsecond) :: Calendar.rata_die
  def naive_datetime_to_rata_die(year, month, day, hour, minute, second, {microsecond, _}) do
    days = to_rata_die_day(year, month, day)
    {parts, ppd} = combine_time_to_day_fraction(hour, minute, second, microsecond)
    {days, {parts, ppd}}
  end

  def naive_datetime_to_rata_die(year, month, day, hour, minute, second, microsecond) do
    naive_datetime_to_rata_die(year, month, day, hour, minute, second, {microsecond, 0})
  end

  @doc """
  Converts a Rata Die to the datetime format specified by this calendar.

  ## Examples

      iex> Calendar.ISO.naive_datetime_from_rata_die({1, {0, 86400}})
      {1, 1, 1, 0, 0, 0, {0, 6}}

      iex> Calendar.ISO.naive_datetime_from_rata_die({730120, {0, 86400}})
      {2000, 1, 1, 0, 0, 0, {0, 6}}

      iex> Calendar.ISO.naive_datetime_from_rata_die({730120, {43200, 86400}})
      {2000, 1, 1, 12, 0, 0, {0, 6}}

  """
  @spec naive_datetime_from_rata_die(Calendar.rata_die) ::
        {Calendar.year, Calendar.month, Calendar.day,
         Calendar.hour, Calendar.minute, Calendar.second, Calendar.microsecond}
  def naive_datetime_from_rata_die({days, {parts_in_day, parts_of_day}}) do
    {year, month, day} = from_rata_die_day(days)
    {hour, minute, second, microsecond} = extract_from_day_fraction(parts_in_day, parts_of_day)
    {year, month, day, hour, minute, second, {microsecond, 6}}
  end

  @doc """
  Returns the normalized day fraction of the specified time.

  ## Examples

      iex> Calendar.ISO.time_to_day_fraction(0, 0, 0, {0, 6})
      {0, 86400000000}
      iex> Calendar.ISO.time_to_day_fraction(12, 34, 56, {123, 6})
      {45296000123, 86400000000}

  """
  @spec time_to_day_fraction(Calendar.hour, Calendar.minute,
                             Calendar.second, Calendar.microsecond) :: Calendar.day_fraction
  def time_to_day_fraction(hour, minute, second, {microsecond, _}) do
    combine_time_to_day_fraction(hour, minute, second, microsecond)
  end

  @doc """
  Converts a day fraction to this Calendar's representation of time.

  ## Examples

      iex> Calendar.ISO.time_from_day_fraction({1,2})
      {12, 0, 0, {0, 6}}
      iex> Calendar.ISO.time_from_day_fraction({13,24})
      {13, 0, 0, {0, 6}}

  """
  @spec time_from_day_fraction(Calendar.day_fraction) ::
        {Calendar.hour, Calendar.minute, Calendar.second, Calendar.microsecond}
  def time_from_day_fraction({parts_in_day, parts_per_day}) do
    {hour, minute, second, microsecond} = extract_from_day_fraction(parts_in_day, parts_per_day)
    {hour, minute, second, {microsecond, 6}}
  end

  defp combine_time_to_day_fraction(hour, minute, second, microsecond) do
    combined_seconds = hour * @seconds_per_hour + minute * @seconds_per_minute + second
    {combined_seconds * @microseconds_per_second + microsecond, @seconds_per_day * @microseconds_per_second}
  end

  # Converts a year, month, day in only a count of days since the Rata Die epoch.
  defp to_rata_die_day(year, month, day) do
    # Rata Die starts at year 1, rather than at year 0.
    :calendar.date_to_gregorian_days(year, month, day) - 365
  end

  # Calculates {year, month, day} from the count of days since the Rata Die epoch.
  defp from_rata_die_day(days) do
    :calendar.gregorian_days_to_date(days + 365)
  end

  # Calculates {hours, minutes, seconds, microseconds} from the fraction of time passed in the last Rata Die day.
  defp extract_from_day_fraction(parts_in_day, parts_per_day) do
    total_microseconds = div(parts_in_day * @seconds_per_day * @microseconds_per_second, parts_per_day)
    {hours, rest_microseconds1} = div_mod(total_microseconds, @seconds_per_hour * @microseconds_per_second)
    {minutes, rest_microseconds2} = div_mod(rest_microseconds1, @seconds_per_minute * @microseconds_per_second)
    {seconds, microseconds} = div_mod(rest_microseconds2, @microseconds_per_second)
    {hours, minutes, seconds, microseconds}
  end

  defp div_mod(int1, int2) do
    div = div(int1, int2)
    mod = int1 - (div * int2)
    {div, mod}
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
  Converts the given time into a string.
  """
  def time_to_string(hour, minute, second, microsecond, format \\ :extended)
  def time_to_string(hour, minute, second, {_, 0}, format) do
    case format do
      :extended -> time_to_string_extended(hour, minute, second)
      :basic -> time_to_string_basic(hour, minute, second)
    end
  end

  def time_to_string(hour, minute, second, {microsecond, precision}, format) do
    time = case format do
      :extended -> time_to_string_extended(hour, minute, second)
      :basic -> time_to_string_basic(hour, minute, second)
    end

    time <> "." <> (microsecond |> zero_pad(6) |> binary_part(0, precision))
  end

  defp time_to_string_extended(hour, minute, second) do
    zero_pad(hour, 2) <> ":" <> zero_pad(minute, 2) <> ":" <> zero_pad(second, 2)
  end

  defp time_to_string_basic(hour, minute, second) do
    zero_pad(hour, 2) <> zero_pad(minute, 2) <> zero_pad(second, 2)
  end

  @doc """
  Converts the given date into a string.
  """
  def date_to_string(year, month, day) do
    zero_pad(year, 4) <> "-" <> zero_pad(month, 2) <> "-" <> zero_pad(day, 2)
  end

  defp date_to_string(year, month, day, :extended), do: date_to_string(year, month, day)
  defp date_to_string(year, month, day, :basic) do
    zero_pad(year, 4) <> zero_pad(month, 2) <> zero_pad(day, 2)
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

  def valid_date?(year, month, day) do
    :calendar.valid_date(year, month, day) and year <= 9999
  end

  def valid_time?(hour, minute, second, {microsecond, _}) do
    hour in 0..23 and minute in 0..59 and second in 0..60 and microsecond in 0..999_999
  end

  def day_rollover_relative_to_midnight_utc() do
    {0, 1}
  end

  defp offset_to_string(utc, std, zone, format \\ :extended)
  defp offset_to_string(0, 0, "Etc/UTC", _format), do: "Z"
  defp offset_to_string(utc, std, _zone, format) do
    total  = utc + std
    second = abs(total)
    minute = second |> rem(3600) |> div(60)
    hour   = second |> div(3600)
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

  defp zero_pad(val, count) do
    num = Integer.to_string(val)
    :binary.copy("0", count - byte_size(num)) <> num
  end

  ## Helpers

  @doc false
  def from_unix(integer, unit) when is_integer(integer) do
    total = System.convert_time_unit(integer, unit, :microsecond)
    if total in @unix_range_microseconds do
      microsecond = rem(total, 1_000_000)
      precision = precision_for_unit(unit)
      {date, time} = :calendar.gregorian_seconds_to_datetime(@unix_epoch + div(total, 1_000_000))
      {:ok, date, time, {microsecond, precision}}
    else
      {:error, :invalid_unix_time}
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
  def date_to_iso8601(year, month, day, format \\ :extended) do
    date_to_string(year, month, day, format)
  end

  @doc false
  def time_to_iso8601(hour, minute, second, microsecond, format \\ :extended) do
    time_to_string(hour, minute, second, microsecond, format)
  end

  @doc false
  def naive_datetime_to_iso8601(year, month, day, hour, minute, second, microsecond, format \\ :extended) do
    date_to_string(year, month, day, format) <> "T" <> time_to_string(hour, minute, second, microsecond, format)
  end

  @doc false
  def datetime_to_iso8601(year, month, day, hour, minute, second, microsecond,
                          time_zone, _zone_abbr, utc_offset, std_offset, format \\ :extended) do
    date_to_string(year, month, day, format) <> "T" <>
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

  @doc false
  def rata_die_to_unit({days, {parts, ppd}}, unit) do
    day_microseconds = days * @seconds_per_day * @microseconds_per_second
    microseconds = div(parts * @seconds_per_day * @microseconds_per_second, ppd)
    System.convert_time_unit(day_microseconds + microseconds, :microsecond, unit)
  end
end
