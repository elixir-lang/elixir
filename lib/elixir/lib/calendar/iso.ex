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

  @gregorian_epoch 1

  @unix_epoch :calendar.datetime_to_gregorian_seconds {{1970, 1, 1}, {0, 0, 0}}
  @unix_start 1_000_000 * -@unix_epoch
  @unix_end 1_000_000 * (-@unix_epoch + :calendar.datetime_to_gregorian_seconds({{9999, 12, 31}, {23, 59, 59}}))
  @unix_range_microseconds @unix_start..@unix_end

  @type year  :: 0..9999
  @type month :: 1..12
  @type day   :: 1..31

  @seconds_per_minute 60
  @seconds_per_hour 60 * 60
  @seconds_per_day 24 * 60 * 60 # Note that this does _not_ handle Leap Seconds.
  @microseconds_per_second 1_000_000

  import Integer, only: [floor_div: 2, div_mod: 2]

  @doc """
  Returns the normalized Rata Die representation of the specified date.

  ## Examples

      iex> Calendar.ISO.datetime_to_rata_die(~N[0001-01-01T00:00:00] |> DateTime.from_naive!("Etc/UTC"))
      {1, {0, 86400000000}}
      iex> Calendar.ISO.datetime_to_rata_die(~N[2000-01-01T12:00:00] |> DateTime.from_naive!("Etc/UTC"))
      {730120, {43200000000, 86400000000}}
      iex> Calendar.ISO.datetime_to_rata_die(~N[2016-09-18T13:00:14] |> DateTime.from_naive!("Etc/UTC"))
      {736225, {46814000000, 86400000000}}
  """
  # TODO: Conversion Datetime or NaiveDateTime -> RataDie?
  @spec datetime_to_rata_die(Calendar.DateTime) :: Calendar.rata_die
  def datetime_to_rata_die(%{calendar: _calendar, year: year, month: month, day: day, hour: hour, minute: minute, second: second, microsecond: {microsecond, _},
                             std_offset: std_offset, utc_offset: utc_offset}) do
    # Baseline to epoch.  This will be zero for a Gregorian calendar
    days = to_rata_die_day(year, month, day)
    {parts, ppd} = combine_time_to_day_fraction(hour, minute, second, microsecond, std_offset, utc_offset)
    # Applying negative UTC offsets might result in a negative day fraction.
    if (parts < 0) do
      {days - 1, {ppd - parts, ppd}}
    else
      {days, {parts, ppd}}
    end

  end

  @doc """
  Converts a Rata Die to the datetime format specified by this calendar.

  ## Examples

  iex> Calendar.ISO.datetime_from_rata_die({1, {0, 86400}})
  %DateTime{calendar: Calendar.ISO, day: 1, hour: 0, microsecond: {0, 6},
  minute: 0, month: 1, second: 0, std_offset: 0, time_zone: "Etc/UTC",
  utc_offset: 0, year: 1, zone_abbr: "UTC"}
  iex> Calendar.ISO.datetime_from_rata_die({730120, {0, 86400}})
  %DateTime{calendar: Calendar.ISO, day: 1, hour: 0, microsecond: {0, 6},
  minute: 0, month: 1, second: 0, std_offset: 0, time_zone: "Etc/UTC",
  utc_offset: 0, year: 2000, zone_abbr: "UTC"}
  iex> Calendar.ISO.datetime_from_rata_die({730120, {43200, 86400}})
  %DateTime{calendar: Calendar.ISO, day: 1, hour: 12, microsecond: {0, 6},
  minute: 0, month: 1, second: 0, std_offset: 0, time_zone: "Etc/UTC",
  utc_offset: 0, year: 2000, zone_abbr: "UTC"}
  """
  # TODO: Conversion RataDie -> DateTime or NaiveDateTime?
  @spec datetime_from_rata_die(Calendar.rata_die) :: Calendar.DateTime
  def datetime_from_rata_die({days, {parts_in_day, parts_of_day}}) do
    {year, month, day} = from_rata_die_day(days)
    {hour, minute, second, microsecond} = extract_from_day_fraction(parts_in_day, parts_of_day)
    {:ok, naive} = NaiveDateTime.new(year, month, day, hour, minute, second, {microsecond, 6})
    {:ok, datetime} = DateTime.from_naive(naive, "Etc/UTC")
    datetime
  end
  @doc """
  Returns the normalized Day Fraction of the specified time.

  ## Examples

  iex> Calendar.ISO.time_to_day_fraction(~T[00:00:00])
  {0, 86400000000}
  iex> Calendar.ISO.time_to_day_fraction(~T[12:34:56.123])
  {45296123000, 86400000000}
  """
  @spec time_to_day_fraction(Calendar.Time) :: Calendar.day_fraction
  def time_to_day_fraction(%{hour: hour, minute: minute, second: second, microsecond: {microsecond, _}}) do
    combine_time_to_day_fraction(hour, minute, second, microsecond)
  end

  @doc """
  Converts a Day Fraction to this Calendar's representation of time.

  ## Examples
  iex> Calendar.ISO.time_from_day_fraction({1,2})
  ~T[12:00:00.000000]
  iex> Calendar.ISO.time_from_day_fraction({13,24})
  ~T[13:00:00.000000]
  """
  @spec time_from_day_fraction(Calendar.day_fraction) :: Calendar.Time
  def time_from_day_fraction({parts_in_day, parts_per_day}) do
    {hour, minute, second, microsecond} = extract_from_day_fraction(parts_in_day, parts_per_day)
    {:ok, time} = Time.new(hour, minute, second, microsecond)
    time
  end

  defp combine_time_to_day_fraction(hour, minute, second, microsecond, std_offset \\ 0, utc_offset \\ 0) do
    combined_seconds = hour * @seconds_per_hour + minute * @seconds_per_minute + second - std_offset - utc_offset
    {combined_seconds * @microseconds_per_second + microsecond, @seconds_per_day * @microseconds_per_second}
  end

  # Converts a year, month, day in only a count of days since the Rata Die epoch.
  @spec to_rata_die_day(integer, pos_integer, pos_integer) :: Calendar.rata_die
  defp to_rata_die_day(year, month, day) do
    (@gregorian_epoch - 1) +

    # Normal year arithmetic with 365 days in a year
    (365 * (year - 1)) +

    # Adjust for leap years.
    floor_div(year - 1, 4) - floor_div(year - 1, 100) + floor_div(year - 1, 400) +

    # At this point we have the number of days from the start of the epoch
    # for the given number of years.  Now calculate add the days held by the
    # month
    floor_div((367 * month) - 362, 12) +

    # And ajust by zero, minus one or -minus two days depending on whether
    # leap_year? and if month is January or later (since February is either 28
    # or 29 days)
    rata_die_adjust_for_leap_year(year, month, day) +

    # And then the day of the month is added
    day
  end

  # Calculates {year, month, day} from the count of days since the Rata Die epoch.
  @spec from_rata_die_day(integer) :: {integer, pos_integer, pos_integer}
  defp from_rata_die_day(days) do
    {year, days_in_year} = extract_year_from_rata_die(days)
    {month, day} = extract_month_from_rata_die(days_in_year, leap_year?(year))
    {year, month, day}
  end

  # Calculates {hours, minutes, seconds, microseconds} from the fraction of time passed in the last Rata Die day.
  @spec extract_from_day_fraction(non_neg_integer, pos_integer) :: {non_neg_integer, non_neg_integer, non_neg_integer, non_neg_integer}
  def extract_from_day_fraction(parts_in_day, parts_per_day) do
    total_microseconds = div(parts_in_day * @seconds_per_day * @microseconds_per_second, parts_per_day)
    {hours, rest_microseconds1} = div_mod(total_microseconds, @seconds_per_hour * @microseconds_per_second)
    {minutes, rest_microseconds2} = div_mod(rest_microseconds1, @seconds_per_minute * @microseconds_per_second)
    {seconds, microseconds} = div_mod(rest_microseconds2, @microseconds_per_second)
    {hours, minutes, seconds, microseconds}
  end

  # Adjust the amount of days when we are past February.
  @spec rata_die_adjust_for_leap_year(integer, pos_integer, pos_integer) :: integer
  defp rata_die_adjust_for_leap_year(year, month, _day) do
    cond do
      month <= 2       ->  0
      leap_year?(year) -> -1
      true             -> -2
    end
  end

  # Extracts the year and the remaining amount of days in the year
  # from the count of days since the Rata Die epoch.
  @spec extract_year_from_rata_die(integer) :: {integer, pos_integer}
  defp extract_year_from_rata_die(days) do
    d0   = days - @gregorian_epoch
    {n400, d1} = div_mod(d0, 146_097)
    {n100, d2} = div_mod(d1, 36_524)
    {n4, d3}   = div_mod(d2, 1_461)
    {n1, d4}   = div_mod(d3, 365)
    days = d4 + 1

    year = (400 * n400) + (100 * n100) + (4 * n4) + n1
    if (n100 == 4) or (n1 == 4), do: {year, days}, else: {year + 1, days}
  end

  # Extracts the month and the day in the month
  # from the count of days since the beginning of a year, and if it is a leap year or not.
  @spec extract_month_from_rata_die(pos_integer, boolean) :: {pos_integer, pos_integer}
  defp extract_month_from_rata_die(days_in_year, leap_year) do
    month_lengths = [
      31,
      if(leap_year, do: 29, else: 28),
      31,
      30,
      31,
      30,
      31,
      31,
      30,
      31,
      30,
      31
    ]

    Enum.reduce_while(month_lengths, {1, days_in_year},
      fn
        month_days, {month_count, rest_days} when rest_days <= month_days ->
          {:halt, {month_count, rest_days}}
        month_days, {month_count, rest_days} ->
          {:cont, {month_count + 1, rest_days - month_days}}
      end
    )
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
  def parse_microsecond("," <> rest) do
    parse_microsecond("." <> rest)
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
