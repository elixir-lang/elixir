defmodule Calendar.ISO do
  @moduledoc """
  The default calendar implementation, a Gregorian calendar following ISO 8601.

  This calendar implements a proleptic Gregorian calendar and
  is therefore compatible with the calendar used in most countries
  today. The proleptic means the Gregorian rules for leap years are
  applied for all time, consequently the dates give different results
  before the year 1583 from when the Gregorian calendar was adopted.

  ## ISO 8601 compliance

  The ISO 8601 specification is feature-rich, but allows applications
  to selectively implement most parts of it. The choices Elixir makes
  are catalogued below.

  ### Features

  The standard library supports a minimal set of possible ISO 8601 features.
  Specifically, the parser only supports calendar dates and does not support
  ordinal and week formats.

  By default Elixir only parses extended-formatted date/times. You can opt-in
  to parse basic-formatted date/times.

  `NaiveDateTime.to_iso8601/2` and `DateTime.to_iso8601/2` allow you to produce
  either basic or extended formatted strings, and `Calendar.strftime/2` allows
  you to format datetimes however else you desire.

  Elixir does not support reduced accuracy formats (for example, a date without
  the day component) nor decimal precisions in the lowest component (such as
  `10:01:25,5`). No functions exist to parse ISO 8601 durations or time intervals.

  #### Examples

  Elixir expects the extended format by default when parsing:

      iex> Calendar.ISO.parse_naive_datetime("2015-01-23T23:50:07")
      {:ok, {2015, 1, 23, 23, 50, 7, {0, 0}}}
      iex> Calendar.ISO.parse_naive_datetime("20150123T235007")
      {:error, :invalid_format}

  Parsing can be restricted to basic if desired:

      iex> Calendar.ISO.parse_naive_datetime("20150123T235007Z", :basic)
      {:ok, {2015, 1, 23, 23, 50, 7, {0, 0}}}
      iex> Calendar.ISO.parse_naive_datetime("20150123T235007Z", :extended)
      {:error, :invalid_format}

  Only calendar dates are supported in parsing; ordinal and week dates are not.

      iex> Calendar.ISO.parse_date("2015-04-15")
      {:ok, {2015, 4, 15}}
      iex> Calendar.ISO.parse_date("2015-105")
      {:error, :invalid_format}
      iex> Calendar.ISO.parse_date("2015-W16")
      {:error, :invalid_format}
      iex> Calendar.ISO.parse_date("2015-W016-3")
      {:error, :invalid_format}

  Years, months, days, hours, minutes, and seconds must be fully specified:

      iex> Calendar.ISO.parse_date("2015-04-15")
      {:ok, {2015, 4, 15}}
      iex> Calendar.ISO.parse_date("2015-04")
      {:error, :invalid_format}
      iex> Calendar.ISO.parse_date("2015")
      {:error, :invalid_format}

      iex> Calendar.ISO.parse_time("23:50:07.0123456")
      {:ok, {23, 50, 7, {12345, 6}}}
      iex> Calendar.ISO.parse_time("23:50:07")
      {:ok, {23, 50, 7, {0, 0}}}
      iex> Calendar.ISO.parse_time("23:50")
      {:error, :invalid_format}
      iex> Calendar.ISO.parse_time("23")
      {:error, :invalid_format}

  ### Extensions

  The parser and formatter adopt one ISO 8601 extension: extended year notation.

  This allows dates to be prefixed with a `+` or `-` sign, extending the range of
  expressible years from the default (`0000..9999`) to `-9999..9999`. Elixir still
  restricts years in this format to four digits.

  #### Examples

      iex> Calendar.ISO.parse_date("-2015-01-23")
      {:ok, {-2015, 1, 23}}
      iex> Calendar.ISO.parse_date("+2015-01-23")
      {:ok, {2015, 1, 23}}

      iex> Calendar.ISO.parse_naive_datetime("-2015-01-23 23:50:07")
      {:ok, {-2015, 1, 23, 23, 50, 7, {0, 0}}}
      iex> Calendar.ISO.parse_naive_datetime("+2015-01-23 23:50:07")
      {:ok, {2015, 1, 23, 23, 50, 7, {0, 0}}}

      iex> Calendar.ISO.parse_utc_datetime("-2015-01-23 23:50:07Z")
      {:ok, {-2015, 1, 23, 23, 50, 7, {0, 0}}, 0}
      iex> Calendar.ISO.parse_utc_datetime("+2015-01-23 23:50:07Z")
      {:ok, {2015, 1, 23, 23, 50, 7, {0, 0}}, 0}

  ### Additions

  ISO 8601 does not allow a whitespace instead of `T` as a separator
  between date and times, both when parsing and formatting.
  This is a common enough representation, Elixir allows it during parsing.

  The formatting of dates in `NaiveDateTime.to_iso8601/1` and `DateTime.to_iso8601/1`
  do produce specification-compliant string representations using the `T` separator.

  #### Examples

      iex> Calendar.ISO.parse_naive_datetime("2015-01-23 23:50:07.0123456")
      {:ok, {2015, 1, 23, 23, 50, 7, {12345, 6}}}
      iex> Calendar.ISO.parse_naive_datetime("2015-01-23T23:50:07.0123456")
      {:ok, {2015, 1, 23, 23, 50, 7, {12345, 6}}}

      iex> Calendar.ISO.parse_utc_datetime("2015-01-23 23:50:07.0123456Z")
      {:ok, {2015, 1, 23, 23, 50, 7, {12345, 6}}, 0}
      iex> Calendar.ISO.parse_utc_datetime("2015-01-23T23:50:07.0123456Z")
      {:ok, {2015, 1, 23, 23, 50, 7, {12345, 6}}, 0}

  """

  @behaviour Calendar

  @unix_epoch 62_167_219_200
  unix_start = (315_537_897_600 + @unix_epoch) * -1_000_000
  unix_end = 315_569_519_999_999_999 - @unix_epoch * 1_000_000
  @unix_range_microseconds unix_start..unix_end

  @typedoc """
  "Before the Current Era" or "Before the Common Era" (BCE), for those years less than `1`.
  """
  @type bce :: 0

  @typedoc """
  The "Current Era" or the "Common Era" (CE) which starts in year `1`.
  """
  @type ce :: 1

  @typedoc """
  The calendar era.

  The ISO calendar has two eras:
  * [CE](`t:ce/0`) - which starts in year `1` and is defined as era `1`.
  * [BCE](`t:bce/0`) - for those years less than `1` and is defined as era `0`.
  """
  @type era :: bce | ce
  @type year :: -9999..9999
  @type month :: 1..12
  @type day :: 1..31
  @type hour :: 0..23
  @type minute :: 0..59
  @type second :: 0..59
  @type weekday :: :monday | :tuesday | :wednesday | :thursday | :friday | :saturday | :sunday

  @typedoc """
  Microseconds with stored precision.

  The precision represents the number of digits that must be used when
  representing the microseconds to external format. If the precision is 0,
  it means microseconds must be skipped.
  """
  @type microsecond :: {0..999_999, 0..6}

  @typedoc """
  Integer that represents the day of the week, where 1 is Monday and 7 is Sunday.
  """
  @type day_of_week :: 1..7

  @type day_of_year :: 1..366
  @type quarter_of_year :: 1..4
  @type year_of_era :: {1..10000, era}

  @seconds_per_minute 60
  @seconds_per_hour 60 * 60
  # Note that this does *not* handle leap seconds.
  @seconds_per_day 24 * 60 * 60
  @last_second_of_the_day @seconds_per_day - 1
  @microseconds_per_second 1_000_000
  @parts_per_day @seconds_per_day * @microseconds_per_second

  @datetime_seps [?\s, ?T]
  @ext_date_sep ?-
  @ext_time_sep ?:

  @days_per_nonleap_year 365
  @days_per_leap_year 366

  # The ISO epoch starts, in this implementation,
  # with ~D[0000-01-01]. Era "1" starts
  # on ~D[0001-01-01] which is 366 days later.
  @iso_epoch 366

  [match_basic_date, match_ext_date, guard_date, read_date] =
    quote do
      [
        <<y1, y2, y3, y4, m1, m2, d1, d2>>,
        <<y1, y2, y3, y4, @ext_date_sep, m1, m2, @ext_date_sep, d1, d2>>,
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

  [match_basic_time, match_ext_time, guard_time, read_time] =
    quote do
      [
        <<h1, h2, i1, i2, s1, s2>>,
        <<h1, h2, @ext_time_sep, i1, i2, @ext_time_sep, s1, s2>>,
        h1 >= ?0 and h1 <= ?9 and h2 >= ?0 and h2 <= ?9 and i1 >= ?0 and i1 <= ?9 and i2 >= ?0 and
          i2 <= ?9 and s1 >= ?0 and s1 <= ?9 and s2 >= ?0 and s2 <= ?9,
        {
          (h1 - ?0) * 10 + (h2 - ?0),
          (i1 - ?0) * 10 + (i2 - ?0),
          (s1 - ?0) * 10 + (s2 - ?0)
        }
      ]
    end

  defguardp is_year(year) when year in -9999..9999
  defguardp is_year_BCE(year) when year in -9999..0
  defguardp is_year_CE(year) when year in 1..9999
  defguardp is_month(month) when month in 1..12
  defguardp is_day(day) when day in 1..31
  defguardp is_hour(hour) when hour in 0..23
  defguardp is_minute(minute) when minute in 0..59
  defguardp is_second(second) when second in 0..59

  defguardp is_microsecond(microsecond, precision)
            when microsecond in 0..999_999 and precision in 0..6

  defguardp is_time_zone(term) when is_binary(term)
  defguardp is_zone_abbr(term) when is_binary(term)
  defguardp is_utc_offset(offset) when is_integer(offset)
  defguardp is_std_offset(offset) when is_integer(offset)

  @doc """
  Parses a time `string` in the `:extended` format.

  For more information on supported strings, see how this
  module implements [ISO 8601](#module-iso-8601-compliance).

  ## Examples

      iex> Calendar.ISO.parse_time("23:50:07")
      {:ok, {23, 50, 7, {0, 0}}}

      iex> Calendar.ISO.parse_time("23:50:07Z")
      {:ok, {23, 50, 7, {0, 0}}}
      iex> Calendar.ISO.parse_time("T23:50:07Z")
      {:ok, {23, 50, 7, {0, 0}}}

  """
  @doc since: "1.10.0"
  @impl true
  def parse_time(string) when is_binary(string),
    do: parse_time(string, :extended)

  @doc """
  Parses a time `string` according to a given `format`.

  The `format` can either be `:basic` or `:extended`.

  For more information on supported strings, see how this
  module implements [ISO 8601](#module-iso-8601-compliance).

  ## Examples

      iex> Calendar.ISO.parse_time("235007", :basic)
      {:ok, {23, 50, 7, {0, 0}}}
      iex> Calendar.ISO.parse_time("235007", :extended)
      {:error, :invalid_format}

  """
  @doc since: "1.12.0"
  def parse_time("T" <> string, format) when is_binary(string),
    do: do_parse_time(string, format)

  def parse_time(string, format) when is_binary(string),
    do: do_parse_time(string, format)

  defp do_parse_time(<<unquote(match_basic_time), rest::binary>>, :basic)
       when unquote(guard_time) do
    {hour, minute, second} = unquote(read_time)
    parse_formatted_time(hour, minute, second, rest)
  end

  defp do_parse_time(<<unquote(match_ext_time), rest::binary>>, :extended)
       when unquote(guard_time) do
    {hour, minute, second} = unquote(read_time)
    parse_formatted_time(hour, minute, second, rest)
  end

  defp do_parse_time(_, _) do
    {:error, :invalid_format}
  end

  defp parse_formatted_time(hour, minute, second, rest) do
    with {microsecond, rest} <- parse_microsecond(rest),
         {_offset, ""} <- parse_offset(rest) do
      if valid_time?(hour, minute, second, microsecond) do
        {:ok, {hour, minute, second, microsecond}}
      else
        {:error, :invalid_time}
      end
    else
      _ -> {:error, :invalid_format}
    end
  end

  @doc """
  Parses a date `string` in the `:extended` format.

  For more information on supported strings, see how this
  module implements [ISO 8601](#module-iso-8601-compliance).

  ## Examples

      iex> Calendar.ISO.parse_date("2015-01-23")
      {:ok, {2015, 1, 23}}

      iex> Calendar.ISO.parse_date("2015:01:23")
      {:error, :invalid_format}
      iex> Calendar.ISO.parse_date("2015-01-32")
      {:error, :invalid_date}

  """
  @doc since: "1.10.0"
  @impl true
  def parse_date(string) when is_binary(string),
    do: parse_date(string, :extended)

  @doc """
  Parses a date `string` according to a given `format`.

  The `format` can either be `:basic` or `:extended`.

  For more information on supported strings, see how this
  module implements [ISO 8601](#module-iso-8601-compliance).

  ## Examples

      iex> Calendar.ISO.parse_date("20150123", :basic)
      {:ok, {2015, 1, 23}}
      iex> Calendar.ISO.parse_date("20150123", :extended)
      {:error, :invalid_format}

  """
  @doc since: "1.12.0"
  def parse_date("-" <> string, format) when is_binary(string),
    do: do_parse_date(string, -1, format)

  def parse_date("+" <> string, format) when is_binary(string),
    do: do_parse_date(string, 1, format)

  def parse_date(string, format) when is_binary(string),
    do: do_parse_date(string, 1, format)

  defp do_parse_date(unquote(match_basic_date), multiplier, :basic) when unquote(guard_date) do
    {year, month, day} = unquote(read_date)
    parse_formatted_date(year, month, day, multiplier)
  end

  defp do_parse_date(unquote(match_ext_date), multiplier, :extended) when unquote(guard_date) do
    {year, month, day} = unquote(read_date)
    parse_formatted_date(year, month, day, multiplier)
  end

  defp do_parse_date(_, _, _) do
    {:error, :invalid_format}
  end

  defp parse_formatted_date(year, month, day, multiplier) do
    year = multiplier * year

    if valid_date?(year, month, day) do
      {:ok, {year, month, day}}
    else
      {:error, :invalid_date}
    end
  end

  @doc """
  Parses a naive datetime `string` in the `:extended` format.

  For more information on supported strings, see how this
  module implements [ISO 8601](#module-iso-8601-compliance).

  ## Examples

      iex> Calendar.ISO.parse_naive_datetime("2015-01-23 23:50:07")
      {:ok, {2015, 1, 23, 23, 50, 7, {0, 0}}}
      iex> Calendar.ISO.parse_naive_datetime("2015-01-23 23:50:07Z")
      {:ok, {2015, 1, 23, 23, 50, 7, {0, 0}}}
      iex> Calendar.ISO.parse_naive_datetime("2015-01-23 23:50:07-02:30")
      {:ok, {2015, 1, 23, 23, 50, 7, {0, 0}}}

      iex> Calendar.ISO.parse_naive_datetime("2015-01-23 23:50:07.0")
      {:ok, {2015, 1, 23, 23, 50, 7, {0, 1}}}
      iex> Calendar.ISO.parse_naive_datetime("2015-01-23 23:50:07,0123456")
      {:ok, {2015, 1, 23, 23, 50, 7, {12345, 6}}}

  """
  @doc since: "1.10.0"
  @impl true
  def parse_naive_datetime(string) when is_binary(string),
    do: parse_naive_datetime(string, :extended)

  @doc """
  Parses a naive datetime `string` according to a given `format`.

  The `format` can either be `:basic` or `:extended`.

  For more information on supported strings, see how this
  module implements [ISO 8601](#module-iso-8601-compliance).

  ## Examples

      iex> Calendar.ISO.parse_naive_datetime("20150123 235007", :basic)
      {:ok, {2015, 1, 23, 23, 50, 7, {0, 0}}}
      iex> Calendar.ISO.parse_naive_datetime("20150123 235007", :extended)
      {:error, :invalid_format}

  """
  @doc since: "1.12.0"
  def parse_naive_datetime("-" <> string, format) when is_binary(string),
    do: do_parse_naive_datetime(string, -1, format)

  def parse_naive_datetime("+" <> string, format) when is_binary(string),
    do: do_parse_naive_datetime(string, 1, format)

  def parse_naive_datetime(string, format) when is_binary(string),
    do: do_parse_naive_datetime(string, 1, format)

  defp do_parse_naive_datetime(
         <<unquote(match_basic_date), datetime_sep, unquote(match_basic_time), rest::binary>>,
         multiplier,
         :basic
       )
       when unquote(guard_date) and datetime_sep in @datetime_seps and unquote(guard_time) do
    {year, month, day} = unquote(read_date)
    {hour, minute, second} = unquote(read_time)
    parse_formatted_naive_datetime(year, month, day, hour, minute, second, rest, multiplier)
  end

  defp do_parse_naive_datetime(
         <<unquote(match_ext_date), datetime_sep, unquote(match_ext_time), rest::binary>>,
         multiplier,
         :extended
       )
       when unquote(guard_date) and datetime_sep in @datetime_seps and unquote(guard_time) do
    {year, month, day} = unquote(read_date)
    {hour, minute, second} = unquote(read_time)
    parse_formatted_naive_datetime(year, month, day, hour, minute, second, rest, multiplier)
  end

  defp do_parse_naive_datetime(_, _, _) do
    {:error, :invalid_format}
  end

  defp parse_formatted_naive_datetime(year, month, day, hour, minute, second, rest, multiplier) do
    year = multiplier * year

    with {microsecond, rest} <- parse_microsecond(rest),
         {_offset, ""} <- parse_offset(rest) do
      cond do
        not valid_date?(year, month, day) ->
          {:error, :invalid_date}

        not valid_time?(hour, minute, second, microsecond) ->
          {:error, :invalid_time}

        true ->
          {:ok, {year, month, day, hour, minute, second, microsecond}}
      end
    else
      _ -> {:error, :invalid_format}
    end
  end

  @doc """
  Parses a UTC datetime `string` in the `:extended` format.

  For more information on supported strings, see how this
  module implements [ISO 8601](#module-iso-8601-compliance).

  ## Examples

      iex> Calendar.ISO.parse_utc_datetime("2015-01-23 23:50:07Z")
      {:ok, {2015, 1, 23, 23, 50, 7, {0, 0}}, 0}

      iex> Calendar.ISO.parse_utc_datetime("2015-01-23 23:50:07+02:30")
      {:ok, {2015, 1, 23, 21, 20, 7, {0, 0}}, 9000}

      iex> Calendar.ISO.parse_utc_datetime("2015-01-23 23:50:07")
      {:error, :missing_offset}

  """
  @doc since: "1.10.0"
  @impl true
  def parse_utc_datetime(string) when is_binary(string),
    do: parse_utc_datetime(string, :extended)

  @doc """
  Parses a UTC datetime `string` according to a given `format`.

  The `format` can either be `:basic` or `:extended`.

  For more information on supported strings, see how this
  module implements [ISO 8601](#module-iso-8601-compliance).

  ## Examples

      iex> Calendar.ISO.parse_utc_datetime("20150123 235007Z", :basic)
      {:ok, {2015, 1, 23, 23, 50, 7, {0, 0}}, 0}
      iex> Calendar.ISO.parse_utc_datetime("20150123 235007Z", :extended)
      {:error, :invalid_format}

  """
  @doc since: "1.12.0"
  def parse_utc_datetime("-" <> string, format) when is_binary(string),
    do: do_parse_utc_datetime(string, -1, format)

  def parse_utc_datetime("+" <> string, format) when is_binary(string),
    do: do_parse_utc_datetime(string, 1, format)

  def parse_utc_datetime(string, format) when is_binary(string),
    do: do_parse_utc_datetime(string, 1, format)

  defp do_parse_utc_datetime(
         <<unquote(match_basic_date), datetime_sep, unquote(match_basic_time), rest::binary>>,
         multiplier,
         :basic
       )
       when unquote(guard_date) and datetime_sep in @datetime_seps and unquote(guard_time) do
    {year, month, day} = unquote(read_date)
    {hour, minute, second} = unquote(read_time)
    parse_formatted_utc_datetime(year, month, day, hour, minute, second, rest, multiplier)
  end

  defp do_parse_utc_datetime(
         <<unquote(match_ext_date), datetime_sep, unquote(match_ext_time), rest::binary>>,
         multiplier,
         :extended
       )
       when unquote(guard_date) and datetime_sep in @datetime_seps and unquote(guard_time) do
    {year, month, day} = unquote(read_date)
    {hour, minute, second} = unquote(read_time)
    parse_formatted_utc_datetime(year, month, day, hour, minute, second, rest, multiplier)
  end

  defp do_parse_utc_datetime(_, _, _) do
    {:error, :invalid_format}
  end

  defp parse_formatted_utc_datetime(year, month, day, hour, minute, second, rest, multiplier) do
    year = multiplier * year

    with {microsecond, rest} <- parse_microsecond(rest),
         {offset, ""} <- parse_offset(rest) do
      cond do
        not valid_date?(year, month, day) ->
          {:error, :invalid_date}

        not valid_time?(hour, minute, second, microsecond) ->
          {:error, :invalid_time}

        offset == 0 ->
          {:ok, {year, month, day, hour, minute, second, microsecond}, offset}

        is_nil(offset) ->
          {:error, :missing_offset}

        true ->
          day_fraction = time_to_day_fraction(hour, minute, second, {0, 0})

          {{year, month, day}, {hour, minute, second, _}} =
            case add_day_fraction_to_iso_days({0, day_fraction}, -offset, 86400) do
              {0, day_fraction} ->
                {{year, month, day}, time_from_day_fraction(day_fraction)}

              {extra_days, day_fraction} ->
                base_days = date_to_iso_days(year, month, day)
                {date_from_iso_days(base_days + extra_days), time_from_day_fraction(day_fraction)}
            end

          {:ok, {year, month, day, hour, minute, second, microsecond}, offset}
      end
    else
      _ -> {:error, :invalid_format}
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
          {hour(), minute(), second(), microsecond()}
  def time_from_day_fraction({0, _}) do
    {0, 0, 0, {0, 6}}
  end

  def time_from_day_fraction({parts_in_day, parts_per_day}) do
    total_microseconds = divide_by_parts_per_day(parts_in_day, parts_per_day)

    {hours, rest_microseconds1} =
      div_rem(total_microseconds, @seconds_per_hour * @microseconds_per_second)

    {minutes, rest_microseconds2} =
      div_rem(rest_microseconds1, @seconds_per_minute * @microseconds_per_second)

    {seconds, microseconds} = div_rem(rest_microseconds2, @microseconds_per_second)
    {hours, minutes, seconds, {microseconds, 6}}
  end

  defp divide_by_parts_per_day(parts_in_day, @parts_per_day), do: parts_in_day

  defp divide_by_parts_per_day(parts_in_day, parts_per_day),
    do: div(parts_in_day * @parts_per_day, parts_per_day)

  # Converts year, month, day to count of days since 0000-01-01.
  @doc false
  def date_to_iso_days(0, 1, 1) do
    0
  end

  def date_to_iso_days(1970, 1, 1) do
    719_528
  end

  def date_to_iso_days(year, month, day) do
    ensure_day_in_month!(year, month, day)

    days_in_previous_years(year) + days_before_month(month) + leap_day_offset(year, month) + day -
      1
  end

  # Converts count of days since 0000-01-01 to {year, month, day} tuple.
  @doc false
  def date_from_iso_days(days) when days in -3_652_059..3_652_424 do
    {year, day_of_year} = days_to_year(days)
    extra_day = if leap_year?(year), do: 1, else: 0
    {month, day_in_month} = year_day_to_year_date(extra_day, day_of_year)
    {year, month, day_in_month + 1}
  end

  defp div_rem(int1, int2) do
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
  @doc since: "1.4.0"
  @spec days_in_month(year, month) :: 28..31
  @impl true
  def days_in_month(year, month) when is_year(year) and is_month(month) do
    days_in_month_guarded(year, month)
  end

  defp days_in_month_guarded(year, 2) do
    if leap_year?(year), do: 29, else: 28
  end

  defp days_in_month_guarded(_, month) when month in [4, 6, 9, 11], do: 30
  defp days_in_month_guarded(_, _), do: 31

  @doc """
  Returns how many months there are in the given year.

  ## Example

      iex> Calendar.ISO.months_in_year(2004)
      12

  """
  @doc since: "1.7.0"
  @impl true
  @spec months_in_year(year) :: 12
  def months_in_year(year) when is_year(year) do
    12
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
  @doc since: "1.3.0"
  @spec leap_year?(year) :: boolean()
  @impl true
  def leap_year?(year) when is_year(year) do
    rem(year, 4) === 0 and (rem(year, 100) !== 0 or rem(year, 400) === 0)
  end

  # TODO: Deprecate me on v1.15
  @doc false
  def day_of_week(year, month, day) do
    day_of_week(year, month, day, :default) |> elem(0)
  end

  @doc """
  Calculates the day of the week from the given `year`, `month`, and `day`.

  It is an integer from 1 to 7, where 1 is the given `starting_on` weekday.
  For example, if `starting_on` is set to `:monday`, then 1 is Monday and
  7 is Sunday.

  `starting_on` can also be `:default`, which is equivalent to `:monday`.

  ## Examples

      iex> Calendar.ISO.day_of_week(2016, 10, 31, :monday)
      {1, 1, 7}
      iex> Calendar.ISO.day_of_week(2016, 11, 1, :monday)
      {2, 1, 7}
      iex> Calendar.ISO.day_of_week(2016, 11, 2, :monday)
      {3, 1, 7}
      iex> Calendar.ISO.day_of_week(2016, 11, 3, :monday)
      {4, 1, 7}
      iex> Calendar.ISO.day_of_week(2016, 11, 4, :monday)
      {5, 1, 7}
      iex> Calendar.ISO.day_of_week(2016, 11, 5, :monday)
      {6, 1, 7}
      iex> Calendar.ISO.day_of_week(2016, 11, 6, :monday)
      {7, 1, 7}
      iex> Calendar.ISO.day_of_week(-99, 1, 31, :monday)
      {4, 1, 7}

      iex> Calendar.ISO.day_of_week(2016, 10, 31, :sunday)
      {2, 1, 7}
      iex> Calendar.ISO.day_of_week(2016, 11, 1, :sunday)
      {3, 1, 7}
      iex> Calendar.ISO.day_of_week(2016, 11, 2, :sunday)
      {4, 1, 7}
      iex> Calendar.ISO.day_of_week(2016, 11, 3, :sunday)
      {5, 1, 7}
      iex> Calendar.ISO.day_of_week(2016, 11, 4, :sunday)
      {6, 1, 7}
      iex> Calendar.ISO.day_of_week(2016, 11, 5, :sunday)
      {7, 1, 7}
      iex> Calendar.ISO.day_of_week(2016, 11, 6, :sunday)
      {1, 1, 7}
      iex> Calendar.ISO.day_of_week(-99, 1, 31, :sunday)
      {5, 1, 7}

      iex> Calendar.ISO.day_of_week(2016, 10, 31, :saturday)
      {3, 1, 7}

  """
  @doc since: "1.11.0"
  @spec day_of_week(year, month, day, :default | weekday) :: {day_of_week(), 1, 7}
  @impl true
  def day_of_week(year, month, day, starting_on) do
    iso_days = date_to_iso_days(year, month, day)
    {iso_days_to_day_of_week(iso_days, starting_on), 1, 7}
  end

  @doc false
  def iso_days_to_day_of_week(iso_days, starting_on) do
    Integer.mod(iso_days + day_of_week_offset(starting_on), 7) + 1
  end

  defp day_of_week_offset(:default), do: 5
  defp day_of_week_offset(:wednesday), do: 3
  defp day_of_week_offset(:thursday), do: 2
  defp day_of_week_offset(:friday), do: 1
  defp day_of_week_offset(:saturday), do: 0
  defp day_of_week_offset(:sunday), do: 6
  defp day_of_week_offset(:monday), do: 5
  defp day_of_week_offset(:tuesday), do: 4

  @doc """
  Calculates the day of the year from the given `year`, `month`, and `day`.

  It is an integer from 1 to 366.

  ## Examples

      iex> Calendar.ISO.day_of_year(2016, 1, 31)
      31
      iex> Calendar.ISO.day_of_year(-99, 2, 1)
      32
      iex> Calendar.ISO.day_of_year(2018, 2, 28)
      59

  """
  @doc since: "1.8.0"
  @spec day_of_year(year, month, day) :: day_of_year()
  @impl true
  def day_of_year(year, month, day) do
    ensure_day_in_month!(year, month, day)
    days_before_month(month) + leap_day_offset(year, month) + day
  end

  @doc """
  Calculates the quarter of the year from the given `year`, `month`, and `day`.

  It is an integer from 1 to 4.

  ## Examples

      iex> Calendar.ISO.quarter_of_year(2016, 1, 31)
      1
      iex> Calendar.ISO.quarter_of_year(2016, 4, 3)
      2
      iex> Calendar.ISO.quarter_of_year(-99, 9, 31)
      3
      iex> Calendar.ISO.quarter_of_year(2018, 12, 28)
      4

  """
  @doc since: "1.8.0"
  @spec quarter_of_year(year, month, day) :: quarter_of_year()
  @impl true
  def quarter_of_year(year, month, day)
      when is_year(year) and is_month(month) and is_day(day) do
    div(month - 1, 3) + 1
  end

  @doc """
  Calculates the year and era from the given `year`.

  The ISO calendar has two eras: the "current era" (CE) which
  starts in year `1` and is defined as era `1`. And "before the current
  era" (BCE) for those years less than `1`, defined as era `0`.

  ## Examples

      iex> Calendar.ISO.year_of_era(1)
      {1, 1}
      iex> Calendar.ISO.year_of_era(2018)
      {2018, 1}
      iex> Calendar.ISO.year_of_era(0)
      {1, 0}
      iex> Calendar.ISO.year_of_era(-1)
      {2, 0}

  """
  @doc since: "1.8.0"
  @spec year_of_era(year) :: {1..10000, era}
  @impl true
  def year_of_era(year) when is_year_CE(year) do
    {year, 1}
  end

  def year_of_era(year) when is_year_BCE(year) do
    {abs(year) + 1, 0}
  end

  @doc """
  Calculates the day and era from the given `year`, `month`, and `day`.

  ## Examples

      iex> Calendar.ISO.day_of_era(0, 1, 1)
      {366, 0}
      iex> Calendar.ISO.day_of_era(1, 1, 1)
      {1, 1}
      iex> Calendar.ISO.day_of_era(0, 12, 31)
      {1, 0}
      iex> Calendar.ISO.day_of_era(0, 12, 30)
      {2, 0}
      iex> Calendar.ISO.day_of_era(-1, 12, 31)
      {367, 0}

  """
  @doc since: "1.8.0"
  @spec day_of_era(year, month, day) :: Calendar.day_of_era()
  @impl true
  def day_of_era(year, month, day) when is_year_CE(year) do
    day = date_to_iso_days(year, month, day) - @iso_epoch + 1
    {day, 1}
  end

  def day_of_era(year, month, day) when is_year_BCE(year) do
    day = abs(date_to_iso_days(year, month, day) - @iso_epoch)
    {day, 0}
  end

  @doc """
  Converts the given time into a string.

  By default, returns times formatted in the "extended" format,
  for human readability. It also supports the "basic" format
  by passing the `:basic` option.

  ## Examples

      iex> Calendar.ISO.time_to_string(2, 2, 2, {2, 6})
      "02:02:02.000002"
      iex> Calendar.ISO.time_to_string(2, 2, 2, {2, 2})
      "02:02:02.00"
      iex> Calendar.ISO.time_to_string(2, 2, 2, {2, 0})
      "02:02:02"

      iex> Calendar.ISO.time_to_string(2, 2, 2, {2, 6}, :basic)
      "020202.000002"
      iex> Calendar.ISO.time_to_string(2, 2, 2, {2, 6}, :extended)
      "02:02:02.000002"

  """
  @impl true
  @doc since: "1.5.0"
  @spec time_to_string(
          Calendar.hour(),
          Calendar.minute(),
          Calendar.second(),
          Calendar.microsecond(),
          :basic | :extended
        ) :: String.t()
  def time_to_string(
        hour,
        minute,
        second,
        {ms_value, ms_precision} = microsecond,
        format \\ :extended
      )
      when is_hour(hour) and is_minute(minute) and is_second(second) and
             is_microsecond(ms_value, ms_precision) and format in [:basic, :extended] do
    time_to_string_guarded(hour, minute, second, microsecond, format)
  end

  defp time_to_string_guarded(hour, minute, second, {_, 0}, format) do
    time_to_string_format(hour, minute, second, format)
  end

  defp time_to_string_guarded(hour, minute, second, {microsecond, precision}, format) do
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

  By default, returns dates formatted in the "extended" format,
  for human readability. It also supports the "basic" format
  by passing the `:basic` option.

  ## Examples

      iex> Calendar.ISO.date_to_string(2015, 2, 28)
      "2015-02-28"
      iex> Calendar.ISO.date_to_string(2017, 8, 1)
      "2017-08-01"
      iex> Calendar.ISO.date_to_string(-99, 1, 31)
      "-0099-01-31"

      iex> Calendar.ISO.date_to_string(2015, 2, 28, :basic)
      "20150228"
      iex> Calendar.ISO.date_to_string(-99, 1, 31, :basic)
      "-00990131"

  """
  @doc since: "1.4.0"
  @spec date_to_string(year, month, day, :basic | :extended) :: String.t()
  @impl true
  def date_to_string(year, month, day, format \\ :extended)
      when is_integer(year) and is_integer(month) and is_integer(day) and
             format in [:basic, :extended] do
    date_to_string_guarded(year, month, day, format)
  end

  defp date_to_string_guarded(year, month, day, :extended) do
    zero_pad(year, 4) <> "-" <> zero_pad(month, 2) <> "-" <> zero_pad(day, 2)
  end

  defp date_to_string_guarded(year, month, day, :basic) do
    zero_pad(year, 4) <> zero_pad(month, 2) <> zero_pad(day, 2)
  end

  @doc """
  Converts the datetime (without time zone) into a string.

  By default, returns datetimes formatted in the "extended" format,
  for human readability. It also supports the "basic" format
  by passing the `:basic` option.

  ## Examples

      iex> Calendar.ISO.naive_datetime_to_string(2015, 2, 28, 1, 2, 3, {4, 6})
      "2015-02-28 01:02:03.000004"
      iex> Calendar.ISO.naive_datetime_to_string(2017, 8, 1, 1, 2, 3, {4, 5})
      "2017-08-01 01:02:03.00000"

      iex> Calendar.ISO.naive_datetime_to_string(2015, 2, 28, 1, 2, 3, {4, 6}, :basic)
      "20150228 010203.000004"

  """
  @doc since: "1.4.0"
  @impl true
  @spec naive_datetime_to_string(
          year,
          month,
          day,
          Calendar.hour(),
          Calendar.minute(),
          Calendar.second(),
          Calendar.microsecond(),
          :basic | :extended
        ) :: String.t()
  def naive_datetime_to_string(
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
      " " <> time_to_string(hour, minute, second, microsecond, format)
  end

  @doc """
  Converts the datetime (with time zone) into a string.

  By default, returns datetimes formatted in the "extended" format,
  for human readability. It also supports the "basic" format
  by passing the `:basic` option.

  ## Examples

      iex> time_zone = "Etc/UTC"
      iex> Calendar.ISO.datetime_to_string(2017, 8, 1, 1, 2, 3, {4, 5}, time_zone, "UTC", 0, 0)
      "2017-08-01 01:02:03.00000Z"
      iex> Calendar.ISO.datetime_to_string(2017, 8, 1, 1, 2, 3, {4, 5}, time_zone, "UTC", 3600, 0)
      "2017-08-01 01:02:03.00000+01:00"
      iex> Calendar.ISO.datetime_to_string(2017, 8, 1, 1, 2, 3, {4, 5}, time_zone, "UTC", 3600, 3600)
      "2017-08-01 01:02:03.00000+02:00"

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

      iex> time_zone = "Europe/Berlin"
      iex> Calendar.ISO.datetime_to_string(2017, 8, 1, 1, 2, 3, {4, 5}, time_zone, "CET", 3600, 0, :basic)
      "20170801 010203.00000+0100 CET Europe/Berlin"

  """
  @doc since: "1.4.0"
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
          Calendar.std_offset(),
          :basic | :extended
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
        std_offset,
        format \\ :extended
      )
      when is_time_zone(time_zone) and is_zone_abbr(zone_abbr) and is_utc_offset(utc_offset) and
             is_std_offset(std_offset) do
    date_to_string(year, month, day, format) <>
      " " <>
      time_to_string(hour, minute, second, microsecond, format) <>
      offset_to_string(utc_offset, std_offset, time_zone, format) <>
      zone_to_string(utc_offset, std_offset, zone_abbr, time_zone)
  end

  @doc false
  def offset_to_string(0, 0, "Etc/UTC", _format), do: "Z"

  def offset_to_string(utc, std, _zone, format) do
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

  defp zone_to_string(_, _, _, "Etc/UTC"), do: ""
  defp zone_to_string(_, _, abbr, zone), do: " " <> abbr <> " " <> zone

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
  def valid_date?(year, month, day)
      when is_integer(year) and is_integer(month) and is_integer(day) do
    is_year(year) and is_month(month) and day in 1..days_in_month(year, month)
  end

  @doc """
  Determines if the date given is valid according to the proleptic Gregorian calendar.

  Leap seconds are not supported by the built-in Calendar.ISO.

  ## Examples

      iex> Calendar.ISO.valid_time?(10, 50, 25, {3006, 6})
      true
      iex> Calendar.ISO.valid_time?(23, 59, 60, {0, 0})
      false
      iex> Calendar.ISO.valid_time?(24, 0, 0, {0, 0})
      false

  """
  @doc since: "1.5.0"
  @impl true
  @spec valid_time?(Calendar.hour(), Calendar.minute(), Calendar.second(), Calendar.microsecond()) ::
          boolean
  def valid_time?(hour, minute, second, {ms_value, ms_precision} = _microsecond)
      when is_integer(hour) and is_integer(minute) and is_integer(second) and is_integer(ms_value) and
             is_integer(ms_value) do
    is_hour(hour) and is_minute(minute) and is_second(second) and
      is_microsecond(ms_value, ms_precision)
  end

  @doc """
  See `c:Calendar.day_rollover_relative_to_midnight_utc/0` for documentation.
  """
  @doc since: "1.5.0"
  @impl true
  @spec day_rollover_relative_to_midnight_utc() :: {0, 1}
  def day_rollover_relative_to_midnight_utc() do
    {0, 1}
  end

  defp sign(total) when total < 0, do: "-"
  defp sign(_), do: "+"

  defp zero_pad(val, count) when val >= 0 do
    num = Integer.to_string(val)
    :binary.copy("0", max(count - byte_size(num), 0)) <> num
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
    case System.convert_time_unit(1, :second, unit) do
      1 -> 0
      10 -> 1
      100 -> 2
      1_000 -> 3
      10_000 -> 4
      100_000 -> 5
      _ -> 6
    end
  end

  defp parse_microsecond("." <> rest) do
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

  defp parse_microsecond("," <> rest) do
    parse_microsecond("." <> rest)
  end

  defp parse_microsecond(rest) do
    {{0, 0}, rest}
  end

  defp parse_microsecond(<<head, tail::binary>>, precision, acc) when head in ?0..?9,
    do: parse_microsecond(tail, precision + 1, <<acc::binary, head>>)

  defp parse_microsecond(rest, precision, acc), do: {acc, precision, rest}

  defp parse_offset(""), do: {nil, ""}
  defp parse_offset("Z"), do: {0, ""}
  defp parse_offset("-00:00"), do: :error

  defp parse_offset(<<?+, hour::2-bytes, ?:, min::2-bytes, rest::binary>>),
    do: parse_offset(1, hour, min, rest)

  defp parse_offset(<<?-, hour::2-bytes, ?:, min::2-bytes, rest::binary>>),
    do: parse_offset(-1, hour, min, rest)

  defp parse_offset(<<?+, hour::2-bytes, min::2-bytes, rest::binary>>),
    do: parse_offset(1, hour, min, rest)

  defp parse_offset(<<?-, hour::2-bytes, min::2-bytes, rest::binary>>),
    do: parse_offset(-1, hour, min, rest)

  defp parse_offset(<<?+, hour::2-bytes, rest::binary>>), do: parse_offset(1, hour, "00", rest)
  defp parse_offset(<<?-, hour::2-bytes, rest::binary>>), do: parse_offset(-1, hour, "00", rest)
  defp parse_offset(_), do: :error

  defp parse_offset(sign, hour, min, rest) do
    with {hour, ""} when hour < 24 <- Integer.parse(hour),
         {min, ""} when min < 60 <- Integer.parse(min) do
      {(hour * 60 + min) * 60 * sign, rest}
    else
      _ -> :error
    end
  end

  @doc false
  def gregorian_seconds_to_iso_days(seconds, microsecond) do
    {days, rest_seconds} = div_rem(seconds, @seconds_per_day)
    microseconds_in_day = rest_seconds * @microseconds_per_second + microsecond
    day_fraction = {microseconds_in_day, @parts_per_day}
    {days, day_fraction}
  end

  @doc false
  def iso_days_to_unit({days, {parts, ppd}}, unit) do
    day_microseconds = days * @parts_per_day
    microseconds = divide_by_parts_per_day(parts, ppd)
    System.convert_time_unit(day_microseconds + microseconds, :microsecond, unit)
  end

  @doc false
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

  defp days_to_year(days) when days < 0 do
    year_estimate = -div(-days, @days_per_nonleap_year) - 1

    {year, days_before_year} =
      days_to_year(year_estimate, days, days_to_end_of_epoch(year_estimate))

    leap_year_pad = if leap_year?(year), do: 1, else: 0
    {year, leap_year_pad + @days_per_nonleap_year + days - days_before_year}
  end

  defp days_to_year(days) do
    year_estimate = div(days, @days_per_nonleap_year)

    {year, days_before_year} =
      days_to_year(year_estimate, days, days_in_previous_years(year_estimate))

    {year, days - days_before_year}
  end

  defp days_to_year(year, days1, days2) when year < 0 and days1 >= days2 do
    days_to_year(year + 1, days1, days_to_end_of_epoch(year + 1))
  end

  defp days_to_year(year, days1, days2) when year >= 0 and days1 < days2 do
    days_to_year(year - 1, days1, days_in_previous_years(year - 1))
  end

  defp days_to_year(year, _days1, days2) do
    {year, days2}
  end

  defp days_to_end_of_epoch(year) when year < 0 do
    previous_year = year + 1

    div(previous_year, 4) - div(previous_year, 100) + div(previous_year, 400) +
      previous_year * @days_per_nonleap_year
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
    {days, rest_seconds} = div_rem(seconds, @seconds_per_day)

    date = date_from_iso_days(days)
    time = seconds_to_time(rest_seconds)
    {date, time}
  end

  defp seconds_to_time(seconds) when seconds in 0..@last_second_of_the_day do
    {hour, rest_seconds} = div_rem(seconds, @seconds_per_hour)
    {minute, second} = div_rem(rest_seconds, @seconds_per_minute)

    {hour, minute, second}
  end

  defp ensure_day_in_month!(year, month, day) when is_integer(day) do
    if day < 1 or day > days_in_month(year, month) do
      raise ArgumentError, "invalid date: #{date_to_string(year, month, day)}"
    end
  end
end
