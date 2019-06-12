defmodule NaiveDateTime do
  @moduledoc """
  A NaiveDateTime struct (without a time zone) and functions.

  The NaiveDateTime struct contains the fields year, month, day, hour,
  minute, second, microsecond and calendar. New naive datetimes can be
  built with the `new/2` and `new/8` functions or using the
  `~N` (see `Kernel.sigil_N/2`) sigil:

      iex> ~N[2000-01-01 23:00:07]
      ~N[2000-01-01 23:00:07]

  The date and time fields in the struct can be accessed directly:

      iex> naive = ~N[2000-01-01 23:00:07]
      iex> naive.year
      2000
      iex> naive.second
      7

  We call them "naive" because this datetime representation does not
  have a time zone. This means the datetime may not actually exist in
  certain areas in the world even though it is valid.

  For example, when daylight saving changes are applied by a region,
  the clock typically moves forward or backward by one hour. This means
  certain datetimes never occur or may occur more than once. Since
  `NaiveDateTime` is not validated against a time zone, such errors
  would go unnoticed.

  Developers should avoid creating the NaiveDateTime structs directly
  and instead, rely on the functions provided by this module as well
  as the ones in third-party calendar libraries.

  ## Comparing naive date times

  Comparisons in Elixir using `==/2`, `>/2`, `</2` and similar are structural
  and based on the `NaiveDateTime` struct fields. For proper comparison
  between naive datetimes, use the `compare/2` function.

  ## Using epochs

  The `add/3` and `diff/3` functions can be used for computing with
  date times or retrieving the number of seconds between instants.
  For example, if there is an interest in computing the number of
  seconds from the Unix epoch (1970-01-01 00:00:00):

      iex> NaiveDateTime.diff(~N[2010-04-17 14:00:00], ~N[1970-01-01 00:00:00])
      1271512800

      iex> NaiveDateTime.add(~N[1970-01-01 00:00:00], 1_271_512_800)
      ~N[2010-04-17 14:00:00]

  Those functions are optimized to deal with common epochs, such
  as the Unix Epoch above or the Gregorian Epoch (0000-01-01 00:00:00).
  """

  @enforce_keys [:year, :month, :day, :hour, :minute, :second]
  defstruct [
    :year,
    :month,
    :day,
    :hour,
    :minute,
    :second,
    microsecond: {0, 0},
    calendar: Calendar.ISO
  ]

  @type t :: %__MODULE__{
          year: Calendar.year(),
          month: Calendar.month(),
          day: Calendar.day(),
          calendar: Calendar.calendar(),
          hour: Calendar.hour(),
          minute: Calendar.minute(),
          second: Calendar.second(),
          microsecond: Calendar.microsecond()
        }

  @doc """
  Returns the current naive datetime in UTC.

  Prefer using `DateTime.utc_now/0` when possible as, opposite
  to `NaiveDateTime`, it will keep the time zone information.

  ## Examples

      iex> naive_datetime = NaiveDateTime.utc_now()
      iex> naive_datetime.year >= 2016
      true

  """
  @doc since: "1.4.0"
  @spec utc_now(Calendar.calendar()) :: t
  def utc_now(calendar \\ Calendar.ISO)

  def utc_now(Calendar.ISO) do
    {:ok, {year, month, day}, {hour, minute, second}, microsecond} =
      Calendar.ISO.from_unix(:os.system_time(), :native)

    %NaiveDateTime{
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond,
      calendar: Calendar.ISO
    }
  end

  def utc_now(calendar) do
    calendar
    |> DateTime.utc_now()
    |> DateTime.to_naive()
  end

  @doc """
  Builds a new ISO naive datetime.

  Expects all values to be integers. Returns `{:ok, naive_datetime}`
  if each entry fits its appropriate range, returns `{:error, reason}`
  otherwise.

  ## Examples

      iex> NaiveDateTime.new(2000, 1, 1, 0, 0, 0)
      {:ok, ~N[2000-01-01 00:00:00]}
      iex> NaiveDateTime.new(2000, 13, 1, 0, 0, 0)
      {:error, :invalid_date}
      iex> NaiveDateTime.new(2000, 2, 29, 0, 0, 0)
      {:ok, ~N[2000-02-29 00:00:00]}
      iex> NaiveDateTime.new(2000, 2, 30, 0, 0, 0)
      {:error, :invalid_date}
      iex> NaiveDateTime.new(2001, 2, 29, 0, 0, 0)
      {:error, :invalid_date}

      iex> NaiveDateTime.new(2000, 1, 1, 23, 59, 59, {0, 1})
      {:ok, ~N[2000-01-01 23:59:59.0]}
      iex> NaiveDateTime.new(2000, 1, 1, 23, 59, 59, 999_999)
      {:ok, ~N[2000-01-01 23:59:59.999999]}
      iex> NaiveDateTime.new(2000, 1, 1, 24, 59, 59, 999_999)
      {:error, :invalid_time}
      iex> NaiveDateTime.new(2000, 1, 1, 23, 60, 59, 999_999)
      {:error, :invalid_time}
      iex> NaiveDateTime.new(2000, 1, 1, 23, 59, 60, 999_999)
      {:error, :invalid_time}
      iex> NaiveDateTime.new(2000, 1, 1, 23, 59, 59, 1_000_000)
      {:error, :invalid_time}

      iex> NaiveDateTime.new(2000, 1, 1, 23, 59, 59, {0, 1}, Calendar.ISO)
      {:ok, ~N[2000-01-01 23:59:59.0]}

  """
  @spec new(
          Calendar.year(),
          Calendar.month(),
          Calendar.day(),
          Calendar.hour(),
          Calendar.minute(),
          Calendar.second(),
          Calendar.microsecond(),
          Calendar.calendar()
        ) :: {:ok, t} | {:error, atom}
  def new(year, month, day, hour, minute, second, microsecond \\ {0, 0}, calendar \\ Calendar.ISO)

  def new(year, month, day, hour, minute, second, microsecond, calendar)
      when is_integer(microsecond) do
    new(year, month, day, hour, minute, second, {microsecond, 6}, calendar)
  end

  def new(year, month, day, hour, minute, second, microsecond, calendar) do
    cond do
      not calendar.valid_date?(year, month, day) ->
        {:error, :invalid_date}

      not calendar.valid_time?(hour, minute, second, microsecond) ->
        {:error, :invalid_time}

      true ->
        naive_datetime = %NaiveDateTime{
          calendar: calendar,
          year: year,
          month: month,
          day: day,
          hour: hour,
          minute: minute,
          second: second,
          microsecond: microsecond
        }

        {:ok, naive_datetime}
    end
  end

  @doc """
  Builds a naive datetime from date and time structs.

  ## Examples

      iex> NaiveDateTime.new(~D[2010-01-13], ~T[23:00:07.005])
      {:ok, ~N[2010-01-13 23:00:07.005]}

  """
  @spec new(Date.t(), Time.t()) :: {:ok, t}
  def new(date, time)

  def new(%Date{calendar: calendar} = date, %Time{calendar: calendar} = time) do
    %{year: year, month: month, day: day} = date
    %{hour: hour, minute: minute, second: second, microsecond: microsecond} = time

    naive_datetime = %NaiveDateTime{
      calendar: calendar,
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond
    }

    {:ok, naive_datetime}
  end

  @doc """
  Adds a specified amount of time to a `NaiveDateTime`.

  Accepts an `amount_to_add` in any `unit` available from `t:System.time_unit/0`.
  Negative values will move backwards in time.

  ## Examples

      # adds seconds by default
      iex> NaiveDateTime.add(~N[2014-10-02 00:29:10], 2)
      ~N[2014-10-02 00:29:12]

      # accepts negative offsets
      iex> NaiveDateTime.add(~N[2014-10-02 00:29:10], -2)
      ~N[2014-10-02 00:29:08]

      # can work with other units
      iex> NaiveDateTime.add(~N[2014-10-02 00:29:10], 2_000, :millisecond)
      ~N[2014-10-02 00:29:12]

      # keeps the same precision
      iex> NaiveDateTime.add(~N[2014-10-02 00:29:10.021], 21, :second)
      ~N[2014-10-02 00:29:31.021]

      # changes below the precision will not be visible
      iex> hidden = NaiveDateTime.add(~N[2014-10-02 00:29:10], 21, :millisecond)
      iex> hidden.microsecond # ~N[2014-10-02 00:29:10]
      {21000, 0}

      # from Gregorian seconds
      iex> NaiveDateTime.add(~N[0000-01-01 00:00:00], 63_579_428_950)
      ~N[2014-10-02 00:29:10]

  Passing a `DateTime` automatically converts it to `NaiveDateTime`,
  discarding the time zone information:

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CET",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: 3600, std_offset: 0, time_zone: "Europe/Warsaw"}
      iex> NaiveDateTime.add(dt, 21, :second)
      ~N[2000-02-29 23:00:28]

  """
  @doc since: "1.4.0"
  @spec add(Calendar.naive_datetime(), integer, System.time_unit()) :: t
  def add(
        %{microsecond: {_, precision}, calendar: calendar} = naive_datetime,
        amount_to_add,
        unit \\ :second
      )
      when is_integer(amount_to_add) do
    ppd = System.convert_time_unit(86400, :second, unit)

    naive_datetime
    |> to_iso_days()
    |> Calendar.ISO.add_day_fraction_to_iso_days(amount_to_add, ppd)
    |> from_iso_days(calendar, precision)
  end

  @doc """
  Subtracts `naive_datetime2` from `naive_datetime1`.

  The answer can be returned in any `unit` available from `t:System.time_unit/0`.

  This function returns the difference in seconds where seconds are measured
  according to `Calendar.ISO`.

  ## Examples

      iex> NaiveDateTime.diff(~N[2014-10-02 00:29:12], ~N[2014-10-02 00:29:10])
      2
      iex> NaiveDateTime.diff(~N[2014-10-02 00:29:12], ~N[2014-10-02 00:29:10], :microsecond)
      2_000_000
      iex> NaiveDateTime.diff(~N[2014-10-02 00:29:10.042], ~N[2014-10-02 00:29:10.021], :millisecond)
      21
      iex> NaiveDateTime.diff(~N[2014-10-02 00:29:10], ~N[2014-10-02 00:29:12])
      -2
      iex> NaiveDateTime.diff(~N[-0001-10-02 00:29:10], ~N[-0001-10-02 00:29:12])
      -2

      # to Gregorian seconds
      iex> NaiveDateTime.diff(~N[2014-10-02 00:29:10], ~N[0000-01-01 00:00:00])
      63579428950

  """
  @doc since: "1.4.0"
  @spec diff(Calendar.naive_datetime(), Calendar.naive_datetime(), System.time_unit()) :: integer
  def diff(
        %{calendar: calendar1} = naive_datetime1,
        %{calendar: calendar2} = naive_datetime2,
        unit \\ :second
      ) do
    if not Calendar.compatible_calendars?(calendar1, calendar2) do
      raise ArgumentError,
            "cannot calculate the difference between #{inspect(naive_datetime1)} and " <>
              "#{inspect(naive_datetime2)} because their calendars are not compatible " <>
              "and thus the result would be ambiguous"
    end

    units1 = naive_datetime1 |> to_iso_days() |> Calendar.ISO.iso_days_to_unit(unit)
    units2 = naive_datetime2 |> to_iso_days() |> Calendar.ISO.iso_days_to_unit(unit)
    units1 - units2
  end

  @doc """
  Returns the given naive datetime with the microsecond field truncated to the
  given precision (`:microsecond`, `:millisecond` or `:second`).

  The given naive datetime is returned unchanged if it already has lower precision
  than the given precision.

  ## Examples

      iex> NaiveDateTime.truncate(~N[2017-11-06 00:23:51.123456], :microsecond)
      ~N[2017-11-06 00:23:51.123456]

      iex> NaiveDateTime.truncate(~N[2017-11-06 00:23:51.123456], :millisecond)
      ~N[2017-11-06 00:23:51.123]

      iex> NaiveDateTime.truncate(~N[2017-11-06 00:23:51.123456], :second)
      ~N[2017-11-06 00:23:51]

  """
  @doc since: "1.6.0"
  @spec truncate(t(), :microsecond | :millisecond | :second) :: t()
  def truncate(%NaiveDateTime{microsecond: microsecond} = naive_datetime, precision) do
    %{naive_datetime | microsecond: Calendar.truncate(microsecond, precision)}
  end

  def truncate(
        %{
          calendar: calendar,
          year: year,
          month: month,
          day: day,
          hour: hour,
          minute: minute,
          second: second,
          microsecond: microsecond
        },
        precision
      ) do
    %NaiveDateTime{
      calendar: calendar,
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: Calendar.truncate(microsecond, precision)
    }
  end

  @doc """
  Converts a `NaiveDateTime` into a `Date`.

  Because `Date` does not hold time information,
  data will be lost during the conversion.

  ## Examples

      iex> NaiveDateTime.to_date(~N[2002-01-13 23:00:07])
      ~D[2002-01-13]

  """
  @spec to_date(Calendar.naive_datetime()) :: Date.t()
  def to_date(%{
        year: year,
        month: month,
        day: day,
        calendar: calendar,
        hour: _,
        minute: _,
        second: _,
        microsecond: _
      }) do
    %Date{year: year, month: month, day: day, calendar: calendar}
  end

  @doc """
  Converts a `NaiveDateTime` into `Time`.

  Because `Time` does not hold date information,
  data will be lost during the conversion.

  ## Examples

      iex> NaiveDateTime.to_time(~N[2002-01-13 23:00:07])
      ~T[23:00:07]

  """
  @spec to_time(Calendar.naive_datetime()) :: Time.t()
  def to_time(%{
        year: _,
        month: _,
        day: _,
        calendar: calendar,
        hour: hour,
        minute: minute,
        second: second,
        microsecond: microsecond
      }) do
    %Time{
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond,
      calendar: calendar
    }
  end

  @doc """
  Converts the given naive datetime to a string according to its calendar.

  ### Examples

      iex> NaiveDateTime.to_string(~N[2000-02-28 23:00:13])
      "2000-02-28 23:00:13"
      iex> NaiveDateTime.to_string(~N[2000-02-28 23:00:13.001])
      "2000-02-28 23:00:13.001"
      iex> NaiveDateTime.to_string(~N[-0100-12-15 03:20:31])
      "-0100-12-15 03:20:31"

  This function can also be used to convert a DateTime to a string without
  the time zone information:

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CET",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: 3600, std_offset: 0, time_zone: "Europe/Warsaw"}
      iex> NaiveDateTime.to_string(dt)
      "2000-02-29 23:00:07"

  """
  @spec to_string(Calendar.naive_datetime()) :: String.t()
  def to_string(%{calendar: calendar} = naive_datetime) do
    %{
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond
    } = naive_datetime

    calendar.naive_datetime_to_string(year, month, day, hour, minute, second, microsecond)
  end

  @doc """
  Parses the extended "Date and time of day" format described by
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601).

  Time zone offset may be included in the string but they will be
  simply discarded as such information is not included in naive date
  times.

  As specified in the standard, the separator "T" may be omitted if
  desired as there is no ambiguity within this function.

  The year parsed by this function is limited to four digits and,
  while ISO 8601 allows datetimes to specify 24:00:00 as the zero
  hour of the next day, this notation is not supported by Elixir.
  Note leap seconds are not supported by the built-in Calendar.ISO.

  ## Examples

      iex> NaiveDateTime.from_iso8601("2015-01-23 23:50:07")
      {:ok, ~N[2015-01-23 23:50:07]}
      iex> NaiveDateTime.from_iso8601("2015-01-23T23:50:07")
      {:ok, ~N[2015-01-23 23:50:07]}
      iex> NaiveDateTime.from_iso8601("2015-01-23T23:50:07Z")
      {:ok, ~N[2015-01-23 23:50:07]}

      iex> NaiveDateTime.from_iso8601("2015-01-23 23:50:07.0")
      {:ok, ~N[2015-01-23 23:50:07.0]}
      iex> NaiveDateTime.from_iso8601("2015-01-23 23:50:07,0123456")
      {:ok, ~N[2015-01-23 23:50:07.012345]}
      iex> NaiveDateTime.from_iso8601("2015-01-23 23:50:07.0123456")
      {:ok, ~N[2015-01-23 23:50:07.012345]}
      iex> NaiveDateTime.from_iso8601("2015-01-23T23:50:07.123Z")
      {:ok, ~N[2015-01-23 23:50:07.123]}

      iex> NaiveDateTime.from_iso8601("2015-01-23P23:50:07")
      {:error, :invalid_format}
      iex> NaiveDateTime.from_iso8601("2015:01:23 23-50-07")
      {:error, :invalid_format}
      iex> NaiveDateTime.from_iso8601("2015-01-23 23:50:07A")
      {:error, :invalid_format}
      iex> NaiveDateTime.from_iso8601("2015-01-23 23:50:61")
      {:error, :invalid_time}
      iex> NaiveDateTime.from_iso8601("2015-01-32 23:50:07")
      {:error, :invalid_date}

      iex> NaiveDateTime.from_iso8601("2015-01-23T23:50:07.123+02:30")
      {:ok, ~N[2015-01-23 23:50:07.123]}
      iex> NaiveDateTime.from_iso8601("2015-01-23T23:50:07.123+00:00")
      {:ok, ~N[2015-01-23 23:50:07.123]}
      iex> NaiveDateTime.from_iso8601("2015-01-23T23:50:07.123-02:30")
      {:ok, ~N[2015-01-23 23:50:07.123]}
      iex> NaiveDateTime.from_iso8601("2015-01-23T23:50:07.123-00:00")
      {:error, :invalid_format}
      iex> NaiveDateTime.from_iso8601("2015-01-23T23:50:07.123-00:60")
      {:error, :invalid_format}
      iex> NaiveDateTime.from_iso8601("2015-01-23T23:50:07.123-24:00")
      {:error, :invalid_format}

  """
  @spec from_iso8601(String.t(), Calendar.calendar()) :: {:ok, t} | {:error, atom}
  def from_iso8601(string, calendar \\ Calendar.ISO)

  def from_iso8601(<<?-, rest::binary>>, calendar) do
    with {:ok, %{year: year} = naive_datetime} <- raw_from_iso8601(rest, calendar) do
      {:ok, %{naive_datetime | year: -year}}
    end
  end

  def from_iso8601(<<rest::binary>>, calendar) do
    raw_from_iso8601(rest, calendar)
  end

  @sep [?\s, ?T]
  [match_date, guard_date, read_date] = Calendar.ISO.__match_date__()
  [match_time, guard_time, read_time] = Calendar.ISO.__match_time__()

  defp raw_from_iso8601(string, calendar) do
    with <<unquote(match_date), sep, unquote(match_time), rest::binary>> <- string,
         true <- unquote(guard_date) and sep in @sep and unquote(guard_time),
         {microsec, rest} <- Calendar.ISO.parse_microsecond(rest),
         {_offset, ""} <- Calendar.ISO.parse_offset(rest) do
      {year, month, day} = unquote(read_date)
      {hour, min, sec} = unquote(read_time)

      with {:ok, iso_naive_dt} <- new(year, month, day, hour, min, sec, microsec, Calendar.ISO) do
        convert(iso_naive_dt, calendar)
      end
    else
      _ -> {:error, :invalid_format}
    end
  end

  @doc """
  Parses the extended "Date and time of day" format described by
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601).

  Raises if the format is invalid.

  ## Examples

      iex> NaiveDateTime.from_iso8601!("2015-01-23T23:50:07.123Z")
      ~N[2015-01-23 23:50:07.123]
      iex> NaiveDateTime.from_iso8601!("2015-01-23T23:50:07,123Z")
      ~N[2015-01-23 23:50:07.123]
      iex> NaiveDateTime.from_iso8601!("2015-01-23P23:50:07")
      ** (ArgumentError) cannot parse "2015-01-23P23:50:07" as naive datetime, reason: :invalid_format

  """
  @spec from_iso8601!(String.t(), Calendar.calendar()) :: t
  def from_iso8601!(string, calendar \\ Calendar.ISO) do
    case from_iso8601(string, calendar) do
      {:ok, value} ->
        value

      {:error, reason} ->
        raise ArgumentError,
              "cannot parse #{inspect(string)} as naive datetime, reason: #{inspect(reason)}"
    end
  end

  @doc """
  Converts the given naive datetime to
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601).

  By default, `NaiveDateTime.to_iso8601/2` returns naive datetimes formatted in the "extended"
  format, for human readability. It also supports the "basic" format through passing the `:basic` option.

  Only supports converting naive datetimes which are in the ISO calendar,
  attempting to convert naive datetimes from other calendars will raise.

  ### Examples

      iex> NaiveDateTime.to_iso8601(~N[2000-02-28 23:00:13])
      "2000-02-28T23:00:13"

      iex> NaiveDateTime.to_iso8601(~N[2000-02-28 23:00:13.001])
      "2000-02-28T23:00:13.001"

      iex> NaiveDateTime.to_iso8601(~N[2000-02-28 23:00:13.001], :basic)
      "20000228T230013.001"

  This function can also be used to convert a DateTime to ISO 8601 without
  the time zone information:

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CET",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: 3600, std_offset: 0, time_zone: "Europe/Warsaw"}
      iex> NaiveDateTime.to_iso8601(dt)
      "2000-02-29T23:00:07"

  """
  @spec to_iso8601(Calendar.naive_datetime(), :basic | :extended) :: String.t()
  def to_iso8601(naive_datetime, format \\ :extended)

  def to_iso8601(%{calendar: Calendar.ISO} = naive_datetime, format)
      when format in [:basic, :extended] do
    %{
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond
    } = naive_datetime

    Calendar.ISO.naive_datetime_to_iso8601(
      year,
      month,
      day,
      hour,
      minute,
      second,
      microsecond,
      format
    )
  end

  def to_iso8601(%{calendar: _} = naive_datetime, format) when format in [:basic, :extended] do
    naive_datetime
    |> convert!(Calendar.ISO)
    |> to_iso8601(format)
  end

  @doc """
  Converts a `NaiveDateTime` struct to an Erlang datetime tuple.

  Only supports converting naive datetimes which are in the ISO calendar,
  attempting to convert naive datetimes from other calendars will raise.

  WARNING: Loss of precision may occur, as Erlang time tuples only store
  hour/minute/second.

  ## Examples

      iex> NaiveDateTime.to_erl(~N[2000-01-01 13:30:15])
      {{2000, 1, 1}, {13, 30, 15}}

  This function can also be used to convert a DateTime to a erl format
  without the time zone information:

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CET",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: 3600, std_offset: 0, time_zone: "Europe/Warsaw"}
      iex> NaiveDateTime.to_erl(dt)
      {{2000, 2, 29}, {23, 00, 07}}

  """
  @spec to_erl(Calendar.naive_datetime()) :: :calendar.datetime()
  def to_erl(%{calendar: _} = naive_datetime) do
    %{year: year, month: month, day: day, hour: hour, minute: minute, second: second} =
      convert!(naive_datetime, Calendar.ISO)

    {{year, month, day}, {hour, minute, second}}
  end

  @doc """
  Converts an Erlang datetime tuple to a `NaiveDateTime` struct.

  Attempting to convert an invalid ISO calendar date will produce an error tuple.

  ## Examples

      iex> NaiveDateTime.from_erl({{2000, 1, 1}, {13, 30, 15}})
      {:ok, ~N[2000-01-01 13:30:15]}
      iex> NaiveDateTime.from_erl({{2000, 1, 1}, {13, 30, 15}}, {5000, 3})
      {:ok, ~N[2000-01-01 13:30:15.005]}
      iex> NaiveDateTime.from_erl({{2000, 13, 1}, {13, 30, 15}})
      {:error, :invalid_date}
      iex> NaiveDateTime.from_erl({{2000, 13, 1}, {13, 30, 15}})
      {:error, :invalid_date}

  """
  @spec from_erl(:calendar.datetime(), Calendar.microsecond(), Calendar.calendar()) ::
          {:ok, t} | {:error, atom}
  def from_erl(tuple, microsecond \\ {0, 0}, calendar \\ Calendar.ISO)

  def from_erl({{year, month, day}, {hour, minute, second}}, microsecond, calendar) do
    with {:ok, iso_naive_dt} <- new(year, month, day, hour, minute, second, microsecond),
         do: convert(iso_naive_dt, calendar)
  end

  @doc """
  Converts an Erlang datetime tuple to a `NaiveDateTime` struct.

  Raises if the datetime is invalid.
  Attempting to convert an invalid ISO calendar date will produce an error tuple.

  ## Examples

      iex> NaiveDateTime.from_erl!({{2000, 1, 1}, {13, 30, 15}})
      ~N[2000-01-01 13:30:15]
      iex> NaiveDateTime.from_erl!({{2000, 1, 1}, {13, 30, 15}}, {5000, 3})
      ~N[2000-01-01 13:30:15.005]
      iex> NaiveDateTime.from_erl!({{2000, 13, 1}, {13, 30, 15}})
      ** (ArgumentError) cannot convert {{2000, 13, 1}, {13, 30, 15}} to naive datetime, reason: :invalid_date

  """
  @spec from_erl!(:calendar.datetime(), Calendar.microsecond(), Calendar.calendar()) :: t
  def from_erl!(tuple, microsecond \\ {0, 0}, calendar \\ Calendar.ISO) do
    case from_erl(tuple, microsecond, calendar) do
      {:ok, value} ->
        value

      {:error, reason} ->
        raise ArgumentError,
              "cannot convert #{inspect(tuple)} to naive datetime, reason: #{inspect(reason)}"
    end
  end

  @doc """
  Compares two `NaiveDateTime` structs.

  Returns `:gt` if first is later than the second
  and `:lt` for vice versa. If the two NaiveDateTime
  are equal `:eq` is returned.

  ## Examples

      iex> NaiveDateTime.compare(~N[2016-04-16 13:30:15], ~N[2016-04-28 16:19:25])
      :lt
      iex> NaiveDateTime.compare(~N[2016-04-16 13:30:15.1], ~N[2016-04-16 13:30:15.01])
      :gt

  This function can also be used to compare a DateTime without
  the time zone information:

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CET",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: 3600, std_offset: 0, time_zone: "Europe/Warsaw"}
      iex> NaiveDateTime.compare(dt, ~N[2000-02-29 23:00:07])
      :eq
      iex> NaiveDateTime.compare(dt, ~N[2000-01-29 23:00:07])
      :gt
      iex> NaiveDateTime.compare(dt, ~N[2000-03-29 23:00:07])
      :lt

  """
  @doc since: "1.4.0"
  @spec compare(Calendar.naive_datetime(), Calendar.naive_datetime()) :: :lt | :eq | :gt
  def compare(%{calendar: calendar1} = naive_datetime1, %{calendar: calendar2} = naive_datetime2) do
    if Calendar.compatible_calendars?(calendar1, calendar2) do
      case {to_iso_days(naive_datetime1), to_iso_days(naive_datetime2)} do
        {first, second} when first > second -> :gt
        {first, second} when first < second -> :lt
        _ -> :eq
      end
    else
      raise ArgumentError, """
      cannot compare #{inspect(naive_datetime1)} with #{inspect(naive_datetime2)}.

      This comparison would be ambiguous as their calendars have incompatible day rollover moments.
      Specify an exact time of day (using `DateTime`s) to resolve this ambiguity
      """
    end
  end

  @doc """
  Converts the given `naive_datetime` from one calendar to another.

  If it is not possible to convert unambiguously between the calendars
  (see `Calendar.compatible_calendars?/2`), an `{:error, :incompatible_calendars}` tuple
  is returned.

  ## Examples

  Imagine someone implements `Calendar.Holocene`, a calendar based on the
  Gregorian calendar that adds exactly 10,000 years to the current Gregorian
  year:

      iex> NaiveDateTime.convert(~N[2000-01-01 13:30:15], Calendar.Holocene)
      {:ok, %NaiveDateTime{calendar: Calendar.Holocene, year: 12000, month: 1, day: 1,
                           hour: 13, minute: 30, second: 15, microsecond: {0, 0}}}

  """
  @doc since: "1.5.0"
  @spec convert(Calendar.naive_datetime(), Calendar.calendar()) ::
          {:ok, t} | {:error, :incompatible_calendars}

  # Keep it multiline for proper function clause errors.
  def convert(
        %{
          calendar: calendar,
          year: year,
          month: month,
          day: day,
          hour: hour,
          minute: minute,
          second: second,
          microsecond: microsecond
        },
        calendar
      ) do
    naive_datetime = %NaiveDateTime{
      calendar: calendar,
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond
    }

    {:ok, naive_datetime}
  end

  def convert(%{calendar: ndt_calendar, microsecond: {_, precision}} = naive_datetime, calendar) do
    if Calendar.compatible_calendars?(ndt_calendar, calendar) do
      result_naive_datetime =
        naive_datetime
        |> to_iso_days
        |> from_iso_days(calendar, precision)

      {:ok, result_naive_datetime}
    else
      {:error, :incompatible_calendars}
    end
  end

  @doc """
  Converts the given `naive_datetime` from one calendar to another.

  If it is not possible to convert unambiguously between the calendars
  (see `Calendar.compatible_calendars?/2`), an ArgumentError is raised.

  ## Examples

  Imagine someone implements `Calendar.Holocene`, a calendar based on the
  Gregorian calendar that adds exactly 10,000 years to the current Gregorian
  year:

      iex> NaiveDateTime.convert!(~N[2000-01-01 13:30:15], Calendar.Holocene)
      %NaiveDateTime{calendar: Calendar.Holocene, year: 12000, month: 1, day: 1,
                     hour: 13, minute: 30, second: 15, microsecond: {0, 0}}

  """
  @doc since: "1.5.0"
  @spec convert!(Calendar.naive_datetime(), Calendar.calendar()) :: t
  def convert!(naive_datetime, calendar) do
    case convert(naive_datetime, calendar) do
      {:ok, value} ->
        value

      {:error, :incompatible_calendars} ->
        raise ArgumentError,
              "cannot convert #{inspect(naive_datetime)} to target calendar #{inspect(calendar)}, " <>
                "reason: #{inspect(naive_datetime.calendar)} and #{inspect(calendar)} " <>
                "have different day rollover moments, making this conversion ambiguous"
    end
  end

  ## Helpers

  # Keep it multiline for proper function clause errors.
  defp to_iso_days(%{
         calendar: calendar,
         year: year,
         month: month,
         day: day,
         hour: hour,
         minute: minute,
         second: second,
         microsecond: microsecond
       }) do
    calendar.naive_datetime_to_iso_days(year, month, day, hour, minute, second, microsecond)
  end

  defp from_iso_days(iso_days, calendar, precision) do
    {year, month, day, hour, minute, second, {microsecond, _}} =
      calendar.naive_datetime_from_iso_days(iso_days)

    %NaiveDateTime{
      calendar: calendar,
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: {microsecond, precision}
    }
  end

  defimpl String.Chars do
    def to_string(naive_datetime) do
      %{
        calendar: calendar,
        year: year,
        month: month,
        day: day,
        hour: hour,
        minute: minute,
        second: second,
        microsecond: microsecond
      } = naive_datetime

      calendar.naive_datetime_to_string(year, month, day, hour, minute, second, microsecond)
    end
  end

  defimpl Inspect do
    def inspect(
          %{
            calendar: calendar,
            year: year,
            month: month,
            day: day,
            hour: hour,
            minute: minute,
            second: second,
            microsecond: microsecond
          },
          opts
        ) do
      calendar.inspect_naive_datetime(year, month, day, hour, minute, second, microsecond, opts)
    end
  end
end
