defmodule NaiveDateTime do
  @moduledoc """
  A NaiveDateTime struct (without a time zone) and functions.

  The NaiveDateTime struct contains the fields year, month, day, hour,
  minute, second, microsecond and calendar. New naive datetimes can be
  built with the `new/7` function or using the `~N` sigil:

      iex> ~N[2000-01-01 23:00:07]
      ~N[2000-01-01 23:00:07]

  Both `new/7` and sigil return a struct where the date fields can
  be accessed directly:

      iex> naive = ~N[2000-01-01 23:00:07]
      iex> naive.year
      2000
      iex> naive.second
      7

  The naive bit implies this datetime representation does
  not have a time zone. This means the datetime may not
  actually exist in certain areas in the world even though
  it is valid.

  For example, when daylight saving changes are applied
  by a region, the clock typically moves forward or backward
  by one hour. This means certain datetimes never occur or
  may occur more than once. Since `NaiveDateTime` is not
  validated against a time zone, such errors would go unnoticed.

  Remember, comparisons in Elixir using `==`, `>`, `<` and friends
  are structural and based on the NaiveDateTime struct fields. For
  proper comparison between naive datetimes, use the `compare/2`
  function.

  Developers should avoid creating the NaiveDateTime struct directly
  and instead rely on the functions provided by this module as well
  as the ones in 3rd party calendar libraries.
  """

  @enforce_keys [:year, :month, :day, :hour, :minute, :second]
  defstruct [:year, :month, :day, :hour, :minute, :second, microsecond: {0, 0}, calendar: Calendar.ISO]

  @type t :: %NaiveDateTime{year: Calendar.year, month: Calendar.month, day: Calendar.day,
                            calendar: Calendar.calendar, hour: Calendar.hour, minute: Calendar.minute,
                            second: Calendar.second, microsecond: Calendar.microsecond}

  @doc """
  Returns the current naive datetime in UTC.

  Prefer using `DateTime.utc_now/0` when possible as, opposite
  to `NaiveDateTime`, it will keep the time zone information.

  ## Examples

      iex> naive_datetime = NaiveDateTime.utc_now()
      iex> naive_datetime.year >= 2016
      true

  """
  @spec utc_now(Calendar.calendar) :: t
  def utc_now(calendar \\ Calendar.ISO)

  def utc_now(Calendar.ISO) do
    {:ok, {year, month, day}, {hour, minute, second}, microsecond} =
      Calendar.ISO.from_unix(:os.system_time, :native)
    %NaiveDateTime{year: year, month: month, day: day,
                   hour: hour, minute: minute, second: second,
                   microsecond: microsecond, calendar: Calendar.ISO}
  end

  def utc_now(calendar) do
    calendar
    |> DateTime.utc_now
    |> DateTime.to_naive
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
      iex> NaiveDateTime.new(2000, 1, 1, 23, 59, 60, 999_999)
      {:ok, ~N[2000-01-01 23:59:60.999999]}
      iex> NaiveDateTime.new(2000, 1, 1, 24, 59, 59, 999_999)
      {:error, :invalid_time}
      iex> NaiveDateTime.new(2000, 1, 1, 23, 60, 59, 999_999)
      {:error, :invalid_time}
      iex> NaiveDateTime.new(2000, 1, 1, 23, 59, 61, 999_999)
      {:error, :invalid_time}
      iex> NaiveDateTime.new(2000, 1, 1, 23, 59, 59, 1_000_000)
      {:error, :invalid_time}

  """
  @spec new(Calendar.year, Calendar.month, Calendar.day,
            Calendar.hour, Calendar.minute, Calendar.second, Calendar.microsecond, Calendar.calendar) ::
        {:ok, t} | {:error, atom}
  def new(year, month, day, hour, minute, second, microsecond \\ {0, 0}, calendar \\ Calendar.ISO) do
    with {:ok, date} <- Date.new(year, month, day, calendar),
         {:ok, time} <- Time.new(hour, minute, second, microsecond, calendar),
         do: new(date, time)
  end

  @doc """
  Builds a naive datetime from date and time structs.

  ## Examples

      iex> NaiveDateTime.new(~D[2010-01-13], ~T[23:00:07.005])
      {:ok, ~N[2010-01-13 23:00:07.005]}

  """
  @spec new(Date.t, Time.t) :: {:ok, t}
  def new(date, time)

  def new(%Date{calendar: calendar, year: year, month: month, day: day},
          %Time{hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: calendar}) do
    {:ok, %NaiveDateTime{calendar: calendar, year: year, month: month, day: day,
                         hour: hour, minute: minute, second: second, microsecond: microsecond}}
  end

  @doc """
  Adds a specified amount of time to a `NaiveDateTime`.

  Accepts an `integer` in any `unit` available from `t:System.time_unit/0`.
  Negative values will be move backwards in time.

  This operation is only possible if both calendars are convertible to `Calendar.ISO`.

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
      iex> hidden.microsecond  # ~N[2014-10-02 00:29:10]
      {21000, 0}

      # from Gregorian seconds
      iex> NaiveDateTime.add(~N[0000-01-01 00:00:00], 63579428950)
      ~N[2014-10-02 00:29:10]

  """
  @spec add(t, integer, System.time_unit) :: t
  def add(%NaiveDateTime{microsecond: {_microsecond, precision}} = naive_datetime,
          integer, unit \\ :second) when is_integer(integer) do
    ndt_microsecond = to_microsecond(naive_datetime)
    added_microsecond = System.convert_time_unit(integer, unit, :microsecond)
    sum = ndt_microsecond + added_microsecond

    microsecond = rem(sum, 1_000_000)
    {{year, month, day}, {hour, minute, second}} =
      sum |> div(1_000_000) |> :calendar.gregorian_seconds_to_datetime
    %NaiveDateTime{year: year, month: month, day: day,
                   hour: hour, minute: minute, second: second,
                   microsecond: {microsecond, precision}}
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

      # to Gregorian seconds
      iex> NaiveDateTime.diff(~N[2014-10-02 00:29:10], ~N[0000-01-01 00:00:00])
      63579428950

  """
  @spec diff(t, t, System.time_unit) :: integer
  def diff(%NaiveDateTime{} = naive_datetime1,
           %NaiveDateTime{} = naive_datetime2,
           unit \\ :second) do
    if not Calendar.compatible_calendars?(naive_datetime1.calendar, naive_datetime2.calendar) do
      raise ArgumentError, "cannot calculate the difference between #{inspect naive_datetime1} and #{inspect naive_datetime2} because their calendars are not compatible and thus the result would be ambiguous"
    end

    units1 = naive_datetime1 |> to_rata_die() |> Calendar.ISO.rata_die_to_unit(unit)
    units2 = naive_datetime2 |> to_rata_die() |> Calendar.ISO.rata_die_to_unit(unit)
    units1 - units2
  end

  @doc """
  Converts a `NaiveDateTime` into a `Date`.

  Because `Date` does not hold time information,
  data will be lost during the conversion.

  ## Examples

      iex> NaiveDateTime.to_date(~N[2002-01-13 23:00:07])
      ~D[2002-01-13]

  """
  @spec to_date(t) :: Date.t
  def to_date(%NaiveDateTime{year: year, month: month, day: day, calendar: calendar}) do
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
  @spec to_time(t) :: Time.t
  def to_time(%NaiveDateTime{hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: calendar}) do
    %Time{hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: calendar}
  end

  @doc """
  Converts the given naive datetime to a string according to its calendar.

  ### Examples

      iex> NaiveDateTime.to_string(~N[2000-02-28 23:00:13])
      "2000-02-28 23:00:13"
      iex> NaiveDateTime.to_string(~N[2000-02-28 23:00:13.001])
      "2000-02-28 23:00:13.001"

  This function can also be used to convert a DateTime to a string without
  the time zone information:

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CET",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: 3600, std_offset: 0, time_zone: "Europe/Warsaw"}
      iex> NaiveDateTime.to_string(dt)
      "2000-02-29 23:00:07"

  """
  @spec to_string(Calendar.naive_datetime) :: String.t
  def to_string(naive_datetime)

  def to_string(%{calendar: calendar, year: year, month: month, day: day,
                  hour: hour, minute: minute, second: second, microsecond: microsecond}) do
    calendar.naive_datetime_to_string(year, month, day, hour, minute, second, microsecond)
  end

  @doc """
  Parses the extended "Date and time of day" format described by
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601).

  Timezone offset may be included in the string but they will be
  simply discarded as such information is not included in naive date
  times.

  As specified in the standard, the separator "T" may be omitted if
  desired as there is no ambiguity within this function.

  Time representations with reduced accuracy are not supported.

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
  @spec from_iso8601(String.t, Calendar.calendar) :: {:ok, t} | {:error, atom}
  def from_iso8601(string, calendar \\ Calendar.ISO)

  def from_iso8601(<<year::4-bytes, ?-, month::2-bytes, ?-, day::2-bytes, sep,
                     hour::2-bytes, ?:, min::2-bytes, ?:, sec::2-bytes, rest::binary>>, calendar) when sep in [?\s, ?T] do
    with {year, ""} <- Integer.parse(year),
         {month, ""} <- Integer.parse(month),
         {day, ""} <- Integer.parse(day),
         {hour, ""} <- Integer.parse(hour),
         {min, ""} <- Integer.parse(min),
         {sec, ""} <- Integer.parse(sec),
         {microsec, rest} <- Calendar.ISO.parse_microsecond(rest),
         {_offset, ""} <- Calendar.ISO.parse_offset(rest) do
      with {:ok, utc_date} <- new(year, month, day, hour, min, sec, microsec, Calendar.ISO),
           do: convert(utc_date, calendar)
    else
      _ -> {:error, :invalid_format}
    end
  end

  def from_iso8601(<<_::binary>>, _calendar) do
    {:error, :invalid_format}
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
  @spec from_iso8601!(String.t, Calendar.calendar) :: t | no_return
  def from_iso8601!(string, calendar \\ Calendar.ISO) do
    case from_iso8601(string, calendar) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot parse #{inspect string} as naive datetime, reason: #{inspect reason}"
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

  This function can also be used to convert a DateTime to ISO8601 without
  the time zone information:

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CET",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: 3600, std_offset: 0, time_zone: "Europe/Warsaw"}
      iex> NaiveDateTime.to_iso8601(dt)
      "2000-02-29T23:00:07"

  """
  @spec to_iso8601(Calendar.naive_datetime, :basic | :extended) :: String.t
  def to_iso8601(naive_datetime, format \\ :extended)

  def to_iso8601(%{year: year, month: month, day: day,
                   hour: hour, minute: minute, second: second, microsecond: microsecond, calendar:
    Calendar.ISO}, format) when format in [:basic, :extended] do
    Calendar.ISO.naive_datetime_to_iso8601(year, month, day, hour, minute, second, microsecond, format)
  end

  def to_iso8601(%{year: _, month: _, day: _,
                   hour: _, minute: _, second: _, microsecond: _, calendar: _} = naive_datetime, format) when format in [:basic, :extended] do
    naive_datetime
    |> convert!(Calendar.ISO)
    |> to_iso8601(format)
  end

  def to_iso8601(_date, format) do
    raise ArgumentError, "NaiveDateTime.to_iso8601/2 expects format to be :extended or :basic, got: #{inspect format}"
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
  @spec to_erl(t) :: :calendar.datetime
  def to_erl(naive_datetime)

  @spec to_erl(Calendar.time) :: :calendar.time
  def to_erl(%{calendar: _, year: _, month: _, day: _,
               hour: _, minute: _, second: _} = naive_datetime) do
    %{year: year, month: month, day: day,
      hour: hour, minute: minute, second: second} = convert!(naive_datetime, Calendar.ISO)
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
      iex> NaiveDateTime.from_erl({{2000, 13, 1},{13, 30, 15}})
      {:error, :invalid_date}
  """
  @spec from_erl(:calendar.datetime, Calendar.microsecond) :: {:ok, t} | {:error, atom}
  def from_erl(tuple, microsecond \\ {0, 0}, calendar \\ Calendar.ISO)

  def from_erl({{year, month, day}, {hour, minute, second}}, microsecond, calendar) do
    with {:ok, utc_date} <- new(year, month, day, hour, minute, second, microsecond),
         do: convert(utc_date, calendar)
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
  @spec from_erl!(:calendar.datetime, Calendar.microsecond) :: t | no_return
  def from_erl!(tuple, microsecond \\ {0, 0}) do
    case from_erl(tuple, microsecond) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot convert #{inspect tuple} to naive datetime, reason: #{inspect reason}"
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
  @spec compare(Calendar.naive_datetime, Calendar.naive_datetime) :: :lt | :eq | :gt
  def compare(%{calendar: calendar1} = naive_datetime1, %{calendar: calendar2} = naive_datetime2) do
    if Calendar.compatible_calendars?(calendar1, calendar2) do
      case {to_rata_die(naive_datetime1), to_rata_die(naive_datetime2)} do
        {first, second} when first > second -> :gt
        {first, second} when first < second -> :lt
        _ -> :eq
      end
    else
      raise ArgumentError, """
      cannot compare #{inspect naive_datetime1} with #{inspect naive_datetime2}.

      This comparison would be ambiguous as their calendars have incompatible day rollover moments.
      Specify an exact time of day (using `DateTime`s) to resolve this ambiguity
      """
    end
  end

  @doc """
  Converts a `NaiveDateTime` struct from one calendar to another.

  If it is not possible to convert unambiguously between the calendars
  (see `Calendar.compatible_calendars?/2`), an `{:error, :incompatible_calendars}` tuple
  is returned.
  """
  @spec convert(NaiveDateTime.t, Calendar.calendar) :: {:ok, NaiveDateTime.t} | {:error, :incompatible_calendars}
  def convert(%{calendar: calendar} = naive_datetime, calendar) do
    {:ok, naive_datetime}
  end

  def convert(%{calendar: ndt_calendar} = naive_datetime, calendar) do
    if Calendar.compatible_calendars?(ndt_calendar, calendar) do
      result_naive_datetime =
        naive_datetime
        |> to_rata_die
        |> from_rata_die(calendar)
      {:ok, result_naive_datetime}
    else
      {:error, :incompatible_calendars}
    end
  end

  @doc """
  Converts a NaiveDateTime from one calendar to another.

  If it is not possible to convert unambiguously between the calendars
  (see `Calendar.compatible_calendars?/2`), an ArgumentError is raised.
  """
  @spec convert!(NaiveDateTime.t, Calendar.calendar) :: NaiveDateTime.t
  def convert!(naive_datetime, calendar) do
    case convert(naive_datetime, calendar) do
      {:ok, value} ->
        value
      {:error, :incompatible_calendars} ->
        raise ArgumentError, "cannot convert #{inspect naive_datetime} to target calendar #{inspect calendar}, reason: #{inspect naive_datetime.calendar} and #{inspect calendar} have different day rollover moments, making this conversion ambiguous"
      {:error, reason} ->
        raise ArgumentError, "cannot convert #{inspect naive_datetime} to target calendar #{inspect calendar}, reason: #{inspect reason}"
    end
  end

  ## Helpers

  defp to_microsecond(%{calendar: _, year: _, month: _, day: _,hour: _,
                        minute: _, second: _, microsecond: {_, _}} = naive_datetime) do
    %{year: year, month: month, day: day, hour: hour, minute: minute, second: second, microsecond: {microsecond, _}} = convert!(naive_datetime, Calendar.ISO)
    second = :calendar.datetime_to_gregorian_seconds(
      {{year, month, day}, {hour, minute, second}}
    )
    second * 1_000_000 + microsecond
  end

  defp to_rata_die(%{calendar: calendar, year: year, month: month, day: day,
                     hour: hour, minute: minute, second: second, microsecond: {microsecond, _precision}}) do
    calendar.naive_datetime_to_rata_die(year, month, day, hour, minute, second, microsecond)
  end

  defp from_rata_die(rata_die, calendar) do
    {year, month, day, hour, minute, second, microsecond} = calendar.naive_datetime_from_rata_die(rata_die)
    %NaiveDateTime{year: year, month: month, day: day, hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: calendar}
  end

  defimpl String.Chars do
    def to_string(%{calendar: calendar, year: year, month: month, day: day,
                     hour: hour, minute: minute, second: second, microsecond: microsecond}) do
      calendar.naive_datetime_to_string(year, month, day, hour, minute, second, microsecond)
    end
  end

  defimpl Inspect do
    def inspect(%{calendar: Calendar.ISO, year: year, month: month, day: day,
                  hour: hour, minute: minute, second: second, microsecond: microsecond}, _) do
      formatted = Calendar.ISO.naive_datetime_to_string(year, month, day, hour, minute, second, microsecond)
      "~N[" <> formatted <> "]"
    end

    def inspect(naive, opts) do
      Inspect.Any.inspect(naive, opts)
    end
  end
end
