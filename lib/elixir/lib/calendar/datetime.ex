defmodule DateTime do
  @moduledoc """
  A datetime implementation with a time zone.

  This datetime can be seen as an ephemeral snapshot
  of a datetime at a given time zone. For such purposes,
  it also includes both UTC and Standard offsets, as
  well as the zone abbreviation field used exclusively
  for formatting purposes.

  Remember, comparisons in Elixir using `==`, `>`, `<` and friends
  are structural and based on the DateTime struct fields. For proper
  comparison between datetimes, use the `compare/2` function.

  Developers should avoid creating the DateTime struct directly
  and instead rely on the functions provided by this module as
  well as the ones in 3rd party calendar libraries.

  ## Where are my functions?

  You will notice this module only contains conversion
  functions as well as functions that work on UTC. This
  is because a proper DateTime implementation requires a
  TimeZone database which currently is not provided as part
  of Elixir.

  Such may be addressed in upcoming versions, meanwhile,
  use 3rd party packages to provide DateTime building and
  similar functionality with time zone backing.
  """

  @enforce_keys [:year, :month, :day, :hour, :minute, :second,
                 :time_zone, :zone_abbr, :utc_offset, :std_offset]
  defstruct [:year, :month, :day, :hour, :minute, :second, :time_zone,
             :zone_abbr, :utc_offset, :std_offset, microsecond: {0, 0}, calendar: Calendar.ISO]

  @type t :: %__MODULE__{year: Calendar.year, month: Calendar.month, day: Calendar.day,
                         calendar: Calendar.calendar, hour: Calendar.hour, minute: Calendar.minute,
                         second: Calendar.second, microsecond: Calendar.microsecond,
                         time_zone: Calendar.time_zone, zone_abbr: Calendar.zone_abbr,
                         utc_offset: Calendar.utc_offset, std_offset: Calendar.std_offset}

  @unix_days :calendar.date_to_gregorian_days({1970, 1, 1}) - 365

  @doc """
  Returns the current datetime in UTC.

  ## Examples

      iex> datetime = DateTime.utc_now()
      iex> datetime.time_zone
      "Etc/UTC"

  """
  @spec utc_now(Calendar.calendar) :: DateTime.t
  def utc_now(calendar \\ Calendar.ISO) do
    System.os_time |> from_unix!(:native, calendar)
  end

  @doc """
  Converts the given Unix time to DateTime.

  The integer can be given in different unit
  according to `System.convert_time_unit/3` and it will
  be converted to microseconds internally.

  Unix times are always in UTC and therefore the DateTime
  will be returned in UTC.

  ## Examples

      iex> DateTime.from_unix(1464096368)
      {:ok, %DateTime{calendar: Calendar.ISO, day: 24, hour: 13, microsecond: {0, 0}, minute: 26,
                      month: 5, second: 8, std_offset: 0, time_zone: "Etc/UTC", utc_offset: 0,
                      year: 2016, zone_abbr: "UTC"}}

      iex> DateTime.from_unix(1432560368868569, :microsecond)
      {:ok, %DateTime{calendar: Calendar.ISO, day: 25, hour: 13, microsecond: {868569, 6}, minute: 26,
                      month: 5, second: 8, std_offset: 0, time_zone: "Etc/UTC", utc_offset: 0,
                      year: 2015, zone_abbr: "UTC"}}

  The unit can also be an integer as in `t:System.time_unit/0`:

      iex> DateTime.from_unix(143256036886856, 1024)
      {:ok, %DateTime{calendar: Calendar.ISO, day: 17, hour: 7, microsecond: {320312, 3},
        minute: 5, month: 3, second: 22, std_offset: 0, time_zone: "Etc/UTC",
        utc_offset: 0, year: 6403, zone_abbr: "UTC"}}

  Negative Unix times are supported, up to -62167219200 seconds,
  which is equivalent to "0000-01-01T00:00:00Z" or 0 Gregorian seconds.
  """
  @spec from_unix(integer, :native | System.time_unit, Calendar.calendar) :: {:ok, DateTime.t} | {:error, atom}
  def from_unix(integer, unit \\ :second, calendar \\ Calendar.ISO) when is_integer(integer) do
    case Calendar.ISO.from_unix(integer, unit) do
      {:ok, {year, month, day}, {hour, minute, second}, microsecond} ->
        iso_datetime = %DateTime{year: year, month: month, day: day,
                                 hour: hour, minute: minute, second: second, microsecond: microsecond,
                                 std_offset: 0, utc_offset: 0, zone_abbr: "UTC", time_zone: "Etc/UTC"}
        convert(iso_datetime, calendar)
      {:error, _} = error ->
        error
    end
  end

  @doc """
  Converts the given Unix time to DateTime.

  The integer can be given in different unit
  according to `System.convert_time_unit/3` and it will
  be converted to microseconds internally.

  Unix times are always in UTC and therefore the DateTime
  will be returned in UTC.

  ## Examples

      iex> DateTime.from_unix!(1464096368)
      %DateTime{calendar: Calendar.ISO, day: 24, hour: 13, microsecond: {0, 0}, minute: 26,
                month: 5, second: 8, std_offset: 0, time_zone: "Etc/UTC", utc_offset: 0,
                year: 2016, zone_abbr: "UTC"}

      iex> DateTime.from_unix!(1432560368868569, :microsecond)
      %DateTime{calendar: Calendar.ISO, day: 25, hour: 13, microsecond: {868569, 6}, minute: 26,
                month: 5, second: 8, std_offset: 0, time_zone: "Etc/UTC", utc_offset: 0,
                year: 2015, zone_abbr: "UTC"}

  """
  @spec from_unix!(integer, :native | System.time_unit, Calendar.calendar) :: DateTime.t
  def from_unix!(integer, unit \\ :second, calendar \\ Calendar.ISO) when is_atom(unit) do
    case from_unix(integer, unit, calendar) do
      {:ok, datetime} ->
        datetime
      {:error, :invalid_unix_time} ->
        raise ArgumentError, "invalid Unix time #{integer}"
    end
  end

  @doc """
  Converts the given NaiveDateTime to DateTime.

  It expects a time zone to put the NaiveDateTime in.
  Currently it only supports "Etc/UTC" as time zone.

  ## Examples

      iex> DateTime.from_naive(~N[2016-05-24 13:26:08.003], "Etc/UTC")
      {:ok, %DateTime{calendar: Calendar.ISO, day: 24, hour: 13, microsecond: {3000, 3}, minute: 26,
                      month: 5, second: 8, std_offset: 0, time_zone: "Etc/UTC", utc_offset: 0,
                      year: 2016, zone_abbr: "UTC"}}
  """
  @spec from_naive(NaiveDateTime.t, Calendar.time_zone) :: {:ok, DateTime.t}
  def from_naive(naive_datetime, time_zone)

  def from_naive(%NaiveDateTime{calendar: calendar,
                                hour: hour, minute: minute, second: second, microsecond: microsecond,
                                year: year, month: month, day: day}, "Etc/UTC") do
    {:ok, %DateTime{calendar: calendar, year: year, month: month, day: day,
                    hour: hour, minute: minute, second: second, microsecond: microsecond,
                    std_offset: 0, utc_offset: 0, zone_abbr: "UTC", time_zone: "Etc/UTC"}}
  end

  @doc """
  Converts the given NaiveDateTime to DateTime.

  It expects a time zone to put the NaiveDateTime in.
  Currently it only supports "Etc/UTC" as time zone.

  ## Examples

      iex> DateTime.from_naive!(~N[2016-05-24 13:26:08.003], "Etc/UTC")
      %DateTime{calendar: Calendar.ISO, day: 24, hour: 13, microsecond: {3000, 3}, minute: 26,
                month: 5, second: 8, std_offset: 0, time_zone: "Etc/UTC", utc_offset: 0,
                year: 2016, zone_abbr: "UTC"}

  """
  @spec from_naive!(non_neg_integer, :native | System.time_unit) :: DateTime.t
  def from_naive!(naive_datetime, time_zone) do
    case from_naive(naive_datetime, time_zone) do
      {:ok, datetime} ->
        datetime
      {:error, reason} ->
        raise ArgumentError, "cannot parse #{inspect naive_datetime} to datetime, reason: #{inspect reason}"
    end
  end

  @doc """
  Converts the given DateTime to Unix time.

  The DateTime is expected to be using the ISO calendar
  with a year greater than or equal to 0.

  It will return the integer with the given unit,
  according to `System.convert_time_unit/3`.

  ## Examples

      iex> 1464096368 |> DateTime.from_unix!() |> DateTime.to_unix()
      1464096368

      iex> dt = %DateTime{calendar: Calendar.ISO, day: 20, hour: 18, microsecond: {273806, 6},
      ...>                minute: 58, month: 11, second: 19, time_zone: "America/Montevideo",
      ...>                utc_offset: -10800, std_offset: 3600, year: 2014, zone_abbr: "UYST"}
      iex> DateTime.to_unix(dt)
      1416517099

      iex> flamel = %DateTime{calendar: Calendar.ISO, day: 22, hour: 8, microsecond: {527771, 6},
      ...>                minute: 2, month: 3, second: 25, std_offset: 0, time_zone: "Etc/UTC",
      ...>                utc_offset: 0, year: 1418, zone_abbr: "UTC"}
      iex> DateTime.to_unix(flamel)
      -17412508655

  """
  @spec to_unix(DateTime.t, System.time_unit) :: non_neg_integer
  def to_unix(datetime, unit \\ :second)

  def to_unix(%DateTime{utc_offset: utc_offset, std_offset: std_offset} = datetime, unit) do
    {days, fraction} = to_rata_die(datetime)
    unix_units = Calendar.ISO.rata_die_to_unit({days - @unix_days, fraction}, unit)
    offset_units = System.convert_time_unit(utc_offset + std_offset, :second, unit)
    unix_units - offset_units
  end

  @doc """
  Converts a `DateTime` into a `NaiveDateTime`.

  Because `NaiveDateTime` does not hold time zone information,
  any time zone related data will be lost during the conversion.

  ## Examples

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CET",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 1},
      ...>                utc_offset: 3600, std_offset: 0, time_zone: "Europe/Warsaw"}
      iex> DateTime.to_naive(dt)
      ~N[2000-02-29 23:00:07.0]

  """
  def to_naive(%DateTime{year: year, month: month, day: day, calendar: calendar,
                         hour: hour, minute: minute, second: second, microsecond: microsecond}) do
    %NaiveDateTime{year: year, month: month, day: day, calendar: calendar,
                   hour: hour, minute: minute, second: second, microsecond: microsecond}
  end

  @doc """
  Converts a `DateTime` into a `Date`.

  Because `Date` does not hold time nor time zone information,
  data will be lost during the conversion.

  ## Examples

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CET",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: 3600, std_offset: 0, time_zone: "Europe/Warsaw"}
      iex> DateTime.to_date(dt)
      ~D[2000-02-29]

  """
  def to_date(%DateTime{year: year, month: month, day: day, calendar: calendar}) do
    %Date{year: year, month: month, day: day, calendar: calendar}
  end

  @doc """
  Converts a `DateTime` into `Time`.

  Because `Time` does not hold date nor time zone information,
  data will be lost during the conversion.

  ## Examples

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CET",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 1},
      ...>                utc_offset: 3600, std_offset: 0, time_zone: "Europe/Warsaw"}
      iex> DateTime.to_time(dt)
      ~T[23:00:07.0]

  """
  def to_time(%DateTime{hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: calendar}) do
    %Time{hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: calendar}
  end

  @doc """
  Converts the given datetime to
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601) format.

  By default, `DateTime.to_iso8601/2` returns datetimes formatted in the "extended"
  format, for human readability. It also supports the "basic" format through passing the `:basic` option.

  Only supports converting datetimes which are in the ISO calendar,
  attempting to convert datetimes from other calendars will raise.

  WARNING: the ISO 8601 datetime format does not contain the time zone nor
  its abbreviation, which means information is lost when converting to such
  format.

  ### Examples

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CET",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: 3600, std_offset: 0, time_zone: "Europe/Warsaw"}
      iex> DateTime.to_iso8601(dt)
      "2000-02-29T23:00:07+01:00"

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "UTC",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: 0, std_offset: 0, time_zone: "Etc/UTC"}
      iex> DateTime.to_iso8601(dt)
      "2000-02-29T23:00:07Z"

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "AMT",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: -14400, std_offset: 0, time_zone: "America/Manaus"}
      iex> DateTime.to_iso8601(dt, :extended)
      "2000-02-29T23:00:07-04:00"

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "AMT",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: -14400, std_offset: 0, time_zone: "America/Manaus"}
      iex> DateTime.to_iso8601(dt, :basic)
      "20000229T230007-0400"
  """
  @spec to_iso8601(Calendar.datetime, :extended | :basic ) :: String.t
  def to_iso8601(datetime, format \\ :extended)

  def to_iso8601(%{calendar: Calendar.ISO, year: year, month: month, day: day,
                  hour: hour, minute: minute, second: second, microsecond: microsecond,
                  time_zone: time_zone, zone_abbr: zone_abbr, utc_offset: utc_offset, std_offset: std_offset}, format) when format in [:extended, :basic] do
    Calendar.ISO.datetime_to_iso8601(year, month, day, hour, minute, second, microsecond,
                                     time_zone, zone_abbr, utc_offset, std_offset, format)
  end

  def to_iso8601(%{calendar: _, year: _, month: _, day: _,
                   hour: _, minute: _, second: _, microsecond: _,
                   time_zone: _, zone_abbr: _, utc_offset: _, std_offset: _} = datetime, format) when format in [:extended, :basic] do
    datetime
    |> convert!(Calendar.ISO)
    |> to_iso8601(format)
  end

  def to_iso8601(_, format) do
    raise ArgumentError, "DateTime.to_iso8601/2 expects format to be :extended or :basic, got: #{inspect format}"
  end

  @doc """
  Parses the extended "Date and time of day" format described by
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601).

  Since ISO8601 does not include the proper time zone, the given
  string will be converted to UTC and its offset in seconds will be
  returned as part of this function. Therefore offset information
  must be present in the string.

  As specified in the standard, the separator "T" may be omitted if
  desired as there is no ambiguity within this function.

  Time representations with reduced accuracy are not supported.

  ## Examples

      iex> DateTime.from_iso8601("2015-01-23T23:50:07Z")
      {:ok, %DateTime{calendar: Calendar.ISO, day: 23, hour: 23, microsecond: {0, 0}, minute: 50, month: 1, second: 7, std_offset: 0,
                      time_zone: "Etc/UTC", utc_offset: 0, year: 2015, zone_abbr: "UTC"}, 0}
      iex> DateTime.from_iso8601("2015-01-23T23:50:07.123+02:30")
      {:ok, %DateTime{calendar: Calendar.ISO, day: 23, hour: 21, microsecond: {123000, 3}, minute: 20, month: 1, second: 7, std_offset: 0,
                      time_zone: "Etc/UTC", utc_offset: 0, year: 2015, zone_abbr: "UTC"}, 9000}
      iex> DateTime.from_iso8601("2015-01-23T23:50:07,123+02:30")
      {:ok, %DateTime{calendar: Calendar.ISO, day: 23, hour: 21, microsecond: {123000, 3}, minute: 20, month: 1, second: 7, std_offset: 0,
                      time_zone: "Etc/UTC", utc_offset: 0, year: 2015, zone_abbr: "UTC"}, 9000}

      iex> DateTime.from_iso8601("2015-01-23P23:50:07")
      {:error, :invalid_format}
      iex> DateTime.from_iso8601("2015-01-23 23:50:07A")
      {:error, :invalid_format}
      iex> DateTime.from_iso8601("2015-01-23T23:50:07")
      {:error, :missing_offset}
      iex> DateTime.from_iso8601("2015-01-23 23:50:61")
      {:error, :invalid_time}
      iex> DateTime.from_iso8601("2015-01-32 23:50:07")
      {:error, :invalid_date}

      iex> DateTime.from_iso8601("2015-01-23T23:50:07.123-00:00")
      {:error, :invalid_format}
      iex> DateTime.from_iso8601("2015-01-23T23:50:07.123-00:60")
      {:error, :invalid_format}

  """
  @spec from_iso8601(String.t, Calendar.calendar) :: {:ok, t, Calendar.utc_offset} | {:error, atom}
  def from_iso8601(string, calendar \\ Calendar.ISO)

  def from_iso8601(<<year::4-bytes, ?-, month::2-bytes, ?-, day::2-bytes, sep,
                     hour::2-bytes, ?:, min::2-bytes, ?:, sec::2-bytes, rest::binary>>, calendar) when sep in [?\s, ?T] do
    with {year, ""} <- Integer.parse(year),
         {month, ""} <- Integer.parse(month),
         {day, ""} <- Integer.parse(day),
         {hour, ""} <- Integer.parse(hour),
         {minute, ""} <- Integer.parse(min),
         {second, ""} <- Integer.parse(sec),
         {microsecond, rest} <- Calendar.ISO.parse_microsecond(rest),
         {:ok, date} <- Date.new(year, month, day),
         {:ok, time} <- Time.new(hour, minute, second, microsecond),
         {:ok, offset} <- parse_offset(rest) do
      %{year: year, month: month, day: day} = date
      %{hour: hour, minute: minute, second: second, microsecond: microsecond} = time

      datetime =
        Calendar.ISO.naive_datetime_to_rata_die(year, month, day, hour, minute, second, microsecond)
        |> apply_tz_offset(offset)
        |> from_rata_die("Etc/UTC", "UTC", 0, 0, calendar)

      {:ok, %{datetime | microsecond: microsecond}, offset}
    else
      {:error, reason} -> {:error, reason}
      _ -> {:error, :invalid_format}
    end
  end

  def from_iso8601(_, _) do
    {:error, :invalid_format}
  end

  defp parse_offset(rest) do
    case Calendar.ISO.parse_offset(rest) do
      {offset, ""} when is_integer(offset) -> {:ok, offset}
      {nil, ""} -> {:error, :missing_offset}
      _ -> {:error, :invalid_format}
    end
  end

  @doc """
  Converts the given datetime to a string according to its calendar.

  ### Examples

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CET",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: 3600, std_offset: 0, time_zone: "Europe/Warsaw"}
      iex> DateTime.to_string(dt)
      "2000-02-29 23:00:07+01:00 CET Europe/Warsaw"

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "UTC",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: 0, std_offset: 0, time_zone: "Etc/UTC"}
      iex> DateTime.to_string(dt)
      "2000-02-29 23:00:07Z"

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "AMT",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: -14400, std_offset: 0, time_zone: "America/Manaus"}
      iex> DateTime.to_string(dt)
      "2000-02-29 23:00:07-04:00 AMT America/Manaus"

  """
  @spec to_string(Calendar.datetime) :: String.t
  def to_string(datetime)

  def to_string(%{calendar: calendar, year: year, month: month, day: day,
                  hour: hour, minute: minute, second: second, microsecond: microsecond,
                  time_zone: time_zone, zone_abbr: zone_abbr, utc_offset: utc_offset, std_offset: std_offset}) do
    calendar.datetime_to_string(year, month, day, hour, minute, second, microsecond,
                                time_zone, zone_abbr, utc_offset, std_offset)
  end

  defimpl String.Chars do
    def to_string(%{calendar: calendar, year: year, month: month, day: day,
                    hour: hour, minute: minute, second: second, microsecond: microsecond,
                    time_zone: time_zone, zone_abbr: zone_abbr, utc_offset: utc_offset, std_offset: std_offset}) do
      calendar.datetime_to_string(year, month, day, hour, minute, second, microsecond,
                                  time_zone, zone_abbr, utc_offset, std_offset)
    end
  end

  @doc """
  Compares two `DateTime` structs.

  Returns `:gt` if first datetime is later than the second
  and `:lt` for vice versa. If the two datetimes are equal
  `:eq` is returned.

  Note that both utc and stc offsets will be taken into
  account when comparison is done.

  ## Examples

      iex> dt1 = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "AMT",
      ...>                 hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                 utc_offset: -14400, std_offset: 0, time_zone: "America/Manaus"}
      iex> dt2 = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CET",
      ...>                 hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                 utc_offset: 3600, std_offset: 0, time_zone: "Europe/Warsaw"}
      iex> DateTime.compare(dt1, dt2)
      :gt

  """
  @spec compare(DateTime.t, DateTime.t) :: :lt | :eq | :gt
  def compare(%DateTime{utc_offset: utc_offset1, std_offset: std_offset1} = datetime1,
              %DateTime{utc_offset: utc_offset2, std_offset: std_offset2} = datetime2) do
    {days1, {parts1, ppd1}} =
      datetime1
      |> to_rata_die()
      |> apply_tz_offset(utc_offset1 + std_offset1)

    {days2, {parts2, ppd2}} =
      datetime2
      |> to_rata_die()
      |> apply_tz_offset(utc_offset2 + std_offset2)

    # Ensure fraction tuples have same denominator.
    rata_die1 = {days1, parts1 * ppd2}
    rata_die2 = {days2, parts2 * ppd1}

    case {rata_die1, rata_die2}  do
      {first, second} when first > second -> :gt
      {first, second} when first < second -> :lt
      _ -> :eq
    end
  end

  @doc """
  Subtracts `datetime2` from `datetime1`.

  The answer can be returned in any `unit` available from `t:System.time_unit/0`.

  This function returns the difference in seconds where seconds are measured
  according to `Calendar.ISO`.

  ## Examples

      iex> dt1 = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "AMT",
      ...>                 hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                 utc_offset: -14400, std_offset: 0, time_zone: "America/Manaus"}
      iex> dt2 = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CET",
      ...>                 hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                 utc_offset: 3600, std_offset: 0, time_zone: "Europe/Warsaw"}
      iex> DateTime.diff(dt1, dt2)
      18000

  """
  @spec diff(DateTime.t, DateTime.t) :: integer()
  def diff(%DateTime{utc_offset: utc_offset1, std_offset: std_offset1} = datetime1,
           %DateTime{utc_offset: utc_offset2, std_offset: std_offset2} = datetime2, unit \\ :seconds) do
    naive_diff =
      (datetime1 |> to_rata_die() |> Calendar.ISO.rata_die_to_unit(unit)) -
      (datetime2 |> to_rata_die() |> Calendar.ISO.rata_die_to_unit(unit))
    offset_diff =
      (utc_offset2 + std_offset2) - (utc_offset1 + std_offset1)
    naive_diff + System.convert_time_unit(offset_diff, :second, unit)
  end

  @doc """
  Converts a DateTime from one calendar to another.

  If this conversion fails for some reason, an `{:error, reason}` tuple is returned.

  ## Examples

      iex> dt1 = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "AMT",
      ...>                 hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                 utc_offset: -14400, std_offset: 0, time_zone: "America/Manaus"}
      iex> DateTime.convert(dt1, Calendar.ISO)
      {:ok, %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "AMT",
                      hour: 23, minute: 0, second: 7, microsecond: {0, 0},
                      utc_offset: -14400, std_offset: 0, time_zone: "America/Manaus"}}

  """
  @spec convert(DateTime.t, Calendar.calendar) :: {:ok, DateTime.t} | {:error, atom}
  def convert(%DateTime{calendar: calendar} = datetime, calendar) do
    {:ok, datetime}
  end

  def convert(%DateTime{} = datetime, calendar) do
    result_datetime =
      datetime
      |> to_rata_die
      |> from_rata_die(datetime, calendar)
    {:ok, result_datetime}
  end

  @doc """
  Converts a `DateTime` struct from one calendar to another.

  If this conversion fails for some reason, an `ArgumentError` is raised.

  ## Examples

      iex> dt1 = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "AMT",
      ...>                 hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                 utc_offset: -14400, std_offset: 0, time_zone: "America/Manaus"}
      iex> DateTime.convert!(dt1, Calendar.ISO)
      %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "AMT",
                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
                utc_offset: -14400, std_offset: 0, time_zone: "America/Manaus"}

  """
  @spec convert!(DateTime.t, Calendar.calendar) :: DateTime.t
  def convert!(datetime, calendar) do
    case convert(datetime, calendar) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot convert #{inspect datetime} to target calendar #{inspect calendar}, reason: #{inspect reason}"
    end
  end

  defp to_rata_die(%DateTime{calendar: calendar,year: year, month: month, day: day,
                             hour: hour, minute: minute, second: second, microsecond: microsecond}) do
    calendar.naive_datetime_to_rata_die(year, month, day, hour, minute, second, microsecond)
  end

  defp from_rata_die(rata_die, datetime, calendar) do
    %{time_zone: time_zone, zone_abbr: zone_abbr, utc_offset: utc_offset, std_offset: std_offset} = datetime
    from_rata_die(rata_die, time_zone, zone_abbr, utc_offset, std_offset, calendar)
  end

  defp from_rata_die(rata_die, time_zone, zone_abbr, utc_offset, std_offset, calendar) do
    {year, month, day, hour, minute, second, microsecond} = calendar.naive_datetime_from_rata_die(rata_die)
    %DateTime{year: year, month: month, day: day,
              hour: hour, minute: minute, second: second, microsecond: microsecond,
              time_zone: time_zone, zone_abbr: zone_abbr, utc_offset: utc_offset, std_offset: std_offset}
  end

  defp apply_tz_offset({days, {parts, ppd}}, offset) do
    # At this time, only offsets in seconds (of which there are 86400 in an ISO 8601 day) are allowed.
    offset_ppd = 86400

    parts = parts * offset_ppd
    offset = offset * ppd
    gcd = Integer.gcd(ppd, offset_ppd)
    result_parts = div(parts - offset, gcd)
    result_ppd = div(ppd * offset_ppd, gcd)
    days_offset = div(result_parts, result_ppd)
    final_parts = rem(result_parts, result_ppd)
    {days + days_offset, {final_parts, result_ppd}}
  end
end
