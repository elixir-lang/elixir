defmodule DateTime do
  @moduledoc """
  A datetime implementation with a time zone.

  This datetime can be seen as an ephemeral snapshot
  of a datetime at a given time zone. For such purposes,
  it also includes both UTC and Standard offsets, as
  well as the zone abbreviation field used exclusively
  for formatting purposes.

  Remember, comparisons in Elixir using `==/2`, `>/2`, `</2` and friends
  are structural and based on the DateTime struct fields. For proper
  comparison between datetimes, use the `compare/2` function.

  The functions on this module work with the `DateTime` struct as well
  as any struct that contains the same fields as the `DateTime` struct.
  Such functions expect `t:Calendar.datetime/0` in their typespecs
  (instead of `t:t/0`).

  Developers should avoid creating the `DateTime` struct directly
  and instead rely on the functions provided by this module as
  well as the ones in 3rd party calendar libraries.

  ## Where are my functions?

  You will notice this module only contains conversion
  functions as well as functions that work on UTC. This
  is because a proper `DateTime` implementation requires a
  time zone database which currently is not provided as part
  of Elixir.

  Such may be addressed in upcoming versions, meanwhile,
  use 3rd party packages to provide `DateTime` building and
  similar functionality with time zone backing.
  """

  @enforce_keys [:year, :month, :day, :hour, :minute, :second] ++
                  [:time_zone, :zone_abbr, :utc_offset, :std_offset]

  defstruct [
    :year,
    :month,
    :day,
    :hour,
    :minute,
    :second,
    :time_zone,
    :zone_abbr,
    :utc_offset,
    :std_offset,
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
          microsecond: Calendar.microsecond(),
          time_zone: Calendar.time_zone(),
          zone_abbr: Calendar.zone_abbr(),
          utc_offset: Calendar.utc_offset(),
          std_offset: Calendar.std_offset()
        }

  @unix_days :calendar.date_to_gregorian_days({1970, 1, 1})

  @doc """
  Returns the current datetime in UTC.

  ## Examples

      iex> datetime = DateTime.utc_now()
      iex> datetime.time_zone
      "Etc/UTC"

  """
  @spec utc_now(Calendar.calendar()) :: t
  def utc_now(calendar \\ Calendar.ISO) do
    System.os_time() |> from_unix!(:native, calendar)
  end

  @doc """
  Converts the given Unix time to `DateTime`.

  The integer can be given in different unit
  according to `System.convert_time_unit/3` and it will
  be converted to microseconds internally.

  Unix times are always in UTC and therefore the DateTime
  will be returned in UTC.

  ## Examples

      iex> {:ok, datetime} = DateTime.from_unix(1_464_096_368)
      iex> datetime
      #DateTime<2016-05-24 13:26:08Z>

      iex> {:ok, datetime} = DateTime.from_unix(1_432_560_368_868_569, :microsecond)
      iex> datetime
      #DateTime<2015-05-25 13:26:08.868569Z>

  The unit can also be an integer as in `t:System.time_unit/0`:

      iex> {:ok, datetime} = DateTime.from_unix(143_256_036_886_856, 1024)
      iex> datetime
      #DateTime<6403-03-17 07:05:22.320Z>

  Negative Unix times are supported, up to -62167219200 seconds,
  which is equivalent to "0000-01-01T00:00:00Z" or 0 Gregorian seconds.
  """
  @spec from_unix(integer, :native | System.time_unit(), Calendar.calendar()) ::
          {:ok, t} | {:error, atom}
  def from_unix(integer, unit \\ :second, calendar \\ Calendar.ISO) when is_integer(integer) do
    case Calendar.ISO.from_unix(integer, unit) do
      {:ok, {year, month, day}, {hour, minute, second}, microsecond} ->
        iso_datetime = %DateTime{
          year: year,
          month: month,
          day: day,
          hour: hour,
          minute: minute,
          second: second,
          microsecond: microsecond,
          std_offset: 0,
          utc_offset: 0,
          zone_abbr: "UTC",
          time_zone: "Etc/UTC"
        }

        convert(iso_datetime, calendar)

      {:error, _} = error ->
        error
    end
  end

  @doc """
  Converts the given Unix time to `DateTime`.

  The integer can be given in different unit
  according to `System.convert_time_unit/3` and it will
  be converted to microseconds internally.

  Unix times are always in UTC and therefore the DateTime
  will be returned in UTC.

  ## Examples

      # An easy way to get the Unix epoch is passing 0 to this function
      iex> DateTime.from_unix!(0)
      #DateTime<1970-01-01 00:00:00Z>

      iex> DateTime.from_unix!(1_464_096_368)
      #DateTime<2016-05-24 13:26:08Z>

      iex> DateTime.from_unix!(1_432_560_368_868_569, :microsecond)
      #DateTime<2015-05-25 13:26:08.868569Z>

  """
  @spec from_unix!(integer, :native | System.time_unit(), Calendar.calendar()) :: t
  def from_unix!(integer, unit \\ :second, calendar \\ Calendar.ISO) when is_atom(unit) do
    case from_unix(integer, unit, calendar) do
      {:ok, datetime} ->
        datetime

      {:error, :invalid_unix_time} ->
        raise ArgumentError, "invalid Unix time #{integer}"
    end
  end

  @doc """
  Converts the given `NaiveDateTime` to `DateTime`.

  It expects a time zone to put the NaiveDateTime in.
  Currently it only supports "Etc/UTC" as time zone.

  ## Examples

      iex> {:ok, datetime} = DateTime.from_naive(~N[2016-05-24 13:26:08.003], "Etc/UTC")
      iex> datetime
      #DateTime<2016-05-24 13:26:08.003Z>

  """
  @doc since: "1.4.0"
  @spec from_naive(NaiveDateTime.t(), Calendar.time_zone()) :: {:ok, t}
  def from_naive(naive_datetime, time_zone)

  def from_naive(%NaiveDateTime{} = naive_datetime, "Etc/UTC") do
    %{
      calendar: calendar,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond,
      year: year,
      month: month,
      day: day
    } = naive_datetime

    datetime = %DateTime{
      calendar: calendar,
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond,
      std_offset: 0,
      utc_offset: 0,
      zone_abbr: "UTC",
      time_zone: "Etc/UTC"
    }

    {:ok, datetime}
  end

  @doc """
  Converts the given `NaiveDateTime` to `DateTime`.

  It expects a time zone to put the NaiveDateTime in.
  Currently it only supports "Etc/UTC" as time zone.

  ## Examples

      iex> DateTime.from_naive!(~N[2016-05-24 13:26:08.003], "Etc/UTC")
      #DateTime<2016-05-24 13:26:08.003Z>

  """
  @doc since: "1.4.0"
  @spec from_naive!(NaiveDateTime.t(), Calendar.time_zone()) :: t
  def from_naive!(naive_datetime, time_zone) do
    case from_naive(naive_datetime, time_zone) do
      {:ok, datetime} ->
        datetime

      {:error, reason} ->
        raise ArgumentError,
              "cannot parse #{inspect(naive_datetime)} to datetime, reason: #{inspect(reason)}"
    end
  end

  @doc """
  Converts the given `datetime` to Unix time.

  The `datetime` is expected to be using the ISO calendar
  with a year greater than or equal to 0.

  It will return the integer with the given unit,
  according to `System.convert_time_unit/3`.

  ## Examples

      iex> 1_464_096_368 |> DateTime.from_unix!() |> DateTime.to_unix()
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
  @spec to_unix(Calendar.datetime(), System.time_unit()) :: integer
  def to_unix(datetime, unit \\ :second)

  def to_unix(%{utc_offset: utc_offset, std_offset: std_offset} = datetime, unit) do
    {days, fraction} = to_iso_days(datetime)
    unix_units = Calendar.ISO.iso_days_to_unit({days - @unix_days, fraction}, unit)
    offset_units = System.convert_time_unit(utc_offset + std_offset, :second, unit)
    unix_units - offset_units
  end

  @doc """
  Converts the given `datetime` into a `NaiveDateTime`.

  Because `NaiveDateTime` does not hold time zone information,
  any time zone related data will be lost during the conversion.

  ## Examples

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CET",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 1},
      ...>                utc_offset: 3600, std_offset: 0, time_zone: "Europe/Warsaw"}
      iex> DateTime.to_naive(dt)
      ~N[2000-02-29 23:00:07.0]

  """
  @spec to_naive(t) :: NaiveDateTime.t()
  def to_naive(%DateTime{} = datetime) do
    %DateTime{
      calendar: calendar,
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond
    } = datetime

    %NaiveDateTime{
      year: year,
      month: month,
      day: day,
      calendar: calendar,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond
    }
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
  @spec to_date(t) :: Date.t()
  def to_date(%DateTime{} = datetime) do
    %{year: year, month: month, day: day, calendar: calendar} = datetime
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
  @spec to_time(t) :: Time.t()
  def to_time(%DateTime{} = datetime) do
    %{hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: calendar} =
      datetime

    %Time{
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond,
      calendar: calendar
    }
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
  @spec to_iso8601(Calendar.datetime(), :extended | :basic) :: String.t()
  def to_iso8601(datetime, format \\ :extended)

  def to_iso8601(%{calendar: Calendar.ISO} = datetime, format)
      when format in [:extended, :basic] do
    %{
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond,
      time_zone: time_zone,
      zone_abbr: zone_abbr,
      utc_offset: utc_offset,
      std_offset: std_offset
    } = datetime

    Calendar.ISO.datetime_to_iso8601(
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
      format
    )
  end

  def to_iso8601(%{calendar: _} = datetime, format) when format in [:extended, :basic] do
    datetime
    |> convert!(Calendar.ISO)
    |> to_iso8601(format)
  end

  @doc """
  Parses the extended "Date and time of day" format described by
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601).

  Since ISO 8601 does not include the proper time zone, the given
  string will be converted to UTC and its offset in seconds will be
  returned as part of this function. Therefore offset information
  must be present in the string.

  As specified in the standard, the separator "T" may be omitted if
  desired as there is no ambiguity within this function.

  Time representations with reduced accuracy are not supported.

  Note that while ISO 8601 allows datetimes to specify 24:00:00 as the
  zero hour of the next day, this notation is not supported by Elixir.

  ## Examples

      iex> {:ok, datetime, 0} = DateTime.from_iso8601("2015-01-23T23:50:07Z")
      iex> datetime
      #DateTime<2015-01-23 23:50:07Z>

      iex> {:ok, datetime, 9000} = DateTime.from_iso8601("2015-01-23T23:50:07.123+02:30")
      iex> datetime
      #DateTime<2015-01-23 21:20:07.123Z>

      iex> {:ok, datetime, 9000} = DateTime.from_iso8601("2015-01-23T23:50:07,123+02:30")
      iex> datetime
      #DateTime<2015-01-23 21:20:07.123Z>

      iex> {:ok, datetime, 0} = DateTime.from_iso8601("-2015-01-23T23:50:07Z")
      iex> datetime
      #DateTime<-2015-01-23 23:50:07Z>

      iex> {:ok, datetime, 9000} = DateTime.from_iso8601("-2015-01-23T23:50:07,123+02:30")
      iex> datetime
      #DateTime<-2015-01-23 21:20:07.123Z>

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
  @doc since: "1.4.0"
  @spec from_iso8601(String.t(), Calendar.calendar()) ::
          {:ok, t, Calendar.utc_offset()} | {:error, atom}
  def from_iso8601(string, calendar \\ Calendar.ISO)

  def from_iso8601(<<?-, rest::binary>>, calendar) do
    raw_from_iso8601(rest, calendar, true)
  end

  def from_iso8601(<<rest::binary>>, calendar) do
    raw_from_iso8601(rest, calendar, false)
  end

  @sep [?\s, ?T]
  [match_date, guard_date, read_date] = Calendar.ISO.__match_date__()
  [match_time, guard_time, read_time] = Calendar.ISO.__match_time__()

  defp raw_from_iso8601(string, calendar, is_negative_datetime) do
    with <<unquote(match_date), sep, unquote(match_time), rest::binary>> <- string,
         true <- unquote(guard_date) and sep in @sep and unquote(guard_time),
         {microsecond, rest} <- Calendar.ISO.parse_microsecond(rest),
         {offset, ""} <- Calendar.ISO.parse_offset(rest) do
      {year, month, day} = unquote(read_date)
      {hour, minute, second} = unquote(read_time)
      year = if is_negative_datetime, do: -year, else: year

      cond do
        not calendar.valid_date?(year, month, day) ->
          {:error, :invalid_date}

        not calendar.valid_time?(hour, minute, second, microsecond) ->
          {:error, :invalid_time}

        offset == 0 ->
          datetime = %DateTime{
            calendar: calendar,
            year: year,
            month: month,
            day: day,
            hour: hour,
            minute: minute,
            second: second,
            microsecond: microsecond,
            std_offset: 0,
            utc_offset: 0,
            zone_abbr: "UTC",
            time_zone: "Etc/UTC"
          }

          {:ok, datetime, 0}

        is_nil(offset) ->
          {:error, :missing_offset}

        true ->
          day_fraction = Calendar.ISO.time_to_day_fraction(hour, minute, second, {0, 0})

          {{year, month, day}, {hour, minute, second, _}} =
            case apply_tz_offset({0, day_fraction}, offset) do
              {0, day_fraction} ->
                {{year, month, day}, Calendar.ISO.time_from_day_fraction(day_fraction)}

              {extra_days, day_fraction} ->
                base_days = Calendar.ISO.date_to_iso_days(year, month, day)

                {Calendar.ISO.date_from_iso_days(base_days + extra_days),
                 Calendar.ISO.time_from_day_fraction(day_fraction)}
            end

          datetime = %DateTime{
            calendar: calendar,
            year: year,
            month: month,
            day: day,
            hour: hour,
            minute: minute,
            second: second,
            microsecond: microsecond,
            std_offset: 0,
            utc_offset: 0,
            zone_abbr: "UTC",
            time_zone: "Etc/UTC"
          }

          {:ok, datetime, offset}
      end
    else
      _ -> {:error, :invalid_format}
    end
  end

  @doc """
  Converts the given `datetime` to a string according to its calendar.

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

      iex> dt = %DateTime{year: -100, month: 12, day: 19, zone_abbr: "CET",
      ...>                hour: 3, minute: 20, second: 31, microsecond: {0, 0},
      ...>                utc_offset: 3600, std_offset: 0, time_zone: "Europe/Stockholm"}
      iex> DateTime.to_string(dt)
      "-0100-12-19 03:20:31+01:00 CET Europe/Stockholm"

  """
  @spec to_string(Calendar.datetime()) :: String.t()
  def to_string(%{calendar: calendar} = datetime) do
    %{
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond,
      time_zone: time_zone,
      zone_abbr: zone_abbr,
      utc_offset: utc_offset,
      std_offset: std_offset
    } = datetime

    calendar.datetime_to_string(
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
    )
  end

  @doc """
  Compares two datetime structs.

  Returns `:gt` if the first datetime is later than the second
  and `:lt` for vice versa. If the two datetimes are equal
  `:eq` is returned.

  Note that both UTC and Standard offsets will be taken into
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
  @doc since: "1.4.0"
  @spec compare(Calendar.datetime(), Calendar.datetime()) :: :lt | :eq | :gt
  def compare(
        %{calendar: _, utc_offset: utc_offset1, std_offset: std_offset1} = datetime1,
        %{calendar: _, utc_offset: utc_offset2, std_offset: std_offset2} = datetime2
      ) do
    {days1, {parts1, ppd1}} =
      datetime1
      |> to_iso_days()
      |> apply_tz_offset(utc_offset1 + std_offset1)

    {days2, {parts2, ppd2}} =
      datetime2
      |> to_iso_days()
      |> apply_tz_offset(utc_offset2 + std_offset2)

    # Ensure fraction tuples have same denominator.
    first = {days1, parts1 * ppd2}
    second = {days2, parts2 * ppd1}

    cond do
      first > second -> :gt
      first < second -> :lt
      true -> :eq
    end
  end

  @doc """
  Subtracts `datetime2` from `datetime1`.

  The answer can be returned in any `unit` available from `t:System.time_unit/0`.

  Leap seconds are not taken into account.

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
      iex> DateTime.diff(dt2, dt1)
      -18000

  """
  @doc since: "1.5.0"
  @spec diff(Calendar.datetime(), Calendar.datetime(), System.time_unit()) :: integer()
  def diff(
        %{utc_offset: utc_offset1, std_offset: std_offset1} = datetime1,
        %{utc_offset: utc_offset2, std_offset: std_offset2} = datetime2,
        unit \\ :second
      ) do
    naive_diff =
      (datetime1 |> to_iso_days() |> Calendar.ISO.iso_days_to_unit(unit)) -
        (datetime2 |> to_iso_days() |> Calendar.ISO.iso_days_to_unit(unit))

    offset_diff = utc_offset2 + std_offset2 - (utc_offset1 + std_offset1)
    naive_diff + System.convert_time_unit(offset_diff, :second, unit)
  end

  @doc """
  Returns the given datetime with the microsecond field truncated to the given
  precision (`:microsecond`, `millisecond` or `:second`).

  ## Examples

      iex> dt1 = %DateTime{year: 2017, month: 11, day: 7, zone_abbr: "CET",
      ...>                 hour: 11, minute: 45, second: 18, microsecond: {123456, 6},
      ...>                 utc_offset: 3600, std_offset: 0, time_zone: "Europe/Paris"}
      iex> DateTime.truncate(dt1, :microsecond)
      #DateTime<2017-11-07 11:45:18.123456+01:00 CET Europe/Paris>

      iex> dt2 = %DateTime{year: 2017, month: 11, day: 7, zone_abbr: "CET",
      ...>                 hour: 11, minute: 45, second: 18, microsecond: {123456, 6},
      ...>                 utc_offset: 3600, std_offset: 0, time_zone: "Europe/Paris"}
      iex> DateTime.truncate(dt2, :millisecond)
      #DateTime<2017-11-07 11:45:18.123+01:00 CET Europe/Paris>

      iex> dt3 = %DateTime{year: 2017, month: 11, day: 7, zone_abbr: "CET",
      ...>                 hour: 11, minute: 45, second: 18, microsecond: {123456, 6},
      ...>                 utc_offset: 3600, std_offset: 0, time_zone: "Europe/Paris"}
      iex> DateTime.truncate(dt3, :second)
      #DateTime<2017-11-07 11:45:18+01:00 CET Europe/Paris>

  """
  @doc since: "1.6.0"
  @spec truncate(t(), :microsecond | :millisecond | :second) :: t()
  def truncate(%DateTime{microsecond: microsecond} = datetime, precision) do
    %{datetime | microsecond: Calendar.truncate(microsecond, precision)}
  end

  @doc """
  Converts a given `datetime` from one calendar to another.

  If it is not possible to convert unambiguously between the calendars
  (see `Calendar.compatible_calendars?/2`), an `{:error, :incompatible_calendars}` tuple
  is returned.

  ## Examples

  Imagine someone implements `Calendar.Holocene`, a calendar based on the
  Gregorian calendar that adds exactly 10,000 years to the current Gregorian
  year:

      iex> dt1 = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "AMT",
      ...>                 hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                 utc_offset: -14400, std_offset: 0, time_zone: "America/Manaus"}
      iex> DateTime.convert(dt1, Calendar.Holocene)
      {:ok, %DateTime{calendar: Calendar.Holocene, day: 29, hour: 23,
                      microsecond: {0, 0}, minute: 0, month: 2, second: 7, std_offset: 0,
                      time_zone: "America/Manaus", utc_offset: -14400, year: 12000,
                      zone_abbr: "AMT"}}

  """
  @doc since: "1.5.0"
  @spec convert(Calendar.datetime(), Calendar.calendar()) ::
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
          microsecond: microsecond,
          time_zone: time_zone,
          zone_abbr: zone_abbr,
          utc_offset: utc_offset,
          std_offset: std_offset
        },
        calendar
      ) do
    datetime = %DateTime{
      calendar: calendar,
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond,
      time_zone: time_zone,
      zone_abbr: zone_abbr,
      utc_offset: utc_offset,
      std_offset: std_offset
    }

    {:ok, datetime}
  end

  def convert(%{calendar: dt_calendar, microsecond: {_, precision}} = datetime, calendar) do
    if Calendar.compatible_calendars?(dt_calendar, calendar) do
      result_datetime =
        datetime
        |> to_iso_days
        |> from_iso_days(datetime, calendar, precision)

      {:ok, result_datetime}
    else
      {:error, :incompatible_calendars}
    end
  end

  @doc """
  Converts a given `datetime` from one calendar to another.

  If it is not possible to convert unambiguously between the calendars
  (see `Calendar.compatible_calendars?/2`), an ArgumentError is raised.

  ## Examples

  Imagine someone implements `Calendar.Holocene`, a calendar based on the
  Gregorian calendar that adds exactly 10,000 years to the current Gregorian
  year:

      iex> dt1 = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "AMT",
      ...>                 hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                 utc_offset: -14400, std_offset: 0, time_zone: "America/Manaus"}
      iex> DateTime.convert!(dt1, Calendar.Holocene)
      %DateTime{calendar: Calendar.Holocene, day: 29, hour: 23,
                microsecond: {0, 0}, minute: 0, month: 2, second: 7, std_offset: 0,
                time_zone: "America/Manaus", utc_offset: -14400, year: 12000,
                zone_abbr: "AMT"}

  """
  @doc since: "1.5.0"
  @spec convert!(Calendar.datetime(), Calendar.calendar()) :: t | no_return
  def convert!(datetime, calendar) do
    case convert(datetime, calendar) do
      {:ok, value} ->
        value

      {:error, :incompatible_calendars} ->
        raise ArgumentError,
              "cannot convert #{inspect(datetime)} to target calendar #{inspect(calendar)}, " <>
                "reason: #{inspect(datetime.calendar)} and #{inspect(calendar)} have different " <>
                "day rollover moments, making this conversion ambiguous"
    end
  end

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

  defp from_iso_days(iso_days, datetime, calendar, precision) do
    %{time_zone: time_zone, zone_abbr: zone_abbr, utc_offset: utc_offset, std_offset: std_offset} =
      datetime

    {year, month, day, hour, minute, second, {microsecond, _}} =
      calendar.naive_datetime_from_iso_days(iso_days)

    %DateTime{
      calendar: calendar,
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: {microsecond, precision},
      time_zone: time_zone,
      zone_abbr: zone_abbr,
      utc_offset: utc_offset,
      std_offset: std_offset
    }
  end

  defp apply_tz_offset(iso_days, offset) do
    Calendar.ISO.add_day_fraction_to_iso_days(iso_days, -offset, 86400)
  end

  defimpl String.Chars do
    def to_string(datetime) do
      %{
        calendar: calendar,
        year: year,
        month: month,
        day: day,
        hour: hour,
        minute: minute,
        second: second,
        microsecond: microsecond,
        time_zone: time_zone,
        zone_abbr: zone_abbr,
        utc_offset: utc_offset,
        std_offset: std_offset
      } = datetime

      calendar.datetime_to_string(
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
      )
    end
  end

  defimpl Inspect do
    def inspect(%{calendar: Calendar.ISO} = datetime, _) do
      %{
        year: year,
        month: month,
        day: day,
        hour: hour,
        minute: minute,
        second: second,
        microsecond: microsecond,
        time_zone: time_zone,
        zone_abbr: zone_abbr,
        utc_offset: utc_offset,
        std_offset: std_offset
      } = datetime

      "#DateTime<" <>
        Calendar.ISO.datetime_to_string(
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
        ) <> ">"
    end

    def inspect(datetime, opts) do
      Inspect.Any.inspect(datetime, opts)
    end
  end
end
