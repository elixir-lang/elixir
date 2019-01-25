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

  Developers should avoid creating the `DateTime` struct directly
  and instead rely on the functions provided by this module as
  well as the ones in third-party calendar libraries.

  ## Time zone database

  Many functions in this module require a time zone database.
  By default, it uses the default time zone database returned by
  `Calendar.get_time_zone_database/0`, which defaults to
  `Calendar.UTCOnlyTimeZoneDatabase` which only handles "Etc/UTC"
  datetimes and returns `{:error, :utc_only_time_zone_database}`
  for any other time zone.

  Other time zone databases (including ones provided by packages)
  can be configure as default either via configuration:

      config :elixir, :time_zone_database, CustomTimeZoneDatabase

  or by calling `Calendar.put_time_zone_database/1`.
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

  It expects a time zone to put the `NaiveDateTime` in.
  If the time zone is "Etc/UTC", it always succeeds. Otherwise,
  the NaiveDateTime is checked against the time zone database
  given as `time_zone_database`. See the "Time zone database"
  section in the module documentation.

  ## Examples

      iex> {:ok, datetime} = DateTime.from_naive(~N[2016-05-24 13:26:08.003], "Etc/UTC")
      iex> datetime
      #DateTime<2016-05-24 13:26:08.003Z>

  When the datetime is ambiguous - for instance during changing from summer
  to winter time - the two possible valid datetimes are returned. First the one
  that happens first, then the one that happens after.

      iex> {:ambiguous, first_dt, second_dt} = DateTime.from_naive(~N[2018-10-28 02:30:00], "Europe/Copenhagen", FakeTimeZoneDatabase)
      iex> first_dt
      #DateTime<2018-10-28 02:30:00+02:00 CEST Europe/Copenhagen>
      iex> second_dt
      #DateTime<2018-10-28 02:30:00+01:00 CET Europe/Copenhagen>

  When there is a gap in wall time - for instance in spring when the clocks are
  turned forward - the latest valid datetime just before the gap and the first
  valid datetime just after the gap.

      iex> {:gap, just_before, just_after} = DateTime.from_naive(~N[2019-03-31 02:30:00], "Europe/Copenhagen", FakeTimeZoneDatabase)
      iex> just_before
      #DateTime<2019-03-31 01:59:59.999999+01:00 CET Europe/Copenhagen>
      iex> just_after
      #DateTime<2019-03-31 03:00:00+02:00 CEST Europe/Copenhagen>

  Most of the time there is one, and just one, valid datetime for a certain
  date and time in a certain time zone.

      iex> {:ok, datetime} = DateTime.from_naive(~N[2018-07-28 12:30:00], "Europe/Copenhagen", FakeTimeZoneDatabase)
      iex> datetime
      #DateTime<2018-07-28 12:30:00+02:00 CEST Europe/Copenhagen>

  This function accepts any map or struct that contains at least the same fields as a `NaiveDateTime`
  struct. The most common example of that is a `DateTime`. In this case the information about the time
  zone of that `DateTime` is completely ignored. This is the same principle as passing a `DateTime` to
  `Date.to_iso8601/2`. `Date.to_iso8601/2` extracts only the date-specific fields (calendar, year,
  month and day) of the given structure and ignores all others.

  This way if you have a `DateTime` in one time zone, you can get the same wall time in another time zone.
  For instance if you have 2018-08-24 10:00:00 in Copenhagen and want a `DateTime` for 2018-08-24 10:00:00
  in UTC you can do:

      iex> cph_datetime = DateTime.from_naive!(~N[2018-08-24 10:00:00], "Europe/Copenhagen", FakeTimeZoneDatabase)
      iex> {:ok, utc_datetime} = DateTime.from_naive(cph_datetime, "Etc/UTC", FakeTimeZoneDatabase)
      iex> utc_datetime
      #DateTime<2018-08-24 10:00:00Z>

  If instead you want a `DateTime` for the same point time in a different time zone see the
  `DateTime.shift_zone/3` function which would convert 2018-08-24 10:00:00 in Copenhagen
  to 2018-08-24 08:00:00 in UTC.
  """
  @doc since: "1.4.0"
  @spec from_naive(
          Calendar.naive_datetime(),
          Calendar.time_zone(),
          Calendar.time_zone_database()
        ) ::
          {:ok, t}
          | {:ambiguous, t, t}
          | {:gap, t, t}
          | {:error,
             :incompatible_calendars | :time_zone_not_found | :utc_only_time_zone_database}

  def from_naive(
        naive_datetime,
        time_zone,
        time_zone_database \\ Calendar.get_time_zone_database()
      )

  def from_naive(naive_datetime, "Etc/UTC", _) do
    utc_period = %{std_offset: 0, utc_offset: 0, zone_abbr: "UTC"}
    {:ok, from_naive_with_period(naive_datetime, "Etc/UTC", utc_period)}
  end

  def from_naive(%{calendar: Calendar.ISO} = naive_datetime, time_zone, time_zone_database) do
    case time_zone_database.time_zone_periods_from_wall_datetime(naive_datetime, time_zone) do
      {:ok, period} ->
        {:ok, from_naive_with_period(naive_datetime, time_zone, period)}

      {:ambiguous, first_period, second_period} ->
        first_datetime = from_naive_with_period(naive_datetime, time_zone, first_period)
        second_datetime = from_naive_with_period(naive_datetime, time_zone, second_period)
        {:ambiguous, first_datetime, second_datetime}

      {:gap, {first_period, first_period_until_wall}, {second_period, second_period_from_wall}} ->
        # `until_wall` is not valid, but any time just before is.
        # So by subtracting a second and adding .999999 seconds
        # we get the last microsecond just before.
        before_naive =
          first_period_until_wall
          |> Map.put(:microsecond, {999_999, 6})
          |> NaiveDateTime.add(-1)

        after_naive = second_period_from_wall

        latest_datetime_before = from_naive_with_period(before_naive, time_zone, first_period)
        first_datetime_after = from_naive_with_period(after_naive, time_zone, second_period)
        {:gap, latest_datetime_before, first_datetime_after}

      {:error, _} = error ->
        error
    end
  end

  def from_naive(%{calendar: calendar} = naive_datetime, time_zone, time_zone_database)
      when calendar != Calendar.ISO do
    # For non-ISO calendars, convert to ISO, create ISO DateTime, and then
    # convert to original calendar
    iso_result =
      with {:ok, in_iso} <- NaiveDateTime.convert(naive_datetime, Calendar.ISO) do
        from_naive(in_iso, time_zone, time_zone_database)
      end

    case iso_result do
      {:ok, dt} ->
        convert(dt, calendar)

      {:ambiguous, dt1, dt2} ->
        with {:ok, dt1converted} <- convert(dt1, calendar),
             {:ok, dt2converted} <- convert(dt2, calendar),
             do: {:ambiguous, dt1converted, dt2converted}

      {:gap, dt1, dt2} ->
        with {:ok, dt1converted} <- convert(dt1, calendar),
             {:ok, dt2converted} <- convert(dt2, calendar),
             do: {:gap, dt1converted, dt2converted}

      {:error, _} = error ->
        error
    end
  end

  defp from_naive_with_period(naive_datetime, time_zone, period) do
    %{std_offset: std_offset, utc_offset: utc_offset, zone_abbr: zone_abbr} = period

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

    %DateTime{
      calendar: calendar,
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond,
      std_offset: std_offset,
      utc_offset: utc_offset,
      zone_abbr: zone_abbr,
      time_zone: time_zone
    }
  end

  @doc """
  Converts the given `NaiveDateTime` to `DateTime`.

  It expects a time zone to put the NaiveDateTime in.
  If the time zone is "Etc/UTC", it always succeeds. Otherwise,
  the NaiveDateTime is checked against the time zone database
  given as `time_zone_database`. See the "Time zone database"
  section in the module documentation.

  ## Examples

      iex> DateTime.from_naive!(~N[2016-05-24 13:26:08.003], "Etc/UTC")
      #DateTime<2016-05-24 13:26:08.003Z>

      iex> DateTime.from_naive!(~N[2018-05-24 13:26:08.003], "Europe/Copenhagen", FakeTimeZoneDatabase)
      #DateTime<2018-05-24 13:26:08.003+02:00 CEST Europe/Copenhagen>

  """
  @doc since: "1.4.0"
  @spec from_naive!(
          NaiveDateTime.t(),
          Calendar.time_zone(),
          Calendar.time_zone_database()
        ) :: t
  def from_naive!(
        naive_datetime,
        time_zone,
        time_zone_database \\ Calendar.get_time_zone_database()
      ) do
    case from_naive(naive_datetime, time_zone, time_zone_database) do
      {:ok, datetime} ->
        datetime

      {:ambiguous, dt1, dt2} ->
        raise ArgumentError,
              "cannot convert #{inspect(naive_datetime)} to datetime because such " <>
                "instant is ambiguous in time zone #{time_zone} as there is an overlap " <>
                "between #{inspect(dt1)} and #{inspect(dt2)}"

      {:gap, dt1, dt2} ->
        raise ArgumentError,
              "cannot convert #{inspect(naive_datetime)} to datetime because such " <>
                "instant does not exist in time zone #{time_zone} as there is a gap " <>
                "between #{inspect(dt1)} and #{inspect(dt2)}"

      {:error, reason} ->
        raise ArgumentError,
              "cannot convert #{inspect(naive_datetime)} to datetime, reason: #{inspect(reason)}"
    end
  end

  @doc """
  Changes the time zone of a `DateTime`.

  Returns a `DateTime` for the same point in time, but instead at
  the time zone provided. It assumes that `DateTime` is valid and
  exists in the given time zone and calendar.

  By default, it uses the default time zone database returned by
  `Calendar.get_time_zone_database/0`, which defaults to
  `Calendar.UTCOnlyTimeZoneDatabase` which only handles "Etc/UTC" datetimes.
  Other time zone databases can be passed as argument or set globally.
  See the "Time zone database" section in the module docs.

  ## Examples

      iex> cph_datetime = DateTime.from_naive!(~N[2018-07-16 12:00:00], "Europe/Copenhagen", FakeTimeZoneDatabase)
      iex> {:ok, pacific_datetime} = DateTime.shift_zone(cph_datetime, "America/Los_Angeles", FakeTimeZoneDatabase)
      iex> pacific_datetime
      #DateTime<2018-07-16 03:00:00-07:00 PDT America/Los_Angeles>

  """
  @doc since: "1.8.0"
  @spec shift_zone(t, Calendar.time_zone(), Calendar.time_zone_database()) ::
          {:ok, t} | {:error, :time_zone_not_found | :utc_only_time_zone_database}
  def shift_zone(datetime, time_zone, time_zone_database \\ Calendar.get_time_zone_database())

  def shift_zone(%{time_zone: time_zone} = datetime, time_zone, _) do
    {:ok, datetime}
  end

  def shift_zone(datetime, time_zone, time_zone_database) do
    %{
      std_offset: std_offset,
      utc_offset: utc_offset,
      calendar: calendar,
      microsecond: {_, precision}
    } = datetime

    datetime
    |> to_iso_days()
    |> apply_tz_offset(utc_offset + std_offset)
    |> shift_zone_for_iso_days_utc(calendar, precision, time_zone, time_zone_database)
  end

  defp shift_zone_for_iso_days_utc(iso_days_utc, calendar, precision, time_zone, time_zone_db) do
    case time_zone_db.time_zone_period_from_utc_iso_days(iso_days_utc, time_zone) do
      {:ok, %{std_offset: std_offset, utc_offset: utc_offset, zone_abbr: zone_abbr}} ->
        {year, month, day, hour, minute, second, {microsecond_without_precision, _}} =
          iso_days_utc
          |> apply_tz_offset(-(utc_offset + std_offset))
          |> calendar.naive_datetime_from_iso_days()

        datetime = %DateTime{
          calendar: calendar,
          year: year,
          month: month,
          day: day,
          hour: hour,
          minute: minute,
          second: second,
          microsecond: {microsecond_without_precision, precision},
          std_offset: std_offset,
          utc_offset: utc_offset,
          zone_abbr: zone_abbr,
          time_zone: time_zone
        }

        {:ok, datetime}

      {:error, _} = error ->
        error
    end
  end

  @doc """
  Returns the current datetime in the provided time zone.

  By default, it uses the default time_zone returned by
  `Calendar.get_time_zone_database/0`, which defaults to
  `Calendar.UTCOnlyTimeZoneDatabase` which only handles "Etc/UTC" datetimes.
  Other time zone databases can be passed as argument or set globally.
  See the "Time zone database" section in the module docs.

  ## Examples

      iex> {:ok, datetime} = DateTime.now("Etc/UTC")
      iex> datetime.time_zone
      "Etc/UTC"
      iex> DateTime.now("Europe/Copenhagen")
      {:error, :utc_only_time_zone_database}
      iex> DateTime.now("not a real time zone name", FakeTimeZoneDatabase)
      {:error, :time_zone_not_found}

  """
  @doc since: "1.8.0"
  @spec now(Calendar.time_zone(), Calendar.time_zone_database()) ::
          {:ok, t} | {:error, :time_zone_not_found | :utc_only_time_zone_database}
  def now(time_zone, time_zone_database \\ Calendar.get_time_zone_database())

  def now("Etc/UTC", _) do
    {:ok, utc_now()}
  end

  def now(time_zone, time_zone_database) do
    shift_zone(utc_now(), time_zone, time_zone_database)
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
  @spec to_naive(Calendar.datetime()) :: NaiveDateTime.t()
  def to_naive(%{
        calendar: calendar,
        year: year,
        month: month,
        day: day,
        hour: hour,
        minute: minute,
        second: second,
        microsecond: microsecond,
        time_zone: _
      }) do
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
  @spec to_date(Calendar.datetime()) :: Date.t()
  def to_date(%{
        year: year,
        month: month,
        day: day,
        calendar: calendar,
        hour: _,
        minute: _,
        second: _,
        microsecond: _,
        time_zone: _
      }) do
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
  @spec to_time(Calendar.datetime()) :: Time.t()
  def to_time(%{
        year: _,
        month: _,
        day: _,
        calendar: calendar,
        hour: hour,
        minute: minute,
        second: second,
        microsecond: microsecond,
        time_zone: _
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

  The year parsed by this function is limited to four digits and,
  while ISO 8601 allows datetimes to specify 24:00:00 as the zero
  hour of the next day, this notation is not supported by Elixir.
  Note leap seconds are not supported by the built-in Calendar.ISO.

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
      iex> DateTime.from_iso8601("2015-01-23T23:50:07")
      {:error, :missing_offset}
      iex> DateTime.from_iso8601("2015-01-23 23:50:61")
      {:error, :invalid_time}
      iex> DateTime.from_iso8601("2015-01-32 23:50:07")
      {:error, :invalid_date}
      iex> DateTime.from_iso8601("2015-01-23T23:50:07.123-00:00")
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

  defp raw_from_iso8601(string, calendar, is_year_negative) do
    with <<unquote(match_date), sep, unquote(match_time), rest::binary>> <- string,
         true <- unquote(guard_date) and sep in @sep and unquote(guard_time),
         {microsecond, rest} <- Calendar.ISO.parse_microsecond(rest),
         {offset, ""} <- Calendar.ISO.parse_offset(rest) do
      {year, month, day} = unquote(read_date)
      {hour, minute, second} = unquote(read_time)
      year = if is_year_negative, do: -year, else: year

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
        %{utc_offset: utc_offset1, std_offset: std_offset1} = datetime1,
        %{utc_offset: utc_offset2, std_offset: std_offset2} = datetime2
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
  Adds a specified amount of time to a `DateTime`.

  Accepts an `amount_to_add` in any `unit` available from `t:System.time_unit/0`.
  Negative values will move backwards in time.

  Takes changes such as summer time/DST into account. This means that adding time
  can cause the wall time to "go backwards" during "fall back" during autumn.
  Adding just a few seconds to a datetime just before "spring forward" can cause wall
  time to increase by more than an hour.

  Fractional second precision stays the same in a similar way to `NaiveDateTime.add/2`.

  ### Examples

      iex> dt = DateTime.from_naive!(~N[2018-11-15 10:00:00], "Europe/Copenhagen", FakeTimeZoneDatabase)
      iex> dt |> DateTime.add(3600, :second, FakeTimeZoneDatabase)
      #DateTime<2018-11-15 11:00:00+01:00 CET Europe/Copenhagen>

      iex> dt = DateTime.from_naive!(~N[2018-11-15 10:00:00], "Etc/UTC")
      iex> dt |> DateTime.add(3600, :second)
      #DateTime<2018-11-15 11:00:00Z>

  When adding 3 seconds just before "spring forward" we go from 1:59:59 to 3:00:02

      iex> dt = DateTime.from_naive!(~N[2019-03-31 01:59:59.123], "Europe/Copenhagen", FakeTimeZoneDatabase)
      iex> dt |> DateTime.add(3, :second, FakeTimeZoneDatabase)
      #DateTime<2019-03-31 03:00:02.123+02:00 CEST Europe/Copenhagen>

  """
  @doc since: "1.8.0"
  @spec add(Calendar.datetime(), integer, System.time_unit(), Calendar.time_zone_database()) ::
          t()
  def add(
        datetime,
        amount_to_add,
        unit \\ :second,
        time_zone_database \\ Calendar.get_time_zone_database()
      )
      when is_integer(amount_to_add) do
    %{
      utc_offset: utc_offset,
      std_offset: std_offset,
      calendar: calendar,
      microsecond: {_, precision}
    } = datetime

    ppd = System.convert_time_unit(86400, :second, unit)
    total_offset = System.convert_time_unit(utc_offset + std_offset, :second, unit)

    result =
      datetime
      |> to_iso_days()
      # Subtract total offset in order to get UTC and add the integer for the addition
      |> Calendar.ISO.add_day_fraction_to_iso_days(amount_to_add - total_offset, ppd)
      |> shift_zone_for_iso_days_utc(calendar, precision, datetime.time_zone, time_zone_database)

    case result do
      {:ok, result_datetime} ->
        result_datetime

      {:error, error} ->
        raise ArgumentError,
              "cannot add #{amount_to_add} #{unit} to #{inspect(datetime)} (with time zone " <>
                "database #{inspect(time_zone_database)}), reason: #{inspect(error)}"
    end
  end

  @doc """
  Returns the given datetime with the microsecond field truncated to the given
  precision (`:microsecond`, `millisecond` or `:second`).

  The given datetime is returned unchanged if it already has lower precision than
  the given precision.

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
  @spec truncate(Calendar.datetime(), :microsecond | :millisecond | :second) :: t()
  def truncate(%DateTime{microsecond: microsecond} = datetime, precision) do
    %{datetime | microsecond: Calendar.truncate(microsecond, precision)}
  end

  def truncate(%{} = datetime_map, precision) do
    truncate(from_map(datetime_map), precision)
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

  def convert(%DateTime{calendar: calendar} = datetime, calendar) do
    {:ok, datetime}
  end

  def convert(%{calendar: calendar} = datetime, calendar) do
    {:ok, from_map(datetime)}
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
  @spec convert!(Calendar.datetime(), Calendar.calendar()) :: t
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

  defp apply_tz_offset(iso_days, 0) do
    iso_days
  end

  defp apply_tz_offset(iso_days, offset) do
    Calendar.ISO.add_day_fraction_to_iso_days(iso_days, -offset, 86400)
  end

  defp from_map(%{} = datetime_map) do
    %DateTime{
      year: datetime_map.year,
      month: datetime_map.month,
      day: datetime_map.day,
      hour: datetime_map.hour,
      minute: datetime_map.minute,
      second: datetime_map.second,
      microsecond: datetime_map.microsecond,
      time_zone: datetime_map.time_zone,
      zone_abbr: datetime_map.zone_abbr,
      utc_offset: datetime_map.utc_offset,
      std_offset: datetime_map.std_offset
    }
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
