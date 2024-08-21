defmodule DateTime do
  @moduledoc """
  A datetime implementation with a time zone.

  This datetime can be seen as a snapshot of a date and time
  at a given time zone. For such purposes, it also includes both
  UTC and Standard offsets, as well as the zone abbreviation
  field used exclusively for formatting purposes. Note future
  datetimes are not necessarily guaranteed to exist, as time
  zones may change any time in the future due to geopolitical
  reasons. See the "Datetimes as snapshots" section for more
  information.

  Remember, comparisons in Elixir using `==/2`, `>/2`, `</2` and friends
  are structural and based on the DateTime struct fields. For proper
  comparison between datetimes, use the `compare/2` function. The
  existence of the `compare/2` function in this module also allows
  using `Enum.min/2` and `Enum.max/2` functions to get the minimum and
  maximum datetime of an `Enum`. For example:

      iex> Enum.min([~U[2022-01-12 00:01:00.00Z], ~U[2021-01-12 00:01:00.00Z]], DateTime)
      ~U[2021-01-12 00:01:00.00Z]

  Developers should avoid creating the `DateTime` struct directly
  and instead rely on the functions provided by this module as
  well as the ones in third-party calendar libraries.

  ## Time zone database

  Many functions in this module require a time zone database.
  A time zone database is a record of the UTC offsets that its locales have
  used at various times in the past, are using, and are expected to use in the
  future.
  Because those plans can change, it needs to be periodically updated.

  By default, `DateTime` uses the default time zone database returned by
  `Calendar.get_time_zone_database/0`, which defaults to
  `Calendar.UTCOnlyTimeZoneDatabase` which only handles "Etc/UTC"
  datetimes and returns `{:error, :utc_only_time_zone_database}`
  for any other time zone.

  Other time zone databases can also be configured. Here are some
  available options and libraries:

    * [`time_zone_info`](https://github.com/hrzndhrn/time_zone_info)
    * [`tz`](https://github.com/mathieuprog/tz)
    * [`tzdata`](https://github.com/lau/tzdata)
    * [`zoneinfo`](https://github.com/smartrent/zoneinfo) -
      recommended for embedded devices

  To use one of them, first make sure it is added as a dependency in `mix.exs`.
  It can then be configured either via configuration:

      config :elixir, :time_zone_database, Tz.TimeZoneDatabase

  or by calling `Calendar.put_time_zone_database/1`:

      Calendar.put_time_zone_database(Tz.TimeZoneDatabase)

  See the proper names in the library installation instructions.

  ## Datetimes as snapshots

  In the first section, we described datetimes as a "snapshot of
  a date and time at a given time zone". To understand precisely
  what we mean, let's see an example.

  Imagine someone in Poland who wants to schedule a meeting with someone
  in Brazil in the next year. The meeting will happen at 2:30 AM
  in the Polish time zone. At what time will the meeting happen in
  Brazil?

  You can consult the time zone database today, one year before,
  using the API in this module and it will give you an answer that
  is valid right now. However, this answer may not be valid in the
  future. Why? Because both Brazil and Poland may change their timezone
  rules, ultimately affecting the result. For example, a country may
  choose to enter or abandon "Daylight Saving Time", which is a
  process where we adjust the clock one hour forward or one hour
  back once per year. Whenever the rules change, the exact instant
  that 2:30 AM in Polish time will be in Brazil may change.

  In other words, whenever working with future DateTimes, there is
  no guarantee the results you get will always be correct, until
  the event actually happens. Therefore, when you ask for a future
  time, the answers you get are a snapshot that reflects the current
  state of the time zone rules. For datetimes in the past, this is
  not a problem, because time zone rules do not change for past
  events.

  To make matters worse, it may be that 2:30 AM in Polish time
  does not actually even exist or it is ambiguous. If a certain
  time zone observes "Daylight Saving Time", they will move their
  clock forward once a year. When this happens, there is a whole
  hour that does not exist. Then, when they move the clock back,
  there is a certain hour that will happen twice. So if you want to
  schedule a meeting when this shift back happens, you would need to
  explicitly say which occurrence of 2:30 AM you mean: the one in
  "Summer Time", which occurs before the shift, or the one
  in "Standard Time", which occurs after it. Applications that are
  date and time sensitive need to take these scenarios into account
  and correctly communicate them to users.

  The good news is: Elixir contains all of the building blocks
  necessary to tackle those problems. The default timezone database
  used by Elixir, `Calendar.UTCOnlyTimeZoneDatabase`, only works
  with UTC, which does not observe those issues. Once you bring
  a proper time zone database, the functions in this module will
  query the database and return the relevant information. For
  example, look at how `DateTime.new/4` returns different results
  based on the scenarios described in this section.

  ## Converting between timezones

  Bearing in mind the cautions above, and assuming you've brought in a full
  timezone database, here are some examples of common shifts between time
  zones.

      # Local time to UTC
      new_york = DateTime.from_naive!(~N[2023-06-26T09:30:00], "America/New_York")
      #=> #DateTime<2023-06-26 09:30:00-04:00 EDT America/New_York>

      utc = DateTime.shift_zone!(new_york, "Etc/UTC")
      #=> ~U[2023-06-26 13:30:00Z]

      # UTC to local time
      DateTime.shift_zone!(utc, "Europe/Paris")
      #=> #DateTime<2023-06-26 15:30:00+02:00 CEST Europe/Paris>

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
  @seconds_per_day 24 * 60 * 60

  @doc """
  Returns the current datetime in UTC.

  If you want the current time in Unix seconds,
  use `System.os_time/1` instead.

  You can also pass a time unit to automatically
  truncate the resulting datetime. This is available
  since v1.15.0.

  The default unit if none gets passed is `:native`,
  which results on a default resolution of microseconds.

  ## Examples

      iex> datetime = DateTime.utc_now()
      iex> datetime.time_zone
      "Etc/UTC"

      iex> datetime = DateTime.utc_now(:second)
      iex> datetime.microsecond
      {0, 0}

  """
  @spec utc_now(Calendar.calendar() | :native | :microsecond | :millisecond | :second) :: t
  def utc_now(calendar_or_time_unit \\ Calendar.ISO) do
    case calendar_or_time_unit do
      unit when unit in [:microsecond, :millisecond, :second, :native] ->
        utc_now(unit, Calendar.ISO)

      calendar ->
        System.os_time() |> from_unix!(:native, calendar)
    end
  end

  @doc """
  Returns the current datetime in UTC, supporting
  a specific calendar and precision.

  If you want the current time in Unix seconds,
  use `System.os_time/1` instead.

  ## Examples

      iex> datetime = DateTime.utc_now(:microsecond, Calendar.ISO)
      iex> datetime.time_zone
      "Etc/UTC"

      iex> datetime = DateTime.utc_now(:second, Calendar.ISO)
      iex> datetime.microsecond
      {0, 0}

  """
  @doc since: "1.15.0"
  @spec utc_now(:native | :microsecond | :millisecond | :second, Calendar.calendar()) :: t
  def utc_now(time_unit, calendar)
      when time_unit in [:native, :microsecond, :millisecond, :second] do
    System.os_time(time_unit) |> from_unix!(time_unit, calendar)
  end

  @doc """
  Builds a datetime from date and time structs.

  It expects a time zone to put the `DateTime` in.
  If the time zone is not passed it will default to `"Etc/UTC"`,
  which always succeeds. Otherwise, the `DateTime` is checked against the time zone database
  given as `time_zone_database`. See the "Time zone database"
  section in the module documentation.

  ## Examples

      iex> DateTime.new(~D[2016-05-24], ~T[13:26:08.003], "Etc/UTC")
      {:ok, ~U[2016-05-24 13:26:08.003Z]}

  When the datetime is ambiguous - for instance during changing from summer
  to winter time - the two possible valid datetimes are returned in a tuple.
  The first datetime is also the one which comes first chronologically, while
  the second one comes last.

      iex> {:ambiguous, first_dt, second_dt} = DateTime.new(~D[2018-10-28], ~T[02:30:00], "Europe/Copenhagen", FakeTimeZoneDatabase)
      iex> first_dt
      #DateTime<2018-10-28 02:30:00+02:00 CEST Europe/Copenhagen>
      iex> second_dt
      #DateTime<2018-10-28 02:30:00+01:00 CET Europe/Copenhagen>

  When there is a gap in wall time - for instance in spring when the clocks are
  turned forward - the latest valid datetime just before the gap and the first
  valid datetime just after the gap.

      iex> {:gap, just_before, just_after} = DateTime.new(~D[2019-03-31], ~T[02:30:00], "Europe/Copenhagen", FakeTimeZoneDatabase)
      iex> just_before
      #DateTime<2019-03-31 01:59:59.999999+01:00 CET Europe/Copenhagen>
      iex> just_after
      #DateTime<2019-03-31 03:00:00+02:00 CEST Europe/Copenhagen>

  Most of the time there is one, and just one, valid datetime for a certain
  date and time in a certain time zone.

      iex> {:ok, datetime} = DateTime.new(~D[2018-07-28], ~T[12:30:00], "Europe/Copenhagen", FakeTimeZoneDatabase)
      iex> datetime
      #DateTime<2018-07-28 12:30:00+02:00 CEST Europe/Copenhagen>

  """
  @doc since: "1.11.0"
  @spec new(Date.t(), Time.t(), Calendar.time_zone(), Calendar.time_zone_database()) ::
          {:ok, t}
          | {:ambiguous, first_datetime :: t, second_datetime :: t}
          | {:gap, t, t}
          | {:error,
             :incompatible_calendars | :time_zone_not_found | :utc_only_time_zone_database}
  def new(
        date,
        time,
        time_zone \\ "Etc/UTC",
        time_zone_database \\ Calendar.get_time_zone_database()
      )

  def new(%Date{calendar: calendar} = date, %Time{calendar: calendar} = time, "Etc/UTC", _db) do
    %{year: year, month: month, day: day} = date
    %{hour: hour, minute: minute, second: second, microsecond: microsecond} = time

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

  def new(date, time, time_zone, time_zone_database) do
    with {:ok, naive_datetime} <- NaiveDateTime.new(date, time) do
      from_naive(naive_datetime, time_zone, time_zone_database)
    end
  end

  @doc """
  Builds a datetime from date and time structs, raising on errors.

  It expects a time zone to put the `DateTime` in.
  If the time zone is not passed it will default to `"Etc/UTC"`,
  which always succeeds. Otherwise, the DateTime is checked against the time zone database
  given as `time_zone_database`. See the "Time zone database"
  section in the module documentation.

  ## Examples

      iex> DateTime.new!(~D[2016-05-24], ~T[13:26:08.003], "Etc/UTC")
      ~U[2016-05-24 13:26:08.003Z]

  When the datetime is ambiguous - for instance during changing from summer
  to winter time - an error will be raised.

      iex> DateTime.new!(~D[2018-10-28], ~T[02:30:00], "Europe/Copenhagen", FakeTimeZoneDatabase)
      ** (ArgumentError) cannot build datetime with ~D[2018-10-28] and ~T[02:30:00] because such instant is ambiguous in time zone Europe/Copenhagen as there is an overlap between #DateTime<2018-10-28 02:30:00+02:00 CEST Europe/Copenhagen> and #DateTime<2018-10-28 02:30:00+01:00 CET Europe/Copenhagen>

  When there is a gap in wall time - for instance in spring when the clocks are
  turned forward - an error will be raised.

      iex> DateTime.new!(~D[2019-03-31], ~T[02:30:00], "Europe/Copenhagen", FakeTimeZoneDatabase)
      ** (ArgumentError) cannot build datetime with ~D[2019-03-31] and ~T[02:30:00] because such instant does not exist in time zone Europe/Copenhagen as there is a gap between #DateTime<2019-03-31 01:59:59.999999+01:00 CET Europe/Copenhagen> and #DateTime<2019-03-31 03:00:00+02:00 CEST Europe/Copenhagen>

  Most of the time there is one, and just one, valid datetime for a certain
  date and time in a certain time zone.

      iex> datetime = DateTime.new!(~D[2018-07-28], ~T[12:30:00], "Europe/Copenhagen", FakeTimeZoneDatabase)
      iex> datetime
      #DateTime<2018-07-28 12:30:00+02:00 CEST Europe/Copenhagen>

  """
  @doc since: "1.11.0"
  @spec new!(Date.t(), Time.t(), Calendar.time_zone(), Calendar.time_zone_database()) :: t
  def new!(
        date,
        time,
        time_zone \\ "Etc/UTC",
        time_zone_database \\ Calendar.get_time_zone_database()
      )

  def new!(date, time, time_zone, time_zone_database) do
    case new(date, time, time_zone, time_zone_database) do
      {:ok, datetime} ->
        datetime

      {:ambiguous, dt1, dt2} ->
        raise ArgumentError,
              "cannot build datetime with #{inspect(date)} and #{inspect(time)} because such " <>
                "instant is ambiguous in time zone #{time_zone} as there is an overlap " <>
                "between #{inspect(dt1)} and #{inspect(dt2)}"

      {:gap, dt1, dt2} ->
        raise ArgumentError,
              "cannot build datetime with #{inspect(date)} and #{inspect(time)} because such " <>
                "instant does not exist in time zone #{time_zone} as there is a gap " <>
                "between #{inspect(dt1)} and #{inspect(dt2)}"

      {:error, reason} ->
        raise ArgumentError,
              "cannot build datetime with #{inspect(date)} and #{inspect(time)}, reason: #{inspect(reason)}"
    end
  end

  @doc """
  Converts the given Unix time to `DateTime`.

  The integer can be given in different unit
  according to `System.convert_time_unit/3` and it will
  be converted to microseconds internally. Up to
  253402300799 seconds is supported.

  Unix times are always in UTC and therefore the DateTime
  will be returned in UTC.

  ## Examples

      iex> {:ok, datetime} = DateTime.from_unix(1_464_096_368)
      iex> datetime
      ~U[2016-05-24 13:26:08Z]

      iex> {:ok, datetime} = DateTime.from_unix(1_432_560_368_868_569, :microsecond)
      iex> datetime
      ~U[2015-05-25 13:26:08.868569Z]

      iex> {:ok, datetime} = DateTime.from_unix(253_402_300_799)
      iex> datetime
      ~U[9999-12-31 23:59:59Z]

      iex> {:error, :invalid_unix_time} = DateTime.from_unix(253_402_300_800)

  The unit can also be an integer as in `t:System.time_unit/0`:

      iex> {:ok, datetime} = DateTime.from_unix(143_256_036_886_856, 1024)
      iex> datetime
      ~U[6403-03-17 07:05:22.320312Z]

  Negative Unix times are supported up to -377705116800 seconds:

      iex> {:ok, datetime} = DateTime.from_unix(-377_705_116_800)
      iex> datetime
      ~U[-9999-01-01 00:00:00Z]

      iex> {:error, :invalid_unix_time} = DateTime.from_unix(-377_705_116_801)

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
      ~U[1970-01-01 00:00:00Z]

      iex> DateTime.from_unix!(1_464_096_368)
      ~U[2016-05-24 13:26:08Z]

      iex> DateTime.from_unix!(1_432_560_368_868_569, :microsecond)
      ~U[2015-05-25 13:26:08.868569Z]

      iex> DateTime.from_unix!(143_256_036_886_856, 1024)
      ~U[6403-03-17 07:05:22.320312Z]

  """
  @spec from_unix!(integer, :native | System.time_unit(), Calendar.calendar()) :: t
  def from_unix!(integer, unit \\ :second, calendar \\ Calendar.ISO) do
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

      iex> DateTime.from_naive(~N[2016-05-24 13:26:08.003], "Etc/UTC")
      {:ok, ~U[2016-05-24 13:26:08.003Z]}

  When the datetime is ambiguous - for instance during changing from summer
  to winter time - the two possible valid datetimes are returned in a tuple.
  The first datetime is also the one which comes first chronologically, while
  the second one comes last.

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
      ~U[2018-08-24 10:00:00Z]

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
          | {:ambiguous, first_datetime :: t, second_datetime :: t}
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
          |> Map.replace!(:microsecond, {999_999, 6})
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
      ~U[2016-05-24 13:26:08.003Z]

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

      iex> {:ok, pacific_datetime} = DateTime.shift_zone(~U[2018-07-16 10:00:00Z], "America/Los_Angeles", FakeTimeZoneDatabase)
      iex> pacific_datetime
      #DateTime<2018-07-16 03:00:00-07:00 PDT America/Los_Angeles>

      iex> DateTime.shift_zone(~U[2018-07-16 10:00:00Z], "bad timezone", FakeTimeZoneDatabase)
      {:error, :time_zone_not_found}

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
  Changes the time zone of a `DateTime` or raises on errors.

  See `shift_zone/3` for more information.

  ## Examples

      iex> DateTime.shift_zone!(~U[2018-07-16 10:00:00Z], "America/Los_Angeles", FakeTimeZoneDatabase)
      #DateTime<2018-07-16 03:00:00-07:00 PDT America/Los_Angeles>

      iex> DateTime.shift_zone!(~U[2018-07-16 10:00:00Z], "bad timezone", FakeTimeZoneDatabase)
      ** (ArgumentError) cannot shift ~U[2018-07-16 10:00:00Z] to "bad timezone" time zone, reason: :time_zone_not_found

  """
  @doc since: "1.10.0"
  @spec shift_zone!(t, Calendar.time_zone(), Calendar.time_zone_database()) :: t
  def shift_zone!(datetime, time_zone, time_zone_database \\ Calendar.get_time_zone_database()) do
    case shift_zone(datetime, time_zone, time_zone_database) do
      {:ok, datetime} ->
        datetime

      {:error, reason} ->
        raise ArgumentError,
              "cannot shift #{inspect(datetime)} to #{inspect(time_zone)} time zone" <>
                ", reason: #{inspect(reason)}"
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

      iex> DateTime.now("bad timezone", FakeTimeZoneDatabase)
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
  Returns the current datetime in the provided time zone or raises on errors

  See `now/2` for more information.

  ## Examples

      iex> datetime = DateTime.now!("Etc/UTC")
      iex> datetime.time_zone
      "Etc/UTC"

      iex> DateTime.now!("Europe/Copenhagen")
      ** (ArgumentError) cannot get current datetime in "Europe/Copenhagen" time zone, reason: :utc_only_time_zone_database

      iex> DateTime.now!("bad timezone", FakeTimeZoneDatabase)
      ** (ArgumentError) cannot get current datetime in "bad timezone" time zone, reason: :time_zone_not_found

  """
  @doc since: "1.10.0"
  @spec now!(Calendar.time_zone(), Calendar.time_zone_database()) :: t
  def now!(time_zone, time_zone_database \\ Calendar.get_time_zone_database()) do
    case now(time_zone, time_zone_database) do
      {:ok, datetime} ->
        datetime

      {:error, reason} ->
        raise ArgumentError,
              "cannot get current datetime in #{inspect(time_zone)} time zone, reason: " <>
                inspect(reason)
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
  @spec to_unix(Calendar.datetime(), :native | System.time_unit()) :: integer
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
  def to_naive(datetime)

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
  def to_date(datetime)

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
  def to_time(datetime)

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
  [ISO 8601:2019](https://en.wikipedia.org/wiki/ISO_8601) format.

  By default, `DateTime.to_iso8601/2` returns datetimes formatted in the "extended"
  format, for human readability. It also supports the "basic" format through passing the `:basic` option.

  Only supports converting datetimes which are in the ISO calendar,
  attempting to convert datetimes from other calendars will raise.
  You can also optionally specify an offset for the formatted string.

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

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "AMT",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: -14400, std_offset: 0, time_zone: "America/Manaus"}
      iex> DateTime.to_iso8601(dt, :extended, 3600)
      "2000-03-01T04:00:07+01:00"

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "AMT",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: -14400, std_offset: 0, time_zone: "America/Manaus"}
      iex> DateTime.to_iso8601(dt, :extended, 0)
      "2000-03-01T03:00:07+00:00"

      iex> dt = %DateTime{year: 2000, month: 3, day: 01, zone_abbr: "UTC",
      ...>                hour: 03, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: 0, std_offset: 0, time_zone: "Etc/UTC"}
      iex> DateTime.to_iso8601(dt, :extended, 0)
      "2000-03-01T03:00:07Z"

      iex> {:ok, dt, offset} = DateTime.from_iso8601("2000-03-01T03:00:07Z")
      iex> "2000-03-01T03:00:07Z" = DateTime.to_iso8601(dt, :extended, offset)
  """
  @spec to_iso8601(Calendar.datetime(), :basic | :extended, nil | integer()) :: String.t()
  def to_iso8601(datetime, format \\ :extended, offset \\ nil)

  def to_iso8601(%{calendar: Calendar.ISO} = datetime, format, nil)
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
      utc_offset: utc_offset,
      std_offset: std_offset
    } = datetime

    datetime_to_string(year, month, day, hour, minute, second, microsecond, format) <>
      Calendar.ISO.offset_to_string(utc_offset, std_offset, time_zone, format)
  end

  def to_iso8601(
        %{calendar: Calendar.ISO, microsecond: {_, precision}, time_zone: "Etc/UTC"} = datetime,
        format,
        0
      )
      when format in [:extended, :basic] do
    {year, month, day, hour, minute, second, {microsecond, _}} = shift_by_offset(datetime, 0)

    datetime_to_string(year, month, day, hour, minute, second, {microsecond, precision}, format) <>
      "Z"
  end

  def to_iso8601(%{calendar: Calendar.ISO} = datetime, format, offset)
      when format in [:extended, :basic] do
    {_, precision} = datetime.microsecond
    {year, month, day, hour, minute, second, {microsecond, _}} = shift_by_offset(datetime, offset)

    datetime_to_string(year, month, day, hour, minute, second, {microsecond, precision}, format) <>
      Calendar.ISO.offset_to_string(offset, 0, nil, format)
  end

  def to_iso8601(%{calendar: _} = datetime, format, offset) when format in [:extended, :basic] do
    datetime
    |> convert!(Calendar.ISO)
    |> to_iso8601(format, offset)
  end

  defp shift_by_offset(%{calendar: calendar} = datetime, offset) do
    total_offset = datetime.utc_offset + datetime.std_offset

    datetime
    |> to_iso_days()
    # Subtract total original offset in order to get UTC and add the new offset
    |> Calendar.ISO.add_day_fraction_to_iso_days(offset - total_offset, 86400)
    |> calendar.naive_datetime_from_iso_days()
  end

  defp datetime_to_string(year, month, day, hour, minute, second, microsecond, format) do
    Calendar.ISO.date_to_string(year, month, day, format) <>
      "T" <>
      Calendar.ISO.time_to_string(hour, minute, second, microsecond, format)
  end

  @doc """
  Parses the extended "Date and time of day" format described by
  [ISO 8601:2019](https://en.wikipedia.org/wiki/ISO_8601).

  Since ISO 8601 does not include the proper time zone, the given
  string will be converted to UTC and its offset in seconds will be
  returned as part of this function. Therefore offset information
  must be present in the string.

  As specified in the standard, the separator "T" may be omitted if
  desired as there is no ambiguity within this function.

  Note leap seconds are not supported by the built-in Calendar.ISO.

  ## Examples

      iex> {:ok, datetime, 0} = DateTime.from_iso8601("2015-01-23T23:50:07Z")
      iex> datetime
      ~U[2015-01-23 23:50:07Z]

      iex> {:ok, datetime, 9000} = DateTime.from_iso8601("2015-01-23T23:50:07.123+02:30")
      iex> datetime
      ~U[2015-01-23 21:20:07.123Z]

      iex> {:ok, datetime, 9000} = DateTime.from_iso8601("2015-01-23T23:50:07,123+02:30")
      iex> datetime
      ~U[2015-01-23 21:20:07.123Z]

      iex> {:ok, datetime, 0} = DateTime.from_iso8601("-2015-01-23T23:50:07Z")
      iex> datetime
      ~U[-2015-01-23 23:50:07Z]

      iex> {:ok, datetime, 9000} = DateTime.from_iso8601("-2015-01-23T23:50:07,123+02:30")
      iex> datetime
      ~U[-2015-01-23 21:20:07.123Z]

      iex> {:ok, datetime, 9000} = DateTime.from_iso8601("20150123T235007.123+0230", :basic)
      iex> datetime
      ~U[2015-01-23 21:20:07.123Z]

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
  @spec from_iso8601(String.t(), Calendar.calendar() | :extended | :basic) ::
          {:ok, t, Calendar.utc_offset()} | {:error, atom}
  def from_iso8601(string, format_or_calendar \\ Calendar.ISO)

  def from_iso8601(string, format) when format in [:basic, :extended] do
    from_iso8601(string, Calendar.ISO, format)
  end

  def from_iso8601(string, calendar) when is_atom(calendar) do
    from_iso8601(string, calendar, :extended)
  end

  @doc """
  Converts from ISO8601 specifying both a calendar and a mode.

  See `from_iso8601/2` for more information.

  ## Examples

      iex> {:ok, datetime, 9000} = DateTime.from_iso8601("2015-01-23T23:50:07,123+02:30", Calendar.ISO, :extended)
      iex> datetime
      ~U[2015-01-23 21:20:07.123Z]

      iex> {:ok, datetime, 9000} = DateTime.from_iso8601("20150123T235007.123+0230", Calendar.ISO, :basic)
      iex> datetime
      ~U[2015-01-23 21:20:07.123Z]

  """
  @spec from_iso8601(String.t(), Calendar.calendar(), :extended | :basic) ::
          {:ok, t, Calendar.utc_offset()} | {:error, atom}
  def from_iso8601(string, calendar, format) do
    with {:ok, {year, month, day, hour, minute, second, microsecond}, offset} <-
           Calendar.ISO.parse_utc_datetime(string, format) do
      datetime = %DateTime{
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

      with {:ok, converted} <- convert(datetime, calendar) do
        {:ok, converted, offset}
      end
    end
  end

  @doc """
  Converts a number of gregorian seconds to a `DateTime` struct.

  The returned `DateTime` will have `UTC` timezone, if you want other timezone, please use
  `DateTime.shift_zone/3`.

  ## Examples

      iex> DateTime.from_gregorian_seconds(1)
      ~U[0000-01-01 00:00:01Z]
      iex> DateTime.from_gregorian_seconds(63_755_511_991, {5000, 3})
      ~U[2020-05-01 00:26:31.005Z]
      iex> DateTime.from_gregorian_seconds(-1)
      ~U[-0001-12-31 23:59:59Z]

  """
  @doc since: "1.11.0"
  @spec from_gregorian_seconds(integer(), Calendar.microsecond(), Calendar.calendar()) :: t
  def from_gregorian_seconds(
        seconds,
        {microsecond, precision} \\ {0, 0},
        calendar \\ Calendar.ISO
      )
      when is_integer(seconds) do
    iso_days = Calendar.ISO.gregorian_seconds_to_iso_days(seconds, microsecond)

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
      std_offset: 0,
      utc_offset: 0,
      zone_abbr: "UTC",
      time_zone: "Etc/UTC"
    }
  end

  @doc """
  Converts a `DateTime` struct to a number of gregorian seconds and microseconds.

  ## Examples

      iex> dt = %DateTime{year: 0000, month: 1, day: 1, zone_abbr: "UTC",
      ...>                hour: 0, minute: 0, second: 1, microsecond: {0, 0},
      ...>                utc_offset: 0, std_offset: 0, time_zone: "Etc/UTC"}
      iex> DateTime.to_gregorian_seconds(dt)
      {1, 0}

      iex> dt = %DateTime{year: 2020, month: 5, day: 1, zone_abbr: "UTC",
      ...>                hour: 0, minute: 26, second: 31, microsecond: {5000, 0},
      ...>                utc_offset: 0, std_offset: 0, time_zone: "Etc/UTC"}
      iex> DateTime.to_gregorian_seconds(dt)
      {63_755_511_991, 5000}

      iex> dt = %DateTime{year: 2020, month: 5, day: 1, zone_abbr: "CET",
      ...>                hour: 1, minute: 26, second: 31, microsecond: {5000, 0},
      ...>                utc_offset: 3600, std_offset: 0, time_zone: "Europe/Warsaw"}
      iex> DateTime.to_gregorian_seconds(dt)
      {63_755_511_991, 5000}

  """
  @doc since: "1.11.0"
  @spec to_gregorian_seconds(Calendar.datetime()) :: {integer(), non_neg_integer()}
  def to_gregorian_seconds(
        %{
          std_offset: std_offset,
          utc_offset: utc_offset,
          microsecond: {microsecond, _}
        } = datetime
      ) do
    {days, day_fraction} =
      datetime
      |> to_iso_days()
      |> apply_tz_offset(utc_offset + std_offset)

    seconds_in_day = seconds_from_day_fraction(day_fraction)
    {days * @seconds_per_day + seconds_in_day, microsecond}
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
  Returns `true` if the first datetime is strictly earlier than the second.

  ## Examples

      iex> DateTime.before?(~U[2021-01-01 11:00:00Z], ~U[2022-02-02 11:00:00Z])
      true
      iex> DateTime.before?(~U[2021-01-01 11:00:00Z], ~U[2021-01-01 11:00:00Z])
      false
      iex> DateTime.before?(~U[2022-02-02 11:00:00Z], ~U[2021-01-01 11:00:00Z])
      false

  """
  @doc since: "1.15.0"
  @spec before?(Calendar.datetime(), Calendar.datetime()) :: boolean()
  def before?(datetime1, datetime2) do
    compare(datetime1, datetime2) == :lt
  end

  @doc """
  Returns `true` if the first datetime is strictly later than the second.

  ## Examples

      iex> DateTime.after?(~U[2022-02-02 11:00:00Z], ~U[2021-01-01 11:00:00Z])
      true
      iex> DateTime.after?(~U[2021-01-01 11:00:00Z], ~U[2021-01-01 11:00:00Z])
      false
      iex> DateTime.after?(~U[2021-01-01 11:00:00Z], ~U[2022-02-02 11:00:00Z])
      false

  """
  @doc since: "1.15.0"
  @spec after?(Calendar.datetime(), Calendar.datetime()) :: boolean()
  def after?(datetime1, datetime2) do
    compare(datetime1, datetime2) == :gt
  end

  @doc """
  Subtracts `datetime2` from `datetime1`.

  The answer can be returned in any `:day`, `:hour`, `:minute`, or any `unit`
  available from `t:System.time_unit/0`. The unit is measured according to
  `Calendar.ISO` and defaults to `:second`.

  Fractional results are not supported and are truncated.

  ## Examples

      iex> DateTime.diff(~U[2024-01-15 10:00:10Z], ~U[2024-01-15 10:00:00Z])
      10

  This function also considers timezone offsets:

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
      iex> DateTime.diff(dt1, dt2, :hour)
      5
      iex> DateTime.diff(dt2, dt1, :hour)
      -5

  """
  @doc since: "1.5.0"
  @spec diff(
          Calendar.datetime(),
          Calendar.datetime(),
          :day | :hour | :minute | System.time_unit()
        ) :: integer()
  def diff(datetime1, datetime2, unit \\ :second)

  def diff(datetime1, datetime2, :day) do
    diff(datetime1, datetime2, :second) |> div(86400)
  end

  def diff(datetime1, datetime2, :hour) do
    diff(datetime1, datetime2, :second) |> div(3600)
  end

  def diff(datetime1, datetime2, :minute) do
    diff(datetime1, datetime2, :second) |> div(60)
  end

  def diff(
        %{utc_offset: utc_offset1, std_offset: std_offset1} = datetime1,
        %{utc_offset: utc_offset2, std_offset: std_offset2} = datetime2,
        unit
      ) do
    if not is_integer(unit) and
         unit not in ~w(second millisecond microsecond nanosecond)a do
      raise ArgumentError,
            "unsupported time unit. Expected :day, :hour, :minute, :second, :millisecond, :microsecond, :nanosecond, or a positive integer, got #{inspect(unit)}"
    end

    naive_diff =
      (datetime1 |> to_iso_days() |> Calendar.ISO.iso_days_to_unit(unit)) -
        (datetime2 |> to_iso_days() |> Calendar.ISO.iso_days_to_unit(unit))

    offset_diff = utc_offset2 + std_offset2 - (utc_offset1 + std_offset1)
    naive_diff + System.convert_time_unit(offset_diff, :second, unit)
  end

  @doc """
  Adds a specified amount of time to a `DateTime`.

  Accepts an `amount_to_add` in any `unit`. `unit` can be `:day`,
  `:hour`, `:minute`, `:second` or any subsecond precision from
  `t:System.time_unit/0`. It defaults to `:second`. Negative values
  will move backwards in time.

  This function always considers the unit to be computed according
  to the `Calendar.ISO`.

  This function relies on a contiguous representation of time,
  ignoring the wall time and timezone changes. For example, if you add
  one day when there are summer time/daylight saving time changes,
  it will also change the time forward or backward by one hour,
  so the elapsed time is precisely 24 hours. Similarly, adding just
  a few seconds to a datetime just before "spring forward" can cause
  wall time to increase by more than an hour.

  While this means this function is precise in terms of elapsed time,
  its result may be misleading in certain use cases. For example, if a
  user requests a meeting to happen every day at 15:00 and you use this
  function to compute all future meetings by adding day after day, this
  function may change the meeting time to 14:00 or 16:00 if there are
  changes to the current timezone. Computing of recurring datetimes is
  not currently supported in Elixir's standard library but it is available
  by third-party libraries.

  ### Examples

      iex> dt = DateTime.from_naive!(~N[2018-11-15 10:00:00], "Europe/Copenhagen", FakeTimeZoneDatabase)
      iex> dt |> DateTime.add(3600, :second, FakeTimeZoneDatabase)
      #DateTime<2018-11-15 11:00:00+01:00 CET Europe/Copenhagen>

      iex> DateTime.add(~U[2018-11-15 10:00:00Z], 3600, :second)
      ~U[2018-11-15 11:00:00Z]

  When adding 3 seconds just before "spring forward" we go from 1:59:59 to 3:00:02:

      iex> dt = DateTime.from_naive!(~N[2019-03-31 01:59:59.123], "Europe/Copenhagen", FakeTimeZoneDatabase)
      iex> dt |> DateTime.add(3, :second, FakeTimeZoneDatabase)
      #DateTime<2019-03-31 03:00:02.123+02:00 CEST Europe/Copenhagen>

  When adding 1 day during "spring forward", the hour also changes:

      iex> dt = DateTime.from_naive!(~N[2019-03-31 01:00:00], "Europe/Copenhagen", FakeTimeZoneDatabase)
      iex> dt |> DateTime.add(1, :day, FakeTimeZoneDatabase)
      #DateTime<2019-04-01 02:00:00+02:00 CEST Europe/Copenhagen>

  This operation merges the precision of the naive date time with the given unit:

      iex> result = DateTime.add(~U[2014-10-02 00:29:10Z], 21, :millisecond)
      ~U[2014-10-02 00:29:10.021Z]
      iex> result.microsecond
      {21000, 3}

  To shift a datetime by a `Duration` and according to its underlying calendar, use `DateTime.shift/3`.

  """
  @doc since: "1.8.0"
  @spec add(
          Calendar.datetime(),
          integer,
          :day | :hour | :minute | System.time_unit(),
          Calendar.time_zone_database()
        ) ::
          t()
  def add(
        datetime,
        amount_to_add,
        unit \\ :second,
        time_zone_database \\ Calendar.get_time_zone_database()
      )

  def add(datetime, amount_to_add, :day, time_zone_database) when is_integer(amount_to_add) do
    add(datetime, amount_to_add * 86400, :second, time_zone_database)
  end

  def add(datetime, amount_to_add, :hour, time_zone_database) when is_integer(amount_to_add) do
    add(datetime, amount_to_add * 3600, :second, time_zone_database)
  end

  def add(datetime, amount_to_add, :minute, time_zone_database) when is_integer(amount_to_add) do
    add(datetime, amount_to_add * 60, :second, time_zone_database)
  end

  def add(%{calendar: calendar} = datetime, amount_to_add, unit, time_zone_database)
      when is_integer(amount_to_add) do
    %{
      microsecond: {_, precision},
      time_zone: time_zone,
      utc_offset: utc_offset,
      std_offset: std_offset
    } = datetime

    if not is_integer(unit) and unit not in ~w(second millisecond microsecond nanosecond)a do
      raise ArgumentError,
            "unsupported time unit. Expected :day, :hour, :minute, :second, :millisecond, :microsecond, :nanosecond, or a positive integer, got #{inspect(unit)}"
    end

    precision = max(Calendar.ISO.time_unit_to_precision(unit), precision)

    result =
      datetime
      |> to_iso_days()
      |> Calendar.ISO.shift_time_unit(amount_to_add, unit)
      |> apply_tz_offset(utc_offset + std_offset)
      |> shift_zone_for_iso_days_utc(calendar, precision, time_zone, time_zone_database)

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
  Shifts given `datetime` by `duration` according to its calendar.

  Allowed units are: `:year`, `:month`, `:week`, `:day`, `:hour`, `:minute`, `:second`, `:microsecond`.

  This operation is equivalent to shifting the datetime wall clock
  (in other words, the value as someone in that timezone would see
  on their watch), then applying the time zone offset to convert it
  to UTC, and finally computing the new timezone in case of shifts.
  This ensures `shift/3` always returns a valid datetime.

  On the other hand, time zones that observe "Daylight Saving Time"
  or other changes, across summer/winter time will add/remove hours
  from the resulting datetime:

      dt = DateTime.new!(~D[2019-03-31], ~T[01:00:00], "Europe/Copenhagen")
      DateTime.shift(dt, hour: 1)
      #=> #DateTime<2019-03-31 03:00:00+02:00 CEST Europe/Copenhagen>

      dt = DateTime.new!(~D[2018-11-04], ~T[00:00:00], "America/Los_Angeles")
      DateTime.shift(dt, hour: 2)
      #=> #DateTime<2018-11-04 01:00:00-08:00 PST America/Los_Angeles>

  In case you don't want these changes to happen automatically or you
  want to surface time zone conflicts to the user, you can shift
  the datetime as a naive datetime and then use `from_naive/2`:

      dt |> NaiveDateTime.shift(duration) |> DateTime.from_naive(dt.time_zone)

  When using the default ISO calendar, durations are collapsed and
  applied in the order of months, then seconds and microseconds:

  * when shifting by 1 year and 2 months the date is actually shifted by 14 months
  * weeks, days and smaller units are collapsed into seconds and microseconds

  When shifting by month, days are rounded down to the nearest valid date.

  ## Examples

      iex> DateTime.shift(~U[2016-01-01 00:00:00Z], month: 2)
      ~U[2016-03-01 00:00:00Z]
      iex> DateTime.shift(~U[2016-01-01 00:00:00Z], year: 1, week: 4)
      ~U[2017-01-29 00:00:00Z]
      iex> DateTime.shift(~U[2016-01-01 00:00:00Z], minute: -25)
      ~U[2015-12-31 23:35:00Z]
      iex> DateTime.shift(~U[2016-01-01 00:00:00Z], minute: 5, microsecond: {500, 4})
      ~U[2016-01-01 00:05:00.0005Z]

      # leap years
      iex> DateTime.shift(~U[2024-02-29 00:00:00Z], year: 1)
      ~U[2025-02-28 00:00:00Z]
      iex> DateTime.shift(~U[2024-02-29 00:00:00Z], year: 4)
      ~U[2028-02-29 00:00:00Z]

      # rounding down
      iex> DateTime.shift(~U[2015-01-31 00:00:00Z], month: 1)
      ~U[2015-02-28 00:00:00Z]

  """
  @doc since: "1.17.0"
  @spec shift(Calendar.datetime(), Duration.duration(), Calendar.time_zone_database()) :: t
  def shift(datetime, duration, time_zone_database \\ Calendar.get_time_zone_database())

  def shift(%{calendar: calendar, time_zone: "Etc/UTC"} = datetime, duration, _time_zone_database) do
    %{
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond
    } = datetime

    {year, month, day, hour, minute, second, microsecond} =
      calendar.shift_naive_datetime(
        year,
        month,
        day,
        hour,
        minute,
        second,
        microsecond,
        __duration__!(duration)
      )

    %DateTime{
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond,
      time_zone: "Etc/UTC",
      zone_abbr: "UTC",
      std_offset: 0,
      utc_offset: 0
    }
  end

  def shift(%{calendar: calendar} = datetime, duration, time_zone_database) do
    %{
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond,
      std_offset: std_offset,
      utc_offset: utc_offset,
      time_zone: time_zone
    } = datetime

    {year, month, day, hour, minute, second, {_, precision} = microsecond} =
      calendar.shift_naive_datetime(
        year,
        month,
        day,
        hour,
        minute,
        second,
        microsecond,
        __duration__!(duration)
      )

    result =
      calendar.naive_datetime_to_iso_days(year, month, day, hour, minute, second, microsecond)
      |> apply_tz_offset(utc_offset + std_offset)
      |> shift_zone_for_iso_days_utc(calendar, precision, time_zone, time_zone_database)

    case result do
      {:ok, result_datetime} ->
        result_datetime

      {:error, error} ->
        raise ArgumentError,
              "cannot shift #{inspect(datetime)} to #{inspect(duration)} (with time zone " <>
                "database #{inspect(time_zone_database)}), reason: #{inspect(error)}"
    end
  end

  @doc false
  defdelegate __duration__!(params), to: Duration, as: :new!

  @doc """
  Returns the given datetime with the microsecond field truncated to the given
  precision (`:microsecond`, `:millisecond` or `:second`).

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

  defp seconds_from_day_fraction({parts_in_day, @seconds_per_day}),
    do: parts_in_day

  defp seconds_from_day_fraction({parts_in_day, parts_per_day}),
    do: div(parts_in_day * @seconds_per_day, parts_per_day)

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
    def inspect(datetime, _) do
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
        std_offset: std_offset,
        calendar: calendar
      } = datetime

      formatted =
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

      case datetime do
        %{utc_offset: 0, std_offset: 0, time_zone: "Etc/UTC", year: year}
        when calendar != Calendar.ISO or year in -9999..9999 ->
          "~U[" <> formatted <> suffix(calendar) <> "]"

        _ ->
          "#DateTime<" <> formatted <> suffix(calendar) <> ">"
      end
    end

    defp suffix(Calendar.ISO), do: ""
    defp suffix(calendar), do: " " <> inspect(calendar)
  end
end
