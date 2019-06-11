defmodule Calendar do
  @moduledoc """
  This module defines the responsibilities for working with
  calendars, dates, times and datetimes in Elixir.

  Currently it defines types and the minimal implementation
  for a calendar behaviour in Elixir. The goal of the Calendar
  features in Elixir is to provide a base for interoperability
  instead of full-featured datetime API.

  For the actual date, time and datetime structures, see `Date`,
  `Time`, `NaiveDateTime` and `DateTime`.

  Note the year, month, day, etc. designations are overspecified
  (i.e. an integer instead of `1..12` for months) because different
  calendars may have a different number of days per month, months per year and so on.
  """

  @type year :: integer
  @type month :: pos_integer
  @type day :: pos_integer
  @type week :: pos_integer
  @type day_of_week :: non_neg_integer
  @type era :: non_neg_integer

  @type hour :: non_neg_integer
  @type minute :: non_neg_integer
  @type second :: non_neg_integer

  @typedoc """
  The internal time format is used when converting between calendars.

  It represents time as a fraction of a day (starting from midnight).
  `parts_in_day` specifies how much of the day is already passed,
  while `parts_per_day` signifies how many parts there fit in a day.
  """
  @type day_fraction :: {parts_in_day :: non_neg_integer, parts_per_day :: pos_integer}

  @typedoc """
  The internal date format that is used when converting between calendars.

  This is the number of days including the fractional part that has passed of
  the last day since 0000-01-01+00:00T00:00.000000 in ISO 8601 notation (also
  known as midnight 1 January BC 1 of the proleptic Gregorian calendar).
  """
  @type iso_days :: {days :: integer, day_fraction}

  @typedoc """
  Microseconds with stored precision.

  The precision represents the number of digits that must be used when
  representing the microseconds to external format. If the precision is 0,
  it means microseconds must be skipped.
  """
  @type microsecond :: {0..999_999, 0..6}

  @typedoc "A calendar implementation"
  @type calendar :: module

  @typedoc "The time zone ID according to the IANA tz database (e.g. Europe/Zurich)"
  @type time_zone :: String.t()

  @typedoc "The time zone abbreviation (e.g. CET or CEST or BST etc.)"
  @type zone_abbr :: String.t()

  @typedoc "The time zone UTC offset in seconds"
  @type utc_offset :: integer

  @typedoc "The time zone standard offset in seconds (not zero in summer times)"
  @type std_offset :: integer

  @typedoc "Any map/struct that contains the date fields"
  @type date :: %{optional(any) => any, calendar: calendar, year: year, month: month, day: day}

  @typedoc "Any map/struct that contains the time fields"
  @type time :: %{
          optional(any) => any,
          hour: hour,
          minute: minute,
          second: second,
          microsecond: microsecond
        }

  @typedoc "Any map/struct that contains the naive_datetime fields"
  @type naive_datetime :: %{
          optional(any) => any,
          calendar: calendar,
          year: year,
          month: month,
          day: day,
          hour: hour,
          minute: minute,
          second: second,
          microsecond: microsecond
        }

  @typedoc "Any map/struct that contains the datetime fields"
  @type datetime :: %{
          optional(any) => any,
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

  @typedoc """
  Specifies the time zone database for calendar operations.

  Many functions in the `DateTime` module require a time zone database.
  By default, it uses the default time zone database returned by
  `Calendar.get_time_zone_database/0`, which defaults to
  `Calendar.UTCOnlyTimeZoneDatabase` which only handles "Etc/UTC"
  datetimes and returns `{:error, :utc_only_time_zone_database}`
  for any other time zone.

  Other time zone databases (including ones provided by packages)
  can be configure as default either via configuration:

      config :elixir, :time_zone_database, CustomTimeZoneDatabase

  or by calling `Calendar.put_time_zone_database/1`.

  See `Calendar.TimeZoneDatabase` for more information on custom
  time zone databases.
  """
  @type time_zone_database :: module()

  @doc """
  Returns how many days there are in the given year-month.
  """
  @callback days_in_month(year, month) :: day

  @doc """
  Returns how many months there are in the given year.
  """
  @callback months_in_year(year) :: month

  @doc """
  Returns `true` if the given year is a leap year.

  A leap year is a year of a longer length than normal. The exact meaning
  is up to the calendar. A calendar must return `false` if it does not support
  the concept of leap years.
  """
  @callback leap_year?(year) :: boolean

  @doc """
  Calculates the day of the week from the given `year`, `month`, and `day`.
  """
  @callback day_of_week(year, month, day) :: day_of_week()

  @doc """
  Calculates the day of the year from the given `year`, `month`, and `day`.
  """
  @callback day_of_year(year, month, day) :: non_neg_integer()

  @doc """
  Calculates the quarter of the year from the given `year`, `month`, and `day`.
  """
  @callback quarter_of_year(year, month, day) :: non_neg_integer()

  @doc """
  Calculates the year and era from the given `year`.
  """
  @callback year_of_era(year) :: {year, era}

  @doc """
  Calculates the day and era from the given `year`, `month`, and `day`.
  """
  @callback day_of_era(year, month, day) :: {non_neg_integer(), era}

  @doc """
  Converts the date into a string according to the calendar.
  """
  @callback date_to_string(year, month, day) :: String.t()

  @doc """
  Converts the datetime (without time zone) into a string according to the calendar.
  """
  @callback naive_datetime_to_string(year, month, day, hour, minute, second, microsecond) ::
              String.t()

  @doc """
  Converts the datetime (with time zone) into a string according to the calendar.
  """
  @callback datetime_to_string(
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
            ) :: String.t()

  @doc """
  Converts the time into a string according to the calendar.
  """
  @callback time_to_string(hour, minute, second, microsecond) :: String.t()

  @doc """
  Converts the given datetime (without time zone) into the `t:iso_days/0` format.
  """
  @callback naive_datetime_to_iso_days(year, month, day, hour, minute, second, microsecond) ::
              iso_days

  @doc """
  Converts `t:iso_days/0` to the Calendar's datetime format.
  """
  @callback naive_datetime_from_iso_days(iso_days) ::
              {year, month, day, hour, minute, second, microsecond}

  @doc """
  Converts the given time to the `t:day_fraction/0` format.
  """
  @callback time_to_day_fraction(hour, minute, second, microsecond) :: day_fraction

  @doc """
  Converts `t:day_fraction/0` to the Calendar's time format.
  """
  @callback time_from_day_fraction(day_fraction) :: {hour, minute, second, microsecond}

  @doc """
  Define the rollover moment for the given calendar.

  This is the moment, in your calendar, when the current day ends
  and the next day starts.

  The result of this function is used to check if two calendars rollover at
  the same time of day. If they do not, we can only convert datetimes and times
  between them. If they do, this means that we can also convert dates as well
  as naive datetimes between them.

  This day fraction should be in its most simplified form possible, to make comparisons fast.

  ## Examples

    * If, in your Calendar, a new day starts at midnight, return {0, 1}.
    * If, in your Calendar, a new day starts at sunrise, return {1, 4}.
    * If, in your Calendar, a new day starts at noon, return {1, 2}.
    * If, in your Calendar, a new day starts at sunset, return {3, 4}.

  """
  @callback day_rollover_relative_to_midnight_utc() :: day_fraction

  @doc """
  Should return `true` if the given date describes a proper date in the calendar.
  """
  @callback valid_date?(year, month, day) :: boolean

  @doc """
  Should return `true` if the given time describes a proper time in the calendar.
  """
  @callback valid_time?(hour, minute, second, microsecond) :: boolean

  @doc """
  Implements inspect for times, dates, datetimes, and "naive" datetimes.

  The implementation must define inspect for `Date.t()`, `DateTime.t()` and
  `NaiveDateTime.t()` structs.
  """
  @callback inspect(Time.t() | Date.t() | DateTime.t() | MaiveDateTime.t(), Inspect.Opts.t()) ::
              String.t()

  # General Helpers

  @doc """
  Returns `true` if two calendars have the same moment of starting a new day,
  `false` otherwise.

  If two calendars are not compatible, we can only convert datetimes and times
  between them. If they are compatible, this means that we can also convert
  dates as well as naive datetimes between them.
  """
  @doc since: "1.5.0"
  @spec compatible_calendars?(Calendar.calendar(), Calendar.calendar()) :: boolean
  def compatible_calendars?(calendar, calendar), do: true

  def compatible_calendars?(calendar1, calendar2) do
    calendar1.day_rollover_relative_to_midnight_utc() ==
      calendar2.day_rollover_relative_to_midnight_utc()
  end

  @doc """
  Returns a microsecond tuple truncated to a given precision (`:microsecond`,
  `:millisecond` or `:second`).
  """
  @doc since: "1.6.0"
  @spec truncate(Calendar.microsecond(), :microsecond | :millisecond | :second) ::
          Calendar.microsecond()
  def truncate(microsecond_tuple, :microsecond), do: microsecond_tuple

  def truncate({microsecond, precision}, :millisecond) do
    output_precision = min(precision, 3)
    {div(microsecond, 1000) * 1000, output_precision}
  end

  def truncate(_, :second), do: {0, 0}

  @doc """
  Sets the current time zone database.
  """
  @doc since: "1.8.0"
  @spec put_time_zone_database(time_zone_database()) :: :ok
  def put_time_zone_database(database) do
    Application.put_env(:elixir, :time_zone_database, database)
  end

  @doc """
  Gets the current time zone database.
  """
  @doc since: "1.8.0"
  @spec get_time_zone_database() :: time_zone_database()
  def get_time_zone_database() do
    Application.get_env(:elixir, :time_zone_database, Calendar.UTCOnlyTimeZoneDatabase)
  end
end
