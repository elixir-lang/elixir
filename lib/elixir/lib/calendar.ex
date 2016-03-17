defmodule Calendar do
  @moduledoc """
  This module defines the responsibilities for working with
  calendars, dates, times and datetimes in Elixir.

  Currently it defines types but may define the calendar
  behaviour in future versions. For the actual date, time
  and datetime structures, see `Date`, `Time`, `NaiveDateTime`
  and `DateTime`.

  Note the year, month, day, etc designations are over specified
  (i.e. an integer instead of 1..12 for months) because different
  calendars may have a different number of numbers and so on.
  """

  @type year        :: integer
  @type month       :: integer
  @type day         :: integer
  @type hour        :: integer
  @type minute      :: integer
  @type second      :: integer
  @type microsecond :: integer

  @typedoc "A calendar implementation"
  @type calendar    :: module

  @typedoc "The timezone ID according to Unicode's CLDR"
  @type time_zone   :: String.t

  @typedoc "The timezone abbreviation"
  @type zone_abbr   :: String.t

  @typedoc "The timezone UTC offset in seconds"
  @type utc_offset  :: integer

  @typedoc "The timezone standard offset in seconds (not zero in summer times)"
  @type std_offset  :: integer

  @doc false
  # TODO: Remove this on 1.4. It exists only to aid migration of those
  # using the Calendar library.
  defmacro __using__(_opts) do
    %{file: file, line: line} = __CALLER__
    :elixir_errors.warn(line, file, "use Calendar is deprecated as it is now part of Elixir")

    quote do
      alias Calendar.DateTime
      alias Calendar.DateTime.Interval
      alias Calendar.AmbiguousDateTime
      alias Calendar.NaiveDateTime
      alias Calendar.Date
      alias Calendar.Time
      alias Calendar.TimeZoneData
      alias Calendar.TzPeriod
      alias Calendar.Strftime
    end
  end
end

defmodule Date do
  @moduledoc """
  A date implementation.
  """
  defstruct [:year, :month, :day, calendar: Calendar.ISO]
  @type t :: %__MODULE__{year: Calendar.year, month: Calendar.month,
                         day: Calendar.day, calendar: Calendar.calendar}
end

defmodule Time do
  @moduledoc """
  A time implementation.
  """
  defstruct [:hour, :minute, :second, :microsecond]
  @type t :: %__MODULE__{hour: Calendar.hour, minute: Calendar.minute,
                         second: Calendar.second, microsecond: Calendar.microsecond}
end

defmodule NaiveDateTime do
  @moduledoc """
  A naive datetime implementation (without a time zone).

  The naive bit implies this datetime representation does
  not have a timezone. This means the datetime may not
  actually exist in certain areas in the world even though
  it is valid.

  For example, when daylight saving changes are applied
  by a region, the clock typically moves forward or backward
  by one hour. This means certain datetimes never occur or
  may occur more than once. Since `NaiveDateTime` is not
  validated against a timezone, such errors would go unnoticed.
  """
  defstruct [:year, :month, :day, :hour, :minute, :second, :microsecond, calendar: Calendar.ISO]
  @type t :: %__MODULE__{year: Calendar.year, month: Calendar.month, day: Calendar.day,
                         calendar: Calendar.calendar, hour: Calendar.hour, minute: Calendar.minute,
                         second: Calendar.second, microsecond: Calendar.microsecond}
end

defmodule DateTime do
  @moduledoc """
  A datetime implementation with a time zone.

  This datetime can be seen as a ephemeral snapshot
  of a datetime at a given timezone. For such purposes,
  it also includes both UTC and Standard offsets, as
  well as the zone abbreviation field used exclusively
  for formatting purposes.
  """
  defstruct [:year, :month, :day, :hour, :minute, :second, :microsecond,
             :time_zone, :zone_abbr, :utc_offset, :std_offset, calendar: Calendar.ISO]
  @type t :: %__MODULE__{year: Calendar.year, month: Calendar.month, day: Calendar.day,
                         calendar: Calendar.calendar, hour: Calendar.hour, minute: Calendar.minute,
                         second: Calendar.second, microsecond: Calendar.microsecond,
                         time_zone: Calendar.time_zone, zone_abbr: Calendar.zone_abbr,
                         utc_offset: Calendar.utc_offset, std_offset: Calendar.std_offset}
end
