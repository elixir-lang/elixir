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

  @typedoc "The time zone ID according to the IANA tz database (e.g. Europe/Zurich)"
  @type time_zone   :: String.t

  @typedoc "The time zone abbreviation (e.g. CET or CEST or BST etc.)"
  @type zone_abbr   :: String.t

  @typedoc "The time zone UTC offset in seconds"
  @type utc_offset  :: integer

  @typedoc "The time zone standard offset in seconds (not zero in summer times)"
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

  @doc """
  Builds a new ISO date.

  Expects all values to be integers. Returns `{:ok, time}` if each
  entry fits its appropriate range, returns `:error` otherwise.

  ## Examples

      iex> Date.new(2000, 1, 1)
      {:ok, %Date{year: 2000, month: 1, day: 1}}
      iex> Date.new(2000, 13, 1)
      {:error, :invalid_date}
      iex> Date.new(2000, 2, 29)
      {:ok, %Date{year: 2000, month: 2, day: 29}}
      iex> Date.new(2000, 2, 30)
      {:error, :invalid_date}
      iex> Date.new(2001, 2, 29)
      {:error, :invalid_date}
  """
  @spec new(integer, integer, integer) :: {:ok, Date.t} | {:error, atom}
  def new(year, month, day) when is_integer(year) and is_integer(month) and is_integer(day) do
    if Calendar.ISO.valid_date?(year, month, day) do
      {:ok, %Date{year: year, month: month, day: day}}
    else
      {:error, :invalid_date}
    end
  end
end

defmodule Time do
  @moduledoc """
  A time implementation.
  """
  defstruct [:hour, :minute, :second, :microsecond]
  @type t :: %__MODULE__{hour: Calendar.hour, minute: Calendar.minute,
                         second: Calendar.second, microsecond: Calendar.microsecond}

  @doc """
  Builds a new time.

  Expects all values to be integers. Returns `{:ok, time}` if each
  entry fits its appropriate range, returns `{:error, reason}` otherwise.

  Note a time may have 60 seconds in case of leap seconds.

  ## Examples

      iex> Time.new(0, 0, 0, 0)
      {:ok, %Time{hour: 0, minute: 0, second: 0, microsecond: 0}}
      iex> Time.new(23, 59, 59, 999_999)
      {:ok, %Time{hour: 23, minute: 59, second: 59, microsecond: 999_999}}
      iex> Time.new(23, 59, 50, 999_999)
      {:ok, %Time{hour: 23, minute: 59, second: 50, microsecond: 999_999}}
      iex> Time.new(24, 59, 59, 999_999)
      {:error, :invalid_time}
      iex> Time.new(23, 60, 59, 999_999)
      {:error, :invalid_time}
      iex> Time.new(23, 59, 61, 999_999)
      {:error, :invalid_time}
      iex> Time.new(23, 59, 59, 1_000_000)
      {:error, :invalid_time}

  """
  @spec new(integer, integer, integer, integer) :: {:ok, Time.t} | {:error, atom}
  def new(hour, minute, second, microsecond \\ 0)
      when is_integer(hour) and is_integer(minute) and is_integer(second) and is_integer(microsecond) do
    if hour in 0..23 and minute in 0..59 and second in 0..60 and microsecond in 0..999_999 do
      {:ok, %Time{hour: hour, minute: minute, second: second, microsecond: microsecond}}
    else
      {:error, :invalid_time}
    end
  end
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

  @doc """
  Builds a new ISO naive date time.

  Expects all values to be integers. Returns `{:ok, naive_date_time}`
  if each entry fits its appropriate range, returns `{:error, reason}`
  otherwise.

  ## Examples

      iex> NaiveDateTime.new(2000, 1, 1, 0, 0, 0)
      {:ok, %NaiveDateTime{year: 2000, month: 1, day: 1, hour: 0, minute: 0, second: 0, microsecond: 0}}
      iex> NaiveDateTime.new(2000, 13, 1, 0, 0, 0)
      {:error, :invalid_date}
      iex> NaiveDateTime.new(2000, 2, 29, 0, 0, 0)
      {:ok, %NaiveDateTime{year: 2000, month: 2, day: 29, hour: 0, minute: 0, second: 0, microsecond: 0}}
      iex> NaiveDateTime.new(2000, 2, 30, 0, 0, 0)
      {:error, :invalid_date}
      iex> NaiveDateTime.new(2001, 2, 29, 0, 0, 0)
      {:error, :invalid_date}

      iex> NaiveDateTime.new(2000, 1, 1, 23, 59, 59, 999_999)
      {:ok, %NaiveDateTime{year: 2000, month: 1, day: 1, hour: 23, minute: 59, second: 59, microsecond: 999_999}}
      iex> NaiveDateTime.new(2000, 1, 1, 23, 59, 60, 999_999)
      {:ok, %NaiveDateTime{year: 2000, month: 1, day: 1, hour: 23, minute: 59, second: 60, microsecond: 999_999}}
      iex> NaiveDateTime.new(2000, 1, 1, 24, 59, 59, 999_999)
      {:error, :invalid_time}
      iex> NaiveDateTime.new(2000, 1, 1, 23, 60, 59, 999_999)
      {:error, :invalid_time}
      iex> NaiveDateTime.new(2000, 1, 1, 23, 59, 61, 999_999)
      {:error, :invalid_time}
      iex> NaiveDateTime.new(2000, 1, 1, 23, 59, 59, 1_000_000)
      {:error, :invalid_time}
  """
  @spec new(integer, integer, integer, integer, integer, integer, integer) ::
        {:ok, NaiveDateTime.t} | {:error, atom}
  def new(year, month, day, hour, minute, second, microsecond \\ 0)
      when is_integer(year) and is_integer(month) and is_integer(day) and
           is_integer(hour) and is_integer(minute) and is_integer(second) and is_integer(microsecond) do
    cond do
      not(Calendar.ISO.valid_date?(year, month, day)) ->
        {:error, :invalid_date}
      not(hour in 0..23 and minute in 0..59 and second in 0..60 and microsecond in 0..999_999) ->
        {:error, :invalid_time}
      true ->
        {:ok, %NaiveDateTime{year: year, month: month, day: day,
                             hour: hour, minute: minute, second: second, microsecond: microsecond}}
    end
  end
end

defmodule DateTime do
  @moduledoc """
  A datetime implementation with a time zone.

  This datetime can be seen as an ephemeral snapshot
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
