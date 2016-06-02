defmodule Calendar do
  @moduledoc """
  This module defines the responsibilities for working with
  calendars, dates, times and datetimes in Elixir.

  Currently it defines types and the minimal implementation
  for a calendar behaviour in Elixir. The goal of the Calendar
  features in Elixir is to provide a base for interoperatibility
  instead of full-featured datetime API.

  For the actual date, time and datetime structures, see `Date`,
  `Time`, `NaiveDateTime` and `DateTime`.

  Note the year, month, day, etc designations are overspecified
  (i.e. an integer instead of 1..12 for months) because different
  calendars may have a different number of days per month, months per year and so on.
  """

  @type year        :: integer
  @type month       :: integer
  @type day         :: integer
  @type hour        :: 0..23
  @type minute      :: 0..59

  @typedoc "From 0 to 60 to account for leap seconds"
  @type second      :: 0..60

  @typedoc """
  Microseconds with stored precision.

  The precision represents the number of digits
  that must be used when representing the microseconds
  to external format. If the precision is 0, it means
  microseconds must be skipped.
  """
  @type microsecond :: {0..999_999, 0..6}

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

  @doc """
  Builds a new date from proleptic year, month and day of month.
  """
  @callback date(year, month, day) :: {:ok, Date.t} | {:error, atom}

  @doc """
  Returns true if the given year is a leap year.

  A leap year is a year of a longer length than normal. The exact meaning
  is up to the calendar. A calendar must return `false` if it does not support
  the concept of leap years.
  """
  @callback leap_year?(year) :: boolean

  @doc """
  Converts the given structure into a string according to the calendar.
  """
  @callback to_string(Date.t | NaiveDateTime.t | DateTime.t) :: String.t

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
  A Date struct and functions.

  The Date struct contains the fields year, month, day and calendar.
  New dates can be built with the `new/3` function or using the `~D`
  sigil:

      iex> ~D[2000-01-01]
      ~D[2000-01-01]

  Both `new/3` and sigil return a struct where the date fields can
  be accessed directly:

      iex> date = ~D[2000-01-01]
      iex> date.year
      2000
      iex> date.month
      1

  Developers should avoid creating the Date struct directly and
  instead rely on the functions provided by this module as well as
  the ones in 3rd party calendar libraries.
  """

  @enforce_keys [:year, :month, :day]
  defstruct [:year, :month, :day, calendar: Calendar.ISO]

  @type t :: %__MODULE__{year: Calendar.year, month: Calendar.month,
                         day: Calendar.day, calendar: Calendar.calendar}

  @doc """
  Builds a new ISO date.

  Expects all values to be integers. Returns `{:ok, time}` if each
  entry fits its appropriate range, returns `:error` otherwise.

  ## Examples

      iex> Date.new(2000, 1, 1)
      {:ok, ~D[2000-01-01]}
      iex> Date.new(2000, 13, 1)
      {:error, :invalid_date}
      iex> Date.new(2000, 2, 29)
      {:ok, ~D[2000-02-29]}

      iex> Date.new(2000, 2, 30)
      {:error, :invalid_date}
      iex> Date.new(2001, 2, 29)
      {:error, :invalid_date}

  """
  @spec new(Calendar.year, Calendar.month, Calendar.day) :: {:ok, Date.t} | {:error, atom}
  def new(year, month, day) do
    Calendar.ISO.date(year, month, day)
  end

  @doc """
  Converts the given date to a string according to its calendar.

  ### Examples

      iex> Date.to_string(~D[2000-02-28])
      "2000-02-28"

  """
  @spec to_string(Date.t) :: String.t
  def to_string(%Date{calendar: calendar} = date) do
    calendar.to_string(date)
  end

  @doc """
  Parses the extended "Date and time of day" format described by ISO8601:2004.

  Timezone offset may be included in the string but they will be
  simply discarded as such information is not included in naive date
  times.

  Time representations with reduced accuracy are not supported.

  ## Examples

      iex> Date.from_iso8601("2015-01-23")
      {:ok, ~D[2015-01-23]}

      iex> Date.from_iso8601("2015:01:23")
      {:error, :invalid_format}
      iex> Date.from_iso8601("2015-01-32")
      {:error, :invalid_date}

  """
  @spec from_iso8601(String.t) :: {:ok, Date.t} | {:error, atom}
  def from_iso8601(<<year::4-bytes, ?-, month::2-bytes, ?-, day::2-bytes>>) do
    with {year, ""}  <- Integer.parse(year),
         {month, ""} <- Integer.parse(month),
         {day, ""}   <- Integer.parse(day) do
      new(year, month, day)
    else
      _ -> {:error, :invalid_format}
    end
  end

  def from_iso8601(<<_::binary>>) do
    {:error, :invalid_format}
  end

  @doc """
  Parses the extended "Date and time of day" format described by ISO8601:2004.

  Raises if the format is invalid.

  ## Examples

      iex> Date.from_iso8601!("2015-01-23")
      ~D[2015-01-23]
      iex> Date.from_iso8601!("2015:01:23")
      ** (ArgumentError) cannot parse "2015:01:23" as date, reason: :invalid_format
  """
  @spec from_iso8601!(String.t) :: Date.t | no_return
  def from_iso8601!(string) do
    case from_iso8601(string) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot parse #{inspect string} as date, reason: #{inspect reason}"
    end
  end

  @doc """
  Converts the given date time to ISO8601.

  Only supports converting date times which are in the ISO calendar,
  attempting to convert date times from other calendars will raise.

  ### Examples

      iex> Date.to_iso8601(~D[2000-02-28])
      "2000-02-28"

  """
  @spec to_iso8601(Date.t) :: String.t
  def to_iso8601(%Date{calendar: Calendar.ISO} = date) do
    Calendar.ISO.to_iso8601(date)
  end

  @doc """
  Converts a `Date` struct to an Erlang date tuple.

  Only supports converting dates which are in the ISO calendar,
  attempting to convert dates from other calendars will raise.

  ## Examples

      iex> Date.to_erl(~D[2000-01-01])
      {2000, 1, 1}

  """
  @spec to_erl(Date.t) :: :calendar.date()
  def to_erl(%Date{calendar: Calendar.ISO, year: year, month: month, day: day}) do
    {year, month, day}
  end

  @doc """
  Converts an Erlang date tuple to a `Date` struct.

  Attempting to convert an invalid ISO calendar date will produce an error tuple.

  ## Examples

      iex> Date.from_erl({2000, 1, 1})
      {:ok, ~D[2000-01-01]}
      iex> Date.from_erl({2000, 13, 1})
      {:error, :invalid_date}
  """
  @spec from_erl(:calendar.date()) :: {:ok, Date.t} | {:error, atom}
  def from_erl({year, month, day}) do
    new(year, month, day)
  end

  @doc """
  Converts an Erlang date tuple but raises for invalid dates.

  ## Examples

      iex> Date.from_erl!({2000, 1, 1})
      ~D[2000-01-01]
      iex> Date.from_erl!({2000, 13, 1})
      ** (ArgumentError) cannot convert {2000, 13, 1} to date, reason: :invalid_date
  """
  @spec from_erl!(:calendar.date()) :: Date.t | no_return
  def from_erl!(tuple) do
    case from_erl(tuple) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot convert #{inspect tuple} to date, reason: #{inspect reason}"
    end
  end

  defimpl String.Chars do
    def to_string(%Date{calendar: calendar} = date) do
      calendar.to_string(date)
    end
  end

  defimpl Inspect do
    def inspect(%Date{calendar: Calendar.ISO} = date, _) do
      "~D[" <> Calendar.ISO.to_string(date) <> "]"
    end

    def inspect(date, opts) do
      Inspect.Any.inspect(date, opts)
    end
  end
end

defmodule Time do
  @moduledoc """
  A Time struct and functions.

  The Time struct contains the fields hour, minute, second and microseconds.
  New times can be built with the `new/4` function or using the `~T`
  sigil:

      iex> ~T[23:00:07.001]
      ~T[23:00:07.001]

  Both `new/4` and sigil return a struct where the time fields can
  be accessed directly:

      iex> time = ~T[23:00:07.001]
      iex> time.hour
      23
      iex> time.microsecond
      {1000, 3}

  Developers should avoid creating the Time struct directly and
  instead rely on the functions provided by this module as well as
  the ones in 3rd party calendar libraries.
  """

  @enforce_keys [:hour, :minute, :second, :microsecond]
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
      {:ok, ~T[00:00:00.000000]}
      iex> Time.new(23, 59, 59, 999_999)
      {:ok, ~T[23:59:59.999999]}
      iex> Time.new(23, 59, 60, 999_999)
      {:ok, ~T[23:59:60.999999]}

      # Time with microseconds and their precision
      iex> Time.new(23, 59, 60, {10_000, 2})
      {:ok, ~T[23:59:60.01]}

      iex> Time.new(24, 59, 59, 999_999)
      {:error, :invalid_time}
      iex> Time.new(23, 60, 59, 999_999)
      {:error, :invalid_time}
      iex> Time.new(23, 59, 61, 999_999)
      {:error, :invalid_time}
      iex> Time.new(23, 59, 59, 1_000_000)
      {:error, :invalid_time}

  """
  @spec new(Calendar.hour, Calendar.minute, Calendar.second, Calendar.microsecond) ::
        {:ok, Time.t} | {:error, atom}
  def new(hour, minute, second, microsecond \\ {0, 0})

  def new(hour, minute, second, microsecond) when is_integer(microsecond) do
    new(hour, minute, second, {microsecond, 6})
  end

  def new(hour, minute, second, {microsecond, precision})
      when is_integer(hour) and is_integer(minute) and is_integer(second) and
           is_integer(microsecond) and is_integer(precision) do
    if hour in 0..23 and minute in 0..59 and second in 0..60 and
       microsecond in 0..999_999 and precision in 0..6 do
      {:ok, %Time{hour: hour, minute: minute, second: second, microsecond: {microsecond, precision}}}
    else
      {:error, :invalid_time}
    end
  end

  @doc """
  Converts the given time to a string.

  ### Examples

      iex> Time.to_string(~T[23:00:00])
      "23:00:00"
      iex> Time.to_string(~T[23:00:00.001])
      "23:00:00.001"
      iex> Time.to_string(~T[23:00:00.123456])
      "23:00:00.123456"

  """
  @spec to_string(Time.t) :: String.t
  def to_string(%Time{} = time) do
    Calendar.ISO.to_string(time)
  end

  @doc """
  Parses the extended "Local time" format described by ISO8601:2004.

  Timezone offset may be included in the string but they will be
  simply discarded as such information is not included in times.

  As specified in the standard, the separator "T" may be omitted if
  desired as there is no ambiguity within this function.

  Time representations with reduced accuracy are not supported.

  ## Examples

      iex> Time.from_iso8601("23:50:07")
      {:ok, ~T[23:50:07]}
      iex> Time.from_iso8601("23:50:07Z")
      {:ok, ~T[23:50:07]}
      iex> Time.from_iso8601("T23:50:07Z")
      {:ok, ~T[23:50:07]}

      iex> Time.from_iso8601("23:50:07.0123456")
      {:ok, ~T[23:50:07.012345]}
      iex> Time.from_iso8601("23:50:07.123Z")
      {:ok, ~T[23:50:07.123]}

      iex> Time.from_iso8601("2015:01:23 23-50-07")
      {:error, :invalid_format}
      iex> Time.from_iso8601("23:50:07A")
      {:error, :invalid_format}
      iex> Time.from_iso8601("23:50:07.")
      {:error, :invalid_format}
      iex> Time.from_iso8601("23:50:61")
      {:error, :invalid_time}

  """
  @spec from_iso8601(String.t) :: {:ok, Time.t} | {:error, atom}
  def from_iso8601(<<?T, h, rest::binary>>) when h in ?0..?9 do
    from_iso8601(<<h, rest::binary>>)
  end

  def from_iso8601(<<hour::2-bytes, ?:, min::2-bytes, ?:, sec::2-bytes, rest::binary>>) do
    with {hour, ""}       <- Integer.parse(hour),
         {min, ""}        <- Integer.parse(min),
         {sec, ""}        <- Integer.parse(sec),
         {microsec, rest} <- Calendar.ISO.parse_microsecond(rest),
         {_offset, ""}    <- Calendar.ISO.parse_offset(rest) do
      new(hour, min, sec, microsec)
    else
      _ -> {:error, :invalid_format}
    end
  end
  def from_iso8601(<<_::binary>>) do
    {:error, :invalid_format}
  end

  @doc """
  Parses the extended "Local time" format described by ISO8601:2004.

  Raises if the format is invalid.

  ## Examples

      iex> Time.from_iso8601!("23:50:07.123Z")
      ~T[23:50:07.123]
      iex> Time.from_iso8601!("2015:01:23 23-50-07")
      ** (ArgumentError) cannot parse "2015:01:23 23-50-07" as time, reason: :invalid_format
  """
  @spec from_iso8601!(String.t) :: Time.t | no_return
  def from_iso8601!(string) do
    case from_iso8601(string) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot parse #{inspect string} as time, reason: #{inspect reason}"
    end
  end

  @doc """
  Converts the given time to ISO8601.

  ### Examples

      iex> Time.to_iso8601(~T[23:00:13])
      "23:00:13"

      iex> Time.to_iso8601(~T[23:00:13.001])
      "23:00:13.001"

  """
  @spec to_iso8601(Time.t) :: String.t
  def to_iso8601(%Time{} = time) do
    Calendar.ISO.to_iso8601(time)
  end

  @doc """
  Converts a `Time` struct to an Erlang time tuple.

  WARNING: Loss of precision may occur, as Erlang time tuples
  only contain hours/minutes/seconds.

  ## Examples

      iex> Time.to_erl(~T[23:30:15.999])
      {23, 30, 15}

  """
  @spec to_erl(Time.t) :: :calendar.time()
  def to_erl(%Time{hour: hour, minute: minute, second: second}) do
    {hour, minute, second}
  end

  @doc """
  Converts an Erlang time tuple to a `Time` struct.

  ## Examples

      iex> Time.from_erl({23, 30, 15}, {5000, 3})
      {:ok, ~T[23:30:15.005]}
      iex> Time.from_erl({24, 30, 15})
      {:error, :invalid_time}
  """
  @spec from_erl(:calendar.time(), Calendar.microsecond) :: {:ok, Time.t} | {:error, atom}
  def from_erl({hour, minute, second}, microsecond \\ {0, 0}) do
    new(hour, minute, second, microsecond)
  end

  @doc """
  Converts an Erlang time tuple to a `Time` struct.

  ## Examples

      iex> Time.from_erl!({23, 30, 15})
      ~T[23:30:15]
      iex> Time.from_erl!({23, 30, 15}, {5000, 3})
      ~T[23:30:15.005]
      iex> Time.from_erl!({24, 30, 15})
      ** (ArgumentError) cannot convert {24, 30, 15} to time, reason: :invalid_time
  """
  @spec from_erl!(:calendar.time(), Calendar.microsecond) :: Time.t | no_return
  def from_erl!(tuple, microsecond \\ {0, 0}) do
    case from_erl(tuple, microsecond) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot convert #{inspect tuple} to time, reason: #{inspect reason}"
    end
  end

  defimpl String.Chars do
    def to_string(time) do
      Calendar.ISO.to_string(time)
    end
  end

  defimpl Inspect do
    def inspect(time, _) do
      "~T[" <> Calendar.ISO.to_string(time) <> "]"
    end
  end
end

defmodule NaiveDateTime do
  @moduledoc """
  A NaiveDateTime struct (without a time zone) and functions.

  The NaiveDateTime struct contains the fields year, month, day, hour,
  minute, second, microsecond and calendar. New naive date times can be
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
  not have a timezone. This means the datetime may not
  actually exist in certain areas in the world even though
  it is valid.

  For example, when daylight saving changes are applied
  by a region, the clock typically moves forward or backward
  by one hour. This means certain datetimes never occur or
  may occur more than once. Since `NaiveDateTime` is not
  validated against a timezone, such errors would go unnoticed.

  Developers should avoid creating the NaiveDateTime struct directly
  and instead rely on the functions provided by this module as well
  as the ones in 3rd party calendar libraries.
  """

  @enforce_keys [:year, :month, :day, :hour, :minute, :second, :microsecond]
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
            Calendar.hour, Calendar.minute, Calendar.second, Calendar.microsecond) ::
        {:ok, NaiveDateTime.t} | {:error, atom}
  def new(year, month, day, hour, minute, second, microsecond \\ {0, 0}) do
    with {:ok, date} <- Calendar.ISO.date(year, month, day),
         {:ok, time} <- Time.new(hour, minute, second, microsecond),
         do: new(date, time)
  end

  @doc """
  Builds a naive date time from date and time structs.

  ## Examples

      iex> NaiveDateTime.new(~D[2010-01-13], ~T[23:00:07.005])
      {:ok, ~N[2010-01-13 23:00:07.005]}

  """
  @spec new(Date.t, Time.t) :: {:ok, NaiveDateTime.t}
  def new(date, time)

  def new(%Date{calendar: calendar, year: year, month: month, day: day},
          %Time{hour: hour, minute: minute, second: second, microsecond: microsecond}) do
    {:ok, %NaiveDateTime{calendar: calendar, year: year, month: month, day: day,
                         hour: hour, minute: minute, second: second, microsecond: microsecond}}
  end

  @doc """
  Converts a `NaiveDateTime` into a `Date`.

  Because `Date` does not hold time information,
  data will be lost during the conversion.

  ## Examples

      iex> NaiveDateTime.to_date(~N[2002-01-13 23:00:07])
      ~D[2002-01-13]

  """
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
  def to_time(%NaiveDateTime{hour: hour, minute: minute, second: second, microsecond: microsecond}) do
    %Time{hour: hour, minute: minute, second: second, microsecond: microsecond}
  end

  @doc """
  Converts the given naive date time to a string according to its calendar.

  ### Examples

      iex> NaiveDateTime.to_string(~N[2000-02-28 23:00:13])
      "2000-02-28 23:00:13"

      iex> NaiveDateTime.to_string(~N[2000-02-28 23:00:13.001])
      "2000-02-28 23:00:13.001"

  """
  @spec to_string(NaiveDateTime.t) :: String.t
  def to_string(%NaiveDateTime{calendar: calendar} = naive) do
    calendar.to_string(naive)
  end

  @doc """
  Parses the extended "Date and time of day" format described by ISO8601:2004.

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
  @spec from_iso8601(String.t) :: {:ok, NaiveDateTime.t} | {:error, atom}
  def from_iso8601(<<year::4-bytes, ?-, month::2-bytes, ?-, day::2-bytes, sep,
                     hour::2-bytes, ?:, min::2-bytes, ?:, sec::2-bytes, rest::binary>>) when sep in [?\s, ?T] do
    with {year, ""}       <- Integer.parse(year),
         {month, ""}      <- Integer.parse(month),
         {day, ""}        <- Integer.parse(day),
         {hour, ""}       <- Integer.parse(hour),
         {min, ""}        <- Integer.parse(min),
         {sec, ""}        <- Integer.parse(sec),
         {microsec, rest} <- Calendar.ISO.parse_microsecond(rest),
         {_offset, ""}    <- Calendar.ISO.parse_offset(rest) do
      new(year, month, day, hour, min, sec, microsec)
    else
      _ -> {:error, :invalid_format}
    end
  end

  def from_iso8601(<<_::binary>>) do
    {:error, :invalid_format}
  end

  @doc """
  Parses the extended "Date and time of day" format described by ISO8601:2004.

  Raises if the format is invalid.

  ## Examples

      iex> NaiveDateTime.from_iso8601!("2015-01-23T23:50:07.123Z")
      ~N[2015-01-23 23:50:07.123]
      iex> NaiveDateTime.from_iso8601!("2015-01-23P23:50:07")
      ** (ArgumentError) cannot parse "2015-01-23P23:50:07" as naive date time, reason: :invalid_format

  """
  @spec from_iso8601!(String.t) :: NaiveDateTime.t | no_return
  def from_iso8601!(string) do
    case from_iso8601(string) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot parse #{inspect string} as naive date time, reason: #{inspect reason}"
    end
  end

  @doc """
  Converts the given naive date time to ISO8601.

  Only supports converting naive date times which are in the ISO calendar,
  attempting to convert naive date times from other calendars will raise.

  ### Examples

      iex> NaiveDateTime.to_iso8601(~N[2000-02-28 23:00:13])
      "2000-02-28T23:00:13"

      iex> NaiveDateTime.to_iso8601(~N[2000-02-28 23:00:13.001])
      "2000-02-28T23:00:13.001"

  """
  @spec to_iso8601(NaiveDateTime.t) :: String.t
  def to_iso8601(%NaiveDateTime{calendar: Calendar.ISO} = naive) do
    Calendar.ISO.to_iso8601(naive)
  end

  @doc """
  Converts a `NaiveDateTime` struct to an Erlang datetime tuple.

  Only supports converting naive date times which are in the ISO calendar,
  attempting to convert naive date times from other calendars will raise.

  WARNING: Loss of precision may occur, as Erlang time tuples only store
  hour/minute/second.

  ## Examples

      iex> NaiveDateTime.to_erl(~N[2000-01-01 13:30:15])
      {{2000, 1, 1}, {13, 30, 15}}
  """
  @spec to_erl(NaiveDateTime.t) :: :calendar.datetime()
  def to_erl(%NaiveDateTime{calendar: Calendar.ISO, year: year, month: month, day: day,
                            hour: hour, minute: minute, second: second}) do
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
  @spec from_erl(:calendar.datetime(), Calendar.microsecond) :: {:ok, NaiveDateTime.t} | {:error, atom}
  def from_erl({{year, month, day}, {hour, minute, second}}, microsecond \\ {0, 0}) do
    new(year, month, day, hour, minute, second, microsecond)
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
      ** (ArgumentError) cannot convert {{2000, 13, 1}, {13, 30, 15}} to naive date time, reason: :invalid_date
  """
  @spec from_erl!(:calendar.datetime(), Calendar.microsecond) :: NaiveDateTime.t | no_return
  def from_erl!(tuple, microsecond \\ {0, 0}) do
    case from_erl(tuple, microsecond) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot convert #{inspect tuple} to naive date time, reason: #{inspect reason}"
    end
  end

  defimpl String.Chars do
    def to_string(%NaiveDateTime{calendar: calendar} = naive) do
      calendar.to_string(naive)
    end
  end

  defimpl Inspect do
    def inspect(%NaiveDateTime{calendar: Calendar.ISO} = naive, _) do
      "~N[" <> Calendar.ISO.to_string(naive) <> "]"
    end

    def inspect(naive, opts) do
      Inspect.Any.inspect(naive, opts)
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

  @enforce_keys [:year, :month, :day, :hour, :minute, :second, :microsecond,
                 :time_zone, :zone_abbr, :utc_offset, :std_offset]
  defstruct [:year, :month, :day, :hour, :minute, :second, :microsecond,
             :time_zone, :zone_abbr, :utc_offset, :std_offset, calendar: Calendar.ISO]

  @type t :: %__MODULE__{year: Calendar.year, month: Calendar.month, day: Calendar.day,
                         calendar: Calendar.calendar, hour: Calendar.hour, minute: Calendar.minute,
                         second: Calendar.second, microsecond: Calendar.microsecond,
                         time_zone: Calendar.time_zone, zone_abbr: Calendar.zone_abbr,
                         utc_offset: Calendar.utc_offset, std_offset: Calendar.std_offset}

  @unix_epoch :calendar.datetime_to_gregorian_seconds {{1970, 1, 1}, {0, 0, 0}}

  @doc """
  Returns the current datetime in UTC.

  ## Examples

      iex> datetime = DateTime.utc_now()
      iex> datetime.time_zone
      "Etc/UTC"

  """
  @spec utc_now() :: DateTime.t
  def utc_now() do
    :os.system_time |> from_unix!(:native)
  end

  @doc """
  Converts the given unix time to DateTime.

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

      iex> DateTime.from_unix(1432560368868569, :microseconds)
      {:ok, %DateTime{calendar: Calendar.ISO, day: 25, hour: 13, microsecond: {868569, 6}, minute: 26,
                      month: 5, second: 8, std_offset: 0, time_zone: "Etc/UTC", utc_offset: 0,
                      year: 2015, zone_abbr: "UTC"}}

  The unit can also be an integer as in `System.time_unit`:

      iex> DateTime.from_unix(1432560368868569, 1024)
      {:ok, %DateTime{calendar: Calendar.ISO, day: 23, hour: 22, microsecond: {211914, 3}, minute: 53,
                      month: 1, second: 43, std_offset: 0, time_zone: "Etc/UTC", utc_offset: 0,
                      year: 46302, zone_abbr: "UTC"}}

  """
  @spec from_unix(non_neg_integer, :native | System.time_unit) :: {:ok, DateTime.t}
  def from_unix(integer, unit \\ :seconds) when is_integer(integer) and integer >= 0 do
    total = System.convert_time_unit(integer, unit, :microseconds)
    microsecond = rem(total, 1_000_000)
    precision = precision_for_unit(unit)
    {{year, month, day}, {hour, minute, second}} =
      :calendar.gregorian_seconds_to_datetime(@unix_epoch + div(total, 1_000_000))

    {:ok, %DateTime{year: year, month: month, day: day,
                    hour: hour, minute: minute, second: second, microsecond: {microsecond, precision},
                    std_offset: 0, utc_offset: 0, zone_abbr: "UTC", time_zone: "Etc/UTC"}}
  end

  def precision_for_unit(unit) do
    subseconds = div System.convert_time_unit(1, :seconds, unit), 10
    precision_for_unit(subseconds, 0)
  end

  defp precision_for_unit(0, precision),
    do: precision
  defp precision_for_unit(_, 6),
    do: 6
  defp precision_for_unit(number, precision),
    do: precision_for_unit(div(number, 10), precision + 1)

  @doc """
  Converts the given unix time to DateTime.

  ## Examples

      iex> DateTime.from_unix!(1464096368)
      %DateTime{calendar: Calendar.ISO, day: 24, hour: 13, microsecond: {0, 0}, minute: 26,
                month: 5, second: 8, std_offset: 0, time_zone: "Etc/UTC", utc_offset: 0,
                year: 2016, zone_abbr: "UTC"}

      iex> DateTime.from_unix!(1432560368868569, :microseconds)
      %DateTime{calendar: Calendar.ISO, day: 25, hour: 13, microsecond: {868569, 6}, minute: 26,
                month: 5, second: 8, std_offset: 0, time_zone: "Etc/UTC", utc_offset: 0,
                year: 2015, zone_abbr: "UTC"}

  """
  @spec from_unix!(non_neg_integer, :native | System.time_unit) :: DateTime.t
  def from_unix!(integer, unit \\ :seconds) when is_atom(unit) do
    {:ok, datetime} = from_unix(integer, unit)
    datetime
  end

  @doc """
  Converts the given DateTime to unix time.

  The DateTime is expected to be UTC using the ISO calendar
  with a year greater than or equal to 1970.

  It will return the integer with the given unit,
  according to `System.convert_time_unit/3`.

  ## Examples

      iex> 1464096368 |> DateTime.from_unix!() |> DateTime.to_unix()
      1464096368

  """
  @spec to_unix(DateTime.t, System.time_unit) :: non_neg_integer
  def to_unix(%DateTime{std_offset: 0, utc_offset: 0, time_zone: "Etc/UTC",
                        hour: hour, minute: minute, second: second, microsecond: {microsecond, _},
                        year: year, month: month, day: day}, unit \\ :seconds) when year >= 1970 do
    seconds = :calendar.datetime_to_gregorian_seconds({{year, month, day}, {hour, minute, second}})
    System.convert_time_unit((seconds - @unix_epoch) * 1_000_000 + microsecond, :microseconds, unit)
  end

  @doc """
  Converts a `DateTime` into a `NaiveDateTime`.

  Because `NaiveDateTime` does not hold timezone information,
  any timezone related data will be lost during the conversion.

  ## Examples

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CEST",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 1},
      ...>                utc_offset: 3600, std_offset: 3600, time_zone: "Europe/Warsaw"}
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

  Because `Date` does not hold time nor timezone information,
  data will be lost during the conversion.

  ## Examples

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CEST",
      ...>                hour: 23, minute: 0, second: 7, microsecond: 0,
      ...>                utc_offset: 3600, std_offset: 3600, time_zone: "Europe/Warsaw"}
      iex> DateTime.to_date(dt)
      ~D[2000-02-29]

  """
  def to_date(%DateTime{year: year, month: month, day: day, calendar: calendar}) do
    %Date{year: year, month: month, day: day, calendar: calendar}
  end

  @doc """
  Converts a `DateTime` into `Time`.

  Because `Time` does not hold date nor timezone information,
  data will be lost during the conversion.

  ## Examples

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CEST",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 1},
      ...>                utc_offset: 3600, std_offset: 3600, time_zone: "Europe/Warsaw"}
      iex> DateTime.to_time(dt)
      ~T[23:00:07.0]

  """
  def to_time(%DateTime{hour: hour, minute: minute, second: second, microsecond: microsecond}) do
    %Time{hour: hour, minute: minute, second: second, microsecond: microsecond}
  end

  @doc """
  Converts the given date time to ISO8601.

  Only supports converting date times which are in the ISO calendar,
  attempting to convert date times from other calendars will raise.

  WARNING: the ISO8601 does not contain the time zone nor its abbreviation,
  which means information is lost when converting to such format. This
  is also why this module does not provide a `from_iso8601/1` function,
  as it is impossible to build a proper `DateTime` from only the
  information in the ISO8601 string.

  ### Examples

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CEST",
      ...>                hour: 23, minute: 0, second: 7, microsecond: 0,
      ...>                utc_offset: 3600, std_offset: 3600, time_zone: "Europe/Warsaw"}
      iex> DateTime.to_iso8601(dt)
      "2000-02-29T23:00:07+02:00"

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "UTC",
      ...>                hour: 23, minute: 0, second: 7, microsecond: 0,
      ...>                utc_offset: 0, std_offset: 0, time_zone: "Etc/UTC"}
      iex> DateTime.to_iso8601(dt)
      "2000-02-29T23:00:07Z"

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "BRM",
      ...>                hour: 23, minute: 0, second: 7, microsecond: 0,
      ...>                utc_offset: -12600, std_offset: 3600, time_zone: "Brazil/Manaus"}
      iex> DateTime.to_iso8601(dt)
      "2000-02-29T23:00:07-02:30"
  """
  @spec to_iso8601(DateTime.t) :: String.t
  def to_iso8601(%DateTime{calendar: Calendar.ISO} = dt) do
    Calendar.ISO.to_iso8601(dt)
  end

  @doc """
  Converts the given date time to a string according to its calendar.

  ### Examples

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CEST",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: 3600, std_offset: 3600, time_zone: "Europe/Warsaw"}
      iex> DateTime.to_string(dt)
      "2000-02-29 23:00:07+02:00 CEST Europe/Warsaw"

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "UTC",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: 0, std_offset: 0, time_zone: "Etc/UTC"}
      iex> DateTime.to_string(dt)
      "2000-02-29 23:00:07Z"

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "BRM",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: -12600, std_offset: 3600, time_zone: "Brazil/Manaus"}
      iex> DateTime.to_string(dt)
      "2000-02-29 23:00:07-02:30 BRM Brazil/Manaus"
  """
  @spec to_string(DateTime.t) :: String.t
  def to_string(%DateTime{calendar: calendar} = dt) do
    calendar.to_string(dt)
  end

  defimpl String.Chars do
    def to_string(%DateTime{calendar: calendar} = dt) do
      calendar.to_string(dt)
    end
  end
end
