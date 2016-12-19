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

  @typedoc "Any map/struct that contains the date fields"
  @type date :: %{calendar: calendar, year: year, month: month, day: day}

  @typedoc "Any map/struct that contains the time fields"
  @type time :: %{hour: hour, minute: minute, second: second, microsecond: microsecond}

  @typedoc "Any map/struct that contains the naive_datetime fields"
  @type naive_date_time :: %{calendar: calendar, year: year, month: month, day: day,
                             hour: hour, minute: minute, second: second, microsecond: microsecond}

  @typedoc "Any map/struct that contains the datetime fields"
  @type date_time :: %{calendar: calendar, year: year, month: month, day: day,
                       hour: hour, minute: minute, second: second, microsecond: microsecond,
                       time_zone: time_zone, zone_abbr: zone_abbr, utc_offset: utc_offset, std_offset: std_offset}

  @doc """
  Returns how many days there are in the given year-month.
  """
  @callback days_in_month(year, month) :: day

  @doc """
  Returns true if the given year is a leap year.

  A leap year is a year of a longer length than normal. The exact meaning
  is up to the calendar. A calendar must return `false` if it does not support
  the concept of leap years.
  """
  @callback leap_year?(year) :: boolean

  @doc """
  Calculates the day of the week from the given `year`, `month`, and `day`.
  """
  @callback day_of_week(year, month, day) :: non_neg_integer()

  @doc """
  Converts the date into a string according to the calendar.
  """
  @callback date_to_string(year, month, day) :: String.t

  @doc """
  Converts the date time (without time zone) into a string according to the calendar.
  """
  @callback naive_datetime_to_string(year, month, day, hour, minute, second, microsecond) :: String.t

  @doc """
  Coverts the date time (with time zone) into a string according to the calendar.
  """
  @callback datetime_to_string(year, month, day, hour, minute, second, microsecond,
                               time_zone, zone_abbr, utc_offset, std_offset) :: String.t
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

  The functions on this module work with the `Date` struct as well
  as any struct that contains the same fields as the `Date` struct,
  such as `NaiveDateTime` and `DateTime`. Such functions expect
  `Calendar.date` in their typespecs (instead of `t`).

  Developers should avoid creating the Date struct directly and
  instead rely on the functions provided by this module as well as
  the ones in 3rd party calendar libraries.
  """

  @enforce_keys [:year, :month, :day]
  defstruct [:year, :month, :day, calendar: Calendar.ISO]

  @type t :: %Date{year: Calendar.year, month: Calendar.month,
                   day: Calendar.day, calendar: Calendar.calendar}

  @doc """
  Returns the current date in UTC.

  ## Examples

      iex> date = Date.utc_today()
      iex> date.year >= 2016
      true

  """
  @spec utc_today() :: t
  def utc_today() do
    {:ok, {year, month, day}, _, _} = Calendar.ISO.from_unix(:os.system_time, :native)
    %Date{year: year, month: month, day: day}
  end

  @doc """
  Returns true if the year in `date` is a leap year.

  ## Examples

      iex> Date.leap_year?(~D[2000-01-01])
      true
      iex> Date.leap_year?(~D[2001-01-01])
      false
      iex> Date.leap_year?(~D[2004-01-01])
      true
      iex> Date.leap_year?(~D[1900-01-01])
      false
      iex> Date.leap_year?(~N[2004-01-01 01:23:45])
      true

  """
  @spec leap_year?(Calendar.date) :: boolean()
  def leap_year?(%{calendar: calendar, year: year}) do
    calendar.leap_year?(year)
  end

  @doc """
  Returns the number of days in the given date month.

  ## Examples

      iex> Date.days_in_month(~D[1900-01-13])
      31
      iex> Date.days_in_month(~D[1900-02-09])
      28
      iex> Date.days_in_month(~N[2000-02-20 01:23:45])
      29

  """
  @spec days_in_month(Calendar.date) :: Calendar.day
  def days_in_month(%{calendar: calendar, year: year, month: month}) do
    calendar.days_in_month(year, month)
  end

  @doc """
  Builds a new ISO date.

  Expects all values to be integers. Returns `{:ok, date}` if each
  entry fits its appropriate range, returns `{:error, reason}` otherwise.

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
  @spec new(Calendar.year, Calendar.month, Calendar.day) :: {:ok, t} | {:error, atom}
  def new(year, month, day) do
    Calendar.ISO.date(year, month, day)
  end

  @doc """
  Converts the given date to a string according to its calendar.

  ### Examples

      iex> Date.to_string(~D[2000-02-28])
      "2000-02-28"
      iex> Date.to_string(~N[2000-02-28 01:23:45])
      "2000-02-28"

  """
  @spec to_string(Calendar.date) :: String.t
  def to_string(%{calendar: calendar, year: year, month: month, day: day}) do
    calendar.date_to_string(year, month, day)
  end

  @doc """
  Parses the extended "Date and time of day" format described by
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601).

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
  @spec from_iso8601(String.t) :: {:ok, t} | {:error, atom}
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
  Parses the extended "Date and time of day" format described by
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601).

  Raises if the format is invalid.

  ## Examples

      iex> Date.from_iso8601!("2015-01-23")
      ~D[2015-01-23]
      iex> Date.from_iso8601!("2015:01:23")
      ** (ArgumentError) cannot parse "2015:01:23" as date, reason: :invalid_format
  """
  @spec from_iso8601!(String.t) :: t | no_return
  def from_iso8601!(string) do
    case from_iso8601(string) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot parse #{inspect string} as date, reason: #{inspect reason}"
    end
  end

  @doc """
  Converts the given datetime to
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601).

  Only supports converting datetimes which are in the ISO calendar,
  attempting to convert datetimes from other calendars will raise.

  ### Examples

      iex> Date.to_iso8601(~D[2000-02-28])
      "2000-02-28"
      iex> Date.to_iso8601(~N[2000-02-28 01:23:45])
      "2000-02-28"

  """
  @spec to_iso8601(Calendar.date) :: String.t
  def to_iso8601(%{calendar: Calendar.ISO, year: year, month: month, day: day}) do
    Calendar.ISO.date_to_iso8601(year, month, day)
  end

  @doc """
  Converts a `Date` struct to an Erlang date tuple.

  Only supports converting dates which are in the ISO calendar,
  attempting to convert dates from other calendars will raise.

  ## Examples

      iex> Date.to_erl(~D[2000-01-01])
      {2000, 1, 1}
      iex> Date.to_erl(~N[2000-01-01 01:23:45])
      {2000, 1, 1}

  """
  @spec to_erl(Calendar.date) :: :calendar.date
  def to_erl(%{calendar: Calendar.ISO, year: year, month: month, day: day}) do
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
  @spec from_erl(:calendar.date) :: {:ok, t} | {:error, atom}
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
  @spec from_erl!(:calendar.date) :: t | no_return
  def from_erl!(tuple) do
    case from_erl(tuple) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot convert #{inspect tuple} to date, reason: #{inspect reason}"
    end
  end

  @doc """
  Compares two `Date` structs.

  Returns `:gt` if first date is later than the second
  and `:lt` for vice versa. If the two dates are equal
  `:eq` is returned.

  ## Examples

      iex> Date.compare(~D[2016-04-16], ~D[2016-04-28])
      :lt

  This function can also be used to compare across more
  complex calendar types by considering only the date fields:

      iex> Date.compare(~D[2016-04-16], ~N[2016-04-28 01:23:45])
      :lt
      iex> Date.compare(~D[2016-04-16], ~N[2016-04-16 01:23:45])
      :eq
      iex> Date.compare(~N[2016-04-16 12:34:56], ~N[2016-04-16 01:23:45])
      :eq

  """
  @spec compare(Calendar.date, Calendar.date) :: :lt | :eq | :gt
  def compare(date1, date2) do
    case {to_erl(date1), to_erl(date2)} do
      {first, second} when first > second -> :gt
      {first, second} when first < second -> :lt
      _ -> :eq
    end
  end

  @doc """
  Calculates the day of the week of a given `Date` struct.

  Returns the day of the week as an integer. For the ISO 8601
  calendar (the default), it is an integer from 1 to 7, where
  1 is Monday and 7 is Sunday.

  ## Examples

      iex> Date.day_of_week(~D[2016-10-31])
      1
      iex> Date.day_of_week(~D[2016-11-01])
      2
      iex> Date.day_of_week(~N[2016-11-01 01:23:45])
      2
  """
  @spec day_of_week(Calendar.date) :: non_neg_integer()
  def day_of_week(%{calendar: calendar, year: year, month: month, day: day}) do
    calendar.day_of_week(year, month, day)
  end

  ## Helpers

  defimpl String.Chars do
    def to_string(%{calendar: calendar, year: year, month: month, day: day}) do
      calendar.date_to_string(year, month, day)
    end
  end

  defimpl Inspect do
    def inspect(%{calendar: Calendar.ISO, year: year, month: month, day: day}, _) do
      "~D[" <> Calendar.ISO.date_to_string(year, month, day) <> "]"
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

  The functions on this module work with the `Time` struct as well
  as any struct that contains the same fields as the `Time` struct,
  such as `NaiveDateTime` and `DateTime`. Such functions expect
  `Calendar.time` in their typespecs (instead of `t`).

  Developers should avoid creating the Time struct directly and
  instead rely on the functions provided by this module as well as
  the ones in 3rd party calendar libraries.
  """

  @enforce_keys [:hour, :minute, :second]
  defstruct [:hour, :minute, :second, microsecond: {0, 0}]

  @type t :: %Time{hour: Calendar.hour, minute: Calendar.minute,
                   second: Calendar.second, microsecond: Calendar.microsecond}

  @doc """
  Returns the current time in UTC.

  ## Examples

      iex> time = Time.utc_now()
      iex> time.hour >= 0
      true

  """
  @spec utc_now() :: t
  def utc_now() do
    {:ok, _, {hour, minute, second}, microsecond} = Calendar.ISO.from_unix(:os.system_time, :native)
    %Time{hour: hour, minute: minute, second: second, microsecond: microsecond}
  end

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

      iex> Time.to_string(~N[2015-01-01 23:00:00.001])
      "23:00:00.001"
      iex> Time.to_string(~N[2015-01-01 23:00:00.123456])
      "23:00:00.123456"

  """
  @spec to_string(Calendar.time) :: String.t
  def to_string(%{hour: hour, minute: minute, second: second, microsecond: microsecond}) do
    Calendar.ISO.time_to_string(hour, minute, second, microsecond)
  end

  @doc """
  Parses the extended "Local time" format described by
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601).

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
  @spec from_iso8601(String.t) :: {:ok, t} | {:error, atom}
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
  Parses the extended "Local time" format described by
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601).

  Raises if the format is invalid.

  ## Examples

      iex> Time.from_iso8601!("23:50:07.123Z")
      ~T[23:50:07.123]
      iex> Time.from_iso8601!("2015:01:23 23-50-07")
      ** (ArgumentError) cannot parse "2015:01:23 23-50-07" as time, reason: :invalid_format
  """
  @spec from_iso8601!(String.t) :: t | no_return
  def from_iso8601!(string) do
    case from_iso8601(string) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot parse #{inspect string} as time, reason: #{inspect reason}"
    end
  end

  @doc """
  Converts the given time to
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601).

  ### Examples

      iex> Time.to_iso8601(~T[23:00:13])
      "23:00:13"
      iex> Time.to_iso8601(~T[23:00:13.001])
      "23:00:13.001"

      iex> Time.to_iso8601(~N[2015-01-01 23:00:13])
      "23:00:13"
      iex> Time.to_iso8601(~N[2015-01-01 23:00:13.001])
      "23:00:13.001"

  """
  @spec to_iso8601(Calendar.time) :: String.t
  def to_iso8601(%{hour: hour, minute: minute, second: second, microsecond: microsecond}) do
    Calendar.ISO.time_to_iso8601(hour, minute, second, microsecond)
  end

  @doc """
  Converts a `Time` struct to an Erlang time tuple.

  WARNING: Loss of precision may occur, as Erlang time tuples
  only contain hours/minutes/seconds.

  ## Examples

      iex> Time.to_erl(~T[23:30:15.999])
      {23, 30, 15}

      iex> Time.to_erl(~N[2015-01-01 23:30:15.999])
      {23, 30, 15}

  """
  @spec to_erl(Calendar.time) :: :calendar.time
  def to_erl(%{hour: hour, minute: minute, second: second}) do
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
  @spec from_erl(:calendar.time, Calendar.microsecond) :: {:ok, t} | {:error, atom}
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
  @spec from_erl!(:calendar.time, Calendar.microsecond) :: t | no_return
  def from_erl!(tuple, microsecond \\ {0, 0}) do
    case from_erl(tuple, microsecond) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot convert #{inspect tuple} to time, reason: #{inspect reason}"
    end
  end

  @doc """
  Compares two `Time` structs.

  Returns `:gt` if first time is later than the second
  and `:lt` for vice versa. If the two times are equal
  `:eq` is returned

  ## Examples

      iex> Time.compare(~T[16:04:16], ~T[16:04:28])
      :lt
      iex> Time.compare(~T[16:04:16.01], ~T[16:04:16.001])
      :gt

  This function can also be used to compare across more
  complex calendar types by considering only the time fields:

      iex> Time.compare(~N[2015-01-01 16:04:16], ~N[2015-01-01 16:04:28])
      :lt
      iex> Time.compare(~N[2015-01-01 16:04:16.01], ~N[2000-01-01 16:04:16.001])
      :gt

  """
  @spec compare(Calendar.time, Calendar.time) :: :lt | :eq | :gt
  def compare(time1, time2) do
    case {to_tuple(time1), to_tuple(time2)} do
      {first, second} when first > second -> :gt
      {first, second} when first < second -> :lt
      _ -> :eq
    end
  end

  ## Helpers

  defp to_tuple(%{hour: hour, minute: minute, second: second, microsecond: {microsecond, _precision}}) do
    {hour, minute, second, microsecond}
  end

  defimpl String.Chars do
    def to_string(%{hour: hour, minute: minute, second: second, microsecond: microsecond}) do
      Calendar.ISO.time_to_string(hour, minute, second, microsecond)
    end
  end

  defimpl Inspect do
    def inspect(%{hour: hour, minute: minute, second: second, microsecond: microsecond}, _) do
      "~T[" <> Calendar.ISO.time_to_string(hour, minute, second, microsecond) <> "]"
    end
  end
end

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
  @spec utc_now() :: t
  def utc_now() do
    {:ok, {year, month, day}, {hour, minute, second}, microsecond} =
      Calendar.ISO.from_unix(:os.system_time, :native)
    %NaiveDateTime{year: year, month: month, day: day,
                   hour: hour, minute: minute, second: second,
                   microsecond: microsecond}
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
            Calendar.hour, Calendar.minute, Calendar.second, Calendar.microsecond) ::
        {:ok, t} | {:error, atom}
  def new(year, month, day, hour, minute, second, microsecond \\ {0, 0}) do
    with {:ok, date} <- Calendar.ISO.date(year, month, day),
         {:ok, time} <- Time.new(hour, minute, second, microsecond),
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
          %Time{hour: hour, minute: minute, second: second, microsecond: microsecond}) do
    {:ok, %NaiveDateTime{calendar: calendar, year: year, month: month, day: day,
                         hour: hour, minute: minute, second: second, microsecond: microsecond}}
  end

  @doc """
  Adds a specified amount of time to a `NaiveDateTime`.

  Accepts an `integer` in any `unit` available from `t:System.time_unit/0`.
  Negative values will be move backwards in time.

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
      # from gregorian seconds
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
  Subtract `naive_datetime2` from `naive_datetime1`.

  The answer can be returned in any `unit` available from `t:System.time_unit/0`.

  ## Examples

      iex> NaiveDateTime.diff(~N[2014-10-02 00:29:12], ~N[2014-10-02 00:29:10])
      2
      iex> NaiveDateTime.diff(~N[2014-10-02 00:29:12], ~N[2014-10-02 00:29:10], :microsecond)
      2_000_000
      iex> NaiveDateTime.diff(~N[2014-10-02 00:29:10.042], ~N[2014-10-02 00:29:10.021], :millisecond)
      21
      # to gregorian seconds
      iex> NaiveDateTime.diff(~N[2014-10-02 00:29:10], ~N[0000-01-01 00:00:00])
      63579428950
  """
  @spec diff(t, t, System.time_unit) :: integer
  def diff(%NaiveDateTime{} = naive_datetime1,
           %NaiveDateTime{} = naive_datetime2,
           unit \\ :second) do
    ndt1_microsecond = to_microsecond(naive_datetime1)
    ndt2_microsecond = to_microsecond(naive_datetime2)
    difference = ndt1_microsecond - ndt2_microsecond
    System.convert_time_unit(difference, :microsecond, unit)
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
  def to_time(%NaiveDateTime{hour: hour, minute: minute, second: second, microsecond: microsecond}) do
    %Time{hour: hour, minute: minute, second: second, microsecond: microsecond}
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
  @spec from_iso8601(String.t) :: {:ok, t} | {:error, atom}
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
  Parses the extended "Date and time of day" format described by
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601).

  Raises if the format is invalid.

  ## Examples

      iex> NaiveDateTime.from_iso8601!("2015-01-23T23:50:07.123Z")
      ~N[2015-01-23 23:50:07.123]
      iex> NaiveDateTime.from_iso8601!("2015-01-23P23:50:07")
      ** (ArgumentError) cannot parse "2015-01-23P23:50:07" as naive datetime, reason: :invalid_format

  """
  @spec from_iso8601!(String.t) :: t | no_return
  def from_iso8601!(string) do
    case from_iso8601(string) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot parse #{inspect string} as naive datetime, reason: #{inspect reason}"
    end
  end

  @doc """
  Converts the given naive datetime to
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601).

  Only supports converting naive datetimes which are in the ISO calendar,
  attempting to convert naive datetimes from other calendars will raise.

  ### Examples

      iex> NaiveDateTime.to_iso8601(~N[2000-02-28 23:00:13])
      "2000-02-28T23:00:13"

      iex> NaiveDateTime.to_iso8601(~N[2000-02-28 23:00:13.001])
      "2000-02-28T23:00:13.001"

  This function can also be used to convert a DateTime to ISO8601 without
  the time zone information:

      iex> dt = %DateTime{year: 2000, month: 2, day: 29, zone_abbr: "CET",
      ...>                hour: 23, minute: 0, second: 7, microsecond: {0, 0},
      ...>                utc_offset: 3600, std_offset: 0, time_zone: "Europe/Warsaw"}
      iex> NaiveDateTime.to_iso8601(dt)
      "2000-02-29T23:00:07"

  """
  @spec to_iso8601(Calendar.naive_datetime) :: String.t
  def to_iso8601(%{year: year, month: month, day: day,
                   hour: hour, minute: minute, second: second, microsecond: microsecond}) do
    Calendar.ISO.naive_datetime_to_iso8601(year, month, day, hour, minute, second, microsecond)
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
  def to_erl(%{calendar: Calendar.ISO, year: year, month: month, day: day,
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
  @spec from_erl(:calendar.datetime, Calendar.microsecond) :: {:ok, t} | {:error, atom}
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
  are equal `:eq` is returned

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
  def compare(naive_datetime1, naive_datetime2) do
    case {to_tuple(naive_datetime1), to_tuple(naive_datetime2)} do
      {first, second} when first > second -> :gt
      {first, second} when first < second -> :lt
      _ -> :eq
    end
  end

  ## Helpers

  defp to_microsecond(%{calendar: Calendar.ISO, year: year, month: month, day: day,
                        hour: hour, minute: minute, second: second, microsecond: {microsecond, _precision}}) do
    second = :calendar.datetime_to_gregorian_seconds(
      {{year, month, day}, {hour, minute, second}}
    )
    second * 1_000_000 + microsecond
  end

  defp to_tuple(%{calendar: Calendar.ISO, year: year, month: month, day: day,
                  hour: hour, minute: minute, second: second, microsecond: {microsecond, _precision}}) do
    {year, month, day, hour, minute, second, microsecond}
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

defmodule DateTime do
  @moduledoc """
  A datetime implementation with a time zone.

  This datetime can be seen as an ephemeral snapshot
  of a datetime at a given time zone. For such purposes,
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

  @enforce_keys [:year, :month, :day, :hour, :minute, :second,
                 :time_zone, :zone_abbr, :utc_offset, :std_offset]
  defstruct [:year, :month, :day, :hour, :minute, :second, :time_zone,
             :zone_abbr, :utc_offset, :std_offset, microsecond: {0, 0}, calendar: Calendar.ISO]

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

      iex> DateTime.from_unix(1432560368868569, 1024)
      {:ok, %DateTime{calendar: Calendar.ISO, day: 23, hour: 22, microsecond: {211914, 3}, minute: 53,
                      month: 1, second: 43, std_offset: 0, time_zone: "Etc/UTC", utc_offset: 0,
                      year: 46302, zone_abbr: "UTC"}}

  Negative Unix times are supported, up to -#{@unix_epoch} seconds,
  which is equivalent to "0000-01-01T00:00:00Z" or 0 gregorian seconds.

      iex> DateTime.from_unix(-12345678910)
      {:ok, %DateTime{calendar: Calendar.ISO, day: 13, hour: 4, microsecond: {0, 0}, minute: 44,
                      month: 10, second: 50, std_offset: 0, time_zone: "Etc/UTC", utc_offset: 0,
                      year: 1578, zone_abbr: "UTC"}}

  When a Unix time before that moment is passed to `from_unix/2`, `:error` will be returned.
  """
  @spec from_unix(integer, :native | System.time_unit) :: {:ok, DateTime.t} | {:error, atom}
  def from_unix(integer, unit \\ :second) when is_integer(integer) do
    case Calendar.ISO.from_unix(integer, unit) do
      {:ok, {year, month, day}, {hour, minute, second}, microsecond} ->
        {:ok, %DateTime{year: year, month: month, day: day,
                        hour: hour, minute: minute, second: second, microsecond: microsecond,
                        std_offset: 0, utc_offset: 0, zone_abbr: "UTC", time_zone: "Etc/UTC"}}
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

  Negative Unix times are supported, up to -#{@unix_epoch} seconds,
  which is equivalent to "0000-01-01T00:00:00Z" or 0 gregorian seconds.

      iex> DateTime.from_unix(-12345678910)
      {:ok, %DateTime{calendar: Calendar.ISO, day: 13, hour: 4, microsecond: {0, 0}, minute: 44,
                      month: 10, second: 50, std_offset: 0, time_zone: "Etc/UTC", utc_offset: 0,
                      year: 1578, zone_abbr: "UTC"}}

  When a Unix time before that moment is passed to `from_unix!/2`, an ArgumentError will be raised.
  """
  @spec from_unix!(non_neg_integer, :native | System.time_unit) :: DateTime.t
  def from_unix!(integer, unit \\ :second) when is_atom(unit) do
    case from_unix(integer, unit) do
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

  def from_naive(%NaiveDateTime{hour: hour, minute: minute, second: second, microsecond: microsecond,
                                year: year, month: month, day: day}, "Etc/UTC") do
    {:ok, %DateTime{year: year, month: month, day: day,
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

  def to_unix(%DateTime{calendar: Calendar.ISO, std_offset: std_offset, utc_offset: utc_offset,
                        hour: hour, minute: minute, second: second, microsecond: {microsecond, _},
                        year: year, month: month, day: day}, unit) when year >= 0 do
    seconds =
      :calendar.datetime_to_gregorian_seconds({{year, month, day}, {hour, minute, second}})
      |> Kernel.-(utc_offset)
      |> Kernel.-(std_offset)
    System.convert_time_unit((seconds - @unix_epoch) * 1_000_000 + microsecond, :microsecond, unit)
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
  def to_time(%DateTime{hour: hour, minute: minute, second: second, microsecond: microsecond}) do
    %Time{hour: hour, minute: minute, second: second, microsecond: microsecond}
  end

  @doc """
  Converts the given datetime to
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601) format.

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
      iex> DateTime.to_iso8601(dt)
      "2000-02-29T23:00:07-04:00"
  """
  @spec to_iso8601(DateTime.t) :: String.t
  def to_iso8601(%{calendar: Calendar.ISO, year: year, month: month, day: day,
                  hour: hour, minute: minute, second: second, microsecond: microsecond,
                  time_zone: time_zone, zone_abbr: zone_abbr, utc_offset: utc_offset, std_offset: std_offset}) do
    Calendar.ISO.datetime_to_iso8601(year, month, day, hour, minute, second, microsecond,
                                     time_zone, zone_abbr, utc_offset, std_offset)
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
  @spec from_iso8601(String.t) :: {:ok, t, Calendar.utc_offset} | {:error, atom}
  def from_iso8601(<<year::4-bytes, ?-, month::2-bytes, ?-, day::2-bytes, sep,
                     hour::2-bytes, ?:, min::2-bytes, ?:, sec::2-bytes, rest::binary>>) when sep in [?\s, ?T] do
    with {year, ""}       <- Integer.parse(year),
         {month, ""}      <- Integer.parse(month),
         {day, ""}        <- Integer.parse(day),
         {hour, ""}       <- Integer.parse(hour),
         {min, ""}        <- Integer.parse(min),
         {sec, ""}        <- Integer.parse(sec),
         {microsec, rest} <- Calendar.ISO.parse_microsecond(rest),
         {:ok, date}      <- Calendar.ISO.date(year, month, day),
         {:ok, time}      <- Time.new(hour, min, sec, microsec),
         {:ok, offset}    <- parse_offset(rest) do
      %{year: year, month: month, day: day} = date
      %{hour: hour, minute: minute, second: second, microsecond: microsecond} = time

      erl = {{year, month, day}, {hour, minute, second}}
      seconds = :calendar.datetime_to_gregorian_seconds(erl)
      {{year, month, day}, {hour, minute, second}} =
        :calendar.gregorian_seconds_to_datetime(seconds - offset)

      {:ok, %DateTime{year: year, month: month, day: day,
                      hour: hour, minute: minute, second: second, microsecond: microsecond,
                      std_offset: 0, utc_offset: 0, zone_abbr: "UTC", time_zone: "Etc/UTC"}, offset}
    else
      {:error, reason} -> {:error, reason}
      _ -> {:error, :invalid_format}
    end
  end
  def from_iso8601(_) do
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
  @spec to_string(DateTime.t) :: String.t
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
  def compare(%DateTime{} = datetime1, %DateTime{} = datetime2) do
    case {to_unix(datetime1, :microsecond), to_unix(datetime2, :microsecond)} do
      {first, second} when first > second -> :gt
      {first, second} when first < second -> :lt
      _ -> :eq
    end
  end
end
