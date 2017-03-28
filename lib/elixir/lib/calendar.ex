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
  @type month :: integer
  @type day :: integer
  @type hour :: integer
  @type minute :: integer

  @type second :: integer

  @typedoc """
  The internal time format is used when converting between calendars.

  It represents time as a fraction of a day (starting from midnight).
  `parts_in_day` specifies how much of the day is already passed,
  while `parts_per_day` signifies how many parts there fit in a day.
  """
  @type day_fraction :: {parts_in_day :: non_neg_integer, parts_per_day :: pos_integer}

  @typedoc """
  The internal date format that is used when converting between calendars.

  This is the amount of days including the fractional part that has passed of the last day,
  since midnight 1 January AD 1 of the Proleptic Gregorian Calendar
  (0000-01-01+00:00T00:00.00000 in ISO 8601 notation).

  The `parts_per_day` represent how many subparts the current day is subdivided in
  (for different calendars, picking a different `parts_per_day` might make sense).
  The `parts_in_day` represents how many of these `parts_per_day` have passed in the last day.

  Thus, a Rata Die like `{1234, {1, 2}}` should be read as `1234½`.
  """
  @type rata_die :: {days :: integer, day_fraction}

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
  @type time_zone :: String.t

  @typedoc "The time zone abbreviation (e.g. CET or CEST or BST etc.)"
  @type zone_abbr :: String.t

  @typedoc "The time zone UTC offset in seconds"
  @type utc_offset :: integer

  @typedoc "The time zone standard offset in seconds (not zero in summer times)"
  @type std_offset :: integer

  @typedoc "Any map/struct that contains the date fields"
  @type date :: %{optional(any) => any, calendar: calendar, year: year, month: month, day: day}

  @typedoc "Any map/struct that contains the time fields"
  @type time :: %{optional(any) => any, hour: hour, minute: minute, second: second, microsecond: microsecond}

  @typedoc "Any map/struct that contains the naive_datetime fields"
  @type naive_datetime :: %{optional(any) => any, calendar: calendar, year: year, month: month, day: day,
                            hour: hour, minute: minute, second: second, microsecond: microsecond}

  @typedoc "Any map/struct that contains the datetime fields"
  @type datetime :: %{optional(any) => any, calendar: calendar, year: year, month: month, day: day,
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
  Converts the datetime (without time zone) into a string according to the calendar.
  """
  @callback naive_datetime_to_string(year, month, day, hour, minute, second, microsecond) :: String.t

  @doc """
  Converts the datetime (with time zone) into a string according to the calendar.
  """
  @callback datetime_to_string(year, month, day, hour, minute, second, microsecond,
                               time_zone, zone_abbr, utc_offset, std_offset) :: String.t

  @doc """
  Converts the time into a string according to the calendar.
  """
  @callback time_to_string(hour, minute, second, microsecond) :: String.t

  @doc """
  Converts the given datetime (with time zone) into the `t:rata_die` format.
  """
  @callback naive_datetime_to_rata_die(year, month, day, hour, minute, second, microsecond) :: rata_die

  @doc """
  Converts `t:rata_die` to the Calendar's datetime format.
  """
  @callback naive_datetime_from_rata_die(rata_die) :: {year, month, day, hour, minute, second, microsecond}

  @doc """
  Converts the given time to the `t:day_fraction` format.
  """
  @callback time_to_day_fraction(hour, minute, second, microsecond) :: day_fraction

  @doc """
  Converts `t:day_fraction` to the Calendar's time format.
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

  # General Helpers

  @doc """
  Returns `true` if two calendars have the same moment of starting a new day,
  `false` otherwise.

  If two calendars are not compatible, we can only convert datetimes and times
  between them. If they are compatible, this means that we can also convert
  dates as well as naive datetimes between them.
  """
  @spec compatible_calendars?(Calendar.calendar, Calendar.calendar) :: boolean
  def compatible_calendars?(calendar, calendar), do: true
  def compatible_calendars?(calendar1, calendar2) do
    calendar1.day_rollover_relative_to_midnight_utc == calendar2.day_rollover_relative_to_midnight_utc
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

  The functions on this module work with the `Date` struct as well
  as any struct that contains the same fields as the `Date` struct,
  such as `NaiveDateTime` and `DateTime`. Such functions expect
  `t:Calendar.date/0` in their typespecs (instead of `t:t/0`).

  Remember, comparisons in Elixir using `==`, `>`, `<` and friends
  are structural and based on the Date struct fields. For proper
  comparison between dates, use the `compare/2` function.

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
  @spec utc_today(Calendar.calendar) :: t
  def utc_today(calendar \\ Calendar.ISO)

  def utc_today(Calendar.ISO) do
    {:ok, {year, month, day}, _, _} = Calendar.ISO.from_unix(System.os_time, :native)
    %Date{year: year, month: month, day: day}
  end

  def utc_today(calendar) do
    calendar
    |> DateTime.utc_now
    |> DateTime.to_date
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
  def leap_year?(date)

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
  def days_in_month(date)

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
  def new(year, month, day, calendar \\ Calendar.ISO) do
    if calendar.valid_date?(year, month, day) do
      {:ok, %Date{year: year, month: month, day: day, calendar: calendar}}
    else
      {:error, :invalid_date}
    end
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
  def to_string(date)

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
  def from_iso8601(string, calendar \\ Calendar.ISO)

  def from_iso8601(<<year::4-bytes, ?-, month::2-bytes, ?-, day::2-bytes>>, calendar) do
    with {year, ""} <- Integer.parse(year),
         {month, ""} <- Integer.parse(month),
         {day, ""} <- Integer.parse(day) do
      with {:ok, date} <- new(year, month, day, Calendar.ISO),
           do: convert(date, calendar)
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

      iex> Date.from_iso8601!("2015-01-23")
      ~D[2015-01-23]
      iex> Date.from_iso8601!("2015:01:23")
      ** (ArgumentError) cannot parse "2015:01:23" as date, reason: :invalid_format
  """
  @spec from_iso8601!(String.t) :: t | no_return
  def from_iso8601!(string, calendar \\ Calendar.ISO) do
    case from_iso8601(string, calendar) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot parse #{inspect string} as date, reason: #{inspect reason}"
    end
  end

  @doc """
  Converts the given `date` to
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601).

  Only supports converting dates which are in the ISO calendar,
  or other calendars in which the days also start at midnight.
  Attempting to convert dates from other calendars will raise an `ArgumentError`.

  ### Examples

      iex> Date.to_iso8601(~D[2000-02-28])
      "2000-02-28"

  """
  @spec to_iso8601(Date.t) :: String.t
  def to_iso8601(%Date{} = date) do
    %{year: year, month: month, day: day} = convert!(date, Calendar.ISO)
    Calendar.ISO.date_to_iso8601(year, month, day)
  end

  # TODO: Remove on 2.0
  def to_iso8601(%{calendar: Calendar.ISO, year: year, month: month, day: day}) do
    IO.warn "calling Date.to_iso8601/1 with a DateTime or NaiveDateTime structs is deprecated, explicitly convert them into a Date first by using DateTime.to_date/1 or NaiveDateTime.to_date/1 respectively"
    Calendar.ISO.date_to_iso8601(year, month, day)
  end

  @doc """
  Converts a `Date` struct to an Erlang date tuple.

  Only supports converting dates which are in the ISO calendar,
  or other calendars in which the days also start at midnight.
  Attempting to convert dates from other calendars will raise an `ArgumentError`.

  ## Examples

      iex> Date.to_erl(~D[2000-01-01])
      {2000, 1, 1}

  """
  @spec to_erl(Date.t) :: :calendar.date
  def to_erl(%Date{} = date) do
    %{year: year, month: month, day: day} = convert!(date, Calendar.ISO)
    {year, month, day}
  end

  # TODO: Remove on 2.0
  def to_erl(%{calendar: Calendar.ISO, year: year, month: month, day: day}) do
    IO.warn "calling Date.to_erl/1 with a DateTime or NaiveDateTime structs is deprecated, explicitly convert them into a Date first by using DateTime.to_date/1 or NaiveDateTime.to_date/1 respectively"
    {year, month, day}
  end

  @doc """
  Converts an Erlang date tuple to a `Date` struct.

  Only supports converting dates which are in the ISO calendar,
  or other calendars in which the days also start at midnight.
  Attempting to convert dates from other calendars will return an error tuple.

  ## Examples

      iex> Date.from_erl({2000, 1, 1})
      {:ok, ~D[2000-01-01]}
      iex> Date.from_erl({2000, 13, 1})
      {:error, :invalid_date}

  """
  @spec from_erl(:calendar.date) :: {:ok, t} | {:error, atom}
  def from_erl(tuple, calendar \\ Calendar.ISO)

  def from_erl({year, month, day}, calendar) do
    with {:ok, date} <- new(year, month, day, Calendar.ISO),
         do: convert(date, calendar)
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
    if Calendar.compatible_calendars?(date1.calendar, date2.calendar) do
      case {to_rata_die(date1), to_rata_die(date2)} do
        {first, second} when first > second -> :gt
        {first, second} when first < second -> :lt
        _ -> :eq
      end
    else
      raise ArgumentError, """
      cannot compare #{inspect date1} with #{inspect date2}.

      This comparison would be ambiguous as their calendars have incompatible day rollover moments.
      Specify an exact time of day (using `DateTime`s) to resolve this ambiguity
      """
    end
  end

  @doc """
  Converts a date from one calendar to another.

  Returns `{:ok, date}` if the calendars are compatible,
  or `{:error, :incompatible_calendars}` if they are not.

  See also `Calendar.compatible_calendars?/2`.
  """
  @spec convert(Date.t, Calendar.calendar) :: {:ok, Date.t} | {:error, :incompatible_calendars}
  def convert(%Date{calendar: calendar} = date, calendar), do: {:ok, date}
  def convert(%Date{} = date, target_calendar) do
    if Calendar.compatible_calendars?(date.calendar, target_calendar) do
      result_date =
        date
        |> to_rata_die()
        |> from_rata_die(target_calendar)
      {:ok, result_date}
    else
      {:error, :incompatible_calendars}
    end
  end

  @doc """
  Similar to `Date.convert/2`, but raises an `ArgumentError`
  if the conversion between the two calendars is not possible.
  """
  @spec convert!(Date.t, Calendar.calendar) :: Date.t
  def convert!(date, calendar) do
    case convert(date, calendar) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot convert #{inspect date} to target calendar #{inspect calendar}, reason: #{inspect reason}"
    end
  end

  @doc """
  Calculates the difference between two dates,
  in a full number of days, returning `{:ok, difference}`.

  Note that only `Date` structs that follow the same or
  compatible calendars can be compared this way.
  If two calendars are not compatible,
  `{:error, :incompatible_calendars}` is returned.

  ## Examples

      iex> Date.diff(~D[2000-01-03], ~D[2000-01-01])
      2
      iex> Date.diff(~D[2000-01-01], ~D[2000-01-03])
      -2

  """
  @spec diff(Date.t, Date.t) :: {:ok, integer} | {:error, :incompatible_calendars}
  def diff(%Date{} = date1, %Date{} = date2) do
    if Calendar.compatible_calendars?(date1.calendar, date2.calendar) do
      {days1, _} = to_rata_die(date1)
      {days2, _} = to_rata_die(date2)
      days1 - days2
    else
      raise ArgumentError, "cannot calculate the difference between #{inspect date1} and #{inspect date2} because their calendars are not compatible and thus the result would be ambiguous"
    end
  end

  defp to_rata_die(%{calendar: calendar, year: year, month: month, day: day}) do
    calendar.naive_datetime_to_rata_die(year, month, day, 0, 0, 0, {0, 0})
  end

  defp from_rata_die(rata_die, target_calendar) do
    {year, month, day, _, _, _, _} = target_calendar.naive_datetime_from_rata_die(rata_die)
    %Date{year: year, month: month, day: day, calendar: target_calendar}
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
  def day_of_week(date)

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
  `t:Calendar.time/0` in their typespecs (instead of `t:t/0`).

  Remember, comparisons in Elixir using `==`, `>`, `<` and friends
  are structural and based on the Time struct fields. For proper
  comparison between times, use the `compare/2` function.

  Developers should avoid creating the Time struct directly and
  instead rely on the functions provided by this module as well as
  the ones in 3rd party calendar libraries.
  """

  @enforce_keys [:hour, :minute, :second]
  defstruct [:hour, :minute, :second, microsecond: {0, 0}, calendar: Calendar.ISO]

  @type t :: %Time{hour: Calendar.hour, minute: Calendar.minute,
                   second: Calendar.second, microsecond: Calendar.microsecond, calendar: Calendar.calendar}

  @doc """
  Returns the current time in UTC.

  ## Examples

      iex> time = Time.utc_now()
      iex> time.hour >= 0
      true

  """
  @spec utc_now(Calendar.calendar) :: t
  def utc_now(calendar \\ Calendar.ISO) do
    {:ok, _, {hour, minute, second}, microsecond} = Calendar.ISO.from_unix(:os.system_time, :native)
    iso_time = %Time{hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: Calendar.ISO}
    convert!(iso_time, calendar)
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
  @spec new(Calendar.hour, Calendar.minute, Calendar.second, Calendar.microsecond, Calendar.calendar) ::
        {:ok, Time.t} | {:error, atom}
  def new(hour, minute, second, microsecond \\ {0, 0}, calendar \\ Calendar.ISO)

  def new(hour, minute, second, microsecond, calendar) when is_integer(microsecond) do
    new(hour, minute, second, {microsecond, 6}, calendar)
  end

  def new(hour, minute, second, {microsecond, precision}, calendar)
      when is_integer(hour) and is_integer(minute) and is_integer(second) and
           is_integer(microsecond) and is_integer(precision) do
    case calendar.valid_time?(hour, minute, second, {microsecond, precision}) do
      true ->
        {:ok, %Time{hour: hour, minute: minute, second: second, microsecond: {microsecond, precision}, calendar: calendar}}
      false ->
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
  def to_string(time)

  def to_string(%{hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: calendar}) do
    calendar.time_to_string(hour, minute, second, microsecond)
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

      iex> Time.from_iso8601("23:50:07,0123456")
      {:ok, ~T[23:50:07.012345]}
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
  def from_iso8601(string, calendar \\ Calendar.ISO)

  def from_iso8601(<<?T, h, rest::binary>>, calendar) when h in ?0..?9 do
    from_iso8601(<<h, rest::binary>>, calendar)
  end

  def from_iso8601(<<hour::2-bytes, ?:, min::2-bytes, ?:, sec::2-bytes, rest::binary>>, calendar) do
    with {hour, ""} <- Integer.parse(hour),
         {min, ""} <- Integer.parse(min),
         {sec, ""} <- Integer.parse(sec),
         {microsec, rest} <- Calendar.ISO.parse_microsecond(rest),
         {_offset, ""} <- Calendar.ISO.parse_offset(rest) do
      with {:ok, utc_time} <- new(hour, min, sec, microsec, Calendar.ISO),
           do: convert(utc_time, calendar)
    else
      _ -> {:error, :invalid_format}
    end
  end

  def from_iso8601(<<_::binary>>, _calendar) do
    {:error, :invalid_format}
  end

  @doc """
  Parses the extended "Local time" format described by
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601).

  Raises if the format is invalid.

  ## Examples

      iex> Time.from_iso8601!("23:50:07,123Z")
      ~T[23:50:07.123]
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

  """
  @spec to_iso8601(Time.t) :: String.t
  def to_iso8601(time)

  def to_iso8601(%Time{} = time) do
    %{hour: hour, minute: minute, second: second, microsecond: microsecond} = convert!(time, Calendar.ISO)
    Calendar.ISO.time_to_iso8601(hour, minute, second, microsecond)
  end

  def to_iso8601(%{hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: Calendar.ISO}) do
    IO.warn "calling Time.to_erl/1 with a DateTime or NaiveDateTime structs is deprecated, explicitly convert them into a Time first by using DateTime.to_time/1 or NaiveDateTime.to_time/1 respectively"
    Calendar.ISO.time_to_iso8601(hour, minute, second, microsecond)
  end

  @doc """
  Converts a `Time` struct to an Erlang time tuple.

  WARNING: Loss of precision may occur, as Erlang time tuples
  only contain hours/minutes/seconds.

  ## Examples

      iex> Time.to_erl(~T[23:30:15.999])
      {23, 30, 15}

  """
  @spec to_erl(Time.t) :: :calendar.time
  def to_erl(%Time{} = time) do
    %{hour: hour, minute: minute, second: second} = convert!(time, Calendar.ISO)
    {hour, minute, second}
  end

  def to_erl(%{calendar: Calendar.ISO, hour: hour, minute: minute, second: second}) do
    IO.warn "calling Time.to_erl/1 with a DateTime or NaiveDateTime structs is deprecated, explicitly convert them into a Time first by using DateTime.to_time/1 or NaiveDateTime.to_time/1 respectively"
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
  @spec from_erl(:calendar.time, Calendar.microsecond, Calendar.calendar) :: {:ok, t} | {:error, atom}
  def from_erl(tuple, microsecond \\ {0, 0}, calendar \\ Calendar.ISO)

  def from_erl({hour, minute, second}, microsecond, calendar) do
    with {:ok, time} <- new(hour, minute, second, microsecond, Calendar.ISO),
         do: convert(time, calendar)
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
  @spec from_erl!(:calendar.time, Calendar.microsecond, Calendar.calendar) :: t | no_return
  def from_erl!(tuple, microsecond \\ {0, 0}, calendar \\ Calendar.ISO) do
    case from_erl(tuple, microsecond, calendar) do
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
  `:eq` is returned.

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
    {parts1, ppd1} = to_day_fraction(time1)
    {parts2, ppd2} = to_day_fraction(time2)

    case {parts1 * ppd2, parts2 * ppd1} do
      {first, second} when first > second -> :gt
      {first, second} when first < second -> :lt
      _ -> :eq
    end
  end

  @doc """
  Converts the `Time` struct to a different calendar.

  Returns `{:ok, time}` if the conversion was successful,
  or `{:error, reason}` if it was not, for some reason.
  """
  @spec convert(Time.t, Calendar.calendar) :: {:ok, Time.t} | {:error, atom}
  def convert(%Time{calendar: calendar} = time, calendar) do
    {:ok, time}
  end

  def convert(%Time{} = time, calendar) do
    result_time =
      time
      |> to_day_fraction()
      |> calendar.time_from_day_fraction
    {:ok, result_time}
  end

  @doc """
  Similar to `Time.convert/2`, but raises an `ArgumentError`
  if the conversion between the two calendars is not possible.
  """
  @spec convert!(Time.t, Calendar.calendar) :: Time.t
  def convert!(time, calendar) do
    case convert(time, calendar) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot convert #{inspect time} to target calendar #{inspect calendar}, reason: #{inspect reason}"
    end
  end

  @doc """
  Returns the difference between two `Time` structs.
  """
  @spec diff(Time.t, Time.t, System.time_unit) :: integer
  def diff(%Time{} = time1, %Time{} = time2, unit \\ :second) do
    {parts1, ppd1} = to_day_fraction(time1)
    {parts2, ppd2} = to_day_fraction(time2)

    diff_ppd = ppd1 * ppd2
    diff_parts = parts1 * ppd2 - parts2 * ppd1

    # Keep integers in day fraction low.
    gcd = Integer.gcd(diff_parts, diff_ppd)
    diff_parts = div(diff_parts, gcd)
    diff_ppd = div(diff_ppd, gcd)

    Calendar.ISO.rata_die_to_unit({0, {diff_parts, diff_ppd}}, unit)
  end

  ## Helpers

  defp to_day_fraction(%{hour: hour, minute: minute, second: second, microsecond: {_, _} = microsecond, calendar: calendar}) do
    calendar.time_to_day_fraction(hour, minute, second, microsecond)
  end

  defp to_day_fraction(%{hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: calendar}) do
    calendar.time_to_day_fraction(hour, minute, second, {microsecond, 0})
  end

  defimpl String.Chars do
    def to_string(%{hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: calendar}) do
      calendar.time_to_string(hour, minute, second, microsecond)
    end
  end

  defimpl Inspect do
    def inspect(%{hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: Calendar.ISO}, _) do
      "~T[" <> Calendar.ISO.time_to_string(hour, minute, second, microsecond) <> "]"
    end

    def inspect(time, opts) do
      Inspect.Any.inspect(time, opts)
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

  This operation is only possible if both calendars are convertible to `Calendar.ISO`.

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

    {days1, {parts1, ppd1}} = to_rata_die(naive_datetime1)
    {days2, {parts2, ppd2}} = to_rata_die(naive_datetime2)

    diff_days = days1 - days2
    diff_ppd = ppd1 * ppd2
    diff_parts = parts1 * ppd2 - parts2 * ppd1

    # Keep integers in day fraction low.
    gcd = Integer.gcd(diff_parts, diff_ppd)
    diff_parts = div(diff_parts, gcd)
    diff_ppd = div(diff_ppd, gcd)

    {diff_days, {diff_parts, diff_ppd}}
    |> normalize_rata_die
    |> Calendar.ISO.rata_die_to_unit(unit)
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
  def to_iso8601(naive_datetime)

  def to_iso8601(%{year: year, month: month, day: day,
                   hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: Calendar.ISO}) do
    Calendar.ISO.naive_datetime_to_iso8601(year, month, day, hour, minute, second, microsecond)
  end

  def to_iso8601(%{year: _, month: _, day: _,
                   hour: _, minute: _, second: _, microsecond: _, calendar: _} = naive_datetime) do
    naive_datetime
    |> convert!(Calendar.ISO)
    |> to_iso8601
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

  defp normalize_rata_die({diff_days, {diff_parts, diff_ppd}}) when diff_parts < 0 do
    {diff_days - 1, {diff_ppd + diff_parts, diff_ppd}}
  end
  defp normalize_rata_die({diff_days, {diff_parts, diff_ppd}}) do
    {diff_days, {diff_parts, diff_ppd}}
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

  @unix_epoch :calendar.datetime_to_gregorian_seconds {{1970, 1, 1}, {0, 0, 0}}

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


  Negative Unix times are supported, up to -#{@unix_epoch} seconds,
  which is equivalent to "0000-01-01T00:00:00Z" or 0 Gregorian seconds.

      iex> DateTime.from_unix(-12345678910)
      {:ok, %DateTime{calendar: Calendar.ISO, day: 13, hour: 4, microsecond: {0, 0}, minute: 44,
                      month: 10, second: 50, std_offset: 0, time_zone: "Etc/UTC", utc_offset: 0,
                      year: 1578, zone_abbr: "UTC"}}

  When a Unix time before that moment is passed to `from_unix/2`, `:error` will be returned.
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

  Negative Unix times are supported, up to -#{@unix_epoch} seconds,
  which is equivalent to "0000-01-01T00:00:00Z" or 0 Gregorian seconds.

      iex> DateTime.from_unix(-12345678910)
      {:ok, %DateTime{calendar: Calendar.ISO, day: 13, hour: 4, microsecond: {0, 0}, minute: 44,
                      month: 10, second: 50, std_offset: 0, time_zone: "Etc/UTC", utc_offset: 0,
                      year: 1578, zone_abbr: "UTC"}}

  When a Unix time before that moment is passed to `from_unix!/2`, an ArgumentError will be raised.
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

  def to_unix(%DateTime{calendar: Calendar.ISO, std_offset: std_offset, utc_offset: utc_offset,
                        hour: hour, minute: minute, second: second, microsecond: {microsecond, _},
                        year: year, month: month, day: day}, unit) when year >= 0 do
    seconds =
      :calendar.datetime_to_gregorian_seconds({{year, month, day}, {hour, minute, second}})
      |> Kernel.-(utc_offset)
      |> Kernel.-(std_offset)
    System.convert_time_unit((seconds - @unix_epoch) * 1_000_000 + microsecond, :microsecond, unit)
  end

  def to_unix(%DateTime{calendar: Calendar.ISO, year: year}, _unit) when year < 0 do
    raise ArgumentError, "cannot convert DateTimes before the ISO year 0 to a Unix time"
  end

  def to_unix(%DateTime{calendar: _, std_offset: _, utc_offset: _,
                        hour: _, minute: _, second: _, microsecond: {_, _},
                        year: _, month: _, day: _} = datetime, unit) do
    datetime
    |> convert!(Calendar.ISO)
    |> to_unix(unit)
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
  @spec to_iso8601(Calendar.datetime) :: String.t
  def to_iso8601(datetime)

  def to_iso8601(%{calendar: Calendar.ISO, year: year, month: month, day: day,
                  hour: hour, minute: minute, second: second, microsecond: microsecond,
                  time_zone: time_zone, zone_abbr: zone_abbr, utc_offset: utc_offset, std_offset: std_offset}) do
    Calendar.ISO.datetime_to_iso8601(year, month, day, hour, minute, second, microsecond,
                                     time_zone, zone_abbr, utc_offset, std_offset)
  end

  def to_iso8601(%{calendar: _, year: _, month: _, day: _,
                   hour: _, minute: _, second: _, microsecond: _,
                   time_zone: _, zone_abbr: _, utc_offset: _, std_offset: _} = datetime) do
    datetime
    |> convert!(Calendar.ISO)
    |> to_iso8601
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
         {min, ""} <- Integer.parse(min),
         {sec, ""} <- Integer.parse(sec),
         {microsec, rest} <- Calendar.ISO.parse_microsecond(rest),
         {:ok, date} <- Date.new(year, month, day),
         {:ok, time} <- Time.new(hour, min, sec, microsec),
         {:ok, offset} <- parse_offset(rest) do
      %{year: year, month: month, day: day} = date
      %{hour: hour, minute: minute, second: second, microsecond: microsecond} = time

      datetime =
        Calendar.ISO.naive_datetime_to_rata_die(year, month, day, hour, minute, second, microsecond)
        |> apply_tz_offset(offset)
        |> normalize_rata_die
        |> from_rata_die("Etc/UTC", "UTC", 0, 0, calendar)

      {:ok, %{datetime | microsecond: microsec}, offset}
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
  def compare(%DateTime{utc_offset: utc_offset1, std_offset: std_offset1} = datetime1, %DateTime{utc_offset: utc_offset2, std_offset: std_offset2} = datetime2) do
    {days1, {parts1, ppd1}} =
      to_rata_die(datetime1)
      |> apply_tz_offset(utc_offset1)
      |> apply_tz_offset(std_offset1)
    {days2, {parts2, ppd2}} =
      to_rata_die(datetime2)
      |> apply_tz_offset(utc_offset2)
      |> apply_tz_offset(std_offset2)

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
  Returns the difference between two `DateTime` structs,
  in the Calendar.rata_die format: {days, day_fraction}

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
  @spec diff(DateTime.t, DateTime.t) :: Calendar.rata_die
  def diff(%DateTime{utc_offset: utc_offset1, std_offset: std_offset1} = datetime1,
           %DateTime{utc_offset: utc_offset2, std_offset: std_offset2} = datetime2, unit \\ :seconds) do
    {days1, {parts1, ppd1}} =
      to_rata_die(datetime1)
      |> apply_tz_offset(utc_offset1)
      |> apply_tz_offset(std_offset1)
    {days2, {parts2, ppd2}} =
      to_rata_die(datetime2)
      |> apply_tz_offset(utc_offset2)
      |> apply_tz_offset(std_offset2)

    diff_days = days1 - days2
    diff_ppd = ppd1 * ppd2
    diff_parts = parts1 * ppd2 - parts2 * ppd1

    # Keep integers in day fraction low.
    gcd = Integer.gcd(diff_parts, diff_ppd)
    diff_parts = div(diff_parts, gcd)
    diff_ppd = div(diff_ppd, gcd)

    {diff_days, {diff_parts, diff_ppd}}
    |> normalize_rata_die
    |> Calendar.ISO.rata_die_to_unit(unit)
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

  defp normalize_rata_die({diff_days, {diff_parts, diff_ppd}}) when diff_parts < 0 do
    {diff_days - 1, {diff_ppd + diff_parts, diff_ppd}}
  end

  defp normalize_rata_die({diff_days, {diff_parts, diff_ppd}}) do
    {diff_days, {diff_parts, diff_ppd}}
  end
end
