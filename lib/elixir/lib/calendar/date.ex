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

  Developers should avoid creating the Date structs directly
  and instead rely on the functions provided by this module as well
  as the ones in 3rd party calendar libraries.

  ## Comparing dates

  Comparisons in Elixir using `==`, `>`, `<` and similar are structural
  and based on the `Date` struct fields. For proper comparison between
  dates, use the `compare/2` function.

  ## Using epochs

  The `add/2` and `diff/2` functions can be used for computing dates
  or retrieving the amount of days betweens instants. For example, if there
  is an interest in computing the amount of days from the Unix epoch
  (1970-01-01):

      iex> Date.diff(~D[2010-04-17], ~D[1970-01-01])
      14716

      iex> Date.add(~D[1970-01-01], 14716)
      ~D[2010-04-17]

  Those functions are optimized to deal with common epochs, such
  as the Unix Epoch above or the Gregorian Epoch (0000-01-01).
  """

  @enforce_keys [:year, :month, :day]
  defstruct [:year, :month, :day, calendar: Calendar.ISO]

  @type t :: %Date{year: Calendar.year, month: Calendar.month,
                   day: Calendar.day, calendar: Calendar.calendar}

  @doc """
  Returns a Date.Range

  ## Examples

      iex> Date.range(~D[2000-01-01], ~D[2001-01-01])
      #DateRange<~D[2000-01-01], ~D[2001-01-01]>

  """

  @spec range(Date.t, Date.t) :: Date.Range.t
  def range(%{calendar: calendar} = first, %{calendar: calendar} = last) do
    %Date.Range{
      first: first,
      last: last,
      first_rata_die: to_rata_die_days(first),
      last_rata_die: to_rata_die_days(last),
    }
  end

  def range(%Date{}, %Date{}) do
    raise ArgumentError,
      "both dates must have matching calendars"
  end

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
  @spec from_iso8601!(String.t) :: t
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

  By default, `Date.to_iso8601/2` returns dates formatted in the "extended"
  format, for human readability. It also supports the "basic" format through passing the `:basic` option.

  Only supports converting dates which are in the ISO calendar,
  or other calendars in which the days also start at midnight.
  Attempting to convert dates from other calendars will raise an `ArgumentError`.

  ### Examples

      iex> Date.to_iso8601(~D[2000-02-28])
      "2000-02-28"

      iex> Date.to_iso8601(~D[2000-02-28], :basic)
      "20000228"

      iex> Date.to_iso8601(~N[2000-02-28 00:00:00])
      "2000-02-28"

  """
  @spec to_iso8601(Calendar.date, :extended | :basic) :: String.t
  def to_iso8601(date, format \\ :extended) when format in [:basic, :extended] do
    %{year: year, month: month, day: day} = convert!(date, Calendar.ISO)
    Calendar.ISO.date_to_iso8601(year, month, day, format)
  end

  @doc """
  Converts a `Date` struct to an Erlang date tuple.

  Only supports converting dates which are in the ISO calendar,
  or other calendars in which the days also start at midnight.
  Attempting to convert dates from other calendars will raise.

  ## Examples

      iex> Date.to_erl(~D[2000-01-01])
      {2000, 1, 1}

      iex> Date.to_erl(~N[2000-01-01 00:00:00])
      {2000, 1, 1}

  """
  @spec to_erl(Calendar.date) :: :calendar.date
  def to_erl(date) do
    %{year: year, month: month, day: day} = convert!(date, Calendar.ISO)
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
  @spec from_erl!(:calendar.date) :: t
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
  def compare(%{calendar: calendar, year: year1, month: month1, day: day1},
              %{calendar: calendar, year: year2, month: month2, day: day2}) do
    case {{year1, month1, day1}, {year2, month2, day2}} do
      {first, second} when first > second -> :gt
      {first, second} when first < second -> :lt
      _ -> :eq
    end
  end
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
  @spec convert(Calendar.date(), Calendar.calendar) :: {:ok, Date.t} | {:error, :incompatible_calendars}
  def convert(%Date{calendar: calendar} = date, calendar), do: {:ok, date}
  def convert(%{calendar: calendar} = date, target_calendar) do
    if Calendar.compatible_calendars?(calendar, target_calendar) do
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
  Adds the number of days to the given date.

  The days are counted as gregorian days. The date is returned in the same
  calendar as it was given in.

  ## Examples

      iex> Date.add(~D[2000-01-03], -2)
      ~D[2000-01-01]
      iex> Date.add(~D[2000-01-01], 2)
      ~D[2000-01-03]

  """
  @spec add(Date.t, integer()) :: Date.t
  def add(%Date{calendar: calendar} = date, days) do
    {rata_days, fraction} = to_rata_die(date)
    from_rata_die({rata_days + days, fraction}, calendar)
  end

  @doc """
  Calculates the difference between two dates, in a full number of days.

  It returns the number of gregorian days between the dates. Only `Date`
  structs that follow the same or compatible calendars can be compared
  this way. If two calendars are not compatible, it will raise.

  ## Examples

      iex> Date.diff(~D[2000-01-03], ~D[2000-01-01])
      2
      iex> Date.diff(~D[2000-01-01], ~D[2000-01-03])
      -2

  """
  @spec diff(Date.t, Date.t) :: integer
  def diff(%Date{calendar: Calendar.ISO, year: year1, month: month1, day: day1},
           %Date{calendar: Calendar.ISO, year: year2, month: month2, day: day2}) do
    Calendar.ISO.date_to_rata_die_days(year1, month1, day1) -
      Calendar.ISO.date_to_rata_die_days(year2, month2, day2)
  end

  def diff(%Date{} = date1, %Date{} = date2) do
    if Calendar.compatible_calendars?(date1.calendar, date2.calendar) do
      {days1, _} = to_rata_die(date1)
      {days2, _} = to_rata_die(date2)
      days1 - days2
    else
      raise ArgumentError, "cannot calculate the difference between #{inspect date1} and #{inspect date2} because their calendars are not compatible and thus the result would be ambiguous"
    end
  end

  defp to_rata_die(%{calendar: Calendar.ISO, year: year, month: month, day: day}) do
    {Calendar.ISO.date_to_rata_die_days(year, month, day), {0, 86400000000}}
  end
  defp to_rata_die(%{calendar: calendar, year: year, month: month, day: day}) do
    calendar.naive_datetime_to_rata_die(year, month, day, 0, 0, 0, {0, 0})
  end

  @doc false
  def to_rata_die_days(date) do
    {days, _} = to_rata_die(date)
    days
  end

  defp from_rata_die({days, _}, Calendar.ISO) do
    {year, month, day} = Calendar.ISO.date_from_rata_die_days(days)
    %Date{year: year, month: month, day: day, calendar: Calendar.ISO}
  end
  defp from_rata_die(rata_die, target_calendar) do
    {year, month, day, _, _, _, _} = target_calendar.naive_datetime_from_rata_die(rata_die)
    %Date{year: year, month: month, day: day, calendar: target_calendar}
  end

  @doc false
  def from_rata_die_days(rata_die, target_calendar) do
    from_rata_die({rata_die, {0, 86400000000}}, target_calendar)
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
