defmodule Date do
  @moduledoc """
  A Date struct and functions.

  The Date struct contains the fields year, month, day and calendar.
  New dates can be built with the `new/3` function or using the
  `~D` (see `Kernel.sigil_D/2`) sigil:

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
  as the ones in third-party calendar libraries.

  ## Comparing dates

  Comparisons in Elixir using `==/2`, `>/2`, `</2` and similar are structural
  and based on the `Date` struct fields. For proper comparison between
  dates, use the `compare/2` function.

  ## Using epochs

  The `add/2` and `diff/2` functions can be used for computing dates
  or retrieving the number of days between instants. For example, if there
  is an interest in computing the number of days from the Unix epoch
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

  @type t :: %__MODULE__{
          year: Calendar.year(),
          month: Calendar.month(),
          day: Calendar.day(),
          calendar: Calendar.calendar()
        }

  @doc """
  Returns a range of dates.

  A range of dates represents a discrete number of dates where
  the first and last values are dates with matching calendars.

  Ranges of dates can be either increasing (`first <= last`) or
  decreasing (`first > last`). They are also always inclusive.

  ## Examples

      iex> Date.range(~D[1999-01-01], ~D[2000-01-01])
      #DateRange<~D[1999-01-01], ~D[2000-01-01]>

  A range of dates implements the `Enumerable` protocol, which means
  functions in the `Enum` module can be used to work with
  ranges:

      iex> range = Date.range(~D[2001-01-01], ~D[2002-01-01])
      iex> Enum.count(range)
      366
      iex> Enum.member?(range, ~D[2001-02-01])
      true
      iex> Enum.take(range, 3)
      [~D[2001-01-01], ~D[2001-01-02], ~D[2001-01-03]]

  """
  @doc since: "1.5.0"
  @spec range(Calendar.date(), Calendar.date()) :: Date.Range.t()
  def range(%{calendar: calendar} = first, %{calendar: calendar} = last) do
    {first_days, _} = to_iso_days(first)
    {last_days, _} = to_iso_days(last)
    # TODO: Deprecate inferring a range with step of -1 on Elixir v1.16
    step = if first_days <= last_days, do: 1, else: -1
    range(first, first_days, last, last_days, calendar, step)
  end

  def range(%{calendar: _, year: _, month: _, day: _}, %{calendar: _, year: _, month: _, day: _}) do
    raise ArgumentError, "both dates must have matching calendars"
  end

  @doc """
  Returns a range of dates with step.

  ## Examples

      iex> range = Date.range(~D[2001-01-01], ~D[2002-01-01], 2)
      iex> range
      #DateRange<~D[2001-01-01], ~D[2002-01-01], 2>
      iex> Enum.count(range)
      183
      iex> Enum.member?(range, ~D[2001-01-03])
      true
      iex> Enum.take(range, 3)
      [~D[2001-01-01], ~D[2001-01-03], ~D[2001-01-05]]

  """
  @doc since: "1.12.0"
  @spec range(Calendar.date(), Calendar.date(), step :: pos_integer | neg_integer) ::
          Date.Range.t()
  def range(%{calendar: calendar} = first, %{calendar: calendar} = last, step)
      when is_integer(step) and step != 0 do
    {first_days, _} = to_iso_days(first)
    {last_days, _} = to_iso_days(last)
    range(first, first_days, last, last_days, calendar, step)
  end

  def range(
        %{calendar: _, year: _, month: _, day: _} = first,
        %{calendar: _, year: _, month: _, day: _} = last,
        step
      ) do
    raise ArgumentError,
          "both dates must have matching calendar and the step must be an integer " <>
            "different than zero, got: #{inspect(first)}, #{inspect(last)}, #{step}"
  end

  defp range(first, first_days, last, last_days, calendar, step) do
    %Date.Range{
      first: %Date{calendar: calendar, year: first.year, month: first.month, day: first.day},
      last: %Date{calendar: calendar, year: last.year, month: last.month, day: last.day},
      first_in_iso_days: first_days,
      last_in_iso_days: last_days,
      step: step
    }
  end

  @doc """
  Returns the current date in UTC.

  ## Examples

      iex> date = Date.utc_today()
      iex> date.year >= 2016
      true

  """
  @doc since: "1.4.0"
  @spec utc_today(Calendar.calendar()) :: t
  def utc_today(calendar \\ Calendar.ISO)

  def utc_today(Calendar.ISO) do
    {:ok, {year, month, day}, _, _} = Calendar.ISO.from_unix(System.os_time(), :native)
    %Date{year: year, month: month, day: day}
  end

  def utc_today(calendar) do
    calendar
    |> DateTime.utc_now()
    |> DateTime.to_date()
  end

  @doc """
  Returns `true` if the year in the given `date` is a leap year.

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
  @doc since: "1.4.0"
  @spec leap_year?(Calendar.date()) :: boolean()
  def leap_year?(date)

  def leap_year?(%{calendar: calendar, year: year}) do
    calendar.leap_year?(year)
  end

  @doc """
  Returns the number of days in the given `date` month.

  ## Examples

      iex> Date.days_in_month(~D[1900-01-13])
      31
      iex> Date.days_in_month(~D[1900-02-09])
      28
      iex> Date.days_in_month(~N[2000-02-20 01:23:45])
      29

  """
  @doc since: "1.4.0"
  @spec days_in_month(Calendar.date()) :: Calendar.day()
  def days_in_month(date)

  def days_in_month(%{calendar: calendar, year: year, month: month}) do
    calendar.days_in_month(year, month)
  end

  @doc """
  Returns the number of months in the given `date` year.

  ## Example

      iex> Date.months_in_year(~D[1900-01-13])
      12

  """
  @doc since: "1.7.0"
  @spec months_in_year(Calendar.date()) :: Calendar.month()
  def months_in_year(date)

  def months_in_year(%{calendar: calendar, year: year}) do
    calendar.months_in_year(year)
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
  @spec new(Calendar.year(), Calendar.month(), Calendar.day(), Calendar.calendar()) ::
          {:ok, t} | {:error, atom}
  def new(year, month, day, calendar \\ Calendar.ISO) do
    if calendar.valid_date?(year, month, day) do
      {:ok, %Date{year: year, month: month, day: day, calendar: calendar}}
    else
      {:error, :invalid_date}
    end
  end

  @doc """
  Builds a new ISO date.

  Expects all values to be integers. Returns `date` if each
  entry fits its appropriate range, raises if the date is invalid.

  ## Examples

      iex> Date.new!(2000, 1, 1)
      ~D[2000-01-01]
      iex> Date.new!(2000, 13, 1)
      ** (ArgumentError) cannot build date, reason: :invalid_date
      iex> Date.new!(2000, 2, 29)
      ~D[2000-02-29]
  """
  @doc since: "1.11.0"
  @spec new!(Calendar.year(), Calendar.month(), Calendar.day(), Calendar.calendar()) :: t
  def new!(year, month, day, calendar \\ Calendar.ISO) do
    case new(year, month, day, calendar) do
      {:ok, value} ->
        value

      {:error, reason} ->
        raise ArgumentError, "cannot build date, reason: #{inspect(reason)}"
    end
  end

  @doc """
  Converts the given date to a string according to its calendar.

  ### Examples

      iex> Date.to_string(~D[2000-02-28])
      "2000-02-28"
      iex> Date.to_string(~N[2000-02-28 01:23:45])
      "2000-02-28"
      iex> Date.to_string(~D[-0100-12-15])
      "-0100-12-15"

  """
  @spec to_string(Calendar.date()) :: String.t()
  def to_string(date)

  def to_string(%{calendar: calendar, year: year, month: month, day: day}) do
    calendar.date_to_string(year, month, day)
  end

  @doc """
  Parses the extended "Dates" format described by
  [ISO 8601:2019](https://en.wikipedia.org/wiki/ISO_8601).

  The year parsed by this function is limited to four digits.

  ## Examples

      iex> Date.from_iso8601("2015-01-23")
      {:ok, ~D[2015-01-23]}

      iex> Date.from_iso8601("2015:01:23")
      {:error, :invalid_format}

      iex> Date.from_iso8601("2015-01-32")
      {:error, :invalid_date}

  """
  @spec from_iso8601(String.t(), Calendar.calendar()) :: {:ok, t} | {:error, atom}
  def from_iso8601(string, calendar \\ Calendar.ISO) do
    with {:ok, {year, month, day}} <- Calendar.ISO.parse_date(string) do
      convert(%Date{year: year, month: month, day: day}, calendar)
    end
  end

  @doc """
  Parses the extended "Dates" format described by
  [ISO 8601:2019](https://en.wikipedia.org/wiki/ISO_8601).

  Raises if the format is invalid.

  ## Examples

      iex> Date.from_iso8601!("2015-01-23")
      ~D[2015-01-23]
      iex> Date.from_iso8601!("2015:01:23")
      ** (ArgumentError) cannot parse "2015:01:23" as date, reason: :invalid_format

  """
  @spec from_iso8601!(String.t(), Calendar.calendar()) :: t
  def from_iso8601!(string, calendar \\ Calendar.ISO) do
    case from_iso8601(string, calendar) do
      {:ok, value} ->
        value

      {:error, reason} ->
        raise ArgumentError, "cannot parse #{inspect(string)} as date, reason: #{inspect(reason)}"
    end
  end

  @doc """
  Converts the given `date` to
  [ISO 8601:2019](https://en.wikipedia.org/wiki/ISO_8601).

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
  @spec to_iso8601(Calendar.date(), :extended | :basic) :: String.t()
  def to_iso8601(date, format \\ :extended)

  def to_iso8601(%{calendar: Calendar.ISO} = date, format) when format in [:basic, :extended] do
    %{year: year, month: month, day: day} = date
    Calendar.ISO.date_to_string(year, month, day, format)
  end

  def to_iso8601(%{calendar: _} = date, format) when format in [:basic, :extended] do
    date
    |> convert!(Calendar.ISO)
    |> to_iso8601()
  end

  @doc """
  Converts the given `date` to an Erlang date tuple.

  Only supports converting dates which are in the ISO calendar,
  or other calendars in which the days also start at midnight.
  Attempting to convert dates from other calendars will raise.

  ## Examples

      iex> Date.to_erl(~D[2000-01-01])
      {2000, 1, 1}

      iex> Date.to_erl(~N[2000-01-01 00:00:00])
      {2000, 1, 1}

  """
  @spec to_erl(Calendar.date()) :: :calendar.date()
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
  @spec from_erl(:calendar.date(), Calendar.calendar()) :: {:ok, t} | {:error, atom}
  def from_erl(tuple, calendar \\ Calendar.ISO)

  def from_erl({year, month, day}, calendar) do
    with {:ok, date} <- new(year, month, day, Calendar.ISO), do: convert(date, calendar)
  end

  @doc """
  Converts an Erlang date tuple but raises for invalid dates.

  ## Examples

      iex> Date.from_erl!({2000, 1, 1})
      ~D[2000-01-01]
      iex> Date.from_erl!({2000, 13, 1})
      ** (ArgumentError) cannot convert {2000, 13, 1} to date, reason: :invalid_date

  """
  @spec from_erl!(:calendar.date(), Calendar.calendar()) :: t
  def from_erl!(tuple, calendar \\ Calendar.ISO) do
    case from_erl(tuple, calendar) do
      {:ok, value} ->
        value

      {:error, reason} ->
        raise ArgumentError,
              "cannot convert #{inspect(tuple)} to date, reason: #{inspect(reason)}"
    end
  end

  @doc """
  Converts a number of gregorian days to a `Date` struct.

  ## Examples

      iex> Date.from_gregorian_days(1)
      ~D[0000-01-02]
      iex> Date.from_gregorian_days(730_485)
      ~D[2000-01-01]
      iex> Date.from_gregorian_days(-1)
      ~D[-0001-12-31]

  """
  @doc since: "1.11.0"
  @spec from_gregorian_days(integer(), Calendar.calendar()) :: t
  def from_gregorian_days(days, calendar \\ Calendar.ISO) when is_integer(days) do
    from_iso_days({days, 0}, calendar)
  end

  @doc """
  Converts a `date` struct to a number of gregorian days.

  ## Examples

      iex> Date.to_gregorian_days(~D[0000-01-02])
      1
      iex> Date.to_gregorian_days(~D[2000-01-01])
      730_485
      iex> Date.to_gregorian_days(~N[2000-01-01 00:00:00])
      730_485

  """
  @doc since: "1.11.0"
  @spec to_gregorian_days(Calendar.date()) :: integer()
  def to_gregorian_days(date) do
    {days, _} = to_iso_days(date)
    days
  end

  @doc """
  Compares two date structs.

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
  @doc since: "1.4.0"
  @spec compare(Calendar.date(), Calendar.date()) :: :lt | :eq | :gt
  def compare(%{calendar: calendar} = date1, %{calendar: calendar} = date2) do
    %{year: year1, month: month1, day: day1} = date1
    %{year: year2, month: month2, day: day2} = date2

    case {{year1, month1, day1}, {year2, month2, day2}} do
      {first, second} when first > second -> :gt
      {first, second} when first < second -> :lt
      _ -> :eq
    end
  end

  def compare(%{calendar: calendar1} = date1, %{calendar: calendar2} = date2) do
    if Calendar.compatible_calendars?(calendar1, calendar2) do
      case {to_iso_days(date1), to_iso_days(date2)} do
        {first, second} when first > second -> :gt
        {first, second} when first < second -> :lt
        _ -> :eq
      end
    else
      raise ArgumentError, """
      cannot compare #{inspect(date1)} with #{inspect(date2)}.

      This comparison would be ambiguous as their calendars have incompatible day rollover moments.
      Specify an exact time of day (using DateTime) to resolve this ambiguity
      """
    end
  end

  @doc """
  Converts the given `date` from its calendar to the given `calendar`.

  Returns `{:ok, date}` if the calendars are compatible,
  or `{:error, :incompatible_calendars}` if they are not.

  See also `Calendar.compatible_calendars?/2`.

  ## Examples

  Imagine someone implements `Calendar.Holocene`, a calendar based on the
  Gregorian calendar that adds exactly 10,000 years to the current Gregorian
  year:

      iex> Date.convert(~D[2000-01-01], Calendar.Holocene)
      {:ok, %Date{calendar: Calendar.Holocene, year: 12000, month: 1, day: 1}}

  """
  @doc since: "1.5.0"
  @spec convert(Calendar.date(), Calendar.calendar()) ::
          {:ok, t} | {:error, :incompatible_calendars}
  def convert(%{calendar: calendar, year: year, month: month, day: day}, calendar) do
    {:ok, %Date{calendar: calendar, year: year, month: month, day: day}}
  end

  def convert(%{calendar: calendar} = date, target_calendar) do
    if Calendar.compatible_calendars?(calendar, target_calendar) do
      result_date =
        date
        |> to_iso_days()
        |> from_iso_days(target_calendar)

      {:ok, result_date}
    else
      {:error, :incompatible_calendars}
    end
  end

  @doc """
  Similar to `Date.convert/2`, but raises an `ArgumentError`
  if the conversion between the two calendars is not possible.

  ## Examples

  Imagine someone implements `Calendar.Holocene`, a calendar based on the
  Gregorian calendar that adds exactly 10,000 years to the current Gregorian
  year:

      iex> Date.convert!(~D[2000-01-01], Calendar.Holocene)
      %Date{calendar: Calendar.Holocene, year: 12000, month: 1, day: 1}

  """
  @doc since: "1.5.0"
  @spec convert!(Calendar.date(), Calendar.calendar()) :: t
  def convert!(date, calendar) do
    case convert(date, calendar) do
      {:ok, value} ->
        value

      {:error, reason} ->
        raise ArgumentError,
              "cannot convert #{inspect(date)} to target calendar #{inspect(calendar)}, " <>
                "reason: #{inspect(reason)}"
    end
  end

  @doc """
  Adds the number of days to the given `date`.

  The days are counted as Gregorian days. The date is returned in the same
  calendar as it was given in.

  ## Examples

      iex> Date.add(~D[2000-01-03], -2)
      ~D[2000-01-01]
      iex> Date.add(~D[2000-01-01], 2)
      ~D[2000-01-03]
      iex> Date.add(~N[2000-01-01 09:00:00], 2)
      ~D[2000-01-03]
      iex> Date.add(~D[-0010-01-01], -2)
      ~D[-0011-12-30]

  """
  @doc since: "1.5.0"
  @spec add(Calendar.date(), integer()) :: t
  def add(%{calendar: Calendar.ISO} = date, days) do
    %{year: year, month: month, day: day} = date

    {year, month, day} =
      Calendar.ISO.date_to_iso_days(year, month, day)
      |> Kernel.+(days)
      |> Calendar.ISO.date_from_iso_days()

    %Date{calendar: Calendar.ISO, year: year, month: month, day: day}
  end

  def add(%{calendar: calendar} = date, days) do
    {base_days, fraction} = to_iso_days(date)
    from_iso_days({base_days + days, fraction}, calendar)
  end

  @doc """
  Calculates the difference between two dates, in a full number of days.

  It returns the number of Gregorian days between the dates. Only `Date`
  structs that follow the same or compatible calendars can be compared
  this way. If two calendars are not compatible, it will raise.

  ## Examples

      iex> Date.diff(~D[2000-01-03], ~D[2000-01-01])
      2
      iex> Date.diff(~D[2000-01-01], ~D[2000-01-03])
      -2
      iex> Date.diff(~D[0000-01-02], ~D[-0001-12-30])
      3
      iex> Date.diff(~D[2000-01-01], ~N[2000-01-03 09:00:00])
      -2

  """
  @doc since: "1.5.0"
  @spec diff(Calendar.date(), Calendar.date()) :: integer
  def diff(%{calendar: Calendar.ISO} = date1, %{calendar: Calendar.ISO} = date2) do
    %{year: year1, month: month1, day: day1} = date1
    %{year: year2, month: month2, day: day2} = date2

    Calendar.ISO.date_to_iso_days(year1, month1, day1) -
      Calendar.ISO.date_to_iso_days(year2, month2, day2)
  end

  def diff(%{calendar: calendar1} = date1, %{calendar: calendar2} = date2) do
    if Calendar.compatible_calendars?(calendar1, calendar2) do
      {days1, _} = to_iso_days(date1)
      {days2, _} = to_iso_days(date2)
      days1 - days2
    else
      raise ArgumentError,
            "cannot calculate the difference between #{inspect(date1)} and #{inspect(date2)} because their calendars are not compatible and thus the result would be ambiguous"
    end
  end

  @doc false
  def to_iso_days(%{calendar: Calendar.ISO, year: year, month: month, day: day}) do
    {Calendar.ISO.date_to_iso_days(year, month, day), {0, 86_400_000_000}}
  end

  def to_iso_days(%{calendar: calendar, year: year, month: month, day: day}) do
    calendar.naive_datetime_to_iso_days(year, month, day, 0, 0, 0, {0, 0})
  end

  defp from_iso_days({days, _}, Calendar.ISO) do
    {year, month, day} = Calendar.ISO.date_from_iso_days(days)
    %Date{year: year, month: month, day: day, calendar: Calendar.ISO}
  end

  defp from_iso_days(iso_days, target_calendar) do
    {year, month, day, _, _, _, _} = target_calendar.naive_datetime_from_iso_days(iso_days)
    %Date{year: year, month: month, day: day, calendar: target_calendar}
  end

  @doc """
  Calculates the day of the week of a given `date`.

  Returns the day of the week as an integer. For the ISO 8601
  calendar (the default), it is an integer from 1 to 7, where
  1 is Monday and 7 is Sunday.

  An optional `starting_on` value may be supplied, which
  configures the weekday the week starts on. The default value
  for it is `:default`, which translates to `:monday` for the
  built-in ISO calendar. Any other weekday may be given to.

  ## Examples

      iex> Date.day_of_week(~D[2016-10-31])
      1
      iex> Date.day_of_week(~D[2016-11-01])
      2
      iex> Date.day_of_week(~N[2016-11-01 01:23:45])
      2
      iex> Date.day_of_week(~D[-0015-10-30])
      3

      iex> Date.day_of_week(~D[2016-10-31], :sunday)
      2
      iex> Date.day_of_week(~D[2016-11-01], :sunday)
      3
      iex> Date.day_of_week(~N[2016-11-01 01:23:45], :sunday)
      3
      iex> Date.day_of_week(~D[-0015-10-30], :sunday)
      4

  """
  @doc since: "1.4.0"
  @spec day_of_week(Calendar.date(), starting_on :: :default | atom) :: Calendar.day_of_week()
  def day_of_week(date, starting_on \\ :default)

  def day_of_week(%{calendar: calendar, year: year, month: month, day: day}, starting_on) do
    {day_of_week, _first, _last} = calendar.day_of_week(year, month, day, starting_on)
    day_of_week
  end

  @doc """
  Calculates a date that is the first day of the week for the given `date`.

  If the day is already the first day of the week, it returns the
  day itself. For the built-in ISO calendar, the week starts on Monday.
  A weekday rather than `:default` can be given as `starting_on`.

  ## Examples

      iex> Date.beginning_of_week(~D[2020-07-11])
      ~D[2020-07-06]
      iex> Date.beginning_of_week(~D[2020-07-06])
      ~D[2020-07-06]
      iex> Date.beginning_of_week(~D[2020-07-11], :sunday)
      ~D[2020-07-05]
      iex> Date.beginning_of_week(~D[2020-07-11], :saturday)
      ~D[2020-07-11]
      iex> Date.beginning_of_week(~N[2020-07-11 01:23:45])
      ~D[2020-07-06]

  """
  @doc since: "1.11.0"
  @spec beginning_of_week(Calendar.date(), starting_on :: :default | atom) :: Date.t()
  def beginning_of_week(date, starting_on \\ :default)

  def beginning_of_week(%{calendar: Calendar.ISO} = date, starting_on) do
    %{year: year, month: month, day: day} = date
    iso_days = Calendar.ISO.date_to_iso_days(year, month, day)

    {year, month, day} =
      case Calendar.ISO.iso_days_to_day_of_week(iso_days, starting_on) do
        1 ->
          {year, month, day}

        day_of_week ->
          Calendar.ISO.date_from_iso_days(iso_days - day_of_week + 1)
      end

    %Date{calendar: Calendar.ISO, year: year, month: month, day: day}
  end

  def beginning_of_week(%{calendar: calendar} = date, starting_on) do
    %{year: year, month: month, day: day} = date

    case calendar.day_of_week(year, month, day, starting_on) do
      {day_of_week, day_of_week, _} ->
        %Date{calendar: calendar, year: year, month: month, day: day}

      {day_of_week, first_day_of_week, _} ->
        add(date, -(day_of_week - first_day_of_week))
    end
  end

  @doc """
  Calculates a date that is the last day of the week for the given `date`.

  If the day is already the last day of the week, it returns the
  day itself. For the built-in ISO calendar, the week ends on Sunday.
  A weekday rather than `:default` can be given as `starting_on`.

  ## Examples

      iex> Date.end_of_week(~D[2020-07-11])
      ~D[2020-07-12]
      iex> Date.end_of_week(~D[2020-07-05])
      ~D[2020-07-05]
      iex> Date.end_of_week(~D[2020-07-06], :sunday)
      ~D[2020-07-11]
      iex> Date.end_of_week(~D[2020-07-06], :sunday)
      ~D[2020-07-11]
      iex> Date.end_of_week(~D[2020-07-06], :saturday)
      ~D[2020-07-10]
      iex> Date.end_of_week(~N[2020-07-11 01:23:45])
      ~D[2020-07-12]

  """
  @doc since: "1.11.0"
  @spec end_of_week(Calendar.date(), starting_on :: :default | atom) :: Date.t()
  def end_of_week(date, starting_on \\ :default)

  def end_of_week(%{calendar: Calendar.ISO} = date, starting_on) do
    %{year: year, month: month, day: day} = date
    iso_days = Calendar.ISO.date_to_iso_days(year, month, day)

    {year, month, day} =
      case Calendar.ISO.iso_days_to_day_of_week(iso_days, starting_on) do
        7 ->
          {year, month, day}

        day_of_week ->
          Calendar.ISO.date_from_iso_days(iso_days + 7 - day_of_week)
      end

    %Date{calendar: Calendar.ISO, year: year, month: month, day: day}
  end

  def end_of_week(%{calendar: calendar} = date, starting_on) do
    %{year: year, month: month, day: day} = date

    case calendar.day_of_week(year, month, day, starting_on) do
      {day_of_week, _, day_of_week} ->
        %Date{calendar: calendar, year: year, month: month, day: day}

      {day_of_week, _, last_day_of_week} ->
        add(date, last_day_of_week - day_of_week)
    end
  end

  @doc """
  Calculates the day of the year of a given `date`.

  Returns the day of the year as an integer. For the ISO 8601
  calendar (the default), it is an integer from 1 to 366.

  ## Examples

      iex> Date.day_of_year(~D[2016-01-01])
      1
      iex> Date.day_of_year(~D[2016-11-01])
      306
      iex> Date.day_of_year(~D[-0015-10-30])
      303
      iex> Date.day_of_year(~D[2004-12-31])
      366

  """
  @doc since: "1.8.0"
  @spec day_of_year(Calendar.date()) :: Calendar.day()
  def day_of_year(date)

  def day_of_year(%{calendar: calendar, year: year, month: month, day: day}) do
    calendar.day_of_year(year, month, day)
  end

  @doc """
  Calculates the quarter of the year of a given `date`.

  Returns the day of the year as an integer. For the ISO 8601
  calendar (the default), it is an integer from 1 to 4.

  ## Examples

      iex> Date.quarter_of_year(~D[2016-10-31])
      4
      iex> Date.quarter_of_year(~D[2016-01-01])
      1
      iex> Date.quarter_of_year(~N[2016-04-01 01:23:45])
      2
      iex> Date.quarter_of_year(~D[-0015-09-30])
      3

  """
  @doc since: "1.8.0"
  @spec quarter_of_year(Calendar.date()) :: non_neg_integer()
  def quarter_of_year(date)

  def quarter_of_year(%{calendar: calendar, year: year, month: month, day: day}) do
    calendar.quarter_of_year(year, month, day)
  end

  @doc """
  Calculates the year-of-era and era for a given
  calendar year.

  Returns a tuple `{year, era}` representing the
  year within the era and the era number.

  ## Examples

      iex> Date.year_of_era(~D[0001-01-01])
      {1, 1}
      iex> Date.year_of_era(~D[0000-12-31])
      {1, 0}
      iex> Date.year_of_era(~D[-0001-01-01])
      {2, 0}

  """
  @doc since: "1.8.0"
  @spec year_of_era(Calendar.date()) :: {Calendar.year(), non_neg_integer()}
  def year_of_era(date)

  def year_of_era(%{calendar: calendar, year: year}) do
    calendar.year_of_era(year)
  end

  @doc """
  Calculates the day-of-era and era for a given
  calendar `date`.

  Returns a tuple `{day, era}` representing the
  day within the era and the era number.

  ## Examples

      iex> Date.day_of_era(~D[0001-01-01])
      {1, 1}

      iex> Date.day_of_era(~D[0000-12-31])
      {1, 0}

  """
  @doc since: "1.8.0"
  @spec day_of_era(Calendar.date()) :: {Calendar.day(), non_neg_integer()}
  def day_of_era(date)

  def day_of_era(%{calendar: calendar, year: year, month: month, day: day}) do
    calendar.day_of_era(year, month, day)
  end

  @doc """
  Calculates a date that is the first day of the month for the given `date`.

  ## Examples

      iex> Date.beginning_of_month(~D[2000-01-31])
      ~D[2000-01-01]
      iex> Date.beginning_of_month(~D[2000-01-01])
      ~D[2000-01-01]
      iex> Date.beginning_of_month(~N[2000-01-31 01:23:45])
      ~D[2000-01-01]

  """
  @doc since: "1.11.0"
  @spec beginning_of_month(Calendar.date()) :: t()
  def beginning_of_month(%{year: year, month: month, calendar: calendar}) do
    %Date{year: year, month: month, day: 1, calendar: calendar}
  end

  @doc """
  Calculates a date that is the last day of the month for the given `date`.

  ## Examples

      iex> Date.end_of_month(~D[2000-01-01])
      ~D[2000-01-31]
      iex> Date.end_of_month(~D[2000-01-31])
      ~D[2000-01-31]
      iex> Date.end_of_month(~N[2000-01-01 01:23:45])
      ~D[2000-01-31]

  """
  @doc since: "1.11.0"
  @spec end_of_month(Calendar.date()) :: t()
  def end_of_month(%{year: year, month: month, calendar: calendar} = date) do
    day = Date.days_in_month(date)
    %Date{year: year, month: month, day: day, calendar: calendar}
  end

  ## Helpers

  defimpl String.Chars do
    def to_string(%{calendar: calendar, year: year, month: month, day: day}) do
      calendar.date_to_string(year, month, day)
    end
  end

  defimpl Inspect do
    def inspect(%{calendar: calendar, year: year, month: month, day: day}, _) do
      "~D[" <> calendar.date_to_string(year, month, day) <> suffix(calendar) <> "]"
    end

    defp suffix(Calendar.ISO), do: ""
    defp suffix(calendar), do: " " <> inspect(calendar)
  end
end
