defmodule Calendar.Julian do
  @moduledoc """
  A calendar implementation that follows the Julian Calendar.

  This calendar implements the proleptic Julian calendar as defined after
  the year 45BC.
  """

  @behaviour Calendar
  @julian_epoch Date.to_integer_date(~D[0000-12-30])

  @type year  :: -9999..9999
  @type month :: 1..12
  @type day   :: 1..31

  import Integer, only: [floor_div: 2]

  @doc """
  Returns the start of the epoch for the ISO calendar which is
  the elapsed number of days since January 1st, Year 1 in the
  proleptic Gregorian calendar.
  """
  @spec epoch :: integer
  def epoch do
    @julian_epoch
  end

  @doc """
  Returns a `Date` representing a date in the Julian calendar
  """
  def date(year, month, day) do
    %Date{calendar: __MODULE__, year: year, month: month, day: day}
  end

  @doc """
  Returns the ordinal `integer date` of the specified date.

  To enable conversion between dates in different calendars a standard integer
  date is defined that normalizes the differences.

  ## Examples

      iex> Calendar.Julian.to_integer_date(Calendar.Julian.date(1,1,1))
      -1
      iex> Calendar.Julian.to_integer_date(Calendar.Julian.date(2016,3,15))
      736051
      iex> Calendar.Julian.to_integer_date(Calendar.Julian.date(2016,12,31))
      736342
  """
  @spec to_integer_date(Calendar.Date | Calendar.DateTime | Calendar.NaiveDateTime | year, month, day) :: integer
  def to_integer_date(%{calendar: _calendar, year: year, month: month, day: day}) do
    to_integer_date(year, month, day)
  end

  def to_integer_date(year, month, day) do
    year = if year < 0, do: year + 1, else: year
    epoch() - 1 + (365 * (year - 1)) + floor_div(year - 1, 4) + floor_div(367 * month - 362, 12) +
      adjustment_for_leap_year(year, month) + day
  end

  defp adjustment_for_leap_year(year, month) do
    cond do
      month <= 2       -> 0
      leap_year?(year) -> -1
      true             -> -2
    end
  end

  @doc """
  Returns a `date` converted from a universal date.

  ## Examples

      iex> Calendar.Julian.from_integer_date(1)
      %Date{calendar: Calendar.Julian, day: 3, month: 1, year: 1}
      iex> Calendar.Julian.from_integer_date(736328)
      %Date{calendar: Calendar.Julian, day: 17, month: 12, year: 2016}
      
  """
  def from_integer_date(date) when is_integer(date) do
    approx     = floor_div((4 * (date - epoch())) + 1464, 1461)
    year       = if approx <= 0, do: approx - 1, else: approx
    prior_days = date - to_integer_date(date(year, 1, 1))
    correction = cond do
                   date < to_integer_date(date(year, 3, 1)) -> 0
                   leap_year?(year) ->  1
                   true -> 2
                 end
    month       = floor_div(12*(prior_days + correction) + 373, 367)
    day         = 1 + date - to_integer_date(date(year, month, 1))
    date(year, month, day)
  end

  @doc """
  Returns how many days there are in the given year-month.

  For this calendar the number of days in a month is the
  same as for Calendar.ISO - the only difference is the
  determination of when a leap year occurs.

  ## Examples

      iex> Calendar.Julian.days_in_month(1900, 1)
      31
      iex> Calendar.Julian.days_in_month(1900, 2)
      29
      iex> Calendar.Julian.days_in_month(2000, 2)
      29
      iex> Calendar.Julian.days_in_month(2001, 2)
      28
      iex> Calendar.Julian.days_in_month(2004, 2)
      29
      iex> Calendar.Julian.days_in_month(2004, 4)
      30

  """
  @spec days_in_month(year, month) :: 28..31
  def days_in_month(year, month)

  def days_in_month(year, 2) do
    if leap_year?(year), do: 29, else: 28
  end
  def days_in_month(_, month) when month in [4, 6, 9, 11], do: 30
  def days_in_month(_, month) when month in 1..12, do: 31

  @doc """
  Returns if the given year is a leap year.

  ## Examples

      iex> Calendar.Julian.leap_year?(2000)
      true
      iex> Calendar.Julian.leap_year?(2001)
      false
      iex> Calendar.Julian.leap_year?(2004)
      true
      iex> Calendar.Julian.leap_year?(1900)
      true

  """
  @spec leap_year?(year) :: boolean()
  def leap_year?(year) when is_integer(year) and year >= 0 do
    rem(year, 4) == if year > 0, do: 0, else: 3
  end

  @doc """
  Calculates the day of the week from the given `year`, `month`, and `day`.

  It is an integer from 1 to 7, where 1 is Monday and 7 is Sunday.

  ## Examples

      iex> Calendar.Julian.day_of_week(2016, 10, 31)
      7
      iex> Calendar.Julian.day_of_week(2016, 11, 01)
      1
      iex> Calendar.Julian.day_of_week(2016, 11, 02)
      2
      iex> Calendar.Julian.day_of_week(2016, 11, 03)
      3
      iex> Calendar.Julian.day_of_week(2016, 11, 04)
      4
      iex> Calendar.Julian.day_of_week(2016, 11, 05)
      5
      iex> Calendar.Julian.day_of_week(2016, 11, 06)
      6

  """
  @spec day_of_week(year, month, day) :: 1..7
  def day_of_week(year, month, day)
      when is_integer(year) and is_integer(month) and is_integer(day) do
    iso_date = date(year, month, day)
    |> to_integer_date
    |> Calendar.ISO.from_integer_date

    Calendar.ISO.day_of_week(iso_date.year, iso_date.month, iso_date.day)
  end

  @doc """
  Converts the given date into a string.
  """
  def date_to_string(year, month, day) do
    Calendar.ISO.date_to_string(year, month, day)
  end

  @doc """
  Converts the datetime (without time zone) into a string.
  """
  def naive_datetime_to_string(year, month, day, hour, minute, second, microsecond) do
    Calendar.ISO.naive_datetime_to_string(year, month, day, hour, minute, second, microsecond)
  end

  @doc """
  Converts the datetime (with time zone) into a string.
  """
  def datetime_to_string(year, month, day, hour, minute, second, microsecond,
                         time_zone, zone_abbr, utc_offset, std_offset) do
    Calendar.ISO.datetime_to_string(year, month, day, hour, minute, second, microsecond,
                                    time_zone, zone_abbr, utc_offset, std_offset)
  end
end
