defmodule Calendar.Julian do
  # This calendar is used to test conversions between calendars.
  # It implements the Julian Calendar

  import Integer, only: [floor_div: 2]

  def date_to_string(year, month, day), do: "#{year}-#{month}-#{day}(O.S.)"

  def naive_datetime_to_string(year, month, day, hour, minute, second, microsecond), do: "#{year}-#{month}-#{day}(#{Calendar.ISO.time_to_string(hour, minute, second, microsecond)})(O.S.)"

  def time_to_string(hour, minute, second, microsecond), do: Calendar.ISO.time_to_string(hour, minute, second, microsecond)

  def date(year, month, day), do: %Date{year: year, month: month, day: day, calendar: Calendar.Julian}

  def naive_datetime(year, month, day, hour, minute, second, microsecond \\ {0, 0})
  def naive_datetime(year, month, day, hour, minute, second, microsecond) when is_integer(microsecond),
    do: naive_datetime(year, month, day, hour, minute, second, {microsecond, 6})
  def naive_datetime(year, month, day, hour, minute, second, microsecond),
    do: %NaiveDateTime{year: year, month: month, day: day, hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: Calendar.Julian}

  def day_rollover_relative_to_midnight_utc, do: {0, 1}

  def naive_datetime_from_rata_die({days, day_fraction}) do
    {year, month, day} = date_from_rata_die(days)
    {hour, minute, second, microsecond} = time_from_day_fraction(day_fraction)
    {year, month, day, hour, minute, second, microsecond}
  end

  defp date_from_rata_die(days) do
      approx     = floor_div((4 * (days - epoch())) + 1464, 1461)
      year       = if approx <= 0, do: approx - 1, else: approx
      prior_days = days - date_to_rata_die(year, 1, 1)
      correction = cond do
        days < date_to_rata_die(year, 3, 1) -> 0
        leap_year?(year) ->  1
        true -> 2
      end
      month       = floor_div(12*(prior_days + correction) + 373, 367)
      day         = 1 + days - date_to_rata_die(year, month, 1)
      {year, month, day}
  end

  def naive_datetime_to_rata_die(year, month, day, hour, minute, second, microsecond) do
    days = date_to_rata_die(year, month, day)
    day_fraction = time_to_day_fraction(hour, minute, second, microsecond)
    {days, day_fraction}
  end

  defp date_to_rata_die(year, month, day) do
    year = if year < 0, do: year + 1, else: year
    days = epoch() - 1 + (365 * (year - 1)) + floor_div(year - 1, 4) + floor_div(367 * month - 362, 12) +
      adjustment_for_leap_year(year, month) + day
    days
  end

  def time_from_day_fraction(day_fraction), do: Calendar.ISO.time_from_day_fraction(day_fraction)

  def time_to_day_fraction(hour, minute, second, microsecond), do: Calendar.ISO.time_to_day_fraction(hour, minute, second, microsecond)

  def leap_year?(year) when is_integer(year) and year >= 0 do
    rem(year, 4) == if year > 0, do: 0, else: 3
  end

  def days_in_month(year, month)

  def days_in_month(year, 2) do
    if leap_year?(year), do: 29, else: 28
  end
  def days_in_month(_, month) when month in [4, 6, 9, 11], do: 30
  def days_in_month(_, month) when month in 1..12, do: 31

  # Helpers
  defp epoch, do: -1

  defp adjustment_for_leap_year(year, month) do
    cond do
      month <= 2       -> 0
      leap_year?(year) -> -1
      true             -> -2
    end
  end
end
