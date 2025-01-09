defmodule Calendar.WeekStartsSunday do
  # This calendar is used to test day_of_week calculations
  # when the first day of the week is greater than the last
  # day of the week. In this calendar, the week starts on
  # Sunday (7) and end on Saturday (6).

  # The rest of this calendar is a copy/paste of Calendar.Holocene
  # except removing the 1000 year offset.

  @behaviour Calendar

  def date(year, month, day) do
    %Date{year: year, month: month, day: day, calendar: __MODULE__}
  end

  def naive_datetime(year, month, day, hour, minute, second, microsecond \\ {0, 0}) do
    %NaiveDateTime{
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond,
      calendar: __MODULE__
    }
  end

  @impl true
  def date_to_string(year, month, day) do
    "#{year}-#{zero_pad(month, 2)}-#{zero_pad(day, 2)}"
  end

  @impl true
  def naive_datetime_to_string(year, month, day, hour, minute, second, microsecond) do
    "#{year}-#{zero_pad(month, 2)}-#{zero_pad(day, 2)}" <>
      Calendar.ISO.time_to_string(hour, minute, second, microsecond)
  end

  @impl true
  def datetime_to_string(
        year,
        month,
        day,
        hour,
        minute,
        second,
        microsecond,
        _time_zone,
        zone_abbr,
        _utc_offset,
        _std_offset
      ) do
    "#{year}-#{zero_pad(month, 2)}-#{zero_pad(day, 2)}" <>
      Calendar.ISO.time_to_string(hour, minute, second, microsecond) <>
      " #{zone_abbr}"
  end

  @impl true
  def day_of_week(year, month, day, _starting_on) do
    {day_of_week, 1, 7} = Calendar.ISO.day_of_week(year, month, day, :sunday)
    if day_of_week == 1, do: {7, 7, 6}, else: {day_of_week - 1, 7, 6}
  end

  @impl true
  defdelegate time_to_string(hour, minute, second, microsecond), to: Calendar.ISO

  @impl true
  def day_rollover_relative_to_midnight_utc(), do: {0, 1}

  @impl true
  def naive_datetime_from_iso_days(entry) do
    {year, month, day, hour, minute, second, microsecond} =
      Calendar.ISO.naive_datetime_from_iso_days(entry)

    {year, month, day, hour, minute, second, microsecond}
  end

  @impl true
  def naive_datetime_to_iso_days(year, month, day, hour, minute, second, microsecond) do
    Calendar.ISO.naive_datetime_to_iso_days(
      year,
      month,
      day,
      hour,
      minute,
      second,
      microsecond
    )
  end

  defp zero_pad(val, count) when val >= 0 do
    String.pad_leading("#{val}", count, ["0"])
  end

  defp zero_pad(val, count) do
    "-" <> zero_pad(-val, count)
  end

  @impl true
  def parse_date(string) do
    {year, month, day} =
      string
      |> String.split("-")
      |> Enum.map(&String.to_integer/1)
      |> List.to_tuple()

    if valid_date?(year, month, day) do
      {:ok, {year, month, day}}
    else
      {:error, :invalid_date}
    end
  end

  @impl true
  def valid_date?(year, month, day) do
    :calendar.valid_date(year, month, day)
  end

  @impl true
  defdelegate parse_time(string), to: Calendar.ISO

  @impl true
  defdelegate parse_naive_datetime(string), to: Calendar.ISO

  @impl true
  defdelegate parse_utc_datetime(string), to: Calendar.ISO

  @impl true
  defdelegate time_from_day_fraction(day_fraction), to: Calendar.ISO

  @impl true
  defdelegate time_to_day_fraction(hour, minute, second, microsecond), to: Calendar.ISO

  @impl true
  defdelegate leap_year?(year), to: Calendar.ISO

  @impl true
  defdelegate days_in_month(year, month), to: Calendar.ISO

  @impl true
  defdelegate months_in_year(year), to: Calendar.ISO

  @impl true
  defdelegate day_of_year(year, month, day), to: Calendar.ISO

  @impl true
  defdelegate quarter_of_year(year, month, day), to: Calendar.ISO

  @impl true
  defdelegate year_of_era(year, month, day), to: Calendar.ISO

  @impl true
  defdelegate day_of_era(year, month, day), to: Calendar.ISO

  @impl true
  defdelegate valid_time?(hour, minute, second, microsecond), to: Calendar.ISO

  @impl true
  defdelegate iso_days_to_beginning_of_day(iso_days), to: Calendar.ISO

  @impl true
  defdelegate iso_days_to_end_of_day(iso_days), to: Calendar.ISO

  # The Holocene calendar extends most year and day count guards implemented in the ISO calendars.
  @impl true
  def shift_date(_year, _month, _day, _duration) do
    raise "shift_date/4 not implemented"
  end

  @impl true
  def shift_naive_datetime(_year, _month, _day, _hour, _minute, _second, _microsecond, _duration) do
    raise "shift_naive_datetime/8 not implemented"
  end

  @impl true
  def shift_time(_hour, _minute, _second, _microsecond, _duration) do
    raise "shift_time/5 not implemented"
  end
end
