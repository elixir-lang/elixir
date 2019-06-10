defmodule Calendar.Holocene do
  # This calendar is used to test conversions between calendars.
  # It implements the Holocene calendar, which is based on the
  # Proleptic Gregorian calendar with every year + 10000.

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
    "#{year}-#{month}-#{day} (HE)"
  end

  @impl true
  def naive_datetime_to_string(year, month, day, hour, minute, second, microsecond) do
    "#{year}-#{month}-#{day}" <>
      Calendar.ISO.time_to_string(hour, minute, second, microsecond) <> " (HE)"
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
    "#{year}-#{month}-#{day} " <>
      Calendar.ISO.time_to_string(hour, minute, second, microsecond) <> " #{zone_abbr} (HE)"
  end

  @impl true
  defdelegate time_to_string(hour, minute, second, microsecond), to: Calendar.ISO

  @impl true
  def day_rollover_relative_to_midnight_utc(), do: {0, 1}

  @impl true
  def naive_datetime_from_iso_days(entry) do
    {year, month, day, hour, minute, second, microsecond} =
      Calendar.ISO.naive_datetime_from_iso_days(entry)

    {year + 10000, month, day, hour, minute, second, microsecond}
  end

  @impl true
  def naive_datetime_to_iso_days(year, month, day, hour, minute, second, microsecond) do
    Calendar.ISO.naive_datetime_to_iso_days(
      year - 10000,
      month,
      day,
      hour,
      minute,
      second,
      microsecond
    )
  end

  @impl true
  def inspect(%Date{year: year, month: month, day: day}, _opts) do
    date_to_string(year, month, day)
  end

  def inspect(
        %DateTime{
          year: year,
          month: month,
          day: day,
          hour: hour,
          minute: minute,
          second: second,
          microsecond: microsecond,
          time_zone: time_zone,
          zone_abbr: zone_abbr,
          utc_offset: utc_offset,
          std_offset: std_offset
        },
        _opts
      ) do
    datetime_to_string(
      year,
      month,
      day,
      hour,
      minute,
      second,
      microsecond,
      time_zone,
      zone_abbr,
      utc_offset,
      std_offset
    )
  end

  def inspect(
        %NaiveDateTime{
          year: year,
          month: month,
          day: day,
          hour: hour,
          minute: minute,
          second: second,
          microsecond: microsecond
        },
        _opts
      ) do
    naive_datetime_to_string(
      year,
      month,
      day,
      hour,
      minute,
      second,
      microsecond
    )
  end

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
  defdelegate day_of_week(year, month, day), to: Calendar.ISO

  @impl true
  defdelegate day_of_year(year, month, day), to: Calendar.ISO

  @impl true
  defdelegate quarter_of_year(year, month, day), to: Calendar.ISO

  @impl true
  defdelegate year_of_era(year), to: Calendar.ISO

  @impl true
  defdelegate day_of_era(year, month, day), to: Calendar.ISO

  @impl true
  def valid_date?(year, month, day) do
    :calendar.valid_date(year, month, day)
  end

  @impl true
  defdelegate valid_time?(hour, minute, second, microsecond), to: Calendar.ISO
end
