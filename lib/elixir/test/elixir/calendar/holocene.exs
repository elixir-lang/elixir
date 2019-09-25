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

  defp zero_pad(val, count) when val >= 0 do
    num = Integer.to_string(val)
    :binary.copy("0", max(count - byte_size(num), 0)) <> num
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
  def valid_date?(year, month, day) when year >= 10_000 do
    Calendar.ISO.valid_date?(year - 10_000, month, day)
  end

  def valid_date?(year, month, day) when year <= -10_000 do
    Calendar.ISO.valid_date?(year + 10_000, month, day)
  end

  def valid_date?(year, month, day) do
    Calendar.ISO.valid_date?(year, month, day)
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
  defdelegate valid_time?(hour, minute, second, microsecond), to: Calendar.ISO
end
