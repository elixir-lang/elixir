defmodule FakeCalendar do
  def date_to_string(_, _, _), do: "boom"
  def time_to_string(_, _, _, _), do: "boom"
  def naive_datetime_to_string(_, _, _, _, _, _, _), do: "boom"
  def datetime_to_string(_, _, _, _, _, _, _, _, _, _), do: "boom"
  def day_rollover_relative_to_midnight_utc, do: {123_456, 123_457}

  def inspect(date_or_datetime, opts) do
    Inspect.Any.inspect(date_or_datetime, opts)
  end
end

defmodule FakeTimeZoneDatabase do
  @behaviour Calendar.TimeZoneDatabase

  @time_zone_period_cph_summer_2018 %{
    std_offset: 3600,
    utc_offset: 3600,
    zone_abbr: "CEST",
    from_wall: ~N[2018-03-25 03:00:00],
    until_wall: ~N[2018-10-28 03:00:00]
  }

  @time_zone_period_cph_winter_2018_2019 %{
    std_offset: 0,
    utc_offset: 3600,
    zone_abbr: "CET",
    from_wall: ~N[2018-10-28 02:00:00],
    until_wall: ~N[2019-03-31 02:00:00]
  }

  @time_zone_period_cph_summer_2019 %{
    std_offset: 3600,
    utc_offset: 3600,
    zone_abbr: "CEST",
    from_wall: ~N[2019-03-31 03:00:00],
    until_wall: ~N[2019-10-27 03:00:00]
  }

  @spec time_zone_period_from_utc_iso_days(Calendar.iso_days(), Calendar.time_zone()) ::
          {:ok, TimeZoneDatabase.time_zone_period()} | {:error, :time_zone_not_found}
  @impl true
  def time_zone_period_from_utc_iso_days(iso_days, time_zone) do
    {:ok, ndt} = naive_datetime_from_iso_days(iso_days)
    time_zone_periods_from_utc(time_zone, NaiveDateTime.to_erl(ndt))
  end

  @spec time_zone_periods_from_wall_datetime(Calendar.naive_datetime(), Calendar.time_zone()) ::
          {:ok, TimeZoneDatabase.time_zone_period()}
          | {:ambiguous, TimeZoneDatabase.time_zone_period(), TimeZoneDatabase.time_zone_period()}
          | {:gap,
             {TimeZoneDatabase.time_zone_period(), TimeZoneDatabase.time_zone_period_limit()},
             {TimeZoneDatabase.time_zone_period(), TimeZoneDatabase.time_zone_period_limit()}}
          | {:error, :time_zone_not_found}
  @impl true
  def time_zone_periods_from_wall_datetime(naive_datetime, time_zone) do
    time_zone_periods_from_wall(time_zone, NaiveDateTime.to_erl(naive_datetime))
  end

  defp time_zone_periods_from_utc("Europe/Copenhagen", erl_datetime)
       when erl_datetime >= {{2018, 3, 25}, {1, 0, 0}} and
              erl_datetime < {{2018, 10, 28}, {3, 0, 0}} do
    {:ok, @time_zone_period_cph_summer_2018}
  end

  defp time_zone_periods_from_utc("Europe/Copenhagen", erl_datetime)
       when erl_datetime >= {{2018, 10, 28}, {2, 0, 0}} and
              erl_datetime < {{2019, 3, 31}, {1, 0, 0}} do
    {:ok, @time_zone_period_cph_winter_2018_2019}
  end

  defp time_zone_periods_from_utc("Europe/Copenhagen", erl_datetime)
       when erl_datetime >= {{2019, 3, 31}, {1, 0, 0}} and
              erl_datetime < {{2019, 10, 28}, {3, 0, 0}} do
    {:ok, @time_zone_period_cph_summer_2019}
  end

  defp time_zone_periods_from_utc("Europe/Copenhagen", erl_datetime)
       when erl_datetime >= {{2015, 3, 29}, {1, 0, 0}} and
              erl_datetime < {{2015, 10, 25}, {1, 0, 0}} do
    {:ok,
     %{
       std_offset: 3600,
       utc_offset: 3600,
       zone_abbr: "CEST"
     }}
  end

  defp time_zone_periods_from_utc("America/Los_Angeles", erl_datetime)
       when erl_datetime >= {{2018, 3, 11}, {10, 0, 0}} and
              erl_datetime < {{2018, 11, 4}, {9, 0, 0}} do
    {:ok,
     %{
       std_offset: 3600,
       utc_offset: -28800,
       zone_abbr: "PDT"
     }}
  end

  defp time_zone_periods_from_utc(time_zone, _) when time_zone != "Europe/Copenhagen" do
    {:error, :time_zone_not_found}
  end

  defp time_zone_periods_from_wall("Europe/Copenhagen", erl_datetime)
       when erl_datetime >= {{2019, 3, 31}, {2, 0, 0}} and
              erl_datetime < {{2019, 3, 31}, {3, 0, 0}} do
    {:gap,
     {@time_zone_period_cph_winter_2018_2019, @time_zone_period_cph_winter_2018_2019.until_wall},
     {@time_zone_period_cph_summer_2019, @time_zone_period_cph_summer_2019.from_wall}}
  end

  defp time_zone_periods_from_wall("Europe/Copenhagen", erl_datetime)
       when erl_datetime < {{2018, 10, 28}, {3, 0, 0}} and
              erl_datetime >= {{2018, 10, 28}, {2, 0, 0}} do
    {:ambiguous, @time_zone_period_cph_summer_2018, @time_zone_period_cph_winter_2018_2019}
  end

  defp time_zone_periods_from_wall("Europe/Copenhagen", erl_datetime)
       when erl_datetime >= {{2018, 3, 25}, {3, 0, 0}} and
              erl_datetime < {{2018, 10, 28}, {3, 0, 0}} do
    {:ok, @time_zone_period_cph_summer_2018}
  end

  defp time_zone_periods_from_wall("Europe/Copenhagen", erl_datetime)
       when erl_datetime >= {{2018, 10, 28}, {2, 0, 0}} and
              erl_datetime < {{2019, 3, 31}, {2, 0, 0}} do
    {:ok, @time_zone_period_cph_winter_2018_2019}
  end

  defp time_zone_periods_from_wall("Europe/Copenhagen", erl_datetime)
       when erl_datetime >= {{2019, 3, 31}, {3, 0, 0}} and
              erl_datetime < {{2019, 10, 27}, {3, 0, 0}} do
    {:ok, @time_zone_period_cph_summer_2019}
  end

  defp time_zone_periods_from_wall("Europe/Copenhagen", erl_datetime)
       when erl_datetime >= {{2015, 3, 29}, {3, 0, 0}} and
              erl_datetime < {{2015, 10, 25}, {3, 0, 0}} do
    {:ok,
     %{
       std_offset: 3600,
       utc_offset: 3600,
       zone_abbr: "CEST"
     }}
  end

  defp time_zone_periods_from_wall("Europe/Copenhagen", erl_datetime)
       when erl_datetime >= {{2090, 3, 26}, {3, 0, 0}} and
              erl_datetime < {{2090, 10, 29}, {3, 0, 0}} do
    {:ok,
     %{
       std_offset: 3600,
       utc_offset: 3600,
       zone_abbr: "CEST"
     }}
  end

  defp time_zone_periods_from_wall(time_zone, _) when time_zone != "Europe/Copenhagen" do
    {:error, :time_zone_not_found}
  end

  defp naive_datetime_from_iso_days(iso_days) do
    {year, month, day, hour, minute, second, microsecond} =
      Calendar.ISO.naive_datetime_from_iso_days(iso_days)

    NaiveDateTime.new(year, month, day, hour, minute, second, microsecond)
  end
end
