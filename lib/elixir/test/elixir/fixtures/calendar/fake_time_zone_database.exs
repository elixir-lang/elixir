defmodule FakeTimeZoneDatabase do
  @behaviour TimeZoneDatabase
  @tai_utc_second_difference_before_1972_06_30_23_59_60_utc 10

  @time_zone_period_cph_summer_2018 %{
    from_wall: ~N[2018-03-25 03:00:00],
    std_offset: 3600,
    until_wall: ~N[2018-10-28 03:00:00],
    utc_offset: 3600,
    zone_abbr: "CEST"
  }

  @time_zone_period_cph_winter_2018_2019 %{
    from_wall: ~N[2018-10-28 02:00:00],
    std_offset: 0,
    until_wall: ~N[2019-03-31 02:00:00],
    utc_offset: 3600,
    zone_abbr: "CET"
  }

  @time_zone_period_cph_summer_2019 %{
    from_wall: ~N[2019-03-31 03:00:00],
    std_offset: 3600,
    until_wall: ~N[2019-10-27 03:00:00],
    utc_offset: 3600,
    zone_abbr: "CEST"
  }

  @spec by_utc(Calendar.naive_datetime(), Calendar.time_zone()) ::
          {:ok, TimeZoneDatabase.light_time_zone_period()} | {:error, :time_zone_not_found}
  @impl true
  def by_utc(naive_datetime, time_zone) do
    do_by_utc(time_zone, NaiveDateTime.to_erl(naive_datetime))
  end

  @spec by_wall(Calendar.naive_datetime(), Calendar.time_zone()) ::
          {:single, TimeZoneDatabase.light_time_zone_period()}
          | {:ambiguous, TimeZoneDatabase.light_time_zone_period(),
             TimeZoneDatabase.light_time_zone_period()}
          | {:gap, TimeZoneDatabase.time_zone_period(), TimeZoneDatabase.time_zone_period()}
          | {:error, :time_zone_not_found}
  @impl true
  def by_wall(naive_datetime, time_zone) do
    do_by_wall(time_zone, NaiveDateTime.to_erl(naive_datetime))
  end

  defp do_by_utc("Europe/Copenhagen", erl_datetime)
       when erl_datetime >= {{2018, 3, 25}, {1, 0, 0}} and
              erl_datetime < {{2018, 10, 28}, {3, 0, 0}} do
    {:ok, @time_zone_period_cph_summer_2018 |> to_light_period}
  end

  defp do_by_utc("Europe/Copenhagen", erl_datetime)
       when erl_datetime >= {{2018, 10, 28}, {2, 0, 0}} and
              erl_datetime < {{2019, 3, 31}, {2, 0, 0}} do
    {:ok, @time_zone_period_cph_winter_2018_2019 |> to_light_period}
  end

  defp do_by_utc("Europe/Copenhagen", erl_datetime)
       when erl_datetime >= {{2015, 3, 29}, {1, 0, 0}} and
              erl_datetime < {{2015, 10, 25}, {1, 0, 0}} do
    {:ok,
     %{
       std_offset: 3600,
       utc_offset: 3600,
       zone_abbr: "CEST"
     }}
  end

  defp do_by_utc("America/Los_Angeles", erl_datetime)
       when erl_datetime >= {{2018, 3, 11}, {10, 0, 0}} and
              erl_datetime < {{2018, 11, 4}, {9, 0, 0}} do
    {:ok,
     %{
       std_offset: 3600,
       utc_offset: -28800,
       zone_abbr: "PDT"
     }}
  end

  defp do_by_wall("Europe/Copenhagen", erl_datetime)
       when erl_datetime >= {{2019, 3, 31}, {2, 0, 0}} and
              erl_datetime < {{2019, 3, 31}, {3, 0, 0}} do
    {:gap, @time_zone_period_cph_winter_2018_2019, @time_zone_period_cph_summer_2019}
  end

  defp do_by_wall("Europe/Copenhagen", erl_datetime)
       when erl_datetime < {{2018, 10, 28}, {3, 0, 0}} and
              erl_datetime >= {{2018, 10, 28}, {2, 0, 0}} do
    {:ambiguous, @time_zone_period_cph_summer_2018 |> to_light_period,
     @time_zone_period_cph_winter_2018_2019 |> to_light_period}
  end

  defp do_by_wall("Europe/Copenhagen", erl_datetime)
       when erl_datetime >= {{2018, 3, 25}, {3, 0, 0}} and
              erl_datetime < {{2018, 10, 28}, {3, 0, 0}} do
    {:single, @time_zone_period_cph_summer_2018 |> to_light_period}
  end

  defp do_by_wall("Europe/Copenhagen", erl_datetime)
       when erl_datetime >= {{2018, 10, 28}, {2, 0, 0}} and
              erl_datetime < {{2019, 3, 31}, {2, 0, 0}} do
    {:single, @time_zone_period_cph_winter_2018_2019 |> to_light_period}
  end

  defp do_by_wall("Europe/Copenhagen", erl_datetime)
       when erl_datetime >= {{2019, 3, 31}, {3, 0, 0}} and
              erl_datetime < {{2019, 10, 27}, {3, 0, 0}} do
    {:single, @time_zone_period_cph_summer_2019 |> to_light_period}
  end

  defp do_by_wall("Europe/Copenhagen", erl_datetime)
       when erl_datetime >= {{2015, 3, 29}, {3, 0, 0}} and
              erl_datetime < {{2015, 10, 25}, {3, 0, 0}} do
    {:single,
     %{
       std_offset: 3600,
       utc_offset: 3600,
       zone_abbr: "CEST"
     }}
  end

  defp do_by_wall("Europe/Copenhagen", erl_datetime)
       when erl_datetime >= {{2090, 3, 26}, {3, 0, 0}} and
              erl_datetime < {{2090, 10, 29}, {3, 0, 0}} do
    {:single,
     %{
       std_offset: 3600,
       utc_offset: 3600,
       zone_abbr: "CEST"
     }}
  end

  defp do_by_wall(time_zone, _) when time_zone != "Europe/Copenhagen" do
    {:error, :time_zone_not_found}
  end

  @impl true
  def is_leap_second(%{year: year}) when year < 1972 do
    {:ok, false}
  end

  def is_leap_second(naive_datetime) do
    erl_datetime = naive_datetime |> NaiveDateTime.to_erl()

    leap_seconds_only = leap_seconds() |> Enum.map(fn {dt, _} -> dt end)

    case Enum.member?(leap_seconds_only, erl_datetime) do
      true ->
        {:ok, true}

      false ->
        with :ok <- within_leap_second_data_range(naive_datetime) do
          {:ok, false}
        end
    end
  end

  @spec leap_second_diff(Calendar.naive_datetime(), Calendar.naive_datetime()) :: integer
  @impl true
  def leap_second_diff(datetime1, datetime2) do
    with :ok <- within_leap_second_data_range(datetime1),
         :ok <- within_leap_second_data_range(datetime2) do
      tai_diff1 = latest_utc_tai_difference(datetime1)
      tai_diff2 = latest_utc_tai_difference(datetime2)
      {:ok, tai_diff1 - tai_diff2}
    end
  end

  # For a specific datetime (UTC) return the difference between UTC and TAI
  @spec latest_utc_tai_difference(Calendar.naive_datetime()) :: integer
  defp latest_utc_tai_difference(%{
         year: year,
         month: month,
         day: day,
         hour: hour,
         minute: minute,
         second: second
       })
       when {{year, month, day}, {hour, minute, second}} < {{1972, 6, 30}, {23, 59, 60}} do
    @tai_utc_second_difference_before_1972_06_30_23_59_60_utc
  end

  defp latest_utc_tai_difference(naive_datetime) do
    {_, utc_tai_diff} = latest_leap_second_for_datetime(naive_datetime)
    utc_tai_diff
  end

  @spec latest_leap_second_for_datetime(Calendar.naive_datetime()) ::
          {:calendar.datetime(), integer}
  defp latest_leap_second_for_datetime(naive_datetime) do
    p_erl_datetime = naive_datetime |> NaiveDateTime.to_erl()

    leap_seconds()
    |> Enum.filter(fn {leap_second_only, _tai_diff} -> p_erl_datetime >= leap_second_only end)
    |> List.last()
  end

  @spec within_leap_second_data_range(Calendar.naive_datetime()) ::
          :ok | {:error, :outside_leap_second_data_range}
  defp within_leap_second_data_range(naive_datetime) do
    if NaiveDateTime.to_erl(naive_datetime) > leap_second_data_valid_until() do
      {:error, :outside_leap_second_data_range}
    else
      :ok
    end
  end

  defp leap_seconds do
    [
      {{{1972, 6, 30}, {23, 59, 60}}, 11},
      {{{1972, 12, 31}, {23, 59, 60}}, 12},
      {{{1973, 12, 31}, {23, 59, 60}}, 13},
      {{{1974, 12, 31}, {23, 59, 60}}, 14},
      {{{1975, 12, 31}, {23, 59, 60}}, 15},
      {{{1976, 12, 31}, {23, 59, 60}}, 16},
      {{{1977, 12, 31}, {23, 59, 60}}, 17},
      {{{1978, 12, 31}, {23, 59, 60}}, 18},
      {{{1979, 12, 31}, {23, 59, 60}}, 19},
      {{{1981, 6, 30}, {23, 59, 60}}, 20},
      {{{1982, 6, 30}, {23, 59, 60}}, 21},
      {{{1983, 6, 30}, {23, 59, 60}}, 22},
      {{{1985, 6, 30}, {23, 59, 60}}, 23},
      {{{1987, 12, 31}, {23, 59, 60}}, 24},
      {{{1989, 12, 31}, {23, 59, 60}}, 25},
      {{{1990, 12, 31}, {23, 59, 60}}, 26},
      {{{1992, 6, 30}, {23, 59, 60}}, 27},
      {{{1993, 6, 30}, {23, 59, 60}}, 28},
      {{{1994, 6, 30}, {23, 59, 60}}, 29},
      {{{1995, 12, 31}, {23, 59, 60}}, 30},
      {{{1997, 6, 30}, {23, 59, 60}}, 31},
      {{{1998, 12, 31}, {23, 59, 60}}, 32},
      {{{2005, 12, 31}, {23, 59, 60}}, 33},
      {{{2008, 12, 31}, {23, 59, 60}}, 34},
      {{{2012, 6, 30}, {23, 59, 60}}, 35},
      {{{2015, 6, 30}, {23, 59, 60}}, 36},
      {{{2016, 12, 31}, {23, 59, 60}}, 37}
    ]
  end

  defp leap_second_data_valid_until do
    {{2018, 12, 28}, {0, 0, 0}}
  end

  @spec to_light_period(TimeZoneDatabase.time_zone_period()) ::
          TimeZoneDatabase.light_time_zone_period()
  defp to_light_period(full_period) do
    Map.drop(full_period, [:from_wall, :until_wall])
  end
end
