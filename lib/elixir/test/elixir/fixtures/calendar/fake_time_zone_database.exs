defmodule FakeTimeZoneDatabase do
  @behaviour TimeZoneDatabase

  @time_zone_period_cph_summer_2018 %{
    from_wall: {{2018, 3, 25}, {3, 0, 0}},
    std_offset: 3600,
    until_wall: {{2018, 10, 28}, {3, 0, 0}},
    utc_offset: 3600,
    zone_abbr: "CEST"
  }

  @time_zone_period_cph_winter_2018_2019 %{
    from_wall: {{2018, 10, 28}, {2, 0, 0}},
    std_offset: 0,
    until_wall: {{2019, 3, 31}, {2, 0, 0}},
    utc_offset: 3600,
    zone_abbr: "CET"
  }

  @time_zone_period_cph_summer_2019 %{
    from_wall: {{2019, 3, 31}, {3, 0, 0}},
    std_offset: 3600,
    until_wall: {{2019, 10, 27}, {3, 0, 0}},
    utc_offset: 3600,
    zone_abbr: "CEST"
  }

  def by_utc(naive_datetime, time_zone) do
    erl_datetime = naive_datetime |> NaiveDateTime.to_erl()

    do_by_utc(time_zone, erl_datetime)
  end

  def by_wall(naive_datetime, time_zone) do
    erl_datetime = naive_datetime |> NaiveDateTime.to_erl()

    do_by_wall(time_zone, erl_datetime)
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
    {:gap, @time_zone_period_cph_winter_2018_2019 |> to_full_period_datetime_tuple,
     @time_zone_period_cph_summer_2019 |> to_full_period_datetime_tuple}
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

  def is_leap_second(naive_datetime) do
    erl_datetime = naive_datetime |> NaiveDateTime.to_erl()

    leap_seconds_only = leap_seconds() |> Enum.map(fn {dt, _} -> dt end)

    case Enum.member?(leap_seconds_only, erl_datetime) do
      true ->
        {:ok, true}

      false ->
        if erl_datetime > leap_second_data_valid_until() do
          {:error, :outside_leap_second_data_validity_range}
        else
          {:ok, false}
        end
    end
  end

  defp leap_seconds do
    [
      {{{1971, 12, 31}, {23, 59, 60}}, 10},
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

  defp to_light_period(full_period_iso_seconds) do
    Map.drop(full_period_iso_seconds, [:from_wall, :until_wall])
  end

  defp to_full_period_datetime_tuple(full_period_iso_seconds) do
    from_wall_erl =
      full_period_iso_seconds.from_wall
      |> NaiveDateTime.from_erl!()

    until_wall_erl =
      full_period_iso_seconds.until_wall
      |> NaiveDateTime.from_erl!()

    %{full_period_iso_seconds | from_wall: from_wall_erl, until_wall: until_wall_erl}
  end
end
