defmodule FakeTimeZoneDatabase do
  @behaviour TimeZoneDatabase

  @time_zone_period_cph_summer_2018 %{
    from_wall: 63_689_166_000,
    std_offset: 3600,
    until_wall: 63_707_914_800,
    utc_offset: 3600,
    zone_abbr: "CEST"
  }

  @time_zone_period_cph_winter_2018_2019 %{
    from_wall: 63_707_911_200,
    std_offset: 0,
    until_wall: 63_721_216_800,
    utc_offset: 3600,
    zone_abbr: "CET"
  }

  @time_zone_period_cph_summer_2019 %{
    from_wall: 63_721_220_400,
    std_offset: 3600,
    until_wall: 63_739_364_400,
    utc_offset: 3600,
    zone_abbr: "CEST"
  }

  def by_utc(time_zone, erl_datetime) do
    gregorian_seconds = :calendar.datetime_to_gregorian_seconds(erl_datetime)
    do_by_utc(time_zone, gregorian_seconds)
  end

  def by_wall(time_zone, erl_datetime) do
    gregorian_seconds = :calendar.datetime_to_gregorian_seconds(erl_datetime)
    do_by_wall(time_zone, gregorian_seconds)
  end

  defp do_by_utc("Europe/Copenhagen", gregorian_seconds)
       when gregorian_seconds >= 63_689_158_800 and gregorian_seconds < 63_707_914_800 do
    {:ok, @time_zone_period_cph_summer_2018 |> to_light_period}
  end

  defp do_by_utc("Europe/Copenhagen", gregorian_seconds)
       when gregorian_seconds >= 63_689_158_800 and gregorian_seconds < 63_707_907_600 do
    {:ok, @time_zone_period_cph_winter_2018_2019 |> to_light_period}
  end

  defp do_by_utc("America/Los_Angeles", gregorian_seconds)
       when gregorian_seconds >= 63_687_981_600 and gregorian_seconds < 63_708_541_200 do
    {:ok,
     %{
       std_offset: 3600,
       utc_offset: -28800,
       zone_abbr: "PDT"
     }}
  end

  defp do_by_wall("Europe/Copenhagen", gregorian_seconds)
       when gregorian_seconds >= 63_721_216_800 and gregorian_seconds < 63_721_220_400 do
    {:gap, @time_zone_period_cph_winter_2018_2019 |> to_full_period_datetime_tuple,
     @time_zone_period_cph_summer_2019 |> to_full_period_datetime_tuple}
  end

  defp do_by_wall("Europe/Copenhagen", gregorian_seconds)
       when gregorian_seconds < 63_707_914_800 and gregorian_seconds >= 63_707_911_200 do
    {:ambiguous, @time_zone_period_cph_summer_2018 |> to_light_period,
     @time_zone_period_cph_winter_2018_2019 |> to_light_period}
  end

  defp do_by_wall("Europe/Copenhagen", gregorian_seconds)
       when gregorian_seconds >= 63_689_166_000 and gregorian_seconds < 63_707_914_800 do
    {:single, @time_zone_period_cph_summer_2018 |> to_light_period}
  end

  defp do_by_wall("Europe/Copenhagen", gregorian_seconds)
       when gregorian_seconds >= 63_707_911_200 and gregorian_seconds < 63_721_216_800 do
    {:single, @time_zone_period_cph_winter_2018_2019 |> to_light_period}
  end

  defp do_by_wall("Europe/Copenhagen", gregorian_seconds)
       when gregorian_seconds >= 63_721_220_400 and gregorian_seconds < 63_739_364_400 do
    {:single, @time_zone_period_cph_summer_2019 |> to_light_period}
  end

  defp do_by_wall(time_zone, _) when time_zone != "Europe/Copenhagen" do
    {:error, :time_zone_not_found}
  end

  def leap_seconds do
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

  def leap_second_data_valid_until do
    {{2018, 12, 28}, {0, 0, 0}}
  end

  defp to_light_period(full_period_iso_seconds) do
    Map.drop(full_period_iso_seconds, [:from_wall, :until_wall])
  end

  defp to_full_period_datetime_tuple(full_period_iso_seconds) do
    from_wall_erl = full_period_iso_seconds.from_wall |> :calendar.gregorian_seconds_to_datetime()

    until_wall_erl =
      full_period_iso_seconds.until_wall |> :calendar.gregorian_seconds_to_datetime()

    %{full_period_iso_seconds | from_wall: from_wall_erl, until_wall: until_wall_erl}
  end
end
