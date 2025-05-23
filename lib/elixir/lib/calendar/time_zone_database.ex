# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Calendar.TimeZoneDatabase do
  @moduledoc """
  This module defines a behaviour for providing time zone data.

  IANA provides time zone data that includes data about different
  UTC offsets and standard offsets for time zones.
  """

  @typedoc """
  A period where a certain combination of UTC offset, standard offset, and zone
  abbreviation is in effect.

  For example, one period could be the summer of 2018 in the `Europe/London` timezone,
  where summer time/daylight saving time is in effect and lasts from spring to autumn.
  In autumn, the `std_offset` changes along with the `zone_abbr` so a different
  period is needed during winter.
  """
  @type time_zone_period :: %{
          optional(any) => any,
          utc_offset: Calendar.utc_offset(),
          std_offset: Calendar.std_offset(),
          zone_abbr: Calendar.zone_abbr()
        }

  @typedoc """
  Limit for when a certain time zone period begins or ends.

  A beginning is inclusive. An ending is exclusive. For example, if a period is from
  `2015-03-29 01:00:00` and until `2015-10-25 01:00:00`, the period includes and
  begins from the beginning of `2015-03-29 01:00:00` and lasts until just before
  `2015-10-25 01:00:00`.

  A beginning or end for certain periods are infinite, such as the latest
  period for time zones without DST or plans to change. However, for the purpose
  of this behaviour, they are only used for gaps in wall time where the needed
  period limits are at a certain time.
  """
  @type time_zone_period_limit :: Calendar.naive_datetime()

  @doc """
  Time zone period for a point in time in UTC for a specific time zone.

  Takes a time zone name and a point in time for UTC and returns a
  `time_zone_period` for that point in time.
  """
  @doc since: "1.8.0"
  @callback time_zone_period_from_utc_iso_days(Calendar.iso_days(), Calendar.time_zone()) ::
              {:ok, time_zone_period}
              | {:error, :time_zone_not_found | :utc_only_time_zone_database}

  @doc """
  Possible time zone periods for a certain time zone and wall clock date and time.

  When the provided naive datetime is ambiguous, return a tuple with `:ambiguous`
  and the two possible periods. The periods in the tuple must be sorted with the
  first element being the one that begins first.

  When the provided naive datetime is in a gap, such as during the "spring forward" when going
  from winter time to summer time, return a tuple with `:gap` and two periods with limits
  in a nested tuple. The first nested two-tuple is the period before the gap and a naive datetime
  with a limit for when the period ends (wall time). The second nested two-tuple is the period
  just after the gap and a datetime (wall time) for when the period begins just after the gap.

  If there is only a single possible period for the provided `datetime`, then return a tuple
  with `:ok` and the `time_zone_period`.
  """
  @doc since: "1.8.0"
  @callback time_zone_periods_from_wall_datetime(Calendar.naive_datetime(), Calendar.time_zone()) ::
              {:ok, time_zone_period}
              | {:ambiguous, time_zone_period, time_zone_period}
              | {:gap, {time_zone_period, time_zone_period_limit},
                 {time_zone_period, time_zone_period_limit}}
              | {:error, :time_zone_not_found | :utc_only_time_zone_database}
end

defmodule Calendar.UTCOnlyTimeZoneDatabase do
  @moduledoc """
  Built-in time zone database that works only in the `Etc/UTC` timezone.

  For all other time zones, it returns `{:error, :utc_only_time_zone_database}`.
  """

  @behaviour Calendar.TimeZoneDatabase

  @impl true
  def time_zone_period_from_utc_iso_days(_, "Etc/UTC"),
    do: {:ok, %{std_offset: 0, utc_offset: 0, zone_abbr: "UTC"}}

  def time_zone_period_from_utc_iso_days(_, _),
    do: {:error, :utc_only_time_zone_database}

  @impl true
  def time_zone_periods_from_wall_datetime(_, "Etc/UTC"),
    do: {:ok, %{std_offset: 0, utc_offset: 0, zone_abbr: "UTC"}}

  def time_zone_periods_from_wall_datetime(_, _),
    do: {:error, :utc_only_time_zone_database}
end
