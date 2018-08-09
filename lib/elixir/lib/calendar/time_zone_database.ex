defmodule TimeZoneDatabase do
  @moduledoc """
  This module defines a behaviour for providing time zone data.

  IANA provides time zone data that includes data about different UTC offsets,
  standard offsets for timezones as well as leap second data.
  """

  @typedoc """
  Limit for when a certain time zone period begins and ends.

  It can either be an integer representing ISO seconds or `:min` or `:max`.

  `:min` basically means "since the beginning of time" and `:max` "until forever".
  """
  @type time_zone_period_limit :: Calendar.ISO.iso_seconds() | :min | :max

  @typedoc """
  A period where a certain combination of UTC offset, standard offset and zone
  abbreviation is in effect.

  `from_utc` and `from_wall` is inclusive while `until_utc` and `until_wall` is
  exclusive. Eg. if a period from 63594810000 and until 63612954000, the period
  includes and begins from the begining of second 63594810000 and lasts until
  just before second 63612954000.
  """
  @type time_zone_period :: %{
          utc_offset: Calendar.utc_offset(),
          std_offset: Calendar.std_offset(),
          zone_abbr: Calendar.zone_abbr(),
          from_wall: time_zone_period_limit,
          until_wall: time_zone_period_limit
        }

  @typedoc """
  Like `time_zone_period`, but without `from_wall` and `until_wall`.
  """
  @type light_time_zone_period :: %{
          utc_offset: Calendar.utc_offset(),
          std_offset: Calendar.std_offset(),
          zone_abbr: Calendar.zone_abbr()
        }

  @doc """
  Takes a time zone name and a point in time for UTC and returns a
  `time_zone_period` for that point in time.
  """
  @callback by_utc(Calendar.time_zone(), Calendar.ISO.iso_seconds()) ::
              {:ok, light_time_zone_period} | {:error, :time_zone_not_found}

  @doc """
  When the provided `iso_seconds` is ambiguous for the datetime a tuple with `:ambiguous` and a list of two possible
  periods. The periods in the list are sorted with the first element being the one that begins first.

  When the provided `iso_seconds` datetime is in a gap - for instance during the "spring forward" when going
  from winter time to summer time, a tuple with `:gap` and a list of two time zone periods are returned. The first
  period in the list is the period before the gap and the second period is the period just after the gap.

  If there is only a single possible period for the provided `iso_seconds`, the a tuple with `:single`
  and the `time_zone_period` is returned.
  """
  @callback by_wall(Calendar.time_zone(), Calendar.ISO.iso_seconds()) ::
              {:single, light_time_zone_period}
              | {:ambiguous, [light_time_zone_period]}
              | {:gap, [time_zone_period]}
              | {:error, :time_zone_not_found}

  @doc """
  Returns a list of all known leap seconds. Each element in the list is a tuple
  with the first element being the UTC datetime for the leap second and the
  second element being the difference between TAI and UTC in seconds. (TAI-UTC)
  """
  @callback leap_seconds() :: [{:calendar.datetime(), integer()}]

  @doc """
  Returns a datetime tuple with the UTC datetime for when the leap second
  data returned by `leap_seconds/0` is valid until.

  International Earth Rotation and Reference Systems Service (IERS)
  periodically announces if there will be any leap seconds for the next
  ~6 months.
  """
  @callback leap_second_data_valid_until() :: :calendar.datetime()
end
