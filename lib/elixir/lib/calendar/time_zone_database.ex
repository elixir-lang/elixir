defmodule TimeZoneDatabase do
  @moduledoc """
  This module defines a behaviour for providing time zone data.

  IANA provides time zone data that includes data about different UTC offsets,
  standard offsets for timezones as well as leap second data.
  """

  @typedoc """
  Limit for when a certain time zone period begins and ends.

  It can either be a `Calendar.naive_datetime` or `:min` or `:max`.

  `:min` basically means "since the beginning of time" and `:max` "until forever".
  """
  @type time_zone_period_limit :: Calendar.naive_datetime() | :min | :max

  @typedoc """
  A period where a certain combination of UTC offset, standard offset and zone
  abbreviation is in effect.
  """
  @type time_zone_period :: %{
          optional(any) => any,
          utc_offset: Calendar.utc_offset(),
          std_offset: Calendar.std_offset(),
          zone_abbr: Calendar.zone_abbr()
        }

  @typedoc """
  Like `time_zone_period`, but with two added fields indicating when the period
  begins and ends. In wall time. The fields are `from_wall` and `until_wall`.

  `from_wall` is inclusive. `until_wall` is exclusive. Eg. if a period is from
  2015-03-29 01:00:00 and until 2015-10-25 01:00:00, the period includes and
  begins from the begining of 2015-03-29 01:00:00 and lasts until just before
  2015-10-25 01:00:00.
  """
  @type time_zone_period_with_wall_limits :: %{
          optional(any) => any,
          utc_offset: Calendar.utc_offset(),
          std_offset: Calendar.std_offset(),
          zone_abbr: Calendar.zone_abbr(),
          from_wall: time_zone_period_limit(),
          until_wall: time_zone_period_limit()
        }

  @doc """
  Takes a time zone name and a point in time for UTC and returns a
  `time_zone_period` for that point in time.
  """
  @callback time_zone_period_from_utc_iso_days(Calendar.iso_days(), Calendar.time_zone()) ::
              {:ok, time_zone_period} | {:error, :time_zone_not_found}

  @doc """
  When the provided `datetime` is ambiguous a tuple with `:ambiguous` and a list of two possible
  periods. The periods in the list are sorted with the first element being the one that begins first.

  When the provided `datetime` is in a gap - for instance during the "spring forward" when going
  from winter time to summer time, a tuple with `:gap` and a list of two time zone periods are returned. The first
  period in the list is the period before the gap and the second period is the period just after the gap.

  If there is only a single possible period for the provided `datetime`, the a tuple with `:single`
  and the `time_zone_period` is returned.
  """
  @callback time_zone_periods_from_wall_datetime(Calendar.naive_datetime(), Calendar.time_zone()) ::
              {:single, time_zone_period}
              | {:ambiguous, time_zone_period, time_zone_period}
              | {:gap, time_zone_period_with_wall_limits, time_zone_period_with_wall_limits}
              | {:error, :time_zone_not_found}

  @doc """
  Takes a `Calendar.naive_datetime` and returns {:ok, true} if it is a
  leap second. {:ok, false} if it is not.

  It cannot be predicted exactly when all leap seconds will be introduced in
  the future. Every six months it is announced whether there will be a leap
  second or not at the end of the coming June or December. If this function is
  queried with a datetime that is so far into the future that it is has not
  yet been announced if there will be a leap second or not
  `{:error, :outside_leap_second_data_range}` should be returned.
  """
  @callback is_leap_second(Calander.naive_datetime()) ::
              {:ok, boolean} | {:error, :outside_leap_second_data_range}

  @doc """
  Takes two `Calendar.naive_datetime`s. They should represent UTC datetimes.

  Returns the difference in leap seconds between them. For instance when passed
  `~N[2018-01-01 00:00:00]` and `~N[2014-01-01 00:00:00]` it should return `{:ok, 2}`
  representing two leap seconds.
  """
  @callback leap_second_diff(Calendar.naive_datetime(), Calendar.naive_datetime()) ::
              {:ok, integer}
              | {:error, :outside_leap_second_data_range}
end

defmodule TimeZoneDatabaseClient do
  @moduledoc """
  Module used by Elixir for getting time zone data from a `TimeZoneDatabase` client.
  """

  @typedoc """
  Either a `TimeZoneDatabase.t()` or a `:from_config` atom.

  This can be passed to functions in e.g. the `DateTime` module. If `:from_config`
  is passed, a `TimeZoneDatabase` set via the `set_database/1` function is used.
  """
  @type tz_db_or_config :: TimeZoneDatabase.t() | :from_config

  @doc """
  Function for setting a global time zone database.

  Takes a module that implements the TimeZoneDatabase behaviour.
  """
  def set_database(time_zone_database) do
    :elixir_config.put(:time_zone_database, time_zone_database)
  end

  @doc false
  @spec time_zone_periods_from_wall_datetime(
          Calendar.naive_datetime(),
          Calendar.time_zone(),
          tz_db_or_config
        ) ::
          {:single, TimeZoneDatabase.time_zone_period()}
          | {:ambiguous, TimeZoneDatabase.time_zone_period(), TimeZoneDatabase.time_zone_period()}
          | {:gap, TimeZoneDatabase.time_zone_period_with_wall_limits(),
             TimeZoneDatabase.time_zone_period_with_wall_limits()}
          | {:error, :time_zone_not_found}
          | {:error, :no_time_zone_database}
  def time_zone_periods_from_wall_datetime(
        %{calendar: Calendar.ISO} = naive_datetime,
        time_zone,
        tz_db_or_config
      ) do
    with {:ok, time_zone_database} <- time_zone_database_from_tz_db_or_config(tz_db_or_config) do
      time_zone_database.time_zone_periods_from_wall_datetime(naive_datetime, time_zone)
    end
  end

  @doc false
  @spec time_zone_period_from_utc_iso_days(
          Calendar.iso_days(),
          Calendar.time_zone(),
          tz_db_or_config
        ) ::
          {:ok, TimeZoneDatabase.time_zone_period()}
          | {:error, :time_zone_not_found}
          | {:error, :no_time_zone_database}
  def time_zone_period_from_utc_iso_days(
        iso_days,
        time_zone,
        tz_db_or_config
      ) do
    with {:ok, time_zone_database} <- time_zone_database_from_tz_db_or_config(tz_db_or_config) do
      time_zone_database.time_zone_period_from_utc_iso_days(iso_days, time_zone)
    end
  end

  @doc false
  @spec is_leap_second(Calendar.naive_datetime(), tz_db_or_config) ::
          {:ok, boolean}
          | {:error, :outside_leap_second_data_range}
          | {:error, :no_time_zone_database}
  def is_leap_second(naive_datetime, tz_db_or_config) do
    with {:ok, time_zone_database} <- time_zone_database_from_tz_db_or_config(tz_db_or_config) do
      time_zone_database.is_leap_second(naive_datetime)
    end
  end

  @doc false
  @spec leap_second_diff(Calendar.naive_datetime(), Calendar.naive_datetime(), tz_db_or_config) ::
          {:ok, boolean}
          | {:error, :no_time_zone_database}
          | {:error, :outside_leap_second_data_range}
  def leap_second_diff(datetime1, datetime2, tz_db_or_config) do
    with {:ok, time_zone_database} <- time_zone_database_from_tz_db_or_config(tz_db_or_config) do
      time_zone_database.leap_second_diff(datetime1, datetime2)
    end
  end

  @spec time_zone_database_from_tz_db_or_config(tz_db_or_config) ::
          {:ok, TimeZoneDatabase.t()} | {:error, :no_time_zone_database}
  defp time_zone_database_from_tz_db_or_config(:from_config) do
    case :elixir_config.get(:time_zone_database, :no_time_zone_database) do
      :no_time_zone_database -> {:error, :no_time_zone_database}
      atom when is_atom(atom) -> {:ok, atom}
    end
  end

  defp time_zone_database_from_tz_db_or_config(time_zone_database) do
    {:ok, time_zone_database}
  end
end
