defmodule TimeZoneDatabase do
  @moduledoc """
  This module defines a behaviour for providing time zone data.

  IANA provides time zone data that includes data about different UTC offsets,
  standard offsets for timezones as well as leap second data.
  """

  @typedoc """
  Limit for when a certain time zone period begins and ends.

  It can either be an Erlang-style datetime tuple or `:min` or `:max`.

  `:min` basically means "since the beginning of time" and `:max` "until forever".
  """
  @type time_zone_period_limit :: Calendar.datetime() | :min | :max

  @typedoc """
  A period where a certain combination of UTC offset, standard offset and zone
  abbreviation is in effect.

  `from_utc` and `from_wall` is inclusive while `until_utc` and `until_wall` is
  exclusive. Eg. if a period from {{2015, 3, 29}, {1, 0, 0}} and until
  {{2015, 10, 25}, {1, 0, 0}}, the period includes and begins from the begining
  of {{2015, 3, 29}, {1, 0, 0}} and lasts until just before
  {{2015, 10, 25}, {1, 0, 0}}.
  """
  @type time_zone_period :: %{
          utc_offset: Calendar.utc_offset(),
          std_offset: Calendar.std_offset(),
          zone_abbr: Calendar.zone_abbr(),
          from_wall: time_zone_period_limit(),
          until_wall: time_zone_period_limit()
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
  @callback by_utc(Calendar.time_zone(), Calendar.naive_datetime()) ::
              {:ok, light_time_zone_period} | {:error, :time_zone_not_found}

  @doc """
  When the provided `datetime` is ambiguous a tuple with `:ambiguous` and a list of two possible
  periods. The periods in the list are sorted with the first element being the one that begins first.

  When the provided `datetime` is in a gap - for instance during the "spring forward" when going
  from winter time to summer time, a tuple with `:gap` and a list of two time zone periods are returned. The first
  period in the list is the period before the gap and the second period is the period just after the gap.

  If there is only a single possible period for the provided `datetime`, the a tuple with `:single`
  and the `time_zone_period` is returned.
  """
  @callback by_wall(Calendar.time_zone(), Calendar.naive_datetime()) ::
              {:single, light_time_zone_period}
              | {:ambiguous, light_time_zone_period, light_time_zone_period}
              | {:gap, time_zone_period, time_zone_period}
              | {:error, :time_zone_not_found}

  @doc """
  Returns a datetime tuple with the UTC datetime for when the leap second
  data returned by `leap_seconds/0` is valid until.

  International Earth Rotation and Reference Systems Service (IERS)
  periodically announces if there will be any leap seconds for the next
  ~6 months.
  """
  @callback leap_second_data_valid_until() :: Calendar.naive_datetime()
end

defmodule TimeZoneDatabaseClient do
  @moduledoc """
  Module used by Elixir for getting time zone data from a TimeZoneDatabase client.
  """

  @type tz_db_or_config :: TimeZoneDatabase.t() | :from_config

  @no_valid_time_zone_database_error "No valid TimeZoneDatabase provided or configured. Configure with TimeZoneDatabaseClient.set_database/1"

  @doc """
  Function for setting a global time zone database.

  Takes the module name of a module that implements the TimeZoneDatabase behaviour.
  """
  def set_database(time_zone_database) do
    :elixir_config.put(:time_zone_database, time_zone_database)
  end

  @doc false
  @spec by_wall(
          Calendar.naive_datetime(),
          Calendar.time_zone(),
          tz_db_or_config
        ) ::
          {:single, TimeZoneDatabase.light_time_zone_period()}
          | {:ambiguous, TimeZoneDatabase.light_time_zone_period(),
             TimeZoneDatabase.light_time_zone_period()}
          | {:gap, TimeZoneDatabase.time_zone_period(), TimeZoneDatabase.time_zone_period()}
          | {:error, :time_zone_not_found}
  def by_wall(%{calendar: Calendar.ISO} = naive_datetime, time_zone, time_zone_database) do
    time_zone_data_module = time_zone_data_module_from_parameter(time_zone_database)

    try do
      time_zone_data_module.by_wall(time_zone, naive_datetime)
    rescue
      UndefinedFunctionError ->
        raise @no_valid_time_zone_database_error
    end
  end

  @doc false
  @spec by_utc(
          Calendar.naive_datetime(),
          Calendar.time_zone(),
          tz_db_or_config
        ) :: {:ok, TimeZoneDatabase.time_zone_period()} | {:error, :time_zone_not_found}
  def by_utc(%{calendar: Calendar.ISO} = naive_datetime, time_zone, time_zone_database) do
    time_zone_data_module = time_zone_data_module_from_parameter(time_zone_database)

    try do
      time_zone_data_module.by_utc(time_zone, naive_datetime)
    rescue
      UndefinedFunctionError ->
        raise @no_valid_time_zone_database_error
    end
  end

  @spec time_zone_data_module_from_parameter(tz_db_or_config) :: TimeZoneDatabase.t()
  defp time_zone_data_module_from_parameter(:from_config) do
    :elixir_config.get(:time_zone_database, :from_config)
  end

  defp time_zone_data_module_from_parameter(time_zone_data_module) do
    time_zone_data_module
  end
end
