defmodule Duration do
  @moduledoc """
  Struct and functions for handling durations.

  A `Duration` struct represents a collection of time scale units,
  allowing for manipulation and calculation of durations.

  Date and time scale units are represented as integers, allowing for both positive and negative values.

  Microseconds are represented using a tuple `{microsecond, precision}`. This ensures compatibility with
  other calendar types implementing time, such as `Time`, `DateTime`, and `NaiveDateTime`.
  """

  @moduledoc since: "1.17.0"

  @derive {Inspect, optional: [:year, :month, :week, :day, :hour, :minute, :second, :microsecond]}
  defstruct year: 0,
            month: 0,
            week: 0,
            day: 0,
            hour: 0,
            minute: 0,
            second: 0,
            microsecond: {0, 0}

  @typedoc """
  The duration struct type.
  """
  @type t :: %Duration{
          year: integer,
          month: integer,
          week: integer,
          day: integer,
          hour: integer,
          minute: integer,
          second: integer,
          microsecond: {integer, 0..6}
        }

  @typedoc """
  The time unit pair type specifies a pair of a valid time duration unit key and value.
  """
  @type time_unit_pair ::
          {:hour, integer}
          | {:minute, integer}
          | {:second, integer}
          | {:microsecond, {integer, 0..6}}

  @typedoc """
  The date unit pair type specifies a pair of a valid date duration unit key and value.
  """
  @type date_unit_pair ::
          {:year, integer}
          | {:month, integer}
          | {:week, integer}
          | {:day, integer}

  @typedoc """
  The unit pair type specifies a pair of a valid duration unit key and value.
  """
  @type unit_pair :: date_unit_pair | time_unit_pair

  @typedoc """
  The duration type specifies a `%Duration{}` struct or a keyword list of valid duration unit pairs.
  """
  @type duration :: t | [unit_pair]

  @doc """
  Creates a new `Duration` struct from given `unit_pairs`.

  Raises an `ArgumentError` when called with invalid unit pairs.

  ## Examples

      iex> Duration.new!(year: 1, week: 3, hour: 4, second: 1)
      %Duration{year: 1, week: 3, hour: 4, second: 1}
      iex> Duration.new!(second: 1, microsecond: {1000, 6})
      %Duration{second: 1, microsecond: {1000, 6}}
      iex> Duration.new!(month: 2)
      %Duration{month: 2}

  """
  @spec new!(duration()) :: t
  def new!(%Duration{} = duration) do
    duration
  end

  def new!(unit_pairs) do
    Enum.each(unit_pairs, &validate_unit!/1)
    struct!(Duration, unit_pairs)
  end

  @doc false
  @spec new_date_units!(t | [date_unit_pair]) :: t
  def new_date_units!(%Duration{hour: 0, minute: 0, second: 0, microsecond: {0, 0}} = duration) do
    duration
  end

  def new_date_units!(%Duration{}) do
    raise ArgumentError, "duration may not contain time scale units in `new_date_units!/1`"
  end

  def new_date_units!(unit_pairs) do
    Enum.each(unit_pairs, &validate_date_unit!/1)
    struct!(Duration, unit_pairs)
  end

  @doc false
  @spec new_time_units!(t | [time_unit_pair]) :: t
  def new_time_units!(%Duration{year: 0, month: 0, week: 0, day: 0} = duration) do
    duration
  end

  def new_time_units!(%Duration{}) do
    raise ArgumentError, "duration may not contain date scale units in `new_time_units!/1`"
  end

  def new_time_units!(unit_pairs) do
    Enum.each(unit_pairs, &validate_time_unit!/1)
    struct!(Duration, unit_pairs)
  end

  defp validate_unit!({unit, value}) when unit in [:year, :month, :week, :day] do
    validate_date_unit!({unit, value})
  end

  defp validate_unit!({unit, value}) when unit in [:hour, :minute, :second, :microsecond] do
    validate_time_unit!({unit, value})
  end

  defp validate_unit!({unit, _value}) do
    raise ArgumentError,
          "unsupported unit #{inspect(unit)}. Expected :year, :month, :week, :day, :hour, :minute, :second, :microsecond"
  end

  defp validate_date_unit!({unit, _value}) when unit not in [:year, :month, :week, :day] do
    raise ArgumentError, "unsupported unit #{inspect(unit)}. Expected :year, :month, :week, :day"
  end

  defp validate_date_unit!({_unit, value}) when is_integer(value) do
    :ok
  end

  defp validate_date_unit!({unit, value}) do
    raise ArgumentError,
          "unsupported value #{inspect(value)} for #{inspect(unit)}. Expected an integer"
  end

  defp validate_time_unit!({:microsecond, {ms, precision}})
       when is_integer(ms) and precision in 0..6 do
    :ok
  end

  defp validate_time_unit!({:microsecond, microsecond}) do
    raise ArgumentError,
          "unsupported value #{inspect(microsecond)} for :microsecond. Expected a tuple {ms, precision} where precision is an integer from 0 to 6"
  end

  defp validate_time_unit!({unit, _value}) when unit not in [:hour, :minute, :second] do
    raise ArgumentError,
          "unsupported unit #{inspect(unit)}. Expected :hour, :minute, :second, :microsecond"
  end

  defp validate_time_unit!({_unit, value}) when is_integer(value) do
    :ok
  end

  defp validate_time_unit!({unit, value}) do
    raise ArgumentError,
          "unsupported value #{inspect(value)} for #{inspect(unit)}. Expected an integer"
  end

  @doc """
  Adds units of given durations `d1` and `d2`.

  Respects the the highest microsecond precision of the two.

  ## Examples

      iex> Duration.add(%Duration{week: 2, day: 1}, %Duration{day: 2})
      %Duration{week: 2, day: 3}
      iex> Duration.add(%Duration{microsecond: {400, 3}}, %Duration{microsecond: {600, 6}})
      %Duration{microsecond: {1000, 6}}

  """
  @spec add(t, t) :: t
  def add(%Duration{} = d1, %Duration{} = d2) do
    {m1, p1} = d1.microsecond
    {m2, p2} = d2.microsecond

    %Duration{
      year: d1.year + d2.year,
      month: d1.month + d2.month,
      week: d1.week + d2.week,
      day: d1.day + d2.day,
      hour: d1.hour + d2.hour,
      minute: d1.minute + d2.minute,
      second: d1.second + d2.second,
      microsecond: {m1 + m2, max(p1, p2)}
    }
  end

  @doc """
  Subtracts units of given durations `d1` and `d2`.

  Respects the the highest microsecond precision of the two.

  ## Examples

      iex> Duration.subtract(%Duration{week: 2, day: 1}, %Duration{day: 2})
      %Duration{week: 2, day: -1}
      iex> Duration.subtract(%Duration{microsecond: {400, 6}}, %Duration{microsecond: {600, 3}})
      %Duration{microsecond: {-200, 6}}

  """
  @spec subtract(t, t) :: t
  def subtract(%Duration{} = d1, %Duration{} = d2) do
    {m1, p1} = d1.microsecond
    {m2, p2} = d2.microsecond

    %Duration{
      year: d1.year - d2.year,
      month: d1.month - d2.month,
      week: d1.week - d2.week,
      day: d1.day - d2.day,
      hour: d1.hour - d2.hour,
      minute: d1.minute - d2.minute,
      second: d1.second - d2.second,
      microsecond: {m1 - m2, max(p1, p2)}
    }
  end

  @doc """
  Multiplies `duration` units by given `integer`.

  ## Examples

      iex> Duration.multiply(%Duration{day: 1, minute: 15, second: -10}, 3)
      %Duration{day: 3, minute: 45, second: -30}
      iex> Duration.multiply(%Duration{microsecond: {200, 4}}, 3)
      %Duration{microsecond: {600, 4}}

  """
  @spec multiply(t, integer) :: t
  def multiply(%Duration{microsecond: {ms, p}} = duration, integer) when is_integer(integer) do
    %Duration{
      year: duration.year * integer,
      month: duration.month * integer,
      week: duration.week * integer,
      day: duration.day * integer,
      hour: duration.hour * integer,
      minute: duration.minute * integer,
      second: duration.second * integer,
      microsecond: {ms * integer, p}
    }
  end

  @doc """
  Negates `duration` units.

  ## Examples

      iex> Duration.negate(%Duration{day: 1, minute: 15, second: -10})
      %Duration{day: -1, minute: -15, second: 10}
      iex> Duration.negate(%Duration{microsecond: {500000, 4}})
      %Duration{microsecond: {-500000, 4}}

  """
  @spec negate(t) :: t
  def negate(%Duration{microsecond: {ms, p}} = duration) do
    %Duration{
      year: -duration.year,
      month: -duration.month,
      week: -duration.week,
      day: -duration.day,
      hour: -duration.hour,
      minute: -duration.minute,
      second: -duration.second,
      microsecond: {-ms, p}
    }
  end
end
