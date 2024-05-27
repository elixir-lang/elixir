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

  @type unit_pair ::
          {:year, integer}
          | {:month, integer}
          | {:week, integer}
          | {:day, integer}
          | {:hour, integer}
          | {:minute, integer}
          | {:second, integer}
          | {:microsecond, {integer, 0..6}}

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
  @spec new!([unit_pair]) :: t
  def new!(unit_pairs) do
    Enum.each(unit_pairs, &validate_duration_unit!/1)
    struct!(Duration, unit_pairs)
  end

  defp validate_duration_unit!({:microsecond, {ms, precision}})
       when is_integer(ms) and precision in 0..6 do
    :ok
  end

  defp validate_duration_unit!({:microsecond, microsecond}) do
    raise ArgumentError,
          "expected a tuple {ms, precision} for microsecond where precision is an integer from 0 to 6, got #{inspect(microsecond)}"
  end

  defp validate_duration_unit!({unit, _value})
       when unit not in [:year, :month, :week, :day, :hour, :minute, :second] do
    raise ArgumentError, "unexpected unit #{inspect(unit)}"
  end

  defp validate_duration_unit!({_unit, value}) when is_integer(value) do
    :ok
  end

  defp validate_duration_unit!({unit, value}) do
    raise ArgumentError, "expected an integer for #{inspect(unit)}, got #{inspect(value)}"
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

  @doc """
  Parses an ISO 8601 formatted duration string to a `Duration` struct.

  A decimal fraction may be specified for seconds only, using either a comma or a full stop.

  ## Examples

      iex> Duration.from_iso8601("P1Y2M3DT4H5M6S")
      {:ok, %Duration{year: 1, month: 2, day: 3, hour: 4, minute: 5, second: 6}}
      iex> Duration.from_iso8601("PT10H30M")
      {:ok, %Duration{hour: 10, minute: 30, second: 0}}
      iex> Duration.from_iso8601("P3Y-2MT3H")
      {:ok, %Duration{year: 3, month: -2, hour: 3}}
      iex> Duration.from_iso8601("P1YT4.650S")
      {:ok, %Duration{year: 1, second: 4, microsecond: {650000, 3}}}

  """
  @spec from_iso8601(String.t()) :: {:ok, t} | {:error, atom}
  def from_iso8601(string) when is_binary(string) do
    case Calendar.ISO.parse_duration(string) do
      {:ok, duration} ->
        {:ok, new!(duration)}

      error ->
        error
    end
  end

  @doc """
  Same as `from_iso8601/1` but raises an ArgumentError.

  ## Examples

      iex> Duration.from_iso8601!("P1Y2M3DT4H5M6S")
      %Duration{year: 1, month: 2, day: 3, hour: 4, minute: 5, second: 6}

  """
  @spec from_iso8601!(String.t()) :: t
  def from_iso8601!(string) when is_binary(string) do
    case from_iso8601(string) do
      {:ok, duration} ->
        duration

      {:error, reason} ->
        raise ArgumentError, ~s/failed to parse duration "#{string}". reason: #{inspect(reason)}/
    end
  end
end
