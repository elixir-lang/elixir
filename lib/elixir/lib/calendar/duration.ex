defmodule Duration do
  @moduledoc """
  Struct and functions for handling durations.

  A `Duration` struct represents a collection of time scale units,
  allowing for manipulation and calculation of durations.

  Date and time scale units are represented as integers, allowing for both positive and negative values.

  Microseconds are represented using a tuple `{microsecond, precision}`.
  This ensures compatibility with other calendar types implementing time, such as `Time`, `DateTime`, and `NaiveDateTime`.
  """

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
          microsecond: {integer, integer}
        }

  @type unit ::
          {:year, integer}
          | {:month, integer}
          | {:week, integer}
          | {:day, integer}
          | {:hour, integer}
          | {:minute, integer}
          | {:second, integer}
          | {:microsecond, {integer, integer}}

  @doc """
  Creates a new `Duration` struct from given `units`.

  Raises a KeyError when called with invalid units.

  ## Examples

      iex> Duration.new(month: 2)
      %Duration{month: 2}

  """
  @spec new([unit]) :: t
  def new(units) do
    case Keyword.get(units, :microsecond) do
      nil ->
        :noop

      ms when is_tuple(ms) ->
        :noop

      _ ->
        raise "microseconds must be a tuple {ms, precision}"
    end

    struct!(Duration, units)
  end

  @doc """
  Adds units of given durations `d1` and `d2`.

  Respects the the highest microsecond precision of the two.

  ## Examples

      iex> Duration.add(%Duration{week: 2, day: 1}, %Duration{day: 2})
      %Duration{week: 2, day: 3}

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
