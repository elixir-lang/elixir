defmodule Duration do
  @moduledoc """
  The Duration type.
  """

  defstruct year: 0,
            month: 0,
            week: 0,
            day: 0,
            hour: 0,
            minute: 0,
            second: 0,
            millisecond: 0,
            microsecond: 0

  @typedoc "Duration in calendar units"
  @type t :: %Duration{
          year: integer,
          month: integer,
          week: integer,
          day: integer,
          hour: integer,
          minute: integer,
          second: integer,
          millisecond: integer,
          microsecond: integer
        }

  @typedoc "Individually valid Duration units"
  @type unit ::
          {:year, integer}
          | {:month, integer}
          | {:week, integer}
          | {:day, integer}
          | {:hour, integer}
          | {:minute, integer}
          | {:second, integer}
          | {:millisecond, integer}
          | {:microsecond, integer}

  @doc """
  Create `Duration` struct from valid duration units.

  Raises a KeyError when called with invalid units.

  ## Examples

      iex> Duration.new(month: 2)
      %Duration{month: 2}

  """
  @spec new([unit]) :: t
  def new(units) do
    struct!(Duration, units)
  end

  @doc """
  Adds two durations.

  ## Examples

      iex> Duration.add(%Duration{week: 2, day: 1}, %Duration{day: 2})
      %Duration{week: 2, day: 3}

  """
  @spec add(t, t) :: t
  def add(%Duration{} = d1, %Duration{} = d2) do
    %Duration{
      year: d1.year + d2.year,
      month: d1.month + d2.month,
      week: d1.week + d2.week,
      day: d1.day + d2.day,
      hour: d1.hour + d2.hour,
      minute: d1.minute + d2.minute,
      second: d1.second + d2.second,
      millisecond: d1.millisecond + d2.millisecond,
      microsecond: d1.microsecond + d2.microsecond
    }
  end

  @doc """
  Subtracts two durations.

  ## Examples

      iex> Duration.subtract(%Duration{week: 2, day: 1}, %Duration{day: 2})
      %Duration{week: 2, day: -1}

  """
  @spec subtract(t, t) :: t
  def subtract(%Duration{} = d1, %Duration{} = d2) do
    %Duration{
      year: d1.year - d2.year,
      month: d1.month - d2.month,
      week: d1.week - d2.week,
      day: d1.day - d2.day,
      hour: d1.hour - d2.hour,
      minute: d1.minute - d2.minute,
      second: d1.second - d2.second,
      millisecond: d1.millisecond - d2.millisecond,
      microsecond: d1.microsecond - d2.microsecond
    }
  end

  @doc """
  Multiplies all duration units by given integer.

  ## Examples

      iex> Duration.multiply(%Duration{day: 1, minute: 15, second: -10}, 3)
      %Duration{day: 3, minute: 45, second: -30}

  """
  @spec multiply(t, integer) :: t
  def multiply(%Duration{} = duration, integer) when is_integer(integer) do
    %Duration{
      year: duration.year * integer,
      month: duration.month * integer,
      week: duration.week * integer,
      day: duration.day * integer,
      hour: duration.hour * integer,
      minute: duration.minute * integer,
      second: duration.second * integer,
      millisecond: duration.millisecond * integer,
      microsecond: duration.microsecond * integer
    }
  end

  @doc """
  Negates all duration units.

  ## Examples

      iex> Duration.negate(%Duration{day: 1, minute: 15, second: -10})
      %Duration{day: -1, minute: -15, second: 10}

  """
  @spec negate(t) :: t
  def negate(%Duration{} = duration) do
    %Duration{
      year: -duration.year,
      month: -duration.month,
      week: -duration.week,
      day: -duration.day,
      hour: -duration.hour,
      minute: -duration.minute,
      second: -duration.second,
      millisecond: -duration.millisecond,
      microsecond: -duration.microsecond
    }
  end

  @doc false
  @spec invalid_units([unit], :date | :time) :: keyword()
  def invalid_units(duration_units, calendar_type)

  def invalid_units(duration_units, :date) do
    Enum.filter(
      [:hour, :minute, :second, :millisecond, :microsecond],
      &Keyword.has_key?(duration_units, &1)
    )
  end

  def invalid_units(duration_units, :time) do
    Enum.filter(
      [:year, :month, :week, :day],
      &Keyword.has_key?(duration_units, &1)
    )
  end
end
