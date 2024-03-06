defmodule Duration do
  @moduledoc """
  The Duration type.
  """

  @seconds_per_day 86400

  @default [year: 0, month: 0, week: 0, day: 0, hour: 0, minute: 0, second: 0, microsecond: 0]
  @fields Keyword.keys(@default)

  defstruct @default

  @typedoc "Duration in calendar units"
  @type t :: %Duration{
          year: integer,
          month: integer,
          week: integer,
          day: integer,
          hour: integer,
          minute: integer,
          second: integer,
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
          | {:microsecond, integer}

  @doc """
  Create `Duration` struct from valid duration units.

  Returns `{:error, :invalid_duration}` when called with invalid units.

  ## Examples

      iex> Duration.new(month: 2)
      {:ok, %Duration{month: 2}}
      iex> Duration.new(months: 2)
      {:error, :invalid_duration}

  """
  @spec new([unit]) :: {:ok, t} | {:error, :invalid_duration}
  def new(units) do
    case Keyword.validate(units, @fields) do
      {:ok, units} ->
        {:ok, struct(Duration, units)}

      {:error, _invalid_keys} ->
        {:error, :invalid_duration}
    end
  end

  @doc """
  Same as `new/1` but raises a KeyError when called with invalid units.

  ## Examples

      iex> Duration.new!(month: 2)
      %Duration{month: 2}

  """
  @spec new!([unit]) :: t
  def new!(units) do
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
      microsecond: -duration.microsecond
    }
  end

  @doc """
  Compares two durations.

  Returns `:gt` if the first duration is longer than the second and `:lt` for vice versa.
  If the two durations are equal in length in seconds `:eq` is returned.

  ## Examples

      iex> Duration.compare(%Duration{hour: 1, minute: 15}, %Duration{hour: 2, minute: -45})
      :eq
      iex> Duration.compare(%Duration{year: 1, minute: 15}, %Duration{minute: 15})
      :gt
      iex> Duration.compare(%Duration{day: 1, minute: 15}, %Duration{day: 2})
      :lt

  """
  @spec compare(t, t) :: :lt | :eq | :gt
  def compare(%Duration{} = d1, %Duration{} = d2) do
    case {to_seconds(d1), to_seconds(d2)} do
      {first, second} when first > second -> :gt
      {first, second} when first < second -> :lt
      _ -> :eq
    end
  end

  @doc """
  Converts duration to seconds.

  ## Examples

      iex> Duration.to_seconds(%Duration{day: 1, minute: 15, second: -10})
      87290

  """
  @spec to_seconds(t) :: integer
  def to_seconds(%Duration{
        year: year,
        month: month,
        week: week,
        day: day,
        hour: hour,
        minute: minute,
        second: second,
        microsecond: microsecond
      }) do
    Enum.sum([
      year * 365 * @seconds_per_day,
      month * 30 * @seconds_per_day,
      week * 7 * @seconds_per_day,
      day * @seconds_per_day,
      hour * 60 * 60,
      minute * 60,
      second,
      div(microsecond, 1_000_000)
    ])
  end

  @doc """
  Converts seconds to duration.

  ## Examples

      iex> Duration.from_seconds(87290)
      %Duration{day: 1, minute: 14, second: 50}

  """
  @spec from_seconds(integer) :: t
  def from_seconds(seconds) do
    {years, seconds} = div_rem(seconds, 365 * @seconds_per_day)
    {months, seconds} = div_rem(seconds, 30 * @seconds_per_day)
    {weeks, seconds} = div_rem(seconds, 7 * @seconds_per_day)
    {days, seconds} = div_rem(seconds, @seconds_per_day)
    {hours, seconds} = div_rem(seconds, 60 * 60)
    {minutes, seconds} = div_rem(seconds, 60)
    {seconds, microseconds} = div_rem(seconds, 1)

    %Duration{
      year: years,
      month: months,
      week: weeks,
      day: days,
      hour: hours,
      minute: minutes,
      second: seconds,
      microsecond: microseconds * 1_000_000
    }
  end

  defp div_rem(dividend, divisor) do
    {div(dividend, divisor), rem(dividend, divisor)}
  end
end
