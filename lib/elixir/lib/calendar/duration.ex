defmodule Duration do
  @moduledoc """
  The Duration type.
  """

  @default [year: 0, month: 0, week: 0, day: 0, hour: 0, minute: 0, second: 0, microsecond: 0]
  @fields Keyword.keys(@default)

  defstruct @default

  @typedoc "Duration in calendar units"
  @type t :: %__MODULE__{
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
        {:ok, struct(__MODULE__, units)}

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
    struct!(__MODULE__, units)
  end

  @doc """
  Adds two durations to one new duration.

  ## Examples

      iex> Duration.add(%Duration{week: 2, day: 1}, %Duration{day: 2})
      %Duration{week: 2, day: 3}

  """
  @spec add(t, t) :: t
  def add(%__MODULE__{} = d1, %__MODULE__{} = d2) do
    %__MODULE__{
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
  Subtracts two durations to one new duration.

  ## Examples

      iex> Duration.subtract(%Duration{week: 2, day: 1}, %Duration{day: 2})
      %Duration{week: 2, day: -1}

  """
  @spec subtract(t, t) :: t
  def subtract(%__MODULE__{} = d1, %__MODULE__{} = d2) do
    %__MODULE__{
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
  Multiplies all Duration units by given integer.

  ## Examples

      iex> Duration.multiply(%Duration{day: 1, minute: 15, second: -10}, 3)
      %Duration{day: 3, minute: 45, second: -30}

  """
  @spec multiply(t, integer) :: t
  def multiply(%__MODULE__{} = duration, integer) when is_integer(integer) do
    %__MODULE__{
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
  Negates all units of a Duration.

  ## Examples

      iex> Duration.negate(%Duration{day: 1, minute: 15, second: -10})
      %Duration{day: -1, minute: -15, second: 10}

  """
  @spec negate(t) :: t
  def negate(%__MODULE__{} = duration) do
    %__MODULE__{
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
end
