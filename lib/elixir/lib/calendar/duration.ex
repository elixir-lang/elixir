defmodule Calendar.Duration do
  @moduledoc """
  The Duration type following ISO 8601.

  https://en.wikipedia.org/wiki/ISO_8601#Durations

  TODO:
  - Implement utility functions
  - Implement arithmetic functions
  """

  defstruct year: 0, month: 0, week: 0, day: 0, hour: 0, minute: 0, second: 0, microsecond: 0

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

  @spec new!([unit]) :: t
  def new!(units) do
    struct!(__MODULE__, units)
  end

  @spec new([unit]) :: {:ok, t} | {:error, :invalid_duration}
  def new(units) do
    case Keyword.validate(units, Map.keys(%__MODULE__{}) -- [:__struct__]) do
      {:ok, units} ->
        {:ok, struct(__MODULE__, units)}

      {:error, _invalid_keys} ->
        {:error, :invalid_duration}
    end
  end
end
