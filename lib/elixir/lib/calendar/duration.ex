defmodule Calendar.Duration do
  @moduledoc """
  The Duration type following ISO 8601.

  https://en.wikipedia.org/wiki/ISO_8601#Durations

  TODO:
  - Implement parser
  - Implement ~P Sigil
  - Implement utility functions
  - Implement arithmetic functions
  """

  defstruct year: 0, month: 0, week: 0, day: 0, hour: 0, minute: 0, second: 0, microsecond: 0

  @typedoc "Duration in calendar units"
  @type t :: %__MODULE__{
          year: integer(),
          month: integer(),
          week: integer(),
          day: integer(),
          hour: integer(),
          minute: integer(),
          second: integer(),
          microsecond: integer()
        }

  @typedoc "Individually valid Duration units"
  @type duration_unit ::
          {:year, integer()}
          | {:month, integer()}
          | {:week, integer()}
          | {:day, integer()}
          | {:hour, integer()}
          | {:minute, integer()}
          | {:second, integer()}
          | {:microsecond, integer()}

  @spec new!([duration_unit]) :: t()
  def new!(duration_units) do
    Keyword.validate!(duration_units, Map.keys(%__MODULE__{}) -- [:__struct__])
    struct!(__MODULE__, duration_units)
  end

  @spec to_shift_options(t()) :: [duration_unit()]
  def to_shift_options(duration) do
    [
      year: duration.year,
      month: duration.month,
      week: duration.week,
      day: duration.day,
      hour: duration.hour,
      minute: duration.minute,
      second: duration.second,
      microsecond: duration.microsecond
    ]
  end
end
