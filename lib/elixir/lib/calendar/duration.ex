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
  @type unit ::
          {:year, integer()}
          | {:month, integer()}
          | {:week, integer()}
          | {:day, integer()}
          | {:hour, integer()}
          | {:minute, integer()}
          | {:second, integer()}
          | {:microsecond, integer()}

  @spec new!([unit]) :: t()
  def new!(units) do
    struct!(__MODULE__, units)
  end

  @spec new([unit]) :: {:ok, t()} | {:error, :invalid_duration}
  def new(units) do
    case Keyword.validate(units, Map.keys(%__MODULE__{}) -- [:__struct__]) do
      {:ok, units} ->
        {:ok, struct(__MODULE__, units)}

      {:error, _invalid_keys} ->
        {:error, :invalid_duration}
    end
  end

  @duration_regex ~r/P(?:(?<year>-?\d+)Y)?(?:(?<month>-?\d+)M)?(?:(?<week>-?\d+)W)?(?:(?<day>-?\d+)D)?(?:T(?:(?<hour>-?\d+)H)?(?:(?<minute>-?\d+)M)?(?:(?<second>-?\d+)S)?)?/
  @spec parse!(String.t()) :: t()
  def parse!(duration_string) when is_binary(duration_string) do
    case Regex.named_captures(@duration_regex, duration_string) do
      %{
        "year" => year,
        "month" => month,
        "week" => week,
        "day" => day,
        "hour" => hour,
        "minute" => minute,
        "second" => second
      } ->
        new!(
          year: parse_unit(year),
          month: parse_unit(month),
          week: parse_unit(week),
          day: parse_unit(day),
          hour: parse_unit(hour),
          minute: parse_unit(minute),
          second: parse_unit(second)
        )

      _ ->
        raise ArgumentError, "invalid duration string"
    end
  end

  defp parse_unit(""), do: 0
  defp parse_unit(part), do: String.to_integer(part)
end
