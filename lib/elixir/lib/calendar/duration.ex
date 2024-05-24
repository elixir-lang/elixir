defmodule Duration do
  @moduledoc """
  Struct and functions for handling durations.

  A `Duration` struct represents a collection of time scale units,
  allowing for manipulation and calculation of durations.

  Date and time scale units are represented as integers, allowing for
  both positive and negative values.

  Microseconds are represented using a tuple `{microsecond, precision}`.
  This ensures compatibility with other calendar types implementing time,
  such as `Time`, `DateTime`, and `NaiveDateTime`.

  ## Shifting

  The most common use of durations in Elixir's standard library is to
  "shift" the calendar types.

      iex> Date.shift(~D[2016-01-03], month: 2)
      ~D[2016-03-03]

  In the example above, `Date.shift/2` automatically converts the units
  into a `Duration` struct, although one can also be given directly:

      iex> Date.shift(~D[2016-01-03], Duration.new!(month: 2))
      ~D[2016-03-03]

  It is important to notice that shifting is not an arithmetic operation.
  For example, adding `date + 1 month + 1 month` does not yield the same
  result as `date + 2 months`. Let's see an example:

      iex> ~D[2016-01-31] |> Date.shift(month: 1) |> Date.shift(month: 1)
      ~D[2016-03-29]

      iex> ~D[2016-01-31] |> Date.shift(month: 2)
      ~D[2016-03-31]

  As you can see above, the results differ, which explains why operations
  with durations are called "shift" rather than "add". This happens because,
  once we add one month to `2016-01-31`, we get `2016-02-29`. Then adding
  one extra month gives us `2016-03-29` instead of `2016-03-31`.

  In particular, when applying durations to `Calendar.ISO` types:

    * larger units (such as years and months) are applied before
      smaller ones (such as weeks, hours, days, and so on)

    * in case of non-existing dates, the results are rounded down to the
      nearest valid date

  ## Intervals

  Durations in Elixir can be combined with stream operations to build intervals.
  For example, to retrieve the next three Wednesdays starting from 17th April, 2024:

      iex> ~D[2024-04-17] |> Stream.iterate(&Date.shift(&1, week: 1)) |> Enum.take(3)
      [~D[2024-04-17], ~D[2024-04-24], ~D[2024-05-01]]

  However, once again, it is important to remember that shifting a duration is not
  arithmetic, so you may want to use the functions in this module depending on what
  you to achieve. Compare the results of both examples below:

      # Adding one month after the other
      iex> date = ~D[2016-01-31]
      iex> duration = Duration.new!(month: 1)
      iex> stream = Stream.iterate(date, fn prev_date -> Date.shift(prev_date, duration) end)
      iex> Enum.take(stream, 3)
      [~D[2016-01-31], ~D[2016-02-29], ~D[2016-03-29]]

      # Multiplying durations by an index
      iex> date = ~D[2016-01-31]
      iex> duration = Duration.new!(month: 1)
      iex> stream = Stream.from_index(fn i -> Date.shift(date, Duration.multiply(duration, i)) end)
      iex> Enum.take(stream, 3)
      [~D[2016-01-31], ~D[2016-02-29], ~D[2016-03-31]]

  The second example consistently points to the last day of the month,
  as it performs operations on the duration, rather than shifting date
  after date.
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
  The unit pair type specifies a pair of a valid duration unit key and value.
  """
  @type unit_pair ::
          {:year, integer}
          | {:month, integer}
          | {:week, integer}
          | {:day, integer}
          | {:hour, integer}
          | {:minute, integer}
          | {:second, integer}
          | {:microsecond, {integer, 0..6}}

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

  defp validate_unit!({:microsecond, {ms, precision}})
       when is_integer(ms) and precision in 0..6 do
    :ok
  end

  defp validate_unit!({:microsecond, microsecond}) do
    raise ArgumentError,
          "unsupported value #{inspect(microsecond)} for :microsecond. Expected a tuple {ms, precision} where precision is an integer from 0 to 6"
  end

  defp validate_unit!({unit, _value})
       when unit not in [:year, :month, :week, :day, :hour, :minute, :second] do
    raise ArgumentError,
          "unknown unit #{inspect(unit)}. Expected :year, :month, :week, :day, :hour, :minute, :second, :microsecond"
  end

  defp validate_unit!({_unit, value}) when is_integer(value) do
    :ok
  end

  defp validate_unit!({unit, value}) do
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

  @doc """
  Converts the given duration to ISO 8601:TODO format.

  ## Examples

  iex> Duration.new!([]) |> Duration.to_iso8601()
  "PT0S"
  iex> Duration.new!(year: 3) |> Duration.to_iso8601()
  "P3Y"
  iex> Duration.new!(year: 3, day: 6, minute: 9) |> Duration.to_iso8601()
  "P3Y6DT9M"
  iex> Duration.new!(second: 30) |> Duration.to_iso8601()
  "PT30S"
  iex> Duration.new!(hour: 2, microsecond: {1000, 6}) |> Duration.to_iso8601()
  "PT2H0.001S"
  """

  @spec to_iso8601(t) :: String.t()
  def to_iso8601(%Duration{
        year: 0,
        month: 0,
        week: 0,
        day: 0,
        hour: 0,
        minute: 0,
        second: 0,
        microsecond: {0, 0}
      }) do
    "PT0S"
  end

  def to_iso8601(%Duration{} = d) do
    "P#{to_iso8601_left_part(d)}#{to_iso8601_right_part(d)}"
  end

  defp to_iso8601_left_part(d) do
    year = unless d.year == 0, do: "#{d.year}Y"
    month = unless d.month == 0, do: "#{d.month}M"
    week = unless d.week == 0, do: "#{d.week}W"
    day = unless d.day == 0, do: "#{d.day}D"

    "#{year}#{month}#{week}#{day}"
  end

  defp to_iso8601_right_part(%Duration{hour: 0, minute: 0, second: 0, microsecond: {0, 0}}) do
    ""
  end

  defp to_iso8601_right_part(d) do
    hour = unless d.hour == 0, do: "#{d.hour}H"
    minute = unless d.minute == 0, do: "#{d.minute}M"

    second =
      case d do
        %Duration{second: 0, microsecond: {0, 0}} ->
          ""

        %Duration{microsecond: {0, _}} ->
          "#{d.second}S"

        %Duration{microsecond: {microsecond, _}} ->
          microsecond =
            microsecond
            |> to_string()
            |> String.pad_leading(6, "0")
            |> String.trim_trailing("0")

          "#{d.second}.#{microsecond}S"
      end

    "T#{hour}#{minute}#{second}"
  end
end
