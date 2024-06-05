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

  It is important to note that shifting is not an arithmetic operation.
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

    * units are collapsed into months (`:year` and `:month`),
      seconds (`:week`, `:day`, `:hour`, `:minute`, `:second`)
      and microseconds (`:microsecond`) before they are applied

    * 1 year is equivalent to 12 months, 1 week is equivalent to 7 days.
      Therefore, 4 weeks _are not_ equivalent to 1 month

    * in case of non-existing dates, the results are rounded down to the
      nearest valid date

  As the `shift/2` functions are calendar aware, they are guaranteed to return
  valid date/times, considering leap years as well as DST in applicable time zones.

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

  @microseconds_per_second 1_000_000

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

      iex> Duration.add(Duration.new!(week: 2, day: 1), Duration.new!(day: 2))
      %Duration{week: 2, day: 3}
      iex> Duration.add(Duration.new!(microsecond: {400, 3}), Duration.new!(microsecond: {600, 6}))
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

      iex> Duration.subtract(Duration.new!(week: 2, day: 1), Duration.new!(day: 2))
      %Duration{week: 2, day: -1}
      iex> Duration.subtract(Duration.new!(microsecond: {400, 6}), Duration.new!(microsecond: {600, 3}))
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

      iex> Duration.multiply(Duration.new!(day: 1, minute: 15, second: -10), 3)
      %Duration{day: 3, minute: 45, second: -30}
      iex> Duration.multiply(Duration.new!(microsecond: {200, 4}), 3)
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

      iex> Duration.negate(Duration.new!(day: 1, minute: 15, second: -10))
      %Duration{day: -1, minute: -15, second: 10}
      iex> Duration.negate(Duration.new!(microsecond: {500000, 4}))
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
  Parses an [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601#Durations) formatted duration string to a `Duration` struct.

  Duration strings, as well as individual units, may be prefixed with plus/minus signs so that:

  - `-PT6H3M` parses as `%Duration{hour: -6, minute: -3}`
  - `-PT6H-3M` parses as `%Duration{hour: -6, minute: 3}`
  - `+PT6H3M` parses as `%Duration{hour: 6, minute: 3}`
  - `+PT6H-3M` parses as `%Duration{hour: 6, minute: -3}`

  Duration designators must be provided in order of magnitude: `P[n]Y[n]M[n]W[n]DT[n]H[n]M[n]S`.

  Only seconds may be specified with a decimal fraction, using either a comma or a full stop: `P1DT4,5S`.

  ## Examples

      iex> Duration.from_iso8601("P1Y2M3DT4H5M6S")
      {:ok, %Duration{year: 1, month: 2, day: 3, hour: 4, minute: 5, second: 6}}
      iex> Duration.from_iso8601("P3Y-2MT3H")
      {:ok, %Duration{year: 3, month: -2, hour: 3}}
      iex> Duration.from_iso8601("-PT10H-30M")
      {:ok, %Duration{hour: -10, minute: 30}}
      iex> Duration.from_iso8601("PT4.650S")
      {:ok, %Duration{second: 4, microsecond: {650000, 3}}}

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
  Same as `from_iso8601/1` but raises an `ArgumentError`.

  ## Examples

      iex> Duration.from_iso8601!("P1Y2M3DT4H5M6S")
      %Duration{year: 1, month: 2, day: 3, hour: 4, minute: 5, second: 6}
      iex> Duration.from_iso8601!("P10D")
      %Duration{day: 10}

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

  @doc """
  Converts the given `duration` to an [ISO 8601-2:2019](https://en.wikipedia.org/wiki/ISO_8601) formatted string.

  Note this function implements the *extension* of ISO 8601:2019. This extensions allows weeks to
  appear between months and days: `P3M3W3D`, making it fully compatible with any `Duration` struct.

  ## Examples

      iex> Duration.to_iso8601(Duration.new!(year: 3))
      "P3Y"
      iex> Duration.to_iso8601(Duration.new!(day: 40, hour: 12, minute: 42, second: 12))
      "P40DT12H42M12S"
      iex> Duration.to_iso8601(Duration.new!(second: 30))
      "PT30S"

      iex> Duration.to_iso8601(Duration.new!([]))
      "PT0S"

      iex> Duration.to_iso8601(Duration.new!(second: 1, microsecond: {2_200, 3}))
      "PT1.002S"
      iex> Duration.to_iso8601(Duration.new!(second: 1, microsecond: {-1_200_000, 4}))
      "PT-0.2000S"
  """

  @spec to_iso8601(t) :: String.t()
  def to_iso8601(%Duration{} = duration) do
    case {to_iso8601_duration_date(duration), to_iso8601_duration_time(duration)} do
      {[], []} -> "PT0S"
      {date, time} -> IO.iodata_to_binary([?P, date, time])
    end
  end

  defp to_iso8601_duration_date(%{year: 0, month: 0, week: 0, day: 0}) do
    []
  end

  defp to_iso8601_duration_date(%{year: year, month: month, week: week, day: day}) do
    [pair(year, ?Y), pair(month, ?M), pair(week, ?W), pair(day, ?D)]
  end

  defp to_iso8601_duration_time(%{hour: 0, minute: 0, second: 0, microsecond: {0, _}}) do
    []
  end

  defp to_iso8601_duration_time(%{hour: hour, minute: minute} = d) do
    [?T, pair(hour, ?H), pair(minute, ?M), second_component(d)]
  end

  defp second_component(%{second: 0, microsecond: {0, _}}) do
    []
  end

  defp second_component(%{second: 0, microsecond: {_, 0}}) do
    ~c"0S"
  end

  defp second_component(%{second: second, microsecond: {_, 0}}) do
    [Integer.to_string(second), ?S]
  end

  defp second_component(%{second: second, microsecond: {ms, p}}) do
    total_ms = second * @microseconds_per_second + ms
    second = total_ms |> div(@microseconds_per_second) |> abs()
    ms = total_ms |> rem(@microseconds_per_second) |> abs()
    sign = if total_ms < 0, do: ?-, else: []

    [
      sign,
      Integer.to_string(second),
      ?.,
      ms |> Integer.to_string() |> String.pad_leading(6, "0") |> binary_part(0, p),
      ?S
    ]
  end

  @compile {:inline, pair: 2}
  defp pair(0, _key), do: []
  defp pair(num, key), do: [Integer.to_string(num), key]
end
