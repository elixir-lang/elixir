defmodule Time do
  @moduledoc """
  A Time struct and functions.

  The Time struct contains the fields hour, minute, second and microseconds.
  New times can be built with the `new/4` function or using the `~T`
  sigil:

      iex> ~T[23:00:07.001]
      ~T[23:00:07.001]

  Both `new/4` and sigil return a struct where the time fields can
  be accessed directly:

      iex> time = ~T[23:00:07.001]
      iex> time.hour
      23
      iex> time.microsecond
      {1000, 3}

  The functions on this module work with the `Time` struct as well
  as any struct that contains the same fields as the `Time` struct,
  such as `NaiveDateTime` and `DateTime`. Such functions expect
  `t:Calendar.time/0` in their typespecs (instead of `t:t/0`).

  Remember, comparisons in Elixir using `==`, `>`, `<` and friends
  are structural and based on the Time struct fields. For proper
  comparison between times, use the `compare/2` function.

  Developers should avoid creating the Time struct directly and
  instead rely on the functions provided by this module as well as
  the ones in 3rd party calendar libraries.
  """

  @enforce_keys [:hour, :minute, :second]
  defstruct [:hour, :minute, :second, microsecond: {0, 0}, calendar: Calendar.ISO]

  @type t :: %Time{hour: Calendar.hour, minute: Calendar.minute,
                   second: Calendar.second, microsecond: Calendar.microsecond, calendar: Calendar.calendar}

  @doc """
  Returns the current time in UTC.

  ## Examples

      iex> time = Time.utc_now()
      iex> time.hour >= 0
      true

  """
  @spec utc_now(Calendar.calendar) :: t
  def utc_now(calendar \\ Calendar.ISO) do
    {:ok, _, {hour, minute, second}, microsecond} = Calendar.ISO.from_unix(:os.system_time, :native)
    iso_time = %Time{hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: Calendar.ISO}
    convert!(iso_time, calendar)
  end

  @doc """
  Builds a new time.

  Expects all values to be integers. Returns `{:ok, time}` if each
  entry fits its appropriate range, returns `{:error, reason}` otherwise.

  Note a time may have 60 seconds in case of leap seconds.

  ## Examples

      iex> Time.new(0, 0, 0, 0)
      {:ok, ~T[00:00:00.000000]}
      iex> Time.new(23, 59, 59, 999_999)
      {:ok, ~T[23:59:59.999999]}
      iex> Time.new(23, 59, 60, 999_999)
      {:ok, ~T[23:59:60.999999]}

      # Time with microseconds and their precision
      iex> Time.new(23, 59, 60, {10_000, 2})
      {:ok, ~T[23:59:60.01]}

      iex> Time.new(24, 59, 59, 999_999)
      {:error, :invalid_time}
      iex> Time.new(23, 60, 59, 999_999)
      {:error, :invalid_time}
      iex> Time.new(23, 59, 61, 999_999)
      {:error, :invalid_time}
      iex> Time.new(23, 59, 59, 1_000_000)
      {:error, :invalid_time}

  """
  @spec new(Calendar.hour, Calendar.minute, Calendar.second, Calendar.microsecond, Calendar.calendar) ::
        {:ok, Time.t} | {:error, atom}
  def new(hour, minute, second, microsecond \\ {0, 0}, calendar \\ Calendar.ISO)

  def new(hour, minute, second, microsecond, calendar) when is_integer(microsecond) do
    new(hour, minute, second, {microsecond, 6}, calendar)
  end

  def new(hour, minute, second, {microsecond, precision}, calendar)
      when is_integer(hour) and is_integer(minute) and is_integer(second) and
           is_integer(microsecond) and is_integer(precision) do
    case calendar.valid_time?(hour, minute, second, {microsecond, precision}) do
      true ->
        {:ok, %Time{hour: hour, minute: minute, second: second, microsecond: {microsecond, precision}, calendar: calendar}}
      false ->
        {:error, :invalid_time}
    end
  end

  @doc """
  Converts the given time to a string.

  ### Examples

      iex> Time.to_string(~T[23:00:00])
      "23:00:00"
      iex> Time.to_string(~T[23:00:00.001])
      "23:00:00.001"
      iex> Time.to_string(~T[23:00:00.123456])
      "23:00:00.123456"

      iex> Time.to_string(~N[2015-01-01 23:00:00.001])
      "23:00:00.001"
      iex> Time.to_string(~N[2015-01-01 23:00:00.123456])
      "23:00:00.123456"

  """
  @spec to_string(Calendar.time) :: String.t
  def to_string(time)

  def to_string(%{hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: calendar}) do
    calendar.time_to_string(hour, minute, second, microsecond)
  end

  @doc """
  Parses the extended "Local time" format described by
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601).

  Timezone offset may be included in the string but they will be
  simply discarded as such information is not included in times.

  As specified in the standard, the separator "T" may be omitted if
  desired as there is no ambiguity within this function.

  Time representations with reduced accuracy are not supported.

  ## Examples

      iex> Time.from_iso8601("23:50:07")
      {:ok, ~T[23:50:07]}
      iex> Time.from_iso8601("23:50:07Z")
      {:ok, ~T[23:50:07]}
      iex> Time.from_iso8601("T23:50:07Z")
      {:ok, ~T[23:50:07]}

      iex> Time.from_iso8601("23:50:07,0123456")
      {:ok, ~T[23:50:07.012345]}
      iex> Time.from_iso8601("23:50:07.0123456")
      {:ok, ~T[23:50:07.012345]}
      iex> Time.from_iso8601("23:50:07.123Z")
      {:ok, ~T[23:50:07.123]}

      iex> Time.from_iso8601("2015:01:23 23-50-07")
      {:error, :invalid_format}
      iex> Time.from_iso8601("23:50:07A")
      {:error, :invalid_format}
      iex> Time.from_iso8601("23:50:07.")
      {:error, :invalid_format}
      iex> Time.from_iso8601("23:50:61")
      {:error, :invalid_time}

  """
  @spec from_iso8601(String.t) :: {:ok, t} | {:error, atom}
  def from_iso8601(string, calendar \\ Calendar.ISO)

  def from_iso8601(<<?T, h, rest::binary>>, calendar) when h in ?0..?9 do
    from_iso8601(<<h, rest::binary>>, calendar)
  end

  def from_iso8601(<<hour::2-bytes, ?:, min::2-bytes, ?:, sec::2-bytes, rest::binary>>, calendar) do
    with {hour, ""} <- Integer.parse(hour),
         {min, ""} <- Integer.parse(min),
         {sec, ""} <- Integer.parse(sec),
         {microsec, rest} <- Calendar.ISO.parse_microsecond(rest),
         {_offset, ""} <- Calendar.ISO.parse_offset(rest) do
      with {:ok, utc_time} <- new(hour, min, sec, microsec, Calendar.ISO),
           do: convert(utc_time, calendar)
    else
      _ -> {:error, :invalid_format}
    end
  end

  def from_iso8601(<<_::binary>>, _calendar) do
    {:error, :invalid_format}
  end

  @doc """
  Parses the extended "Local time" format described by
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601).

  Raises if the format is invalid.

  ## Examples

      iex> Time.from_iso8601!("23:50:07,123Z")
      ~T[23:50:07.123]
      iex> Time.from_iso8601!("23:50:07.123Z")
      ~T[23:50:07.123]
      iex> Time.from_iso8601!("2015:01:23 23-50-07")
      ** (ArgumentError) cannot parse "2015:01:23 23-50-07" as time, reason: :invalid_format
  """
  @spec from_iso8601!(String.t) :: t | no_return
  def from_iso8601!(string) do
    case from_iso8601(string) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot parse #{inspect string} as time, reason: #{inspect reason}"
    end
  end

  @doc """
  Converts the given time to
  [ISO 8601:2004](https://en.wikipedia.org/wiki/ISO_8601).

  By default, `Time.to_iso8601/2` returns times formatted in the "extended"
  format, for human readability. It also supports the "basic" format through passing the `:basic` option.

  ### Examples

      iex> Time.to_iso8601(~T[23:00:13])
      "23:00:13"

      iex> Time.to_iso8601(~T[23:00:13.001])
      "23:00:13.001"

      iex> Time.to_iso8601(~T[23:00:13.001], :basic)
      "230013.001"

  """
  @spec to_iso8601(Time.t, :extended | :basic) :: String.t
  def to_iso8601(time, format \\ :extended)

  def to_iso8601(%Time{} = time, format) when format in [:extended, :basic] do
    %{hour: hour, minute: minute, second: second, microsecond: microsecond} = convert!(time, Calendar.ISO)
    Calendar.ISO.time_to_iso8601(hour, minute, second, microsecond, format)
  end

  def to_iso8601(%{hour: hour, minute: minute, second: second, microsecond: microsecond, calendar:
    Calendar.ISO}, format) when format in [:extended, :basic] do
    IO.warn "calling Time.to_erl/1 with a DateTime or NaiveDateTime structs is deprecated, explicitly convert them into a Time first by using DateTime.to_time/1 or NaiveDateTime.to_time/1 respectively"
    Calendar.ISO.time_to_iso8601(hour, minute, second, microsecond, format)
  end

  def to_iso8601(_date, format) do
    raise ArgumentError, "Time.to_iso8601/2 expects format to be :extended or :basic, got: #{inspect format}"
  end

  @doc """
  Converts a `Time` struct to an Erlang time tuple.

  WARNING: Loss of precision may occur, as Erlang time tuples
  only contain hours/minutes/seconds.

  ## Examples

      iex> Time.to_erl(~T[23:30:15.999])
      {23, 30, 15}

  """
  @spec to_erl(Time.t) :: :calendar.time
  def to_erl(%Time{} = time) do
    %{hour: hour, minute: minute, second: second} = convert!(time, Calendar.ISO)
    {hour, minute, second}
  end

  def to_erl(%{calendar: Calendar.ISO, hour: hour, minute: minute, second: second}) do
    IO.warn "calling Time.to_erl/1 with a DateTime or NaiveDateTime structs is deprecated, explicitly convert them into a Time first by using DateTime.to_time/1 or NaiveDateTime.to_time/1 respectively"
    {hour, minute, second}
  end

  @doc """
  Converts an Erlang time tuple to a `Time` struct.

  ## Examples

      iex> Time.from_erl({23, 30, 15}, {5000, 3})
      {:ok, ~T[23:30:15.005]}
      iex> Time.from_erl({24, 30, 15})
      {:error, :invalid_time}

  """
  @spec from_erl(:calendar.time, Calendar.microsecond, Calendar.calendar) :: {:ok, t} | {:error, atom}
  def from_erl(tuple, microsecond \\ {0, 0}, calendar \\ Calendar.ISO)

  def from_erl({hour, minute, second}, microsecond, calendar) do
    with {:ok, time} <- new(hour, minute, second, microsecond, Calendar.ISO),
         do: convert(time, calendar)
  end

  @doc """
  Converts an Erlang time tuple to a `Time` struct.

  ## Examples

      iex> Time.from_erl!({23, 30, 15})
      ~T[23:30:15]
      iex> Time.from_erl!({23, 30, 15}, {5000, 3})
      ~T[23:30:15.005]
      iex> Time.from_erl!({24, 30, 15})
      ** (ArgumentError) cannot convert {24, 30, 15} to time, reason: :invalid_time

  """
  @spec from_erl!(:calendar.time, Calendar.microsecond, Calendar.calendar) :: t | no_return
  def from_erl!(tuple, microsecond \\ {0, 0}, calendar \\ Calendar.ISO) do
    case from_erl(tuple, microsecond, calendar) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot convert #{inspect tuple} to time, reason: #{inspect reason}"
    end
  end

  @doc """
  Compares two `Time` structs.

  Returns `:gt` if first time is later than the second
  and `:lt` for vice versa. If the two times are equal
  `:eq` is returned.

  ## Examples

      iex> Time.compare(~T[16:04:16], ~T[16:04:28])
      :lt
      iex> Time.compare(~T[16:04:16.01], ~T[16:04:16.001])
      :gt

  This function can also be used to compare across more
  complex calendar types by considering only the time fields:

      iex> Time.compare(~N[2015-01-01 16:04:16], ~N[2015-01-01 16:04:28])
      :lt
      iex> Time.compare(~N[2015-01-01 16:04:16.01], ~N[2000-01-01 16:04:16.001])
      :gt

  """
  @spec compare(Calendar.time, Calendar.time) :: :lt | :eq | :gt
  def compare(time1, time2) do
    {parts1, ppd1} = to_day_fraction(time1)
    {parts2, ppd2} = to_day_fraction(time2)

    case {parts1 * ppd2, parts2 * ppd1} do
      {first, second} when first > second -> :gt
      {first, second} when first < second -> :lt
      _ -> :eq
    end
  end

  @doc """
  Converts the `Time` struct to a different calendar.

  Returns `{:ok, time}` if the conversion was successful,
  or `{:error, reason}` if it was not, for some reason.
  """
  @spec convert(Time.t, Calendar.calendar) :: {:ok, Time.t} | {:error, atom}
  def convert(%Time{calendar: calendar} = time, calendar) do
    {:ok, time}
  end

  def convert(%Time{} = time, calendar) do
    result_time =
      time
      |> to_day_fraction()
      |> calendar.time_from_day_fraction
    {:ok, result_time}
  end

  @doc """
  Similar to `Time.convert/2`, but raises an `ArgumentError`
  if the conversion between the two calendars is not possible.
  """
  @spec convert!(Time.t, Calendar.calendar) :: Time.t
  def convert!(time, calendar) do
    case convert(time, calendar) do
      {:ok, value} ->
        value
      {:error, reason} ->
        raise ArgumentError, "cannot convert #{inspect time} to target calendar #{inspect calendar}, reason: #{inspect reason}"
    end
  end

  @doc """
  Returns the difference between two `Time` structs.

  The answer can be returned in any `unit` available from `t:System.time_unit/0`.

  This function returns the difference in seconds where seconds are measured
  according to `Calendar.ISO`.
  """
  @spec diff(Time.t, Time.t, System.time_unit) :: integer
  def diff(%Time{} = time1, %Time{} = time2, unit \\ :second) do
    fraction1 = to_day_fraction(time1)
    fraction2 = to_day_fraction(time2)
    Calendar.ISO.rata_die_to_unit({0, fraction1}, unit) - Calendar.ISO.rata_die_to_unit({0, fraction2}, unit)
  end

  ## Helpers

  defp to_day_fraction(%{hour: hour, minute: minute, second: second, microsecond: {_, _} = microsecond, calendar: calendar}) do
    calendar.time_to_day_fraction(hour, minute, second, microsecond)
  end

  defp to_day_fraction(%{hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: calendar}) do
    calendar.time_to_day_fraction(hour, minute, second, {microsecond, 0})
  end

  defimpl String.Chars do
    def to_string(%{hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: calendar}) do
      calendar.time_to_string(hour, minute, second, microsecond)
    end
  end

  defimpl Inspect do
    def inspect(%{hour: hour, minute: minute, second: second, microsecond: microsecond, calendar: Calendar.ISO}, _) do
      "~T[" <> Calendar.ISO.time_to_string(hour, minute, second, microsecond) <> "]"
    end

    def inspect(time, opts) do
      Inspect.Any.inspect(time, opts)
    end
  end
end
