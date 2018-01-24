defmodule Date.Range do
  @moduledoc """
  Returns an inclusive range between dates.

  Ranges must be created with the `Date.range/2` function.

  The following fields are public:

    * `:first` - the initial date on the range
    * `:last` - the last date on the range

  The remaining fields are private and should not be accessed.
  """

  @type t :: %__MODULE__{
          first: Date.t(),
          last: Date.t(),
          first_in_iso_days: iso_days(),
          last_in_iso_days: iso_days()
        }

  @typep iso_days() :: Calendar.iso_days()

  defstruct [:first, :last, :first_in_iso_days, :last_in_iso_days]

  defimpl Enumerable do
    def member?(%{first: %{calendar: calendar}} = range, %Date{calendar: calendar} = date) do
      %{
        first: first,
        last: last,
        first_in_iso_days: first_in_iso_days,
        last_in_iso_days: last_in_iso_days
      } = range

      %{year: first_year, month: first_month, day: first_day} = first
      %{year: last_year, month: last_month, day: last_day} = last
      %{year: year, month: month, day: day} = date
      first = {first_year, first_month, first_day}
      last = {last_year, last_month, last_day}
      date = {year, month, day}

      if first_in_iso_days <= last_in_iso_days do
        {:ok, date >= first and date <= last}
      else
        {:ok, date >= last and date <= first}
      end
    end

    def member?(_, _) do
      {:ok, false}
    end

    def count(%{first_in_iso_days: first, last_in_iso_days: last}) do
      {:ok, abs(first - last) + 1}
    end

    def slice(range) do
      %{
        first_in_iso_days: first,
        last_in_iso_days: last,
        first: %{calendar: calendar}
      } = range

      if first <= last do
        {:ok, last - first + 1, &slice_asc(first + &1, &2, calendar)}
      else
        {:ok, first - last + 1, &slice_desc(first - &1, &2, calendar)}
      end
    end

    defp slice_asc(current, 1, calendar), do: [date_from_iso_days(current, calendar)]

    defp slice_asc(current, remaining, calendar) do
      [date_from_iso_days(current, calendar) | slice_asc(current + 1, remaining - 1, calendar)]
    end

    defp slice_desc(current, 1, calendar), do: [date_from_iso_days(current, calendar)]

    defp slice_desc(current, remaining, calendar) do
      [date_from_iso_days(current, calendar) | slice_desc(current - 1, remaining - 1, calendar)]
    end

    def reduce(range, acc, fun) do
      %{
        first_in_iso_days: first_in_iso_days,
        last_in_iso_days: last_in_iso_days,
        first: %{calendar: calendar}
      } = range

      up? = first_in_iso_days <= last_in_iso_days
      reduce(first_in_iso_days, last_in_iso_days, acc, fun, calendar, up?)
    end

    defp reduce(_x, _y, {:halt, acc}, _fun, _calendar, _up?) do
      {:halted, acc}
    end

    defp reduce(x, y, {:suspend, acc}, fun, calendar, up?) do
      {:suspended, acc, &reduce(x, y, &1, fun, calendar, up?)}
    end

    defp reduce(x, y, {:cont, acc}, fun, calendar, up? = true) when x <= y do
      reduce(x + 1, y, fun.(date_from_iso_days(x, calendar), acc), fun, calendar, up?)
    end

    defp reduce(x, y, {:cont, acc}, fun, calendar, up? = false) when x >= y do
      reduce(x - 1, y, fun.(date_from_iso_days(x, calendar), acc), fun, calendar, up?)
    end

    defp reduce(_, _, {:cont, acc}, _fun, _calendar, _up) do
      {:done, acc}
    end

    defp date_from_iso_days(days, Calendar.ISO) do
      {year, month, day} = Calendar.ISO.date_from_iso_days(days)
      %Date{year: year, month: month, day: day, calendar: Calendar.ISO}
    end

    defp date_from_iso_days(days, calendar) do
      {year, month, day, _, _, _, _} =
        calendar.naive_datetime_from_iso_days({days, {0, 86_400_000_000}})

      %Date{year: year, month: month, day: day, calendar: calendar}
    end
  end

  defimpl Inspect do
    def inspect(%Date.Range{first: first, last: last}, _) do
      "#DateRange<" <> inspect(first) <> ", " <> inspect(last) <> ">"
    end
  end
end
