defmodule Date.Range do
  @moduledoc """
  Returns an inclusive range between dates.

  Ranges must be created with the `Date.range/2` function.

  The following fields are public:

    * `:first` - the initial date on the range
    * `:last` - the last date on the range

  The remaining fields are private and should not be accessed.
  """

  @type t :: %__MODULE__{first: Date.t, last: Date.t,
                         first_in_iso_days: Calendar.days,
                         last_in_iso_days: Calendar.days}

  defstruct [:first, :last, :first_in_iso_days, :last_in_iso_days]

  defimpl Enumerable do
    def member?(%{first: %{calendar: calendar, year: first_year, month: first_month, day: first_day},
                  last: %{calendar: calendar, year: last_year, month: last_month, day: last_day},
                  first_in_iso_days: first_in_iso_days, last_in_iso_days: last_in_iso_days},
                %Date{calendar: calendar, year: year, month: month, day: day}) do
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

    def count(%Date.Range{first_in_iso_days: first_in_iso_days, last_in_iso_days: last_in_iso_days}) do
      {:ok, abs(first_in_iso_days - last_in_iso_days) + 1}
    end

    def reduce(%Date.Range{first_in_iso_days: first_in_iso_days, last_in_iso_days: last_in_iso_days,
                           first: %{calendar: calendar}}, acc, fun) do
      reduce(first_in_iso_days, last_in_iso_days, acc, fun, calendar, first_in_iso_days <= last_in_iso_days)
    end

    defp reduce(_x, _y, {:halt, acc}, _fun, _calendar, _up?) do
      {:halted, acc}
    end

    defp reduce(x, y, {:suspend, acc}, fun, calendar, up?) do
      {:suspended, acc, &reduce(x, y, &1, fun, calendar, up?)}
    end

    defp reduce(x, y, {:cont, acc}, fun, calendar, up? = true) when x <= y do
      reduce(x + 1, y, fun.(date_from_iso_days_days(x, calendar), acc), fun, calendar, up?)
    end

    defp reduce(x, y, {:cont, acc}, fun, calendar, up? = false) when x >= y do
      reduce(x - 1, y, fun.(date_from_iso_days_days(x, calendar), acc), fun, calendar, up?)
    end

    defp reduce(_, _, {:cont, acc}, _fun, _calendar, _up) do
      {:done, acc}
    end

    defp date_from_iso_days_days(days, Calendar.ISO) do
      {year, month, day} = Calendar.ISO.date_from_iso_days_days(days)
      %Date{year: year, month: month, day: day, calendar: Calendar.ISO}
    end

    defp date_from_iso_days_days(days, calendar) do
      {year, month, day, _, _, _, _} = calendar.naive_datetime_from_iso_days({days, {0, 86400000000}})
      %Date{year: year, month: month, day: day, calendar: calendar}
    end
  end

  defimpl Inspect do
    def inspect(%Date.Range{first: first, last: last}, _) do
      "#DateRange<" <> inspect(first) <> ", " <> inspect(last) <> ">"
    end
  end
end
