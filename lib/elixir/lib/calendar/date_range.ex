defmodule Date.Range do
  @moduledoc """
  Defines a range of dates.

  A range of dates represents a discrete number of dates where
  the first and last values are dates with matching calendars.

  Ranges of dates can be either increasing (`first <= last`) or
  decreasing (`first > last`). They are also always inclusive.

  A range of dates implements the `Enumerable` protocol, which means
  functions in the `Enum` module can be used to work with
  ranges:

      iex> range = Date.range(~D[2001-01-01], ~D[2002-01-01])
      iex> Enum.count(range)
      366
      iex> Enum.member?(range, ~D[2001-02-01])
      true
      iex> Enum.reduce(range, 0, fn(_date, acc) -> acc - 1 end)
      -366

  """

  @opaque t :: %__MODULE__{first: Date.t, last: Date.t}
  @doc false
  defstruct [:first, :last, :first_rata_die, :last_rata_die]

  defimpl Enumerable do
    def member?(%Date.Range{first: %{calendar: calendar, year: first_year, month: first_month, day: first_day},
              last: %{calendar: calendar, year: last_year, month: last_month, day: last_day},
              first_rata_die: first_rata_die, last_rata_die: last_rata_die},
            %Date{calendar: calendar, year: year, month: month, day: day}) do
      first = {first_year, first_month, first_day}
      last = {last_year, last_month, last_day}
      date = {year, month, day}

      if first_rata_die <= last_rata_die do
        {:ok, date >= first and date <= last}
      else
        {:ok, date >= last and date <= first}
      end
    end

    def member?(_, _) do
      {:ok, false}
    end

    def count(%Date.Range{first_rata_die: first_rata_die, last_rata_die: last_rata_die}) do
      {:ok, abs(first_rata_die - last_rata_die) + 1}
    end

    def reduce(%Date.Range{first_rata_die: first_rata_die, last_rata_die: last_rata_die, first: %{calendar: c}}, acc, fun) do
      reduce(first_rata_die, last_rata_die, acc, &(fun.(Date.from_rata_die_days(&1, c), &2)), first_rata_die <= last_rata_die)
    end

    defp reduce(_x, _y, {:halt, acc}, _fun, _up?) do
      {:halted, acc}
    end

    defp reduce(x, y, {:suspend, acc}, fun, up?) do
      {:suspended, acc, &reduce(x, y, &1, fun, up?)}
    end

    defp reduce(x, y, {:cont, acc}, fun, _up? = true) when x <= y do
      reduce(x + 1, y, fun.(x, acc), fun, _up? = true)
    end

    defp reduce(x, y, {:cont, acc}, fun, _up? = false) when x >= y do
      reduce(x - 1, y, fun.(x, acc), fun, _up? = false)
    end

    defp reduce(_, _, {:cont, acc}, _fun, _up) do
      {:done, acc}
    end
  end

  defimpl Inspect do
    def inspect(%Date.Range{first: first, last: last}, _) do
      "#DateRange<" <> inspect(first) <> ", " <> inspect(last) <> ">"
    end
  end
end
