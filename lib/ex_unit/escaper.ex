defmodule ExUnit::Escaper do
  defrecord Flag, as: nil

  defimpl String::Inspect, for: Flag do
    def inspect(record),   do: record.as
    def to_binary(record), do: record.as
  end

  # Replace _ by a record that when inspected prints "_"
  def escape({ :_, _, false }) do
    quote { unquote(Flag).new(as: "_") }
  end

  def escape(expr) when is_tuple(expr) do
    list_to_tuple(escape(tuple_to_list(expr)))
  end

  def escape(expr) when is_list(expr) do
    Enum.map expr, escape(_)
  end

  def escape(expr) do
    expr
  end
end