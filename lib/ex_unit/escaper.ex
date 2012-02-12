# In some assertions we need to show a guard clause
# in the exception message. This usually works fine
# except if the clause has a "_", which means that
# we cannot show it because evaluating it would
# generate a "unbound _" error.
#
# For such cases, we can use `ExUnit::Escaper.escape`
# to escape the expression replacing all "_" by a flag
# that when inspected returns "_".
defrecord ExUnit::Escaper::Flag, as: nil

defmodule ExUnit::Escaper do
  defimpl String::Inspect, for: Flag do
    def inspect(record),   do: record.as
    def to_binary(record), do: record.as
  end

  # Replace _ by a record that when inspected prints "_"
  def escape({ :_, _, args }) when is_atom(args) do
    Flag.new(as: "_")
  end

  def escape(expr) when is_tuple(expr) do
    list_to_tuple(escape(tuple_to_list(expr)))
  end

  def escape(expr) when is_list(expr) do
    Enum.map expr, escape(&1)
  end

  def escape(expr) do
    expr
  end
end