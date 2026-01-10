defmodule Dialyzer.InRange do
  def string_to_number_in_range(x) do
    String.to_integer(x) in 1..10
  end
end
