defmodule BarFoo do
  def hello do
    :world
  end
end

defprotocol BarFoo.Protocol do
  def to_uppercase(string)
end

defimpl BarFoo.Protocol, for: BitString do
  def to_uppercase(string), do: String.upcase(string)
end
