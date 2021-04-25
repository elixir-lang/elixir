defmodule Bar do
  def hello do
    :world
  end
end

defmodule Bar.Ignore do
  def world do
    :hello
  end
end

defprotocol Bar.Protocol do
  def to_uppercase(string)
end

defimpl Bar.Protocol, for: BitString do
  def to_uppercase(string), do: String.upcase(string)
end
