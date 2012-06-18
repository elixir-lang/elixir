defmodule Range do
  def new(first, last) do
    { __MODULE__, first, last }
  end

  def first(range) do
    elem(range, 2)
  end

  def last(range) do
    elem(range, 3)
  end
end