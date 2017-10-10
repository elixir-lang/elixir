defmodule Dialyzer.Rewrite do
  def interpolation do
    "foo #{:a}"
  end

  def reverse do
    Enum.reverse(1..3)
  end
end
