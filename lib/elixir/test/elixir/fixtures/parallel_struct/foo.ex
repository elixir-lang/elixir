defmodule FooStruct do
  defstruct name: ""
  def bar?(%BarStruct{}), do: true
end
