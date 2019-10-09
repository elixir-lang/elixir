defmodule CodeTest.CheckerWarning do
  def foo(x) when is_atom(x) and is_list(x), do: x
end
