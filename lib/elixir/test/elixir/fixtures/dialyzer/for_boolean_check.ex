defmodule Dialyzer.ForBooleanCheck do
  def foo(enum, potential) when is_binary(potential) do
    for element <- enum, string = Atom.to_string(element), string == potential do
      element
    end
  end
end
