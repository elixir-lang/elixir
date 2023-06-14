defmodule Sample do
  @file "test/elixir/fixtures/fancy_diagnostics/grouped_warnings.ex"

  def a do
    A.bar()
    A.bar()
    A.bar()
    A.bar()
  end
end
