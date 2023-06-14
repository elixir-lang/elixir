defmodule Sample do
  @file "test/elixir/fixtures/fancy_diagnostics/unicode.ex"

  def a do
    10 + "😎"
  end
end
