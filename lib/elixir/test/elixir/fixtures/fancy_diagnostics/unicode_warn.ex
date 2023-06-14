defmodule Sample do
  @file "test/elixir/fixtures/fancy_diagnostics/unicode_warn.ex"

  def a do
    10 + "😎"
  end
end
