defmodule Sample do
  @file "test/elixir/fixtures/fancy_diagnostics/warn_line_column.ex"
  @foo 1

  def bar do
    @foo
    :ok
  end
end
