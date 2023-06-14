defmodule Sample do
  @file "test/elixir/fixtures/fancy_diagnostics/long_warn.ex"

  def atom_case do
    v = "bc"
    case v do
      _ when is_atom(v) -> :ok
      _ -> :fail
    end
  end
end
