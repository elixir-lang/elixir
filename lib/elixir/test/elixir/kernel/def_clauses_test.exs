Code.require_file "../../test_helper.exs", __FILE__

defmodule Kernel.DefClause do
  defmacro defclause(expr, block) do
    quote do: def unquote(expr), unquote(block)
  end
end

defmodule Kernel.ClausesTest do
  use ExUnit.Case, async: true

  import Kernel.DefClause

  # This is just to ensure we won't have warnings
  # for clauses with match after default.
  def clause_with_match_after_default(x, y // nil, { z, w }) do
    x + y + z + w
  end

  # This is simply testing the warning about
  # function overriden consider the current stack
  def foo(0), do: 0
  def bar, do: 1
  defclause foo(1), do: 2

  test :in_operator_in_function_definition do
    assert with_in(3, :it_works)  == :it_works
    assert with_in(3, "it fails") == false
    assert with_in(0, :it_fails)  == false
  end

  defp with_in(x in [1,2,3], other) when is_atom(other), do: other
  defp with_in(_, _), do: false

  test :clauses_without_implementation_can_have_default_args do
    assert a_number == 13
    assert a_number(4) == 17
  end

  defp a_number(x // 0)
  defp a_number(x), do: x + 13
end