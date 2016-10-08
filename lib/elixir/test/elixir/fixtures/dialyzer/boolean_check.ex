defmodule Dialyzer.BooleanCheck do
  def and_check(arg) do
    arg and arg
  end

  def and_check_optimized(arg) do
    is_integer(arg) and arg > 10
  end

  def or_check(arg) do
    arg or arg
  end

  def or_check_optimized(arg) do
    is_integer(arg) or arg > 10
  end
end
