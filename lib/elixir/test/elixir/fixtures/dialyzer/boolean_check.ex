defmodule Dialyzer.BooleanCheck do
  def and_check(arg) when is_boolean(arg) do
    arg and arg
  end

  def and_check_optimized(arg) when is_integer(arg) do
    arg < :infinity and arg
  end

  def or_check(arg) when is_boolean(arg) do
    arg or arg
  end

  def or_check_optimized(arg) when is_integer(arg) do
    arg < :infinity or arg
  end
end
