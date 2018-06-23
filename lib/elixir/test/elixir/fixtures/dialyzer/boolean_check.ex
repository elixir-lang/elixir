defmodule Dialyzer.BooleanCheck do
  def and_check(arg) when is_boolean(arg) do
    arg and arg
  end

  def or_check(arg) when is_boolean(arg) do
    arg or arg
  end
end
