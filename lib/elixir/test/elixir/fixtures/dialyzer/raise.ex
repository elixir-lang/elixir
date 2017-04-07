defmodule Dialyzer.Raise do
  defexception [:message]

  def exception_var() do
    ex = %Dialyzer.Raise{}
    raise ex
  end

  def exception_var(ex = %Dialyzer.Raise{}) do
    raise ex
  end

  def string_var() do
    string = "hello"
    raise string
  end

  def string_var(string) when is_binary(string) do
    raise string
  end
end
