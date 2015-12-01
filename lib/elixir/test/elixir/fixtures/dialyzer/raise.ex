defmodule Dialyzer.Raise do
  defexception [:message]

  def exception_var() do
    e = %Dialyzer.Raise{}
    raise e
  end

  def exception_var(e = %Dialyzer.Raise{}) do
    raise e
  end

  def string_var() do
    string = "hello"
    raise string
  end

  def string_var(string) when is_binary(string) do
    raise string
  end
end
