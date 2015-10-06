defmodule DialyzerRaise do

  defexception [:message]

  def exception_var() do
    e = %DialyzerRaise{}
    raise e
  end

  def exception_var(e = %DialyzerRaise{}) do
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
