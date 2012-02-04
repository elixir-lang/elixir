# Wildcard exception raised in runtime.
defmodule Exception do
  def normalize(exception) when is_exception(exception) do
    exception
  end
end

defexception RuntimeError, message: nil

defexception ErlangError, original: nil do
  def message(exception) do
    IO.puts "ErlangError: #{exception.original}"
  end
end