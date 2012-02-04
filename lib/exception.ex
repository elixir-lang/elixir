# Wildcard exception raised in runtime.
defmodule Exception do
  def normalize(exception) when is_exception(exception) do
    exception
  end

  def normalize(else) do
    ErlangError.new(original: else)
  end
end

defexception RuntimeError, message: nil

defexception ErlangError, original: nil do
  def message(exception) do
    "erlang error #{inspect(exception.original)}"
  end
end