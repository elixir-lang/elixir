# Wildcard exception raised in runtime.
defmodule Exception do
  def normalize(exception) when is_exception(exception) do
    exception
  end

  def normalize(:undef) do
    UndefinedError.from_stacktrace(Code.stacktrace)
  end

  def normalize(else) do
    ErlangError.new(original: else)
  end
end

defexception RuntimeError, message: nil

defexception UndefinedFunctionError, module: nil, function: nil, arity: nil do
  # Erlang > R15
  def from_stacktrace([{ module, function, arity, _ }|_]) do
    new(module: module, function: function, arity: to_arity(arity))
  end

  # Erlang < R15
  def from_stacktrace([{ module, function, arity }|_]) do
    new(module: module, function: function, arity: to_arity(arity))
  end

  # Safe clause
  def from_stacktrace(_) do
    new()
  end

  def message(exception) do
    if exception.function do
      separator =
        case atom_to_list(exception.module) do
        match: '::' ++ _
          "."
        else:
          ":"
        end

      "undefined function #{exception.module}#{separator}#{exception.function}/#{exception.arity}"
    else:
      "undefined function"
    end
  end

  defp to_arity(arity) when is_integer(arity), do: arity
  defp to_arity(list)  when is_list(list),     do: length(list)
end

defexception ErlangError, original: nil do
  def message(exception) do
    "erlang error #{inspect(exception.original)}"
  end
end