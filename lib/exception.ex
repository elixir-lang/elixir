defmodule Exception do
  # Normalize an exception converting Erlang exceptions
  # to Elixir style exceptions.
  def normalize(exception) when is_exception(exception) do
    exception
  end

  def normalize(:badarg) do
    ArgumentError.new
  end

  def normalize(:badarith) do
    ArithmeticError.new
  end

  def normalize(:undef) do
    UndefinedFunctionError.new from_stacktrace(Code.stacktrace)
  end

  def normalize({ :badarg, payload }) do
    ArgumentError.new message: "argument error: #{inspect(payload)}"
  end

  def normalize(else) do
    ErlangError.new original: else
  end

  # Format module, fun and arity to inspection.

  def format_module_fun_arity(module, fun, arity) do
    separator =
      case atom_to_list(module) do
      match: '::' ++ _
        "."
      else:
        ":"
      end

    "#{module}#{separator}#{fun}/#{arity}"
  end

  # Private

  # Erlang >= R15
  def from_stacktrace([{ module, function, arity, _ }|_]) do
    [module: module, function: function, arity: arity]
  end

  # Erlang < R15
  def from_stacktrace([{ module, function, arity }|_]) do
    [module: module, function: function, arity: arity]
  end

  # Safe clause
  def from_stacktrace(_) do
    []
  end
end

defexception RuntimeError,    message: "runtime error"
defexception ArgumentError,   message: "argument error"
defexception ArithmeticError, message: "bad argument in arithmetic expression"

defexception UndefinedFunctionError, module: nil, function: nil, arity: nil do
  def message(exception) do
    if exception.function do
      formatted = Exception.format_module_fun_arity exception.module, exception.function, to_arity(exception.arity)
      "undefined function #{formatted}"
    else:
      "undefined function"
    end
  end

  defp to_arity(arity) when is_integer(arity), do: arity
  defp to_arity(list)  when is_list(list),     do: length(list)
end

defexception ErlangError, original: nil do
  def message(exception) do
    "erlang error: #{inspect(exception.original)}"
  end
end