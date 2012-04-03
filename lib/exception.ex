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

  def normalize(:system_limit) do
    SystemLimitError.new
  end

  def normalize({ :badarity, { fun, args } }) do
    BadArityError.new(function: fun, args: args)
  end

  def normalize({ :badfun, actual }) do
    BadFunctionError.new(actual: actual)
  end

  def normalize({ :badmatch, actual }) do
    MatchError.new(actual: actual)
  end

  def normalize({ :case_clause, actual }) do
    CaseClauseError.new(actual: actual)
  end

  def normalize(:undef) do
    UndefinedFunctionError.new from_stacktrace(System.stacktrace)
  end

  def normalize(:function_clause) do
    FunctionClauseError.new from_stacktrace(System.stacktrace)
  end

  def normalize({ :badarg, payload }) do
    ArgumentError.new message: "argument error: #{inspect(payload)}"
  end

  def normalize(else) do
    ErlangError.new original: else
  end

  # Format module, fun and arity to inspection.
  def format_module_fun_arity(module, fun, arity) do
    << ?:, fun | :binary >> = inspect(fun)

    if is_list(arity) do
      inspected = lc x in arity, do: inspect(x)
      "#{inspect module}.#{fun}(#{Enum.join(inspected, ", ")})"
    else:
      "#{inspect module}.#{fun}/#{arity}"
    end
  end

  # Format stacktrace for inspection.
  def format_stacktrace({module, fun, arity, file_line}) do
    "#{format_file_line(file_line)}#{format_module_fun_arity(module, fun, arity)}"
  end

  # Formats the given file and line given as a Keyword.
  def format_file_line(file_line) do
    format_file_line(Keyword.get(file_line, :file), Keyword.get(file_line, :line))
  end

  # Formats the given file and line.
  def format_file_line(file, line) do
    if file do
      file = to_binary(file)
      if line && line != 0 do
        "#{file}:#{line}: "
      else:
        "#{file}: "
      end
    else:
      ""
    end
  end

  ## Helpers

  defp from_stacktrace([{ module, function, arity, _ }|_]) do
    [module: module, function: function, arity: arity]
  end

  defp from_stacktrace(_), do: []
end

defexception RuntimeError,      message: "runtime error"
defexception ArgumentError,     message: "argument error"
defexception ArithmeticError,   message: "bad argument in arithmetic expression"
defexception SystemLimitError,  message: "a system limit has been reached"

defexception SyntaxError, file: nil, line: nil, description: "syntax error" do
  def message(exception) do
    "#{Exception.format_file_line(exception.file, exception.line)}#{exception.description}"
  end
end

defexception TokenMissingError, file: nil, line: nil, description: "expression is incomplete" do
  def message(exception) do
    "#{Exception.format_file_line(exception.file, exception.line)}#{exception.description}"
  end
end

defexception CompileError, file: nil, line: nil, description: "compile error" do
  def message(exception) do
    "#{Exception.format_file_line(exception.file, exception.line)}#{exception.description}"
  end
end

defexception BadFunctionError, actual: nil do
  def message(exception) do
    "bad function: #{inspect(exception.actual)}"
  end
end

defexception MatchError, actual: nil do
  def message(exception) do
    "no match of right hand side value: #{inspect(exception.actual)}"
  end
end

defexception CaseClauseError, actual: nil do
  def message(exception) do
    "no case clause matching: #{inspect(exception.actual)}"
  end
end

defexception BadArityError, function: nil, args: nil do
  def message(exception) do
    "bad arity error: #{inspect(exception.function)} called with #{inspect(exception.args)}"
  end
end

defexception UndefinedFunctionError, module: nil, function: nil, arity: nil do
  def message(exception) do
    if exception.function do
      formatted = Exception.format_module_fun_arity exception.module, exception.function, to_arity(exception.arity)
      "undefined function: #{formatted}"
    else:
      "undefined function"
    end
  end

  defp to_arity(arity) when is_integer(arity), do: arity
  defp to_arity(list)  when is_list(list),     do: length(list)
end

defexception FunctionClauseError, module: nil, function: nil, arity: nil do
  def message(exception) do
    if exception.function do
      formatted = Exception.format_module_fun_arity exception.module, exception.function, exception.arity
      "no function clause matching: #{formatted}"
    else:
      "no function clause matches"
    end
  end
end

defexception Protocol.UndefinedError, protocol: nil, structure: nil do
  def message(exception) do
    "protocol #{inspect exception.protocol} not implemented for #{inspect exception.structure}"
  end
end

defexception ErlangError, original: nil do
  def message(exception) do
    "erlang error: #{inspect(exception.original)}"
  end
end