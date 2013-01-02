defmodule Exception do
  @moduledoc """
  Several convenience functions to work and pretty print
  exceptions and stacktraces.
  """

  @doc """
  Normalizes an exception converting Erlang exceptions
  to Elixir exceptions. Useful when interfacing Erlang
  code with Elixir code.
  """
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
    UndefinedFunctionError.new from_stacktrace(:erlang.get_stacktrace)
  end

  def normalize(:function_clause) do
    FunctionClauseError.new from_stacktrace(:erlang.get_stacktrace)
  end

  def normalize({ :badarg, payload }) do
    ArgumentError.new message: "argument error: #{inspect(payload)}"
  end

  def normalize(other) do
    ErlangError.new original: other
  end

  @doc """
  Receives a tuple representing a stacktrace entry and formats it.
  """

  # From Macro.Env.stacktrace
  def format_entry({ module, :__MODULE__, 0, file_line }) do
    "#{format_file_line(file_line)}#{inspect module} (module)"
  end

  # From :elixir_compiler
  def format_entry({ _module, :__MODULE__, 2, file_line }) do
    "#{format_file_line(file_line)}(module)"
  end

  # From :elixir_compiler
  def format_entry({ _module, :__FILE__, 2, file_line }) do
    "#{format_file_line(file_line)}(file)"
  end

  def format_entry({module, fun, arity, file_line}) do
    "#{format_file_line(file_line)}#{format_module_fun_arity(module, fun, arity)}"
  end

  @doc """
  Formats the stacktrace.

  A stacktrace must be given as argument. If not, this function
  calculates the current stacktrace and formats it. As consequence,
  the value of `System.stacktrace` is changed.
  """
  def format_stacktrace(trace // nil)

  def format_stacktrace(trace) when is_tuple(trace) do
    IO.puts "Exception.format_stacktrace is deprecated, please use format_entry instead"
    print_stacktrace
  end

  def format_stacktrace(trace) do
    trace = trace || try do
      throw(:stacktrace)
    catch
      :stacktrace -> Enum.drop(:erlang.get_stacktrace, 1)
    end

    case trace do
      [] -> "\n"
      s  -> "    " <> Enum.map_join(s, "\n    ", format_entry(&1)) <> "\n"
    end
  end

  @doc """
  Prints the current stacktrace to standard output.
  """
  def print_stacktrace(trace // nil) do
    IO.write format_stacktrace(trace)
  end

  ## Helpers

  # Check the given module is a valid exception record.
  @doc false
  def check!(module) do
    unless :erlang.function_exported(module, :message, 1) do
      raise "Expected #{inspect module} to implement message/1"
    end
  end

  # Receives a module, fun and arity and returns a string
  # representing such invocation. Arity may also be a list
  # of arguments. It follows the same syntax as in stacktraces.
  @doc false
  def format_module_fun_arity(module, fun, arity) do
    case inspect(fun) do
      << ?:, fun :: binary >> -> :ok
      fun -> :ok
    end

    if is_list(arity) do
      inspected = lc x inlist arity, do: inspect(x)
      "#{inspect module}.#{fun}(#{Enum.join(inspected, ", ")})"
    else
      "#{inspect module}.#{fun}/#{arity}"
    end
  end


  def format_file_line(file_line) do
    format_file_line(Keyword.get(file_line, :file), Keyword.get(file_line, :line))
  end

  def format_file_line(file, line) do
    if file do
      file = to_binary(file)
      if line && line != 0 do
        "#{file}:#{line}: "
      else
        "#{file}: "
      end
    else
      ""
    end
  end

  defp from_stacktrace([{ module, function, arity, _ }|_]) do
    [module: module, function: function, arity: arity]
  end

  defp from_stacktrace(_), do: []
end

defexception RuntimeError,      message: "runtime error"
defexception ArgumentError,     message: "argument error"
defexception ArithmeticError,   message: "bad argument in arithmetic expression"
defexception SystemLimitError,  message: "a system limit has been reached"

defexception SyntaxError, [file: nil, line: nil, description: "syntax error"] do
  def message(exception) do
    "#{Exception.format_file_line(exception.file, exception.line)}#{exception.description}"
  end
end

defexception TokenMissingError, [file: nil, line: nil, description: "expression is incomplete"] do
  def message(exception) do
    "#{Exception.format_file_line(exception.file, exception.line)}#{exception.description}"
  end
end

defexception CompileError, [file: nil, line: nil, description: "compile error"] do
  def message(exception) do
    "#{Exception.format_file_line(exception.file, exception.line)}#{exception.description}"
  end
end

defexception BadFunctionError, [actual: nil] do
  def message(exception) do
    "expected a function, got: #{inspect(exception.actual)}"
  end
end

defexception MatchError, [actual: nil] do
  def message(exception) do
    "no match of right hand side value: #{inspect(exception.actual)}"
  end
end

defexception CaseClauseError, [actual: nil] do
  def message(exception) do
    "no case clause matching: #{inspect(exception.actual)}"
  end
end

defexception BadArityError, [function: nil, args: nil] do
  def message(exception) do
    "bad arity error: #{inspect(exception.function)} called with #{inspect(exception.args)}"
  end
end

defexception UndefinedFunctionError, [module: nil, function: nil, arity: nil] do
  def message(exception) do
    if exception.function do
      formatted = Exception.format_module_fun_arity exception.module, exception.function, to_arity(exception.arity)
      "undefined function: #{formatted}"
    else
      "undefined function"
    end
  end

  defp to_arity(arity) when is_integer(arity), do: arity
  defp to_arity(list)  when is_list(list),     do: length(list)
end

defexception FunctionClauseError, [module: nil, function: nil, arity: nil] do
  def message(exception) do
    if exception.function do
      formatted = Exception.format_module_fun_arity exception.module, exception.function, exception.arity
      "no function clause matching: #{formatted}"
    else
      "no function clause matches"
    end
  end
end

defexception Protocol.UndefinedError, [protocol: nil, structure: nil] do
  def message(exception) do
    "protocol #{inspect exception.protocol} not implemented for #{inspect exception.structure}"
  end
end

defexception ErlangError, [original: nil] do
  def message(exception) do
    "erlang error: #{inspect(exception.original)}"
  end
end

defexception KeyError, key: nil do
  def message(exception) do
    "key not found: #{inspect exception.key}"
  end
end

defexception Enum.OutOfBoundsError, message: "out of bounds error"
