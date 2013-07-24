defexception RuntimeError,      message: "runtime error"
defexception ArgumentError,     message: "argument error"
defexception ArithmeticError,   message: "bad argument in arithmetic expression"
defexception SystemLimitError,  message: "a system limit has been reached"

defexception SyntaxError, [file: nil, line: nil, description: "syntax error"] do
  def message(exception) do
    "#{Exception.format_file_line(exception.file, exception.line, nil)}#{exception.description}"
  end
end

defexception TokenMissingError, [file: nil, line: nil, description: "expression is incomplete"] do
  def message(exception) do
    "#{Exception.format_file_line(exception.file, exception.line, nil)}#{exception.description}"
  end
end

defexception CompileError, [file: nil, line: nil, description: "compile error"] do
  def message(exception) do
    "#{Exception.format_file_line(exception.file, exception.line, nil)}#{exception.description}"
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
      formatted = Exception.format_module_fun_arity exception.module, exception.function, exception.arity
      "undefined function: #{formatted}"
    else
      "undefined function"
    end
  end
end

defexception FunctionClauseError, [module: nil, function: nil, arity: nil] do
  def message(exception) do
    if exception.function do
      formatted = Exception.format_module_fun_arity exception.module, exception.function, exception.arity
      "no function clause matching in #{formatted}"
    else
      "no function clause matches"
    end
  end
end

defexception Protocol.UndefinedError, [protocol: nil, value: nil, description: nil] do
  def message(exception) do
    msg = "protocol #{inspect exception.protocol} not implemented for #{inspect exception.value}"
    if exception.description do
      msg <> ", " <> exception.description
    else
      msg
    end
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

defexception Enum.EmptyError, message: "empty error"

defmodule Exception do
  @moduledoc """
  Several convenience functions to work with and pretty print
  exceptions and stacktraces.
  """

  @doc """
  Normalizes an exception, converting Erlang exceptions
  to Elixir exceptions. It takes the kind spilled by
  `catch` as an argument as a convenience for converting only
  `:errors`, ignorning the others.
  """
  def normalize(:error, exception), do: normalize(exception)
  def normalize(_kind, other), do: other

  @doc """
  Normalizes an exception, converting Erlang exceptions
  to Elixir exceptions. Useful when interfacing Erlang
  code with Elixir code.
  """
  def normalize(exception) when is_exception(exception) do
    exception
  end

  def normalize(:badarg) do
    ArgumentError[]
  end

  def normalize(:badarith) do
    ArithmeticError[]
  end

  def normalize(:system_limit) do
    SystemLimitError[]
  end

  def normalize({ :badarity, { fun, args } }) do
    BadArityError[function: fun, args: args]
  end

  def normalize({ :badfun, actual }) do
    BadFunctionError[actual: actual]
  end

  def normalize({ :badmatch, actual }) do
    MatchError[actual: actual]
  end

  def normalize({ :case_clause, actual }) do
    CaseClauseError[actual: actual]
  end

  def normalize(:undef) do
    { mod, fun, arity } = from_stacktrace(:erlang.get_stacktrace)
    UndefinedFunctionError[module: mod, function: fun, arity: arity]
  end

  def normalize(:function_clause) do
    { mod, fun, arity } = from_stacktrace(:erlang.get_stacktrace)
    FunctionClauseError[module: mod, function: fun, arity: arity]
  end

  def normalize({ :badarg, payload }) do
    ArgumentError[message: "argument error: #{inspect(payload)}"]
  end

  def normalize(other) do
    ErlangError[original: other]
  end

  @doc """
  Receives a tuple representing a stacktrace entry and formats it.
  The current working directory may be given as an argument, which
  is used to prettify the stacktrace.
  """
  def format_stacktrace_entry(entry, cwd // nil)

  # From Macro.Env.stacktrace
  def format_stacktrace_entry({ module, :__MODULE__, 0, file_line }, cwd) do
    "#{format_file_line(file_line, cwd)}#{inspect module} (module)"
  end

  # From :elixir_compiler_*
  def format_stacktrace_entry({ _module, :__MODULE__, 2, file_line }, cwd) do
    "#{format_file_line(file_line, cwd)}(module)"
  end

  # From :elixir_compiler_*
  def format_stacktrace_entry({ _module, :__FILE__, 2, file_line }, cwd) do
    "#{format_file_line(file_line, cwd)}(file)"
  end

  def format_stacktrace_entry({module, fun, arity, file_line}, cwd) do
    "#{format_file_line(file_line, cwd)}#{format_module_fun_arity(module, fun, arity)}"
  end

  def format_stacktrace_entry({fun, arity, file_line}, cwd) do
    "#{format_file_line(file_line, cwd)}#{format_fun_arity(fun, arity)}"
  end

  @doc """
  Formats the stacktrace.

  A stacktrace must be given as an argument. If not, this function
  calculates the current stacktrace and formats it. As a consequence,
  the value of `System.stacktrace` is changed.
  """
  def format_stacktrace(trace // nil) do
    trace = trace || try do
      throw(:stacktrace)
    catch
      :stacktrace -> Enum.drop(:erlang.get_stacktrace, 1)
    end

    case trace do
      [] -> "\n"
      s  -> "    " <> Enum.map_join(s, "\n    ", format_stacktrace_entry(&1)) <> "\n"
    end
  end

  @doc """
  Formats the caller, i.e. the first entry in the stacktrace.
  Notice that due to tail call optimization, the stacktrace
  may not report the direct caller of the function.
  """
  def format_caller(trace // nil) do
    trace = trace || try do
      throw(:stacktrace)
    catch
      :stacktrace -> Enum.drop(:erlang.get_stacktrace, 1)
    end

    if entry = Enum.at(trace, 1) do
      format_stacktrace_entry(entry)
    else
      "nofile:0: "
    end
  end

  ## Helpers

  # Format fun and arity
  @doc false
  def format_fun_arity(fun, arity) do
    if is_list(arity) do
      inspected = lc x inlist arity, do: inspect(x)
      "#{inspect fun}(#{Enum.join(inspected, ", ")})"
    else
      "#{inspect fun}/#{arity}"
    end
  end

  # Receives a module, fun and arity and returns a string
  # representing such invocation. Arity may also be a list
  # of arguments. It follows the same syntax as in stacktraces.
  @doc false
  def format_module_fun_arity(module, fun, arity) do
    fun =
      case inspect(fun) do
        << ?:, erl :: binary >> -> erl
        elixir -> elixir
      end

    if is_list(arity) do
      inspected = lc x inlist arity, do: inspect(x)
      "#{inspect module}.#{fun}(#{Enum.join(inspected, ", ")})"
    else
      "#{inspect module}.#{fun}/#{arity}"
    end
  end

  # Format file and line for exception printing.
  @doc false
  def format_file_line(opts, cwd) do
    format_file_line Keyword.get(opts, :file), Keyword.get(opts, :line), cwd
  end

  def format_file_line(file, line, cwd) do
    if file do
      file = to_binary(file)

      if cwd do
        file = Path.relative_to(file, cwd)
      end

      if line && line != 0 do
        "#{file}:#{line}: "
      else
        "#{file}: "
      end
    else
      ""
    end
  end

  defp from_stacktrace([{ module, function, args, _ }|_]) when is_list(args) do
    { module, function, length(args) }
  end

  defp from_stacktrace([{ module, function, arity, _ }|_]) do
    { module, function, arity }
  end

  defp from_stacktrace(_) do
    { nil, nil, nil }
  end
end
