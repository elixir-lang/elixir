# Some exceptions implement `message/1` instead of `exception/1` mostly
# for bootstrap reasons. It is recommended for applications to implement
# `exception/1` instead of `message/1` as described in `defexception/3` docs.

defexception RuntimeError,      message: "runtime error"
defexception ArgumentError,     message: "argument error"
defexception ArithmeticError,   message: "bad argument in arithmetic expression"
defexception SystemLimitError,  message: "a system limit has been reached"

defexception SyntaxError, [file: nil, line: nil, description: "syntax error"] do
  def message(exception) do
    Exception.format_file_line(Path.relative_to_cwd(exception.file), exception.line) <>
      " " <> exception.description
  end
end

defexception TokenMissingError, [file: nil, line: nil, description: "expression is incomplete"] do
  def message(exception) do
    Exception.format_file_line(Path.relative_to_cwd(exception.file), exception.line) <>
      " " <> exception.description
  end
end

defexception CompileError, [file: nil, line: nil, description: "compile error"] do
  def message(exception) do
    Exception.format_file_line(Path.relative_to_cwd(exception.file), exception.line) <>
      " " <> exception.description
  end
end

defexception BadFunctionError, [term: nil] do
  def message(exception) do
    "expected a function, got: #{inspect(exception.term)}"
  end
end

defexception BadStructError, [struct: nil, term: nil] do
  def message(exception) do
    "expected a struct named #{inspect(exception.struct)}, got: #{inspect(exception.term)}"
  end
end

defexception MatchError, [term: nil] do
  def message(exception) do
    "no match of right hand side value: #{inspect(exception.term)}"
  end
end

defexception CaseClauseError, [term: nil] do
  def message(exception) do
    "no case clause matching: #{inspect(exception.term)}"
  end
end

defexception TryClauseError, [term: nil] do
  def message(exception) do
    "no try clause matching: #{inspect(exception.term)}"
  end
end

defexception BadArityError, [function: nil, args: nil] do
  def message(exception) do
    fun  = exception.function
    args = exception.args
    insp = Enum.map_join(args, ", ", &inspect/1)
    {:arity, arity} = :erlang.fun_info(fun, :arity)
    "#{inspect(fun)} with arity #{arity} called with #{count(length(args), insp)}"
  end

  defp count(0, _insp), do: "no arguments"
  defp count(1, insp),  do: "1 argument (#{insp})"
  defp count(x, insp),  do: "#{x} arguments (#{insp})"
end

defexception UndefinedFunctionError, [module: nil, function: nil, arity: nil] do
  def message(exception) do
    if exception.function do
      formatted = Exception.format_mfa exception.module, exception.function, exception.arity
      "undefined function: #{formatted}"
    else
      "undefined function"
    end
  end
end

defexception FunctionClauseError, [module: nil, function: nil, arity: nil] do
  def message(exception) do
    if exception.function do
      formatted = Exception.format_mfa exception.module, exception.function, exception.arity
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

defexception KeyError, key: nil, term: nil do
  def message(exception) do
    "key #{inspect exception.key} not found in: #{inspect exception.term}"
  end
end

defexception UnicodeConversionError, [:encoded, :message] do
  def exception(opts) do
    UnicodeConversionError[
      encoded: opts[:encoded],
      message: "#{opts[:kind]} #{detail(opts[:rest])}"
    ]
  end

  defp detail(rest) when is_binary(rest) do
    "encoding starting at #{inspect rest}"
  end

  defp detail([h|_]) do
    "code point #{h}"
  end
end

defexception Enum.OutOfBoundsError, message: "out of bounds error"

defexception Enum.EmptyError, message: "empty error"

defexception File.Error, [reason: nil, action: "", path: nil] do
  def message(exception) do
    formatted = iodata_to_binary(:file.format_error(reason exception))
    "could not #{action exception} #{path exception}: #{formatted}"
  end
end

defexception File.CopyError, [reason: nil, action: "", source: nil, destination: nil, on: nil] do
  def message(exception) do
    formatted = iodata_to_binary(:file.format_error(reason exception))
    location  = if on = on(exception), do: ". #{on}", else: ""
    "could not #{action exception} from #{source exception} to " <>
      "#{destination exception}#{location}: #{formatted}"
  end
end

defmodule Exception do
  @moduledoc """
  Functions to work with and pretty print exceptions.

  Note that stacktraces in Elixir are updated on throw,
  errors and exits. For example, at any given moment,
  `System.stacktrace` will return the stacktrace for the
  last throw/error/exit that ocurred in the current process.

  Finally note developers should not rely on the particular
  format of the `format` functions provided by this module.
  They may be changed in future releases in order to better
  suit Elixir's tool chain. In other words, by using the
  functions in this module it is guarantee you will format
  exceptions as in the current Elixir version being used.
  """

  @type t :: tuple
  @type stacktrace :: [stacktrace_entry]
  @type process ::
        pid | atom | {atom, node} | {:global, term} | {:via, module, term}
  @type stacktrace_entry ::
        {module, function, arity_or_args, location} |
        {function, arity_or_args, location}

  @typep arity_or_args :: non_neg_integer | list
  @typep location :: Keyword.t

  @doc """
  Gets the message for an exception.
  """
  def message(exception) do
    exception.message
  end

  @doc """
  Normalizes an exception, converting Erlang exceptions
  to Elixir exceptions.

  It takes the `kind` spilled by `catch` as an argument and
  normalizes only `:error`, returning the untouched payload
  for others.

  The third argument, a stacktrace, is optional. If it is
  not supplied `System.stacktrace/0` will sometimes be used
  to get additional information for the `kind` `:error`. If
  the stacktrace is unknown and `System.stacktrace/0` would
  not return the stacktrace corresponding to the exception
  an empty stacktrace, `[]`, must be used.
  """
  @spec normalize(:error, any, stacktrace) :: t
  @spec normalize(:exit | :throw, any, stacktrace) :: any
  # Generating a stacktrace is expensive, default to nil to only fetch it when
  # needed.
  def normalize(kind, payload, stacktrace \\ nil)

  def normalize(:error, exception, stacktrace) do
    normalize_error(exception, stacktrace)
  end

  def normalize(_kind, payload, _stacktrace), do: payload

  defp normalize_error(exception, _stacktrace) when is_exception(exception) do
    exception
  end

  defp normalize_error(:badarg, _stacktrace) do
    ArgumentError[]
  end

  defp normalize_error(:badarith, _stacktrace) do
    ArithmeticError[]
  end

  defp normalize_error(:system_limit, _stacktrace) do
    SystemLimitError[]
  end

  defp normalize_error({:badarity, {fun, args}}, _stacktrace) do
    BadArityError[function: fun, args: args]
  end

  defp normalize_error({:badfun, term}, _stacktrace) do
    BadFunctionError[term: term]
  end

  defp normalize_error({:badstruct, struct, term}, _stacktrace) do
    BadStructError[struct: struct, term: term]
  end

  defp normalize_error({:badmatch, term}, _stacktrace) do
    MatchError[term: term]
  end

  defp normalize_error({:case_clause, term}, _stacktrace) do
    CaseClauseError[term: term]
  end

  defp normalize_error({:try_clause, term}, _stacktrace) do
    TryClauseError[term: term]
  end

  defp normalize_error(:undef, stacktrace) do
    {mod, fun, arity} = from_stacktrace(stacktrace || :erlang.get_stacktrace)
    UndefinedFunctionError[module: mod, function: fun, arity: arity]
  end

  defp normalize_error(:function_clause, stacktrace) do
    {mod, fun, arity} = from_stacktrace(stacktrace || :erlang.get_stacktrace)
    FunctionClauseError[module: mod, function: fun, arity: arity]
  end

  defp normalize_error({:badarg, payload}, _stacktrace) do
    ArgumentError[message: "argument error: #{inspect(payload)}"]
  end

  defp normalize_error(other, _stacktrace) do
    ErlangError[original: other]
  end

  @doc """
  Normalizes and formats any throw, error and exit.

  The message is formatted and displayed in the same
  format as used by Elixir's CLI.

  The third argument, a stacktrace, is optional. If it is
  not supplied `System.stacktrace/0` will sometimes be used
  to get additional information for the `kind` `:error`. If
  the stacktrace is unknown and `System.stacktrace/0` would
  not return the stacktrace corresponding to the exception
  an empty stacktrace, `[]`, must be used.
  """
  @spec format_banner(:error | :exit | :throw, any, stacktrace) :: String.t
  def format_banner(kind, exception, stacktrace \\ nil)

  def format_banner(:error, exception, stacktrace) do
    exception = normalize_error(exception, stacktrace)
    "** (" <> inspect(exception.__record__(:name)) <> ") " <> exception.message
  end

  def format_banner(:throw, reason, _stacktrace) do
   "** (throw) " <> inspect(reason)
  end

  def format_banner(:exit, reason, _stacktrace) do
    "** (exit) " <> format_exit(reason, <<"\n    ">>)
  end

  @doc """
  Normalizes and formats an exception and stacktrace.

  Behaves the same as `format_banner/3` but also includes
  the formatted stacktrace.
  """
  @spec format(:error | :exit | :throw, any, stacktrace) :: String.t
  def format(kind, exception, stacktrace \\ :erlang.get_stacktrace()) do
    message = format_banner(kind, exception, stacktrace)
    case stacktrace do
      [] -> message
      _ -> message <> "\n" <> format_stacktrace(stacktrace)
    end
  end

  @doc """
  Formats an exit, returns a string.

  Often there are errors/exceptions inside exits. Exits are often
  wrapped by the caller and provide stacktraces too. This function
  formats exits in a way to nicely show the exit reason, caller
  and stacktrace.
  """
  @spec format_exit(any) :: String.t
  def format_exit(reason) do
    format_exit(reason, <<"\n    ">>)
  end

  # 2-Tuple could be caused by an error if the second element is a stacktrace.
  defp format_exit({exception, maybe_stacktrace} = reason, joiner)
      when is_list(maybe_stacktrace) and maybe_stacktrace !== [] do
    try do
      Enum.map(maybe_stacktrace, &format_stacktrace_entry/1)
    else
      formatted_stacktrace ->
        # Assume a non-empty list formattable as stacktrace is a
        # stacktrace, so exit was caused by an error.
        message = "an exception was raised:" <> joiner <>
          format_banner(:error, exception, maybe_stacktrace)
        Enum.join([message | formatted_stacktrace], joiner <> <<"    ">>)
    catch
      :error, _ ->
        # Not a stacktrace, was an exit.
        format_exit_reason(reason)
    end
  end

  # 2-Tuple could be an exit caused by mfa if second element is mfa,
  defp format_exit({reason2, {mod, fun, args}} = reason, joiner) do
    try do
      format_mfa(mod, fun, args)
    else
      mfa ->
        # Assume tuple formattable as an mfa is an mfa, so exit was caused by
        # failed mfa.
        "exited in: " <> mfa <> joiner <>
          "** (EXIT) " <> format_exit(reason2, joiner <> <<"    ">>)
    catch
      :error, _ ->
        # Not an mfa, was an exit.
        format_exit_reason(reason)
    end
  end

  defp format_exit(reason, _joiner) do
    format_exit_reason(reason)
  end

  defp format_exit_reason(:normal), do: "normal"
  defp format_exit_reason(:shutdown), do: "shutdown"

  defp format_exit_reason({:shutdown, reason}) do
    "shutdown: #{inspect(reason)}"
  end

  defp format_exit_reason(:timeout), do: "time out"
  defp format_exit_reason(:killed), do: "killed"
  defp format_exit_reason(:noconnection), do: "no connection"

  defp format_exit_reason(:noproc) do
    "no process"
  end

  defp format_exit_reason({:nodedown, node_name}) when is_atom(node_name) do
    "no connection to #{node_name}"
  end

  defp format_exit_reason(reason), do: inspect(reason)

  @doc """
  Receives a stacktrace entry and formats it into a string.
  """
  @spec format_stacktrace_entry(stacktrace_entry) :: String.t
  def format_stacktrace_entry(entry)

  # From Macro.Env.stacktrace
  def format_stacktrace_entry({module, :__MODULE__, 0, location}) do
    format_location(location) <> inspect(module) <> " (module)"
  end

  # From :elixir_compiler_*
  def format_stacktrace_entry({_module, :__MODULE__, 1, location}) do
    format_location(location) <> "(module)"
  end

  # From :elixir_compiler_*
  def format_stacktrace_entry({_module, :__FILE__, 1, location}) do
    format_location(location) <> "(file)"
  end

  def format_stacktrace_entry({module, fun, arity, location}) do
    format_application(module) <> format_location(location) <> format_mfa(module, fun, arity)
  end

  def format_stacktrace_entry({fun, arity, location}) do
    format_location(location) <> format_fa(fun, arity)
  end

  defp format_application(module) do
    case :application.get_application(module) do
      {:ok, app} -> "(" <> atom_to_binary(app) <> ") "
      :undefined   -> ""
    end
  end

  @doc """
  Formats the stacktrace.

  A stacktrace must be given as an argument. If not, this function
  calculates a new stacktrace based on the caller and formats it. As
  a consequence, the value of `System.stacktrace` is changed.
  """
  def format_stacktrace(trace \\ nil) do
    trace = trace || try do
      throw(:stacktrace)
    catch
      :stacktrace -> Enum.drop(:erlang.get_stacktrace, 1)
    end
    case trace do
      [] -> "\n"
      s  -> "    " <> Enum.map_join(s, "\n    ", &format_stacktrace_entry(&1)) <> "\n"
    end
  end

  @doc """
  Receives an anonymous function and arity and formats it as
  shown in stacktraces. The arity may also be a list of arguments.

  ## Examples

      Exception.format_fa(fn -> end, 1)
      #=> "#Function<...>/1"

  """
  def format_fa(fun, arity) do
    "#{inspect fun}#{format_arity(arity)}"
  end

  @doc """
  Receives a module, fun and arity and formats it
  as shown in stacktraces. The arity may also be a list
  of arguments.

  ## Examples

      iex> Exception.format_mfa Foo, :bar, 1
      "Foo.bar/1"

      iex> Exception.format_mfa Foo, :bar, []
      "Foo.bar()"

      iex> Exception.format_mfa nil, :bar, []
      "nil.bar()"

  Anonymous functions are reported as -func/arity-anonfn-count-,
  where func is the name of the enclosing function. Convert to
  "anonymous fn in func/arity"
  """
  def format_mfa(module, fun, arity) when is_atom(fun) do
    fun =
      case inspect(fun) do
        ":" <> fun -> fun
        fun -> fun
      end

    case match?("\"-" <> _, fun) and String.split(fun, "-") do
      [ "\"", outer_fun, "fun", _count, "\"" ] ->
        "anonymous fn#{format_arity(arity)} in #{inspect module}.#{outer_fun}"
      _ ->
        "#{inspect module}.#{fun}#{format_arity(arity)}"
    end
  end

  defp format_arity(arity) when is_list(arity) do
    inspected = for x <- arity, do: inspect(x)
    "(#{Enum.join(inspected, ", ")})"
  end

  defp format_arity(arity), do: "/#{arity}"

  @doc """
  Formats the given file and line as shown in stacktraces.
  If any of the values are nil, they are omitted.

  ## Examples

      iex> Exception.format_file_line("foo", 1)
      "foo:1:"

      iex> Exception.format_file_line("foo", nil)
      "foo:"

      iex> Exception.format_file_line(nil, nil)
      ""

  """
  def format_file_line(file, line) do
    format_file_line(file, line, "")
  end

  defp format_file_line(file, line, suffix) do
    if file do
      if line && line != 0 do
        "#{file}:#{line}:#{suffix}"
      else
        "#{file}:#{suffix}"
      end
    else
      ""
    end
  end

  defp format_location(opts) do
    format_file_line Keyword.get(opts, :file), Keyword.get(opts, :line), " "
  end

  defp from_stacktrace([{module, function, args, _}|_]) when is_list(args) do
    {module, function, length(args)}
  end

  defp from_stacktrace([{module, function, arity, _}|_]) do
    {module, function, arity}
  end

  defp from_stacktrace(_) do
    {nil, nil, nil}
  end
end
