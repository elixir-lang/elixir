defmodule Exception do
  @moduledoc """
  Functions to format throw/catch/exit and exceptions.

  Note that stacktraces in Elixir are updated on throw,
  errors and exits. For example, at any given moment,
  `System.stacktrace/0` will return the stacktrace for the
  last throw/error/exit that occurred in the current process.

  Do not rely on the particular format returned by the `format*`
  functions in this module. They may be changed in future releases
  in order to better suit Elixir's tool chain. In other words,
  by using the functions in this module it is guaranteed you will
  format exceptions as in the current Elixir version being used.
  """

  @typedoc "The exception type"
  @type t :: %{
    required(:__struct__) => module,
    required(:__exception__) => true,
    atom => any
  }

  @typedoc "The kind handled by formatting functions"
  @type kind :: :error | :exit | :throw | {:EXIT, pid}

  @type stacktrace :: [stacktrace_entry]
  @type stacktrace_entry ::
        {module, atom, arity_or_args, location} |
        {(... -> any), arity_or_args, location}

  @typep arity_or_args :: non_neg_integer | list
  @typep location :: Keyword.t

  @callback exception(term) :: t
  @callback message(t) :: String.t

  @doc """
  Returns `true` if the given `term` is an exception.
  """
  def exception?(term)

  def exception?(%{__struct__: struct, __exception__: true}) when is_atom(struct),
    do: true

  def exception?(_), do: false

  @doc """
  Gets the message for an `exception`.
  """
  def message(%{__struct__: module, __exception__: true} = exception) when is_atom(module) do
    try do
      module.message(exception)
    rescue
      e ->
        "got #{inspect e.__struct__} with message #{inspect message(e)} " <>
        "while retrieving Exception.message/1 for #{inspect(exception)}"
    else
      x when is_binary(x) -> x
      x ->
        "got #{inspect(x)} " <>
        "while retrieving Exception.message/1 for #{inspect(exception)} " <>
        "(expected a string)"
    end
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
  @spec normalize(kind, payload, stacktrace) :: payload when payload: var

  # Generating a stacktrace is expensive, default to nil
  # to only fetch it when needed.
  def normalize(kind, payload, stacktrace \\ nil)

  def normalize(:error, exception, stacktrace) do
    if exception?(exception) do
      exception
    else
      ErlangError.normalize(exception, stacktrace)
    end
  end

  def normalize(_kind, payload, _stacktrace) do
    payload
  end

  @doc """
  Normalizes and formats any throw/error/exit.

  The message is formatted and displayed in the same
  format as used by Elixir's CLI.

  The third argument, a stacktrace, is optional. If it is
  not supplied `System.stacktrace/0` will sometimes be used
  to get additional information for the `kind` `:error`. If
  the stacktrace is unknown and `System.stacktrace/0` would
  not return the stacktrace corresponding to the exception
  an empty stacktrace, `[]`, must be used.
  """
  @spec format_banner(kind, any, stacktrace | nil) :: String.t
  def format_banner(kind, exception, stacktrace \\ nil)

  def format_banner(:error, exception, stacktrace) do
    exception = normalize(:error, exception, stacktrace)
    "** (" <> inspect(exception.__struct__) <> ") " <> message(exception)
  end

  def format_banner(:throw, reason, _stacktrace) do
   "** (throw) " <> inspect(reason)
  end

  def format_banner(:exit, reason, _stacktrace) do
    "** (exit) " <> format_exit(reason, <<"\n    ">>)
  end

  def format_banner({:EXIT, pid}, reason, _stacktrace) do
    "** (EXIT from #{inspect pid}) " <> format_exit(reason, <<"\n    ">>)
  end

  @doc """
  Normalizes and formats throw/errors/exits and stacktraces.

  It relies on `format_banner/3` and `format_stacktrace/1`
  to generate the final format.

  Note that `{:EXIT, pid}` do not generate a stacktrace though
  (as they are retrieved as messages without stacktraces).
  """

  @spec format(kind, any, stacktrace | nil) :: String.t

  def format(kind, payload, stacktrace \\ nil)

  def format({:EXIT, _} = kind, any, _) do
    format_banner(kind, any)
  end

  def format(kind, payload, stacktrace) do
    stacktrace = stacktrace || System.stacktrace
    message = format_banner(kind, payload, stacktrace)
    case stacktrace do
      [] -> message
      _  -> message <> "\n" <> format_stacktrace(stacktrace)
    end
  end

  @doc """
  Formats an exit. It returns a string.

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

  # :supervisor.start_link returns this error reason when it fails to init
  # because a child's start_link raises.
  defp format_exit({:shutdown,
      {:failed_to_start_child, child, {:EXIT, reason}}}, joiner) do
    format_start_child(child, reason, joiner)
  end

  # :supervisor.start_link returns this error reason when it fails to init
  # because a child's start_link returns {:error, reason}.
  defp format_exit({:shutdown, {:failed_to_start_child, child, reason}},
      joiner) do
    format_start_child(child, reason, joiner)
  end

  # 2-Tuple could be an exit caused by mfa if second element is mfa, args
  # must be a list of arguments - max length 255 due to max arity.
  defp format_exit({reason2, {mod, fun, args}} = reason, joiner)
      when length(args) < 256 do
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

  defp format_exit_reason(:calling_self), do: "process attempted to call itself"
  defp format_exit_reason(:timeout), do: "time out"
  defp format_exit_reason(:killed), do: "killed"
  defp format_exit_reason(:noconnection), do: "no connection"

  defp format_exit_reason(:noproc) do
    "no process: the process is not alive or there's no process currently associated with the given name, possibly because its application isn't started"
  end

  defp format_exit_reason({:nodedown, node_name}) when is_atom(node_name) do
    "no connection to #{node_name}"
  end

  # :gen_server exit reasons

  defp format_exit_reason({:already_started, pid}) do
    "already started: " <> inspect(pid)
  end

  defp format_exit_reason({:bad_return_value, value}) do
    "bad return value: " <> inspect(value)
  end

  defp format_exit_reason({:bad_call, request}) do
    "bad call: " <> inspect(request)
  end

  defp format_exit_reason({:bad_cast, request}) do
    "bad cast: " <> inspect(request)
  end

  # :supervisor.start_link error reasons

  # If value is a list will be formatted by mfa exit in format_exit/1
  defp format_exit_reason({:bad_return, {mod, :init, value}})
      when is_atom(mod) do
    format_mfa(mod, :init, 1) <> " returned a bad value: " <> inspect(value)
  end

  defp format_exit_reason({:bad_start_spec, start_spec}) do
    "bad start spec: invalid children: " <> inspect(start_spec)
  end

  defp format_exit_reason({:start_spec, start_spec}) do
    "bad start spec: " <> format_sup_spec(start_spec)
  end

  defp format_exit_reason({:supervisor_data, data}) do
    "bad supervisor data: " <> format_sup_data(data)
  end

  defp format_exit_reason(reason), do: inspect(reason)

  defp format_start_child(child, reason, joiner) do
    "shutdown: failed to start child: " <> inspect(child) <> joiner <>
      "** (EXIT) " <> format_exit(reason, joiner <> <<"    ">>)
  end

  defp format_sup_data({:invalid_type, type}) do
    "invalid type: " <> inspect(type)
  end

  defp format_sup_data({:invalid_strategy, strategy}) do
    "invalid strategy: " <> inspect(strategy)
  end

  defp format_sup_data({:invalid_intensity, intensity}) do
    "invalid intensity: " <> inspect(intensity)
  end

  defp format_sup_data({:invalid_period, period}) do
    "invalid period: " <> inspect(period)
  end

  defp format_sup_data(other), do: inspect(other)

  defp format_sup_spec({:invalid_child_spec, child_spec}) do
   "invalid child spec: " <> inspect(child_spec)
  end

  defp format_sup_spec({:invalid_child_type, type}) do
    "invalid child type: " <> inspect(type)
  end

  defp format_sup_spec({:invalid_mfa, mfa}) do
    "invalid mfa: " <> inspect(mfa)
  end

  defp format_sup_spec({:invalid_restart_type, restart}) do
    "invalid restart type: " <> inspect(restart)
  end

  defp format_sup_spec({:invalid_shutdown, shutdown}) do
    "invalid shutdown: " <> inspect(shutdown)
  end

  defp format_sup_spec({:invalid_module, mod}) do
    "invalid module: " <> inspect(mod)
  end

  defp format_sup_spec({:invalid_modules, modules}) do
    "invalid modules: " <> inspect(modules)
  end

  defp format_sup_spec(other), do: inspect(other)

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
    # We cannot use Application due to bootstrap issues
    case :application.get_application(module) do
      {:ok, app} -> "(" <> Atom.to_string(app) <> ") "
      :undefined -> ""
    end
  end

  @doc """
  Formats the stacktrace.

  A stacktrace must be given as an argument. If not, the stacktrace
  is retrieved from `Process.info/2`.
  """
  def format_stacktrace(trace \\ nil) do
    trace = trace || case Process.info(self(), :current_stacktrace) do
      {:current_stacktrace, t} -> Enum.drop(t, 3)
    end

    case trace do
      [] -> "\n"
      _ -> "    " <> Enum.map_join(trace, "\n    ", &format_stacktrace_entry(&1)) <> "\n"
    end
  end

  @doc """
  Receives an anonymous function and arity and formats it as
  shown in stacktraces. The arity may also be a list of arguments.

  ## Examples

      Exception.format_fa(fn -> nil end, 1)
      #=> "#Function<...>/1"

  """
  def format_fa(fun, arity) when is_function(fun) do
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
  def format_mfa(module, fun, arity) when is_atom(module) and is_atom(fun) do
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

  defp format_arity(arity) when is_integer(arity) do
    "/" <> Integer.to_string(arity)
  end

  @doc """
  Formats the given `file` and `line` as shown in stacktraces.
  If any of the values are `nil`, they are omitted.

  ## Examples

      iex> Exception.format_file_line("foo", 1)
      "foo:1:"

      iex> Exception.format_file_line("foo", nil)
      "foo:"

      iex> Exception.format_file_line(nil, nil)
      ""

  """
  def format_file_line(file, line, suffix \\ "") do
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

  defp format_location(opts) when is_list(opts) do
    format_file_line Keyword.get(opts, :file), Keyword.get(opts, :line), " "
  end
end

# Some exceptions implement "message/1" instead of "exception/1" mostly
# for bootstrap reasons. It is recommended for applications to implement
# "exception/1" instead of "message/1" as described in "defexception/1"
# docs.

defmodule RuntimeError do
  defexception message: "runtime error"
end

defmodule ArgumentError do
  defexception message: "argument error"
end

defmodule ArithmeticError do
  defexception message: "bad argument in arithmetic expression"
end

defmodule SystemLimitError do
  defexception []

  def message(_) do
    "a system limit has been reached"
  end
end

defmodule SyntaxError do
  defexception [:file, :line, description: "syntax error"]

  def message(exception) do
    Exception.format_file_line(Path.relative_to_cwd(exception.file), exception.line) <>
      " " <> exception.description
  end
end

defmodule TokenMissingError do
  defexception [:file, :line, description: "expression is incomplete"]

  def message(%{file: file, line: line, description: description}) do
    Exception.format_file_line(file && Path.relative_to_cwd(file), line) <>
      " " <> description
  end
end

defmodule CompileError do
  defexception [:file, :line, description: "compile error"]

  def message(%{file: file, line: line, description: description}) do
    Exception.format_file_line(file && Path.relative_to_cwd(file), line) <>
      " " <> description
  end
end

defmodule BadFunctionError do
  defexception [:term]

  def message(exception) do
    "expected a function, got: #{inspect(exception.term)}"
  end
end

defmodule BadStructError do
  defexception [:struct, :term]

  def message(exception) do
    "expected a struct named #{inspect(exception.struct)}, got: #{inspect(exception.term)}"
  end
end

defmodule BadMapError do
  defexception [:term]

  def message(exception) do
    "expected a map, got: #{inspect(exception.term)}"
  end
end

defmodule BadBooleanError do
  defexception [:term, :operator]

  def message(exception) do
    "expected a boolean on left-side of \"#{exception.operator}\", got: #{inspect(exception.term)}"
  end
end

defmodule MatchError do
  defexception [:term]

  def message(exception) do
    "no match of right hand side value: #{inspect(exception.term)}"
  end
end

defmodule CaseClauseError do
  defexception [:term]

  def message(exception) do
    "no case clause matching: #{inspect(exception.term)}"
  end
end

defmodule WithClauseError do
  defexception [:term]

  def message(exception) do
    "no with clause matching: #{inspect(exception.term)}"
  end
end

defmodule CondClauseError do
  defexception []

  def message(_exception) do
    "no cond clause evaluated to a true value"
  end
end

defmodule TryClauseError do
  defexception [:term]

  def message(exception) do
    "no try clause matching: #{inspect(exception.term)}"
  end
end

defmodule BadArityError do
  defexception [:function, :args]

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

defmodule UndefinedFunctionError do
  defexception [:module, :function, :arity, :reason, :exports]

  def message(%{reason: nil, module: module, function: function, arity: arity} = e) do
    cond do
      is_nil(function) or is_nil(arity) ->
        "undefined function"
      not is_nil(module) and :code.is_loaded(module) === false ->
        message(%{e | reason: :"module could not be loaded"})
      true ->
        message(%{e | reason: :"function not exported"})
    end
  end

  def message(%{reason: :"module could not be loaded", module: module, function: function, arity: arity}) do
    "function " <> Exception.format_mfa(module, function, arity) <>
      " is undefined (module #{inspect module} is not available)"
  end

  def message(%{reason: :"function not exported",  module: module, function: function, arity: arity, exports: exports}) do
    suffix =
      if macro_exported?(module, function, arity) do
        ". However there is a macro with the same name and arity." <>
          " Be sure to require #{inspect(module)} if you intend to invoke this macro"
      else
        did_you_mean(module, function, arity, exports)
      end

    "function " <>
      Exception.format_mfa(module, function, arity) <>
      " is undefined or private" <>
      suffix
  end

  def message(%{reason: :"function not available", module: module, function: function, arity: arity}) do
    "nil." <> fa = Exception.format_mfa(nil, function, arity)
    "function " <> Exception.format_mfa(module, function, arity) <>
    " is undefined (function #{fa} is not available)"
  end

  def message(%{reason: reason,  module: module, function: function, arity: arity}) do
    "function " <> Exception.format_mfa(module, function, arity) <> " is undefined (#{reason})"
  end

  @function_threshold 0.77
  @max_suggestions 5

  defp did_you_mean(module, function, _arity, exports) do
    exports = exports || exports_for(module)

    result =
      case Keyword.take(exports, [function]) do
        [] ->
          base = Atom.to_string(function)
          for {key, val} <- exports,
              dist = String.jaro_distance(base, Atom.to_string(key)),
              dist >= @function_threshold,
            do: {dist, key, val}
        arities ->
          for {key, val} <- arities, do: {1.0, key, val}
      end
      |> Enum.sort(&elem(&1, 0) >= elem(&2, 0))
      |> Enum.take(@max_suggestions)
      |> Enum.sort(&elem(&1, 1) <= elem(&2, 1))

    case result do
      []          -> ""
      suggestions -> ". Did you mean one of:\n\n#{Enum.map(suggestions, &format_fa/1)}"
    end
  end

  defp format_fa({_dist, fun, arity}) do
    fun = with ":" <> fun <- inspect(fun), do: fun
    "      * " <> fun <> "/" <> Integer.to_string(arity) <> "\n"
  end

  defp exports_for(module) do
    if function_exported?(module, :__info__, 1) do
      module.__info__(:macros) ++ module.__info__(:functions)
    else
      module.module_info(:exports)
    end
  rescue
    # In case the module was removed while we are computing this
    UndefinedFunctionError -> []
  end
end

defmodule FunctionClauseError do
  defexception [:module, :function, :arity]

  def message(exception) do
    if exception.function do
      formatted = Exception.format_mfa exception.module, exception.function, exception.arity
      "no function clause matching in #{formatted}"
    else
      "no function clause matches"
    end
  end
end

defmodule Code.LoadError do
  defexception [:file, :message]

  def exception(opts) do
    file = Keyword.fetch!(opts, :file)
    %Code.LoadError{message: "could not load #{file}", file: file}
  end
end

defmodule Protocol.UndefinedError do
  defexception [:protocol, :value, description: ""]

  def message(exception) do
    msg = "protocol #{inspect exception.protocol} not implemented for #{inspect exception.value}"
    case exception.description do
      "" -> msg
      descr -> msg <> ", " <> descr
    end
  end
end

defmodule KeyError do
  defexception [:key, :term]

  def message(exception) do
    msg = "key #{inspect exception.key} not found"
    if exception.term != nil do
      msg <> " in: #{inspect exception.term}"
    else
      msg
    end
  end
end

defmodule UnicodeConversionError do
  defexception [:encoded, :message]

  def exception(opts) do
    %UnicodeConversionError{
      encoded: Keyword.fetch!(opts, :encoded),
      message: "#{Keyword.fetch!(opts, :kind)} #{detail Keyword.fetch!(opts, :rest)}"
    }
  end

  defp detail(rest) when is_binary(rest) do
    "encoding starting at #{inspect rest}"
  end

  defp detail([h | _]) when is_integer(h) do
    "code point #{h}"
  end

  defp detail([h | _]) do
    detail(h)
  end
end

defmodule Enum.OutOfBoundsError do
  defexception message: "out of bounds error"
end

defmodule Enum.EmptyError do
  defexception message: "empty error"
end

defmodule File.Error do
  defexception [:reason, :path, action: ""]

  def message(%{action: action, reason: reason, path: path}) do
    formatted =
      case {action, reason} do
        {"remove directory", :eexist} ->
          "directory is not empty"
        _ ->
          IO.iodata_to_binary(:file.format_error(reason))
      end

    "could not #{action} #{inspect(path)}: #{formatted}"
  end
end

defmodule File.CopyError do
  defexception [:reason, :source, :destination, on: "", action: ""]

  def message(exception) do
    formatted =
      IO.iodata_to_binary(:file.format_error(exception.reason))

    location =
      case exception.on do
        "" -> ""
        on -> ". #{on}"
      end

    "could not #{exception.action} from #{inspect(exception.source)} to " <>
      "#{inspect(exception.destination)}#{location}: #{formatted}"
  end
end

defmodule ErlangError do
  defexception [:original]

  def message(exception) do
    "erlang error: #{inspect(exception.original)}"
  end

  @doc false
  def normalize(:badarg, _stacktrace) do
    %ArgumentError{}
  end

  def normalize(:badarith, _stacktrace) do
    %ArithmeticError{}
  end

  def normalize(:system_limit, _stacktrace) do
    %SystemLimitError{}
  end

  def normalize(:cond_clause, _stacktrace) do
    %CondClauseError{}
  end

  def normalize({:badarity, {fun, args}}, _stacktrace) do
    %BadArityError{function: fun, args: args}
  end

  def normalize({:badfun, term}, _stacktrace) do
    %BadFunctionError{term: term}
  end

  def normalize({:badstruct, struct, term}, _stacktrace) do
    %BadStructError{struct: struct, term: term}
  end

  def normalize({:badmatch, term}, _stacktrace) do
    %MatchError{term: term}
  end

  def normalize({:badmap, term}, _stacktrace) do
    %BadMapError{term: term}
  end

  def normalize({:badbool, op, term}, _stacktrace) do
    %BadBooleanError{operator: op, term: term}
  end

  def normalize({:badkey, key}, stacktrace) do
    term =
      case stacktrace || :erlang.get_stacktrace do
        [{Map, :get_and_update!, [map, _, _], _} | _] -> map
        [{Map, :update!, [map, _, _], _} | _] -> map
        [{:maps, :update, [_, _, map], _} | _] -> map
        [{:maps, :get, [_, map], _} | _] -> map
        _ -> nil
      end
    %KeyError{key: key, term: term}
  end

  def normalize({:badkey, key, map}, _stacktrace) do
    %KeyError{key: key, term: map}
  end

  def normalize({:case_clause, term}, _stacktrace) do
    %CaseClauseError{term: term}
  end

  def normalize({:with_clause, term}, _stacktrace) do
    %WithClauseError{term: term}
  end

  def normalize({:try_clause, term}, _stacktrace) do
    %TryClauseError{term: term}
  end

  def normalize(:undef, stacktrace) do
    stacktrace = stacktrace || :erlang.get_stacktrace
    {mod, fun, arity} = from_stacktrace(stacktrace)
    %UndefinedFunctionError{module: mod, function: fun, arity: arity}
  end

  def normalize(:function_clause, stacktrace) do
    {mod, fun, arity} = from_stacktrace(stacktrace || :erlang.get_stacktrace)
    %FunctionClauseError{module: mod, function: fun, arity: arity}
  end

  def normalize({:badarg, payload}, _stacktrace) do
    %ArgumentError{message: "argument error: #{inspect(payload)}"}
  end

  def normalize(other, _stacktrace) do
    %ErlangError{original: other}
  end

  defp from_stacktrace([{module, function, args, _} | _]) when is_list(args) do
    {module, function, length(args)}
  end

  defp from_stacktrace([{module, function, arity, _} | _]) do
    {module, function, arity}
  end

  defp from_stacktrace(_) do
    {nil, nil, nil}
  end
end
