defmodule Exception do
  @moduledoc """
  Functions to format throw/catch/exit and exceptions.

  Note that stacktraces in Elixir are only available inside
  catch and rescue by using the `__STACKTRACE__/0` variable.

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
          optional(atom) => any
        }

  @typedoc "The kind handled by formatting functions"
  @type kind :: :error | non_error_kind
  @typep non_error_kind :: :exit | :throw | {:EXIT, pid}

  @type stacktrace :: [stacktrace_entry]
  @type stacktrace_entry ::
          {module, atom, arity_or_args, location}
          | {(... -> any), arity_or_args, location}

  @typep arity_or_args :: non_neg_integer | list
  @typep location :: keyword

  @callback exception(term) :: t
  @callback message(t) :: String.t()

  @doc """
  Called from `Exception.blame/3` to augment the exception struct.

  Can be used to collect additional information about the exception
  or do some additional expensive computation.
  """
  @callback blame(t, stacktrace) :: {t, stacktrace}
  @optional_callbacks [blame: 2]

  @doc """
  Returns `true` if the given `term` is an exception.
  """
  def exception?(term)
  def exception?(%_{__exception__: true}), do: true
  def exception?(_), do: false

  @doc """
  Gets the message for an `exception`.
  """
  def message(%module{__exception__: true} = exception) do
    try do
      module.message(exception)
    rescue
      caught_exception ->
        "got #{inspect(caught_exception.__struct__)} with message " <>
          "#{inspect(message(caught_exception))} while retrieving Exception.message/1 " <>
          "for #{inspect(exception)}"
    else
      result when is_binary(result) ->
        result

      result ->
        "got #{inspect(result)} " <>
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

  The third argument is the stacktrace which is used to enrich
  a normalized error with more information. It is only used when
  the kind is an error.
  """
  @spec normalize(:error, any, stacktrace) :: t
  @spec normalize(non_error_kind, payload, stacktrace) :: payload when payload: var
  def normalize(kind, payload, stacktrace \\ [])
  def normalize(:error, %_{__exception__: true} = payload, _stacktrace), do: payload
  def normalize(:error, payload, stacktrace), do: ErlangError.normalize(payload, stacktrace)
  def normalize(_kind, payload, _stacktrace), do: payload

  @doc """
  Normalizes and formats any throw/error/exit.

  The message is formatted and displayed in the same
  format as used by Elixir's CLI.

  The third argument is the stacktrace which is used to enrich
  a normalized error with more information. It is only used when
  the kind is an error.
  """
  @spec format_banner(kind, any, stacktrace) :: String.t()
  def format_banner(kind, exception, stacktrace \\ [])

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
    "** (EXIT from #{inspect(pid)}) " <> format_exit(reason, <<"\n    ">>)
  end

  @doc """
  Normalizes and formats throw/errors/exits and stacktraces.

  It relies on `format_banner/3` and `format_stacktrace/1`
  to generate the final format.

  If `kind` is `{:EXIT, pid}`, it does not generate a stacktrace,
  as such exits are retrieved as messages without stacktraces.
  """
  @spec format(kind, any, stacktrace) :: String.t()
  def format(kind, payload, stacktrace \\ [])

  def format({:EXIT, _} = kind, any, _) do
    format_banner(kind, any)
  end

  def format(kind, payload, stacktrace) do
    message = format_banner(kind, payload, stacktrace)

    case stacktrace do
      [] -> message
      _ -> message <> "\n" <> format_stacktrace(stacktrace)
    end
  end

  @doc """
  Attaches information to exceptions for extra debugging.

  This operation is potentially expensive, as it reads data
  from the file system, parses beam files, evaluates code and
  so on.

  If the exception module implements the optional `c:blame/2`
  callback, it will be invoked to perform the computation.
  """
  @doc since: "1.5.0"
  @spec blame(:error, any, stacktrace) :: {t, stacktrace}
  @spec blame(non_error_kind, payload, stacktrace) :: {payload, stacktrace} when payload: var
  def blame(kind, error, stacktrace)

  def blame(:error, error, stacktrace) do
    %module{} = struct = normalize(:error, error, stacktrace)

    if Code.ensure_loaded?(module) and function_exported?(module, :blame, 2) do
      module.blame(struct, stacktrace)
    else
      {struct, stacktrace}
    end
  end

  def blame(_kind, reason, stacktrace) do
    {reason, stacktrace}
  end

  @doc """
  Blames the invocation of the given module, function and arguments.

  This function will retrieve the available clauses from bytecode
  and evaluate them against the given arguments. The clauses are
  returned as a list of `{args, guards}` pairs where each argument
  and each top-level condition in a guard separated by `and`/`or`
  is wrapped in a tuple with blame metadata.

  This function returns either `{:ok, definition, clauses}` or `:error`.
  Where `definition` is `:def`, `:defp`, `:defmacro` or `:defmacrop`.
  """
  @doc since: "1.5.0"
  @spec blame_mfa(module, function, args :: [term]) ::
          {:ok, :def | :defp | :defmacro | :defmacrop, [{args :: [term], guards :: [term]}]}
          | :error
  def blame_mfa(module, function, args)
      when is_atom(module) and is_atom(function) and is_list(args) do
    try do
      blame_mfa(module, function, length(args), args)
    rescue
      _ -> :error
    end
  end

  defp blame_mfa(module, function, arity, call_args) do
    with [_ | _] = path <- :code.which(module),
         {:ok, {_, [debug_info: debug_info]}} <- :beam_lib.chunks(path, [:debug_info]),
         {:debug_info_v1, backend, data} <- debug_info,
         {:ok, %{definitions: defs}} <- backend.debug_info(:elixir_v1, module, data, []),
         {_, kind, _, clauses} <- List.keyfind(defs, {function, arity}, 0) do
      clauses =
        for {meta, ex_args, guards, _block} <- clauses do
          scope = :elixir_erl.scope(meta)

          {erl_args, scope} =
            :elixir_erl_clauses.match(&:elixir_erl_pass.translate_args/2, ex_args, scope)

          {args, binding} =
            [call_args, ex_args, erl_args]
            |> Enum.zip()
            |> Enum.map_reduce([], &blame_arg/2)

          guards = Enum.map(guards, &blame_guard(&1, scope, binding))
          {args, guards}
        end

      {:ok, kind, clauses}
    else
      _ -> :error
    end
  end

  defp blame_arg({call_arg, ex_arg, erl_arg}, binding) do
    {match?, binding} = blame_arg(erl_arg, call_arg, binding)
    {blame_wrap(match?, rewrite_arg(ex_arg)), binding}
  end

  defp blame_arg(erl_arg, call_arg, binding) do
    binding = :orddict.store(:VAR, call_arg, binding)

    try do
      {:value, _, binding} = :erl_eval.expr({:match, 0, erl_arg, {:var, 0, :VAR}}, binding, :none)
      {true, binding}
    rescue
      _ -> {false, binding}
    end
  end

  defp rewrite_arg(arg) do
    Macro.prewalk(arg, fn
      {:%{}, meta, [__struct__: Range, first: first, last: last]} ->
        {:.., meta, [first, last]}

      other ->
        other
    end)
  end

  defp blame_guard({{:., _, [:erlang, op]}, meta, [left, right]}, scope, binding)
       when op == :andalso or op == :orelse do
    guards = [
      blame_guard(left, scope, binding),
      blame_guard(right, scope, binding)
    ]

    {rewrite_guard_call(op), meta, guards}
  end

  defp blame_guard(ex_guard, scope, binding) do
    {erl_guard, _} = :elixir_erl_pass.translate(ex_guard, scope)

    match? =
      try do
        {:value, true, _} = :erl_eval.expr(erl_guard, binding, :none)
        true
      rescue
        _ -> false
      end

    blame_wrap(match?, rewrite_guard(ex_guard))
  end

  defp rewrite_guard(guard) do
    Macro.prewalk(guard, fn
      {{:., _, [:erlang, :element]}, _, [{{:., _, [:erlang, :+]}, _, [int, 1]}, arg]} ->
        {:elem, [], [arg, int]}

      {{:., _, [:erlang, :element]}, _, [int, arg]} when is_integer(int) ->
        {:elem, [], [arg, int - 1]}

      {:., _, [:erlang, call]} ->
        rewrite_guard_call(call)

      other ->
        other
    end)
  end

  defp rewrite_guard_call(:orelse), do: :or
  defp rewrite_guard_call(:andalso), do: :and
  defp rewrite_guard_call(:"=<"), do: :<=
  defp rewrite_guard_call(:"/="), do: :!=
  defp rewrite_guard_call(:"=:="), do: :===
  defp rewrite_guard_call(:"=/="), do: :!==

  defp rewrite_guard_call(op) when op in [:band, :bor, :bnot, :bsl, :bsr, :bxor],
    do: {:., [], [Bitwise, op]}

  defp rewrite_guard_call(op) when op in [:xor, :element, :size], do: {:., [], [:erlang, op]}
  defp rewrite_guard_call(op), do: op

  defp blame_wrap(match?, ast), do: %{match?: match?, node: ast}

  @doc """
  Formats an exit. It returns a string.

  Often there are errors/exceptions inside exits. Exits are often
  wrapped by the caller and provide stacktraces too. This function
  formats exits in a way to nicely show the exit reason, caller
  and stacktrace.
  """
  @spec format_exit(any) :: String.t()
  def format_exit(reason) do
    format_exit(reason, <<"\n    ">>)
  end

  # 2-Tuple could be caused by an error if the second element is a stacktrace.
  defp format_exit({exception, maybe_stacktrace} = reason, joiner)
       when is_list(maybe_stacktrace) and maybe_stacktrace !== [] do
    try do
      Enum.map(maybe_stacktrace, &format_stacktrace_entry/1)
    catch
      :error, _ ->
        # Not a stacktrace, was an exit.
        format_exit_reason(reason)
    else
      formatted_stacktrace ->
        # Assume a non-empty list formattable as stacktrace is a
        # stacktrace, so exit was caused by an error.
        message =
          "an exception was raised:" <>
            joiner <> format_banner(:error, exception, maybe_stacktrace)

        Enum.join([message | formatted_stacktrace], joiner <> <<"    ">>)
    end
  end

  # :supervisor.start_link returns this error reason when it fails to init
  # because a child's start_link raises.
  defp format_exit({:shutdown, {:failed_to_start_child, child, {:EXIT, reason}}}, joiner) do
    format_start_child(child, reason, joiner)
  end

  # :supervisor.start_link returns this error reason when it fails to init
  # because a child's start_link returns {:error, reason}.
  defp format_exit({:shutdown, {:failed_to_start_child, child, reason}}, joiner) do
    format_start_child(child, reason, joiner)
  end

  # 2-Tuple could be an exit caused by mfa if second element is mfa, args
  # must be a list of arguments - max length 255 due to max arity.
  defp format_exit({reason2, {mod, fun, args}} = reason, joiner)
       when length(args) < 256 do
    try do
      format_mfa(mod, fun, args)
    catch
      :error, _ ->
        # Not an mfa, was an exit.
        format_exit_reason(reason)
    else
      mfa ->
        # Assume tuple formattable as an mfa is an mfa,
        # so exit was caused by failed mfa.
        "exited in: " <>
          mfa <> joiner <> "** (EXIT) " <> format_exit(reason2, joiner <> <<"    ">>)
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
    "bad child specification, invalid children: " <> inspect(start_spec)
  end

  defp format_exit_reason({:start_spec, start_spec}) do
    "bad child specification, " <> format_sup_spec(start_spec)
  end

  defp format_exit_reason({:supervisor_data, data}) do
    "bad supervisor configuration, " <> format_sup_data(data)
  end

  defp format_exit_reason(reason), do: inspect(reason)

  defp format_start_child(child, reason, joiner) do
    "shutdown: failed to start child: " <>
      inspect(child) <> joiner <> "** (EXIT) " <> format_exit(reason, joiner <> <<"    ">>)
  end

  defp format_sup_data({:invalid_type, type}) do
    "invalid type: " <> inspect(type)
  end

  defp format_sup_data({:invalid_strategy, strategy}) do
    "invalid strategy: " <> inspect(strategy)
  end

  defp format_sup_data({:invalid_intensity, intensity}) do
    "invalid max_restarts (intensity): " <> inspect(intensity)
  end

  defp format_sup_data({:invalid_period, period}) do
    "invalid max_seconds (period): " <> inspect(period)
  end

  defp format_sup_data({:invalid_max_children, max_children}) do
    "invalid max_children: " <> inspect(max_children)
  end

  defp format_sup_data({:invalid_extra_arguments, extra}) do
    "invalid extra_arguments: " <> inspect(extra)
  end

  defp format_sup_data(other), do: "got: #{inspect(other)}"

  defp format_sup_spec({:duplicate_child_name, id}) do
    """
    more than one child specification has the id: #{inspect(id)}.
    If using maps as child specifications, make sure the :id keys are unique.
    If using a module or {module, arg} as child, use Supervisor.child_spec/2 to change the :id, for example:

        children = [
          Supervisor.child_spec({MyWorker, arg}, id: :my_worker_1),
          Supervisor.child_spec({MyWorker, arg}, id: :my_worker_2)
        ]
    """
  end

  defp format_sup_spec({:invalid_child_spec, child_spec}) do
    "invalid child specification: #{inspect(child_spec)}"
  end

  defp format_sup_spec({:invalid_child_type, type}) do
    "invalid child type: #{inspect(type)}. Must be :worker or :supervisor."
  end

  defp format_sup_spec({:invalid_mfa, mfa}) do
    "invalid mfa: #{inspect(mfa)}"
  end

  defp format_sup_spec({:invalid_restart_type, restart}) do
    "invalid restart type: #{inspect(restart)}. Must be :permanent, :transient or :temporary."
  end

  defp format_sup_spec({:invalid_shutdown, shutdown}) do
    "invalid shutdown: #{inspect(shutdown)}. Must be an integer >= 0, :infinity or :brutal_kill."
  end

  defp format_sup_spec({:invalid_module, mod}) do
    "invalid module: #{inspect(mod)}. Must be an atom."
  end

  defp format_sup_spec({:invalid_modules, modules}) do
    "invalid modules: #{inspect(modules)}. Must be a list of atoms or :dynamic."
  end

  defp format_sup_spec(other), do: "got: #{inspect(other)}"

  @doc """
  Receives a stacktrace entry and formats it into a string.
  """
  @spec format_stacktrace_entry(stacktrace_entry) :: String.t()
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
    trace =
      if trace do
        trace
      else
        case Process.info(self(), :current_stacktrace) do
          {:current_stacktrace, t} -> Enum.drop(t, 3)
        end
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
    "#{inspect(fun)}#{format_arity(arity)}"
  end

  @doc """
  Receives a module, fun and arity and formats it
  as shown in stacktraces. The arity may also be a list
  of arguments.

  ## Examples

      iex> Exception.format_mfa(Foo, :bar, 1)
      "Foo.bar/1"

      iex> Exception.format_mfa(Foo, :bar, [])
      "Foo.bar()"

      iex> Exception.format_mfa(nil, :bar, [])
      "nil.bar()"

  Anonymous functions are reported as -func/arity-anonfn-count-,
  where func is the name of the enclosing function. Convert to
  "anonymous fn in func/arity"
  """
  def format_mfa(module, fun, arity) when is_atom(module) and is_atom(fun) do
    case Code.Identifier.extract_anonymous_fun_parent(fun) do
      {outer_name, outer_arity} ->
        "anonymous fn#{format_arity(arity)} in " <>
          "#{Code.Identifier.inspect_as_atom(module)}." <>
          "#{Code.Identifier.inspect_as_function(outer_name)}/#{outer_arity}"

      :error ->
        "#{Code.Identifier.inspect_as_atom(module)}." <>
          "#{Code.Identifier.inspect_as_function(fun)}#{format_arity(arity)}"
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
    format_file_line(Keyword.get(opts, :file), Keyword.get(opts, :line), " ")
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

  @impl true
  def blame(
        %{message: "argument error"} = exception,
        [{:erlang, :apply, [module, function, args], _} | _] = stacktrace
      ) do
    message =
      cond do
        # Note that args may be an empty list even if they were supplied
        not is_atom(module) and is_atom(function) and args == [] ->
          "you attempted to apply #{inspect(function)} on #{inspect(module)}. " <>
            "If you are using apply/3, make sure the module is an atom. " <>
            "If you are using the dot syntax, such as map.field or module.function, " <>
            "make sure the left side of the dot is an atom or a map"

        not is_atom(module) ->
          "you attempted to apply a function on #{inspect(module)}. " <>
            "Modules (the first argument of apply) must always be an atom"

        not is_atom(function) ->
          "you attempted to apply #{inspect(function)} on module #{inspect(module)}. " <>
            "Functions (the second argument of apply) must always be an atom"

        not is_list(args) ->
          "you attempted to apply #{inspect(function)} on module #{inspect(module)} " <>
            "with arguments #{inspect(args)}. Arguments (the third argument of apply) must always be a list"
      end

    {%{exception | message: message}, stacktrace}
  end

  def blame(exception, stacktrace) do
    {exception, stacktrace}
  end
end

defmodule ArithmeticError do
  defexception message: "bad argument in arithmetic expression"

  @unary_ops [:+, :-]
  @binary_ops [:+, :-, :*, :/]
  @binary_funs [:div, :rem]
  @bitwise_binary_funs [:band, :bor, :bxor, :bsl, :bsr]

  @impl true
  def blame(%{message: message} = exception, [{:erlang, fun, args, _} | _] = stacktrace) do
    message =
      message <>
        case {fun, args} do
          {op, [a]} when op in @unary_ops ->
            ": #{op}(#{inspect(a)})"

          {op, [a, b]} when op in @binary_ops ->
            ": #{inspect(a)} #{op} #{inspect(b)}"

          {fun, [a, b]} when fun in @binary_funs ->
            ": #{fun}(#{inspect(a)}, #{inspect(b)})"

          {fun, [a, b]} when fun in @bitwise_binary_funs ->
            ": Bitwise.#{fun}(#{inspect(a)}, #{inspect(b)})"

          {:bnot, [a]} ->
            ": Bitwise.bnot(#{inspect(a)})"

          _ ->
            ""
        end

    {%{exception | message: message}, stacktrace}
  end

  def blame(exception, stacktrace) do
    {exception, stacktrace}
  end
end

defmodule SystemLimitError do
  defexception []

  @impl true
  def message(_) do
    "a system limit has been reached"
  end
end

defmodule SyntaxError do
  defexception [:file, :line, description: "syntax error"]

  @impl true
  def message(exception) do
    Exception.format_file_line(Path.relative_to_cwd(exception.file), exception.line) <>
      " " <> exception.description
  end
end

defmodule TokenMissingError do
  defexception [:file, :line, description: "expression is incomplete"]

  @impl true
  def message(%{file: file, line: line, description: description}) do
    Exception.format_file_line(file && Path.relative_to_cwd(file), line) <> " " <> description
  end
end

defmodule CompileError do
  defexception [:file, :line, description: "compile error"]

  @impl true
  def message(%{file: file, line: line, description: description}) do
    Exception.format_file_line(file && Path.relative_to_cwd(file), line) <> " " <> description
  end
end

defmodule BadFunctionError do
  defexception [:term]

  @impl true
  def message(exception) do
    "expected a function, got: #{inspect(exception.term)}"
  end
end

defmodule BadStructError do
  defexception [:struct, :term]

  @impl true
  def message(exception) do
    "expected a struct named #{inspect(exception.struct)}, got: #{inspect(exception.term)}"
  end
end

defmodule BadMapError do
  defexception [:term]

  @impl true
  def message(exception) do
    "expected a map, got: #{inspect(exception.term)}"
  end
end

defmodule BadBooleanError do
  defexception [:term, :operator]

  @impl true
  def message(exception) do
    "expected a boolean on left-side of \"#{exception.operator}\", got: #{inspect(exception.term)}"
  end
end

defmodule MatchError do
  defexception [:term]

  @impl true
  def message(exception) do
    "no match of right hand side value: #{inspect(exception.term)}"
  end
end

defmodule CaseClauseError do
  defexception [:term]

  @impl true
  def message(exception) do
    "no case clause matching: #{inspect(exception.term)}"
  end
end

defmodule WithClauseError do
  defexception [:term]

  @impl true
  def message(exception) do
    "no with clause matching: #{inspect(exception.term)}"
  end
end

defmodule CondClauseError do
  defexception []

  @impl true
  def message(_exception) do
    "no cond clause evaluated to a true value"
  end
end

defmodule TryClauseError do
  defexception [:term]

  @impl true
  def message(exception) do
    "no try clause matching: #{inspect(exception.term)}"
  end
end

defmodule BadArityError do
  defexception [:function, :args]

  @impl true
  def message(exception) do
    fun = exception.function
    args = exception.args
    insp = Enum.map_join(args, ", ", &inspect/1)
    {:arity, arity} = Function.info(fun, :arity)
    "#{inspect(fun)} with arity #{arity} called with #{count(length(args), insp)}"
  end

  defp count(0, _insp), do: "no arguments"
  defp count(1, insp), do: "1 argument (#{insp})"
  defp count(x, insp), do: "#{x} arguments (#{insp})"
end

defmodule UndefinedFunctionError do
  defexception [:module, :function, :arity, :reason, :message]

  @impl true
  def message(%{message: nil} = exception) do
    %{reason: reason, module: module, function: function, arity: arity} = exception
    {message, _loaded?} = message(reason, module, function, arity)
    message
  end

  def message(%{message: message}) do
    message
  end

  defp message(nil, module, function, arity) do
    cond do
      is_nil(function) or is_nil(arity) ->
        {"undefined function", false}

      is_nil(module) ->
        formatted_fun = Exception.format_mfa(module, function, arity)
        {"function #{formatted_fun} is undefined", false}

      function_exported?(module, :module_info, 0) ->
        message(:"function not exported", module, function, arity)

      true ->
        message(:"module could not be loaded", module, function, arity)
    end
  end

  defp message(:"module could not be loaded", module, function, arity) do
    formatted_fun = Exception.format_mfa(module, function, arity)
    {"function #{formatted_fun} is undefined (module #{inspect(module)} is not available)", false}
  end

  defp message(:"function not exported", module, function, arity) do
    formatted_fun = Exception.format_mfa(module, function, arity)
    {"function #{formatted_fun} is undefined or private", true}
  end

  defp message(reason, module, function, arity) do
    formatted_fun = Exception.format_mfa(module, function, arity)
    {"function #{formatted_fun} is undefined (#{reason})", false}
  end

  @impl true
  def blame(exception, stacktrace) do
    %{reason: reason, module: module, function: function, arity: arity} = exception
    {message, loaded?} = message(reason, module, function, arity)
    message = message <> hint(module, function, arity, loaded?)
    {%{exception | message: message}, stacktrace}
  end

  defp hint(nil, _function, 0, _loaded?) do
    ". If you are using the dot syntax, such as map.field or module.function, " <>
      "make sure the left side of the dot is an atom or a map"
  end

  defp hint(module, function, arity, true) do
    hint_for_loaded_module(module, function, arity, nil)
  end

  defp hint(_module, _function, _arity, _loaded?) do
    ""
  end

  @doc false
  def hint_for_loaded_module(module, function, arity, exports) do
    if macro_exported?(module, function, arity) do
      ". However there is a macro with the same name and arity. " <>
        "Be sure to require #{inspect(module)} if you intend to invoke this macro"
    else
      IO.iodata_to_binary(did_you_mean(module, function, exports))
    end
  end

  @function_threshold 0.77
  @max_suggestions 5

  defp did_you_mean(module, function, exports) do
    exports = exports || exports_for(module)

    result =
      case Keyword.take(exports, [function]) do
        [] ->
          candidates = exports -- deprecated_functions_for(module)
          base = Atom.to_string(function)

          for {key, val} <- candidates,
              dist = String.jaro_distance(base, Atom.to_string(key)),
              dist >= @function_threshold,
              do: {dist, key, val}

        arities ->
          for {key, val} <- arities, do: {1.0, key, val}
      end
      |> Enum.sort(&(elem(&1, 0) >= elem(&2, 0)))
      |> Enum.take(@max_suggestions)
      |> Enum.sort(&(elem(&1, 1) <= elem(&2, 1)))

    case result do
      [] -> []
      suggestions -> [". Did you mean one of:\n\n" | Enum.map(suggestions, &format_fa/1)]
    end
  end

  defp format_fa({_dist, fun, arity}) do
    ["      * ", Code.Identifier.inspect_as_function(fun), ?/, Integer.to_string(arity), ?\n]
  end

  defp exports_for(module) do
    if function_exported?(module, :__info__, 1) do
      module.__info__(:macros) ++ module.__info__(:functions)
    else
      module.module_info(:exports)
    end
  rescue
    # In case the module was removed while we are computing this
    UndefinedFunctionError ->
      []
  end

  defp deprecated_functions_for(module) do
    if function_exported?(module, :__info__, 1) do
      for {name_arity, _message} <- module.__info__(:deprecated), do: name_arity
    else
      []
    end
  rescue
    # In case the module was removed while we are computing this
    UndefinedFunctionError ->
      []
  end
end

defmodule FunctionClauseError do
  defexception [:module, :function, :arity, :kind, :args, :clauses]

  @impl true
  def message(exception) do
    case exception do
      %{function: nil} ->
        "no function clause matches"

      %{module: module, function: function, arity: arity} ->
        formatted = Exception.format_mfa(module, function, arity)
        blamed = blame(exception, &inspect/1, &blame_match/2)
        "no function clause matching in #{formatted}" <> blamed
    end
  end

  @impl true
  def blame(%{module: module, function: function, arity: arity} = exception, stacktrace) do
    case stacktrace do
      [{^module, ^function, args, meta} | rest] when length(args) == arity ->
        exception =
          case Exception.blame_mfa(module, function, args) do
            {:ok, kind, clauses} -> %{exception | args: args, kind: kind, clauses: clauses}
            :error -> %{exception | args: args}
          end

        {exception, [{module, function, arity, meta} | rest]}

      stacktrace ->
        {exception, stacktrace}
    end
  end

  defp blame_match(%{match?: true, node: node}, _), do: Macro.to_string(node)
  defp blame_match(%{match?: false, node: node}, _), do: "-" <> Macro.to_string(node) <> "-"
  defp blame_match(_, string), do: string

  @doc false
  def blame(%{args: nil}, _, _) do
    ""
  end

  def blame(exception, inspect_fun, ast_fun) do
    %{module: module, function: function, arity: arity, kind: kind, args: args, clauses: clauses} =
      exception

    mfa = Exception.format_mfa(module, function, arity)

    formatted_args =
      args
      |> Enum.with_index(1)
      |> Enum.map(fn {arg, i} ->
        ["\n    # ", Integer.to_string(i), "\n    ", pad(inspect_fun.(arg)), "\n"]
      end)

    formatted_clauses =
      if clauses do
        format_clause_fun = fn {args, guards} ->
          code = Enum.reduce(guards, {function, [], args}, &{:when, [], [&2, &1]})
          "    #{kind} " <> Macro.to_string(code, ast_fun) <> "\n"
        end

        top_10 =
          clauses
          |> Enum.take(10)
          |> Enum.map(format_clause_fun)

        [
          "\nAttempted function clauses (showing #{length(top_10)} out of #{length(clauses)}):",
          "\n\n",
          top_10
        ]
      else
        ""
      end

    "\n\nThe following arguments were given to #{mfa}:\n#{formatted_args}#{formatted_clauses}"
  end

  defp pad(string) do
    String.replace(string, "\n", "\n    ")
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

  @impl true
  def message(%{protocol: protocol, value: value, description: description}) do
    "protocol #{inspect(protocol)} not implemented for #{inspect(value)}" <>
      maybe_description(description) <> maybe_available(protocol)
  end

  defp maybe_description(""), do: ""
  defp maybe_description(description), do: ", " <> description

  defp maybe_available(protocol) do
    case protocol.__protocol__(:impls) do
      {:consolidated, []} ->
        ". There are no implementations for this protocol."

      {:consolidated, types} ->
        ". This protocol is implemented for: #{Enum.map_join(types, ", ", &inspect/1)}"

      :not_consolidated ->
        ""
    end
  end
end

defmodule KeyError do
  defexception [:key, :term, :message]

  @impl true
  def message(exception = %{message: nil}), do: message(exception.key, exception.term)
  def message(%{message: message}), do: message

  def message(key, term) do
    message = "key #{inspect(key)} not found"

    if term != nil do
      message <> " in: #{inspect(term)}"
    else
      message
    end
  end

  @impl true
  def blame(exception = %{term: nil}, stacktrace) do
    message = message(exception.key, exception.term)
    {%{exception | message: message}, stacktrace}
  end

  def blame(exception, stacktrace) do
    %{term: term, key: key} = exception
    message = message(key, term)

    if is_atom(key) and (map_with_atom_keys_only?(term) or Keyword.keyword?(term)) do
      hint = did_you_mean(key, available_keys(term))
      message = message <> IO.iodata_to_binary(hint)
      {%{exception | message: message}, stacktrace}
    else
      {%{exception | message: message}, stacktrace}
    end
  end

  defp map_with_atom_keys_only?(term) do
    is_map(term) and Enum.all?(Map.to_list(term), fn {k, _} -> is_atom(k) end)
  end

  defp available_keys(term) when is_map(term), do: Map.keys(term)
  defp available_keys(term) when is_list(term), do: Keyword.keys(term)

  @threshold 0.77
  @max_suggestions 5
  defp did_you_mean(missing_key, available_keys) do
    stringified_key = Atom.to_string(missing_key)

    suggestions =
      for key <- available_keys,
          distance = String.jaro_distance(stringified_key, Atom.to_string(key)),
          distance >= @threshold,
          do: {distance, key}

    case suggestions do
      [] -> []
      suggestions -> [". Did you mean one of:\n\n" | format_suggestions(suggestions)]
    end
  end

  defp format_suggestions(suggestions) do
    suggestions
    |> Enum.sort(&(elem(&1, 0) >= elem(&2, 0)))
    |> Enum.take(@max_suggestions)
    |> Enum.sort(&(elem(&1, 1) <= elem(&2, 1)))
    |> Enum.map(fn {_, key} -> ["      * ", inspect(key), ?\n] end)
  end
end

defmodule UnicodeConversionError do
  defexception [:encoded, :message]

  def exception(opts) do
    %UnicodeConversionError{
      encoded: Keyword.fetch!(opts, :encoded),
      message: "#{Keyword.fetch!(opts, :kind)} #{detail(Keyword.fetch!(opts, :rest))}"
    }
  end

  defp detail(rest) when is_binary(rest) do
    "encoding starting at #{inspect(rest)}"
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

  @impl true
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

  @impl true
  def message(exception) do
    formatted = IO.iodata_to_binary(:file.format_error(exception.reason))

    location =
      case exception.on() do
        "" -> ""
        on -> ". #{on}"
      end

    "could not #{exception.action} from #{inspect(exception.source)} to " <>
      "#{inspect(exception.destination)}#{location}: #{formatted}"
  end
end

defmodule File.LinkError do
  defexception [:reason, :existing, :new, action: ""]

  @impl true
  def message(exception) do
    formatted = IO.iodata_to_binary(:file.format_error(exception.reason))

    "could not #{exception.action} from #{inspect(exception.existing)} to " <>
      "#{inspect(exception.new)}: #{formatted}"
  end
end

defmodule ErlangError do
  defexception [:original]

  @impl true
  def message(exception) do
    "Erlang error: #{inspect(exception.original)}"
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
      case stacktrace do
        [{Map, :get_and_update!, [map, _, _], _} | _] -> map
        [{Map, :update!, [map, _, _], _} | _] -> map
        [{:maps, :update, [_, _, map], _} | _] -> map
        [{:maps, :get, [_, map], _} | _] -> map
        [{:erlang, :map_get, [_, map], _} | _] -> map
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
    {mod, fun, arity} = from_stacktrace(stacktrace)
    %UndefinedFunctionError{module: mod, function: fun, arity: arity}
  end

  def normalize(:function_clause, stacktrace) do
    {mod, fun, arity} = from_stacktrace(stacktrace)
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
