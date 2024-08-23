defmodule Exception do
  @moduledoc """
  Functions for dealing with throw/catch/exit and exceptions.

  This module also defines the behaviour required by custom
  exceptions. To define your own, see `defexception/1`.

  ## Formatting functions

  Several functions in this module help format exceptions.
  Some of these functions expect the stacktrace as argument.
  The stacktrace is typically available inside catch and
  rescue by using the `__STACKTRACE__/0` variable.

  Do not rely on the particular format returned by the
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
  @type non_error_kind :: :exit | :throw | {:EXIT, pid}

  @type stacktrace :: [stacktrace_entry]
  @type stacktrace_entry ::
          {module, atom, arity_or_args, location}
          | {(... -> any), arity_or_args, location}

  @type arity_or_args :: non_neg_integer | list
  @type location :: keyword

  @doc """
  Receives the arguments given to `raise/2` and returns the exception struct.

  The default implementation accepts either a set of keyword arguments
  that is merged into the struct or a string to be used as the exception's message.
  """
  @callback exception(term) :: t

  @doc """
  Receives the exception struct and must return its message.

  Many exceptions have a message field which by default is accessed
  by this function. However, if an exception does not have a message field,
  this function must be explicitly implemented.
  """
  @callback message(t) :: String.t()

  @doc """
  Called from `Exception.blame/3` to augment the exception struct.

  Can be used to collect additional information about the exception
  or do some additional expensive computation.
  """
  @callback blame(t, stacktrace) :: {t, stacktrace}
  @optional_callbacks [blame: 2]

  @doc false
  # Callback for formatting Erlang exceptions
  def format_error(%struct{} = exception, _stacktrace) do
    %{general: message(exception), reason: "#" <> Atom.to_string(struct)}
  end

  @doc false
  @deprecated "Use Kernel.is_exception/1 instead"
  def exception?(term)
  def exception?(%_{__exception__: true}), do: true
  def exception?(_), do: false

  @doc """
  Gets the message for an `exception`.
  """
  @spec message(t) :: String.t()
  def message(%module{__exception__: true} = exception) do
    try do
      module.message(exception)
    rescue
      caught_exception ->
        "got #{inspect(caught_exception.__struct__)} with message " <>
          "#{inspect(message(caught_exception))} while retrieving Exception.message/1 " <>
          "for #{inspect(exception)}. Stacktrace:\n#{format_stacktrace(__STACKTRACE__)}"
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
  @spec blame_mfa(module, function :: atom, args :: [term]) ::
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
          scope = :elixir_erl.scope(meta, true)
          ann = :elixir_erl.get_ann(meta)

          {erl_args, scope} =
            :elixir_erl_clauses.match(ann, &:elixir_erl_pass.translate_args/3, ex_args, scope)

          {args, binding} =
            [call_args, ex_args, erl_args]
            |> Enum.zip()
            |> Enum.map_reduce([], &blame_arg/2)

          guards =
            guards
            |> Enum.map(&blame_guard(&1, ann, scope, binding))
            |> Enum.map(&Macro.prewalk(&1, fn guard -> translate_guard(guard) end))

          {args, guards}
        end

      {:ok, kind, clauses}
    else
      _ -> :error
    end
  end

  defp is_map_node?({:is_map, _, [_]}), do: true
  defp is_map_node?(_), do: false
  defp is_map_key_node?({:is_map_key, _, [_, _]}), do: true
  defp is_map_key_node?(_), do: false

  defp struct_validation_node?(
         {:is_atom, _, [{{:., [], [:erlang, :map_get]}, _, [:__struct__, _]}]}
       ),
       do: true

  defp struct_validation_node?(
         {:==, _, [{{:., [], [:erlang, :map_get]}, _, [:__struct__, _]}, _module]}
       ),
       do: true

  defp struct_validation_node?(_), do: false

  defp is_struct_macro?(
         {:and, _,
          [
            {:and, _, [%{node: node_1 = {_, _, [arg]}}, %{node: node_2 = {_, _, [arg, _]}}]},
            %{node: node_3 = {_, _, [{_, _, [_, arg]}]}}
          ]}
       ),
       do: is_map_node?(node_1) and is_map_key_node?(node_2) and struct_validation_node?(node_3)

  defp is_struct_macro?(
         {:and, _,
          [
            {:and, _,
             [
               {:and, _,
                [
                  %{node: node_1 = {_, _, [arg]}},
                  {:or, _, [%{node: {:is_atom, _, [_]}}, %{node: :fail}]}
                ]},
               %{node: node_2 = {_, _, [arg, _]}}
             ]},
            %{node: node_3 = {_, _, [{_, _, [_, arg]}, _]}}
          ]}
       ),
       do: is_map_node?(node_1) and is_map_key_node?(node_2) and struct_validation_node?(node_3)

  defp is_struct_macro?(_), do: false

  defp translate_guard(guard) do
    if is_struct_macro?(guard) do
      undo_is_struct_guard(guard)
    else
      guard
    end
  end

  defp undo_is_struct_guard({:and, meta, [_, %{node: {_, _, [{_, _, [_, arg]} | optional]}}]}) do
    args =
      case optional do
        [] -> [arg]
        [module] -> [arg, module]
      end

    %{match?: meta[:value], node: {:is_struct, meta, args}}
  end

  defp blame_arg({call_arg, ex_arg, erl_arg}, binding) do
    {match?, binding} = blame_arg(erl_arg, call_arg, binding)
    {blame_wrap(match?, rewrite_arg(ex_arg)), binding}
  end

  defp blame_arg(erl_arg, call_arg, binding) do
    binding = :orddict.store(:VAR, call_arg, binding)

    try do
      ann = :erl_anno.new(0)

      {:value, _, binding} =
        :erl_eval.expr({:match, ann, erl_arg, {:var, ann, :VAR}}, binding, :none)

      {true, binding}
    rescue
      _ -> {false, binding}
    end
  end

  defp rewrite_arg(arg) do
    Macro.prewalk(arg, fn
      {:%{}, meta, [__struct__: Range, first: first, last: last, step: step]} ->
        {:..//, meta, [first, last, step]}

      other ->
        other
    end)
  end

  defp blame_guard({{:., _, [:erlang, op]}, meta, [left, right]}, ann, scope, binding)
       when op == :andalso or op == :orelse do
    guards = [
      blame_guard(left, ann, scope, binding),
      blame_guard(right, ann, scope, binding)
    ]

    kernel_op =
      case op do
        :orelse -> :or
        :andalso -> :and
      end

    evaluate_guard(kernel_op, meta, guards)
  end

  defp blame_guard(ex_guard, ann, scope, binding) do
    ex_guard
    |> blame_guard?(binding, ann, scope)
    |> blame_wrap(rewrite_guard(ex_guard))
  end

  defp blame_guard?(ex_guard, binding, ann, scope) do
    {erl_guard, _} = :elixir_erl_pass.translate(ex_guard, ann, scope)
    {:value, true, _} = :erl_eval.expr(erl_guard, binding, :none)
    true
  rescue
    _ -> false
  end

  defp evaluate_guard(kernel_op, meta, guards = [_, _]) do
    [x, y] = Enum.map(guards, &evaluate_guard/1)

    logic_value =
      case kernel_op do
        :or -> x or y
        :and -> x and y
      end

    {kernel_op, Keyword.put(meta, :value, logic_value), guards}
  end

  defp evaluate_guard(%{match?: value}), do: value
  defp evaluate_guard({_, meta, _}) when is_list(meta), do: meta[:value]

  defp rewrite_guard(guard) do
    Macro.prewalk(guard, fn
      {{:., _, [mod, fun]}, meta, args} -> erl_to_ex(mod, fun, args, meta)
      other -> other
    end)
  end

  defp erl_to_ex(mod, fun, args, meta) do
    case :elixir_rewrite.erl_to_ex(mod, fun, args) do
      {Kernel, fun, args} -> {fun, meta, args}
      {mod, fun, args} -> {{:., [], [mod, fun]}, meta, args}
    end
  end

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
      {:ok, app} ->
        case :application.get_key(app, :vsn) do
          {:ok, vsn} when is_list(vsn) ->
            "(" <> Atom.to_string(app) <> " " <> List.to_string(vsn) <> ") "

          _ ->
            "(" <> Atom.to_string(app) <> ") "
        end

      :undefined ->
        ""
    end
  end

  @doc """
  Formats the stacktrace.

  A stacktrace must be given as an argument. If not, the stacktrace
  is retrieved from `Process.info/2`.
  """
  @spec format_stacktrace(stacktrace | nil) :: String.t()
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
  @spec format_fa(fun, arity) :: String.t()
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
  @spec format_mfa(module, atom, arity_or_args) :: String.t()
  def format_mfa(module, fun, arity) when is_atom(module) and is_atom(fun) do
    case Code.Identifier.extract_anonymous_fun_parent(fun) do
      {outer_name, outer_arity} ->
        "anonymous fn#{format_arity(arity)} in " <>
          "#{Macro.inspect_atom(:literal, module)}." <>
          "#{Macro.inspect_atom(:remote_call, outer_name)}/#{outer_arity}"

      :error ->
        "#{Macro.inspect_atom(:literal, module)}." <>
          "#{Macro.inspect_atom(:remote_call, fun)}#{format_arity(arity)}"
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
  @spec format_file_line(String.t() | nil, non_neg_integer | nil, String.t()) :: String.t()
  def format_file_line(file, line, suffix \\ "") do
    cond do
      is_nil(file) -> ""
      is_nil(line) or line == 0 -> "#{file}:#{suffix}"
      true -> "#{file}:#{line}:#{suffix}"
    end
  end

  @doc """
  Formats the given `file`, `line`, and `column` as shown in stacktraces.

  If any of the values are `nil`, they are omitted.

  ## Examples

      iex> Exception.format_file_line_column("foo", 1, 2)
      "foo:1:2:"

      iex> Exception.format_file_line_column("foo", 1, nil)
      "foo:1:"

      iex> Exception.format_file_line_column("foo", nil, nil)
      "foo:"

      iex> Exception.format_file_line_column("foo", nil, 2)
      "foo:"

      iex> Exception.format_file_line_column(nil, nil, nil)
      ""

  """
  @spec format_file_line_column(
          String.t() | nil,
          non_neg_integer | nil,
          non_neg_integer | nil,
          String.t()
        ) :: String.t()
  def format_file_line_column(file, line, column, suffix \\ "") do
    cond do
      is_nil(file) -> ""
      is_nil(line) or line == 0 -> "#{file}:#{suffix}"
      is_nil(column) or column == 0 -> "#{file}:#{line}:#{suffix}"
      true -> "#{file}:#{line}:#{column}:#{suffix}"
    end
  end

  defp format_location(opts) when is_list(opts) do
    case opts[:column] do
      nil -> format_file_line(Keyword.get(opts, :file), Keyword.get(opts, :line), " ")
      col -> format_file_line_column(Keyword.get(opts, :file), Keyword.get(opts, :line), col, " ")
    end
  end

  @doc false
  def format_delimiter(delimiter) do
    if delimiter |> Atom.to_string() |> String.contains?(["\"", "'"]),
      do: delimiter,
      else: ~s("#{delimiter}")
  end

  @doc false
  def format_snippet(
        {start_line, _start_column} = start_pos,
        {end_line, end_column} = end_pos,
        description,
        file,
        lines,
        start_message,
        end_message
      )
      when start_line < end_line do
    max_digits = digits(end_line)
    general_padding = max(2, max_digits) + 1
    padding = n_spaces(general_padding)

    relevant_lines =
      if end_line - start_line < 5 do
        line_range(lines, start_pos, end_pos, padding, max_digits, start_message, end_message)
      else
        trimmed_inbetween_lines(
          lines,
          start_pos,
          end_pos,
          padding,
          max_digits,
          start_message,
          end_message
        )
      end

    """
     #{padding}#{red("error:")} #{pad_message(description, padding)}
     #{padding}│
    #{relevant_lines}
     #{padding}│
     #{padding}└─ #{Path.relative_to_cwd(file)}:#{end_line}:#{end_column}\
    """
  end

  def format_snippet(
        {start_line, start_column},
        {end_line, end_column},
        description,
        file,
        lines,
        start_message,
        end_message
      )
      when start_line == end_line do
    max_digits = digits(end_line)
    general_padding = max(2, max_digits) + 1
    padding = n_spaces(general_padding)
    formatted_line = [line_padding(end_line, max_digits), to_string(end_line), " │ ", hd(lines)]

    mismatched_closing_line =
      [
        n_spaces(start_column - 1),
        red("│"),
        format_end_message(end_column - start_column, end_message)
      ]

    unclosed_delimiter_line =
      [padding, " │ ", format_start_message(start_column, start_message)]

    below_line = [padding, " │ ", mismatched_closing_line, "\n", unclosed_delimiter_line]

    """
     #{padding}#{red("error:")} #{pad_message(description, padding)}
     #{padding}│
    #{formatted_line}
    #{below_line}
     #{padding}│
     #{padding}└─ #{Path.relative_to_cwd(file)}:#{end_line}:#{end_column}\
    """
  end

  defp line_padding(line_number, max_digits) do
    line_digits = digits(line_number)

    spacing =
      if line_digits == 1 do
        max(2, max_digits)
      else
        max_digits - line_digits + 1
      end

    n_spaces(spacing)
  end

  defp n_spaces(n), do: String.duplicate(" ", n)

  defp digits(number, acc \\ 1)
  defp digits(number, acc) when number < 10, do: acc
  defp digits(number, acc), do: digits(div(number, 10), acc + 1)

  defp trimmed_inbetween_lines(
         lines,
         {start_line, start_column},
         {end_line, end_column},
         padding,
         max_digits,
         start_message,
         end_message
       ) do
    start_padding = line_padding(start_line, max_digits)
    end_padding = line_padding(end_line, max_digits)
    first_line = hd(lines)
    last_line = List.last(lines)

    """
    #{start_padding}#{start_line} │ #{first_line}
     #{padding}│ #{format_start_message(start_column, start_message)}
     ...
    #{end_padding}#{end_line} │ #{last_line}
     #{padding}│ #{format_end_message(end_column, end_message)}\
    """
  end

  defp line_range(
         lines,
         {start_line, start_column},
         {end_line, end_column},
         padding,
         max_digits,
         start_message,
         end_message
       ) do
    Enum.zip_with(lines, start_line..end_line, fn line, line_number ->
      line_padding = line_padding(line_number, max_digits)

      cond do
        line_number == start_line ->
          [
            line_padding,
            to_string(line_number),
            " │ ",
            line,
            "\n",
            padding,
            " │ ",
            format_start_message(start_column, start_message)
          ]

        line_number == end_line ->
          [
            line_padding,
            to_string(line_number),
            " │ ",
            line,
            "\n",
            padding,
            " │ ",
            format_end_message(end_column, end_message)
          ]

        true ->
          [line_padding, to_string(line_number), " │ ", line]
      end
    end)
    |> Enum.intersperse("\n")
  end

  defp format_end_message(end_column, message),
    do: [
      n_spaces(end_column - 1),
      red(message)
    ]

  defp format_start_message(start_column, message),
    do: [n_spaces(start_column - 1), red(message)]

  defp pad_message(message, padding), do: String.replace(message, "\n", "\n #{padding}")

  defp red(string) do
    if IO.ANSI.enabled?() do
      [IO.ANSI.red(), string, IO.ANSI.reset()]
    else
      string
    end
  end
end

# Some exceptions implement "message/1" instead of "exception/1" mostly
# for bootstrap reasons. It is recommended for applications to implement
# "exception/1" instead of "message/1" as described in "defexception/1"
# docs.

defmodule RuntimeError do
  @moduledoc """
  An exception for a generic runtime error.

  This is the exception that `raise/1` raises when you pass it only a string as
  a message:

      iex> raise "oops!"
      ** (RuntimeError) oops!

  You should use this exceptions sparingly, since most of the time it might be
  better to define your own exceptions specific to your application or library.
  Sometimes, however, there are situations in which you don't expect a condition to
  happen, but you want to give a meaningful error message if it does. In those cases,
  `RuntimeError` can be a good choice.

  ## Fields

  `RuntimeError` exceptions have a single field, `:message` (a `t:String.t/0`),
  which is public and can be accessed freely when reading or creating `RuntimeError`
  exceptions.
  """

  defexception message: "runtime error"
end

defmodule ArgumentError do
  @moduledoc """
  An exception raised when an argument to a function is invalid.

  You can raise this exception when you want to signal that an argument to
  a function is invalid.

  `ArgumentError` exceptions have a single field, `:message` (a `t:String.t/0`),
  which is public and can be accessed freely when reading or creating `ArgumentError`
  exceptions.
  """

  defexception message: "argument error"
end

defmodule ArithmeticError do
  @moduledoc """
  An exception raised on invalid arithmetic operations.

  For example, this exception is raised if you divide by `0`:

      iex> 1 / 0
      ** (ArithmeticError) bad argument in arithmetic expression: 1 / 0

  """

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
  @moduledoc """
  An exception raised when a system limit has been reached.

  For example, this can happen if you try to create an atom that is too large.
  """

  defexception message: "a system limit has been reached"
end

defmodule MismatchedDelimiterError do
  @moduledoc """
  An exception raised when a mismatched delimiter is found when parsing code.

  For example:

    * `[1, 2, 3}`
    * `fn a -> )`

  The following fields of this exceptions are public and can be accessed freely:

    * `:file` (`t:Path.t/0` or `nil`) - the file where the error occurred, or `nil` if
      the error occurred in code that did not come from a file
    * `:line` - the line for the opening delimiter
    * `:column` - the column for the opening delimiter
    * `:end_line` - the line for the mismatched closing delimiter
    * `:end_column` - the column for the mismatched closing delimiter
    * `:opening_delimiter` - an atom representing the opening delimiter
    * `:closing_delimiter` - an atom representing the mismatched closing delimiter
    * `:expected_delimiter` - an atom representing the closing delimiter
    * `:description` - a description of the mismatched delimiter error

  """

  defexception [
    :file,
    :line,
    :column,
    :end_line,
    :end_column,
    :opening_delimiter,
    :closing_delimiter,
    :expected_delimiter,
    :snippet,
    description: "mismatched delimiter error"
  ]

  @impl true
  def message(%{
        line: start_line,
        column: start_column,
        end_line: end_line,
        end_column: end_column,
        description: description,
        expected_delimiter: expected_delimiter,
        file: file,
        snippet: snippet
      }) do
    start_pos = {start_line, start_column}
    end_pos = {end_line, end_column}
    lines = String.split(snippet, "\n")
    expected_delimiter = Exception.format_delimiter(expected_delimiter)

    start_message = "└ unclosed delimiter"
    end_message = ~s/└ mismatched closing delimiter (expected #{expected_delimiter})/

    snippet =
      Exception.format_snippet(
        start_pos,
        end_pos,
        description,
        file,
        lines,
        start_message,
        end_message
      )

    format_message(file, end_line, end_column, snippet)
  end

  defp format_message(file, line, column, message) do
    location = Exception.format_file_line_column(Path.relative_to_cwd(file), line, column)
    "mismatched delimiter found on " <> location <> "\n" <> message
  end
end

defmodule SyntaxError do
  @moduledoc """
  An exception raised when there's a syntax error when parsing code.

  The following fields of this exceptions are public and can be accessed freely:

    * `:file` (`t:Path.t/0` or `nil`) - the file where the error occurred, or `nil` if
      the error occurred in code that did not come from a file
    * `:line` - the line where the error occurred
    * `:column` - the column where the error occurred
    * `:description` - a description of the syntax error

  """

  defexception [:file, :line, :column, :snippet, description: "syntax error"]

  @impl true
  def message(%{
        file: file,
        line: line,
        column: column,
        description: description,
        snippet: snippet
      })
      when not is_nil(snippet) and not is_nil(column) do
    snippet =
      :elixir_errors.format_snippet(:error, {line, column}, file, description, snippet, %{})

    format_message(file, line, column, snippet)
  end

  @impl true
  def message(%{
        file: file,
        line: line,
        column: column,
        description: description
      }) do
    snippet =
      :elixir_errors.format_snippet(:error, {line, column}, file, description, nil, %{})

    padded = "   " <> String.replace(snippet, "\n", "\n   ")
    format_message(file, line, column, padded)
  end

  defp format_message(file, line, column, message) do
    location = Exception.format_file_line_column(Path.relative_to_cwd(file), line, column)
    "invalid syntax found on " <> location <> "\n" <> message
  end
end

defmodule TokenMissingError do
  @moduledoc """
  An exception raised when a token is missing when parsing code.

  The following fields of this exceptions are public and can be accessed freely:

    * `:file` (`t:Path.t/0` or `nil`) - the file where the error occurred, or `nil` if
      the error occurred in code that did not come from a file
    * `:line` - the line for the opening delimiter
    * `:column` - the column for the opening delimiter
    * `:end_line` - the line for the end of the string
    * `:end_column` - the column for the end of the string
    * `:opening_delimiter` - an atom representing the opening delimiter
    * `:expected_delimiter` - an atom representing the expected delimiter
    * `:description` - a description of the missing token error
  """

  defexception [
    :file,
    :line,
    :column,
    :end_line,
    :end_column,
    :snippet,
    :opening_delimiter,
    :expected_delimiter,
    description: "expression is incomplete"
  ]

  @impl true
  def message(%{
        file: file,
        line: line,
        column: column,
        end_line: end_line,
        description: description,
        expected_delimiter: expected_delimiter,
        snippet: snippet
      })
      when not is_nil(snippet) and not is_nil(column) and not is_nil(end_line) do
    {trimmed, [last_line | _] = reversed_lines} =
      snippet
      |> String.split("\n")
      |> Enum.reverse()
      |> Enum.split_while(&(&1 == ""))

    end_line = end_line - length(trimmed)
    end_column = String.length(last_line) + 1

    start_pos = {line, column}
    end_pos = {end_line, end_column}
    expected_delimiter = Exception.format_delimiter(expected_delimiter)

    start_message = ~s/└ unclosed delimiter/
    end_message = ~s/└ missing closing delimiter (expected #{expected_delimiter})/

    snippet =
      Exception.format_snippet(
        start_pos,
        end_pos,
        description,
        file,
        Enum.reverse(reversed_lines),
        start_message,
        end_message
      )

    format_message(file, end_line, end_column, snippet)
  end

  @impl true
  def message(%{
        file: file,
        line: line,
        column: column,
        snippet: snippet,
        description: description
      }) do
    snippet =
      :elixir_errors.format_snippet(:error, {line, column}, file, description, snippet, %{})

    format_message(file, line, column, snippet)
  end

  defp format_message(file, line, column, message) do
    location = Exception.format_file_line_column(Path.relative_to_cwd(file), line, column)
    "token missing on " <> location <> "\n" <> message
  end
end

defmodule CompileError do
  @moduledoc """
  An exception raised when there's an error when compiling code.

  The following fields of this exceptions are public and can be accessed freely:

    * `:file` (`t:Path.t/0` or `nil`) - the file where the error occurred, or `nil` if
      the error occurred in code that did not come from a file
    * `:line` (`t:non_neg_integer/0`) - the line where the error occurred

  """

  defexception [:file, :line, description: "compile error"]

  @impl true
  def message(%{file: file, line: line, description: description}) do
    case Exception.format_file_line(file && Path.relative_to_cwd(file), line) do
      "" -> description
      formatted -> formatted <> " " <> description
    end
  end
end

defmodule Kernel.TypespecError do
  @moduledoc """
  An exception raised when there's an error in a typespec.

  The following fields of this exceptions are public and can be accessed freely:

    * `:file` (`t:Path.t/0` or `nil`) - the file where the error occurred, or `nil` if
      the error occurred in code that did not come from a file
    * `:line` (`t:non_neg_integer/0`) - the line where the error occurred

  """

  defexception [:file, :line, :description]

  @impl true
  def message(%{file: file, line: line, description: description}) do
    case Exception.format_file_line(file && Path.relative_to_cwd(file), line) do
      "" -> description
      formatted -> formatted <> " " <> description
    end
  end
end

defmodule BadFunctionError do
  defexception [:term]

  @impl true
  def message(%{term: term}) when is_function(term) do
    "function #{inspect(term)} is invalid, likely because it points to an old version of the code"
  end

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
  @moduledoc """
  An exception raised when something expected a map, but received something else.
  """

  defexception [:term]

  @impl true
  def message(exception) do
    "expected a map, got: #{inspect(exception.term)}"
  end
end

defmodule BadBooleanError do
  @moduledoc """
  An exception raised when an operator expected a boolean, but received something else.
  """

  defexception [:term, :operator]

  @impl true
  def message(exception) do
    "expected a boolean on left-side of \"#{exception.operator}\", got: #{inspect(exception.term)}"
  end
end

defmodule MatchError do
  @moduledoc """
  An exception raised when a pattern match (`=/2`) fails.

  The following fields of this exception are public and can be accessed freely:

    * `:term` (`t:term/0`) - the term that did not match the pattern

  For example, this exception gets raised for code like this:

      [_ | _] = []

  """

  defexception [:term]

  @impl true
  def message(exception) do
    "no match of right hand side value: #{inspect(exception.term)}"
  end
end

defmodule CaseClauseError do
  @moduledoc """
  An exception raised when a term in a `case/2` expression
  does not match any of the defined `->` clauses.

  The following fields of this exception are public and can be accessed freely:

    * `:term` (`t:term/0`) - the term that did not match any of the clauses

  For example, this exception gets raised for a `case/2` like the following:

      case System.unique_integer() do
        bin when is_binary(bin) -> :oops
        :ok -> :neither_this_one
      end

  """

  defexception [:term]

  @impl true
  def message(exception) do
    "no case clause matching: #{inspect(exception.term)}"
  end
end

defmodule WithClauseError do
  @moduledoc """
  An exception raised when a term in a `with/1` expression
  does not match any of the defined `->` clauses in its `else`.

  The following fields of this exception are public and can be accessed freely:

    * `:term` (`t:term/0`) - the term that did not match any of the clauses

  For example, this exception gets raised for a `with/1` like the following, because
  the `{:ok, 2}` term does not match the `:error` or `{:error, _}` clauses in the
  `else`:

      with {:ok, 1} <- {:ok, 2} do
        :woah
      else
        :error -> :error
        {:error, _} -> :error
      end

  """

  defexception [:term]

  @impl true
  def message(exception) do
    "no with clause matching: #{inspect(exception.term)}"
  end
end

defmodule CondClauseError do
  @moduledoc """
  An exception raised when no clauses in a `cond/1` expression evaluate to a truthy value.

  For example, this exception gets raised for a `cond/1` like the following:

      cond do
        1 + 1 == 3 -> :woah
        nil -> "yeah this won't happen
      end

  """

  defexception []

  @impl true
  def message(_exception) do
    "no cond clause evaluated to a truthy value"
  end
end

defmodule TryClauseError do
  @moduledoc """
  An exception raised when a term in a `try/1` expression
  does not match any of the defined `->` clauses in its `else`.

  The following fields of this exception are public and can be accessed freely:

    * `:term` (`t:term/0`) - the term that did not match any of the clauses

  """
  defexception [:term]

  @impl true
  def message(exception) do
    "no try clause matching: #{inspect(exception.term)}"
  end
end

defmodule BadArityError do
  @moduledoc """
  An exception raised when a function is called with the wrong number of arguments.
  """

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
  @moduledoc """
  An exception raised when a function is invoked that is not defined.

  The following fields of this exception are public and can be accessed freely:

    * `:module` (`t:module/0`) - the module name
    * `:function` (`t:atom/0`) - the function name
    * `:arity` (`t:non_neg_integer/0`) - the arity of the function

  For example, if you try to call `MyMod.non_existing_fun("hello", 1)`,
  the error would look like:

      %UndefinedFunctionError{
        module: MyMod,
        function: :non_existing_fun,
        arity: 2,
        # Other private fields...
      }

  """

  defexception [:module, :function, :arity, :reason, :message]

  @impl true
  def message(%{message: nil} = exception) do
    %{reason: reason, module: module, function: function, arity: arity} = exception
    {message, _hint_type} = message(reason, module, function, arity)
    message
  end

  def message(%{message: message}) do
    message
  end

  defp message(nil, module, function, arity) do
    cond do
      is_nil(function) or is_nil(arity) ->
        {"undefined function", :suggest_module}

      is_nil(module) ->
        formatted_fun = Exception.format_mfa(module, function, arity)
        {"function #{formatted_fun} is undefined", :suggest_module}

      function_exported?(module, :module_info, 0) ->
        message(:"function not exported", module, function, arity)

      true ->
        message(:"module could not be loaded", module, function, arity)
    end
  end

  defp message(:"module could not be loaded", module, function, arity) do
    formatted_fun = Exception.format_mfa(module, function, arity)

    {"function #{formatted_fun} is undefined (module #{inspect(module)} is not available)",
     :suggest_module}
  end

  defp message(:"function not exported", module, function, arity) do
    formatted_fun = Exception.format_mfa(module, function, arity)
    {"function #{formatted_fun} is undefined or private", :suggest_function}
  end

  defp message(:"undefined local", nil, function, arity) do
    {"function #{function}/#{arity} is undefined (there is no such import)", :no_hint}
  end

  defp message(reason, module, function, arity) do
    formatted_fun = Exception.format_mfa(module, function, arity)
    {"function #{formatted_fun} is undefined (#{reason})", :suggest_module}
  end

  @impl true
  def blame(exception, stacktrace) do
    %{reason: reason, module: module, function: function, arity: arity} = exception
    {message, hint_type} = message(reason, module, function, arity)
    message = message <> hint(module, function, arity, hint_type)
    {%{exception | message: message}, stacktrace}
  end

  defp hint(_, _, _, :no_hint), do: ""

  defp hint(nil, _function, 0, _hint_type) do
    ". If you are using the dot syntax, such as module.function(), " <>
      "make sure the left-hand side of the dot is a module atom"
  end

  defp hint(module, function, arity, :suggest_function) do
    behaviour_hint(module, function, arity) <>
      hint_for_loaded_module(module, function, arity, nil)
  end

  @max_suggestions 5
  defp hint(module, function, arity, :suggest_module) do
    downcased_module = downcase_module_name(module)
    stripped_module = module |> Atom.to_string() |> String.replace_leading("Elixir.", "")

    candidates =
      for {name, _, _} = candidate <- :code.all_available(),
          downcase_module_name(name) == downcased_module or
            String.ends_with?(List.to_string(name), stripped_module),
          {:module, module} <- [load_module(candidate)],
          function_exported?(module, function, arity),
          do: module

    if candidates != [] do
      suggestions =
        candidates
        |> Enum.take(@max_suggestions)
        |> Enum.sort(:asc)
        |> Enum.map(fn module ->
          ["\n      * ", Exception.format_mfa(module, function, arity)]
        end)

      IO.iodata_to_binary([". Did you mean:\n", suggestions, "\n"])
    else
      ""
    end
  end

  defp load_module({name, _path, _loaded?}) do
    name
    |> List.to_atom()
    |> Code.ensure_loaded()
  end

  defp downcase_module_name(module) do
    module
    |> to_string()
    |> String.downcase(:ascii)
  end

  @doc false
  def hint_for_loaded_module(module, function, arity, exports) do
    cond do
      macro_exported?(module, function, arity) ->
        ". However, there is a macro with the same name and arity. " <>
          "Be sure to require #{inspect(module)} if you intend to invoke this macro"

      message = otp_obsolete(module, function, arity) ->
        ", #{message}"

      true ->
        IO.iodata_to_binary(did_you_mean(module, function, exports))
    end
  end

  defp otp_obsolete(module, function, arity) do
    case :otp_internal.obsolete(module, function, arity) do
      {:removed, [_ | _] = string} -> string
      _ -> nil
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
      suggestions -> [". Did you mean:\n\n" | Enum.map(suggestions, &format_fa/1)]
    end
  end

  defp format_fa({_dist, fun, arity}) do
    ["      * ", Macro.inspect_atom(:remote_call, fun), ?/, Integer.to_string(arity), ?\n]
  end

  defp behaviour_hint(module, function, arity) do
    case behaviours_for(module) do
      [] ->
        ""

      behaviours ->
        case Enum.find(behaviours, &expects_callback?(&1, function, arity)) do
          nil -> ""
          behaviour -> ", but the behaviour #{inspect(behaviour)} expects it to be present"
        end
    end
  rescue
    # In case the module was removed while we are computing this
    UndefinedFunctionError -> ""
  end

  defp behaviours_for(module) do
    :attributes
    |> module.module_info()
    |> Keyword.get(:behaviour, [])
  end

  defp expects_callback?(behaviour, function, arity) do
    callbacks =
      behaviour.behaviour_info(:callbacks) -- behaviour.behaviour_info(:optional_callbacks)

    Enum.member?(callbacks, {function, arity})
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
  @moduledoc """
  An exception raised when a function call doesn't match any defined clause.

  The following fields of this exception are public and can be accessed freely:

    * `:module` (`t:module/0`) - the module name
    * `:function` (`t:atom/0`) - the function name
    * `:arity` (`t:non_neg_integer/0`) - the arity of the function

  For example, if you try to call a function such as `URI.parse/1` with something
  other than a string, the error would look like:

      %FunctionClauseError{
        module: URI,
        function: :parse,
        arity: 1,
        # Other private fields...
      }

  """

  defexception [:module, :function, :arity, :kind, :args, :clauses]

  @clause_limit 10

  @impl true
  def message(exception) do
    case exception do
      %{function: nil} ->
        "no function clause matches"

      %{module: module, function: function, arity: arity} ->
        formatted = Exception.format_mfa(module, function, arity)
        blamed = blame(exception, &inspect/1, &blame_match/1)
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

  defp blame_match(%{match?: true, node: node}), do: Macro.to_string(node)
  defp blame_match(%{match?: false, node: node}), do: "-" <> Macro.to_string(node) <> "-"

  @doc false
  def blame(%{args: nil}, _, _) do
    ""
  end

  def blame(exception, inspect_fun, fun) do
    %{module: module, function: function, arity: arity, kind: kind, args: args, clauses: clauses} =
      exception

    mfa = Exception.format_mfa(module, function, arity)

    format_clause_fun = fn {args, guards} ->
      args = Enum.map_join(args, ", ", fun)
      base = "    #{kind} #{function}(#{args})"
      Enum.reduce(guards, base, &"#{&2} when #{clause_to_string(&1, fun, 0)}") <> "\n"
    end

    "\n\nThe following arguments were given to #{mfa}:\n" <>
      "#{format_args(args, inspect_fun)}" <>
      "#{format_clauses(clauses, format_clause_fun, @clause_limit)}"
  end

  defp clause_to_string({op, _, [left, right]} = node, fun, parent) do
    case Code.Identifier.binary_op(op) do
      {_side, precedence} ->
        left = clause_to_string(left, fun, precedence)
        right = clause_to_string(right, fun, precedence)

        if parent > precedence do
          "(" <> left <> " #{op} " <> right <> ")"
        else
          left <> " #{op} " <> right
        end

      _ ->
        fun.(node)
    end
  end

  defp clause_to_string(node, fun, _precedence), do: fun.(node)

  defp format_args(args, inspect_fun) do
    args
    |> Enum.with_index(1)
    |> Enum.map(fn {arg, i} ->
      [pad("\n# "), Integer.to_string(i), pad("\n"), pad(inspect_fun.(arg)), "\n"]
    end)
  end

  defp format_clauses(clauses, format_clause_fun, limit)
  defp format_clauses(nil, _, _), do: ""
  defp format_clauses([], _, _), do: ""

  defp format_clauses(clauses, format_clause_fun, limit) do
    top_clauses =
      clauses
      |> Enum.take(limit)
      |> Enum.map(format_clause_fun)

    [
      "\nAttempted function clauses (showing #{length(top_clauses)} out of #{length(clauses)}):",
      "\n\n",
      top_clauses,
      non_visible_clauses(length(clauses) - limit)
    ]
  end

  defp non_visible_clauses(n) when n <= 0, do: []
  defp non_visible_clauses(1), do: ["    ...\n    (1 clause not shown)\n"]
  defp non_visible_clauses(n), do: ["    ...\n    (#{n} clauses not shown)\n"]

  defp pad(string) do
    String.replace(string, "\n", "\n    ")
  end
end

defmodule Code.LoadError do
  @moduledoc """
  An exception raised when a file cannot be loaded.

  The following fields of this exception are public and can be accessed freely:

    * `:file` (`t:String.t/0`) - the file name
    * `:reason` (`t:term/0`) - the reason why the file could not be loaded

  """

  defexception [:file, :message, :reason]

  def exception(opts) do
    file = Keyword.fetch!(opts, :file)
    reason = Keyword.fetch!(opts, :reason)
    message = "could not load #{file}. Reason: #{reason}"
    %Code.LoadError{message: message, file: file, reason: reason}
  end
end

defmodule Protocol.UndefinedError do
  @moduledoc """
  An exception raised when a protocol is not implemented for a given value.

  The following fields of this exception are public and can be accessed freely:

    * `:protocol` (`t:module/0`) - the protocol that is not implemented
    * `:value` (`t:term/0`) - the value that does not implement the protocol

  For example, this code:

      Enum.at("A string!", 0)

  would raise the following exception:

      %Protocol.UndefinedError{
        protocol: Enumerable,
        value: "A string!",
        # ...
      }

  """

  defexception [:protocol, :value, description: ""]

  @impl true
  def message(%{protocol: protocol, value: value, description: description}) do
    "protocol #{inspect(protocol)} not implemented for #{inspect(value)} of type " <>
      value_type(value) <> maybe_description(description) <> maybe_available(protocol)
  end

  defp value_type(%{__struct__: struct}), do: "#{inspect(struct)} (a struct)"
  defp value_type(value) when is_atom(value), do: "Atom"
  defp value_type(value) when is_bitstring(value), do: "BitString"
  defp value_type(value) when is_float(value), do: "Float"
  defp value_type(value) when is_function(value), do: "Function"
  defp value_type(value) when is_integer(value), do: "Integer"
  defp value_type(value) when is_list(value), do: "List"
  defp value_type(value) when is_map(value), do: "Map"
  defp value_type(value) when is_pid(value), do: "PID"
  defp value_type(value) when is_port(value), do: "Port"
  defp value_type(value) when is_reference(value), do: "Reference"
  defp value_type(value) when is_tuple(value), do: "Tuple"

  defp maybe_description(""), do: ""
  defp maybe_description(description), do: ", " <> description

  defp maybe_available(protocol) do
    case protocol.__protocol__(:impls) do
      {:consolidated, []} ->
        ". There are no implementations for this protocol."

      {:consolidated, types} ->
        ". This protocol is implemented for the following type(s): " <>
          Enum.map_join(types, ", ", &inspect/1)

      :not_consolidated ->
        ""
    end
  end
end

defmodule KeyError do
  @moduledoc """
  An exception raised when a key is not found in a data structure.

  For example, this is raised by `Map.fetch!/2` when the given key
  cannot be found in the given map.

  The following fields of this exception are public and can be accessed freely:

    * `:term` (`t:term/0`) - the data structure that was searched
    * `:key` (`t:term/0`) - the key that was not found

  """

  defexception [:key, :term, :message]

  @impl true
  def message(exception = %{message: nil}), do: message(exception.key, exception.term)
  def message(%{message: message}), do: message

  defp message(key, term) do
    message = "key #{inspect(key)} not found"

    cond do
      term == nil ->
        message

      is_atom(term) and is_atom(key) ->
        message <>
          " in: #{inspect(term)} (if instead you want to invoke #{inspect(term)}.#{key}(), " <>
          "make sure to add parentheses after the function name)"

      true ->
        message <> " in: #{inspect(term, pretty: true, limit: :infinity)}"
    end
  end

  @impl true
  def blame(exception = %{message: message}, stacktrace) when is_binary(message) do
    {exception, stacktrace}
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
      suggestions -> [". Did you mean:\n\n" | format_suggestions(suggestions)]
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

defmodule MissingApplicationsError do
  @moduledoc """
  An exception that is raised when an application depends on one or more
  missing applications.

  This exception is used by Mix and other tools. It can also be used by library authors
  when their library only requires an external application (like a dependency) for a subset
  of features.

  The fields of this exception are public. See `t:t/0`.

  *Available since v1.18.0.*

  ## Examples

      unless Application.spec(:plug, :vsn) do
        raise MissingApplicationsError,
          description: "application :plug is required for testing Plug-related functionality",
          apps: [{:plug, "~> 1.0"}]
      end

  """

  @moduledoc since: "1.18.0"

  @type t() :: %__MODULE__{
          apps: [{Application.app(), Version.requirement()}, ...],
          description: String.t()
        }

  defexception apps: [], description: "missing applications found"

  @impl true
  def message(%__MODULE__{apps: apps, description: description}) do
    # We explicitly format these as tuples so that they're easier to copy-paste
    # into dependencies.
    formatted_apps =
      Enum.map(apps, fn {app_name, requirement} ->
        ~s(\n  {#{inspect(app_name)}, "#{requirement}"})
      end)

    """
    #{description}

    To address this, include these applications as your dependencies:
    #{formatted_apps}\
    """
  end
end

defmodule Enum.OutOfBoundsError do
  @moduledoc """
  An exception that is raised when a function expects an enumerable to have
  a certain size but finds that it is too small.

  For example, this is raised by `Access.at!/1`.
  """

  defexception message: "out of bounds error"
end

defmodule Enum.EmptyError do
  @moduledoc """
  An exception that is raised when something expects a non-empty enumerable
  but finds an empty one.

  For example, this is raised by `Enum.min/3`.
  """

  defexception message: "empty error"
end

defmodule File.Error do
  @moduledoc """
  An exception that is raised when a file operation fails.

  The following fields of this exception are public and can be accessed freely:

    * `:path` (`t:Path.t/0`) - the path of the file that caused the error
    * `:reason` (`t:File.posix/0`) - the reason for the error

  """

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
  @moduledoc """
  An exception that is raised when copying a file fails.

  The following fields of this exception are public and can be accessed freely:

    * `:source` (`t:Path.t/0`) - the source path
    * `:destination` (`t:Path.t/0`) - the destination path
    * `:reason` (`t:File.posix/0`) - the reason why the file could not be copied

  """

  defexception [:reason, :source, :destination, on: "", action: ""]

  @impl true
  def message(exception) do
    formatted = IO.iodata_to_binary(:file.format_error(exception.reason))

    location =
      case exception.on do
        "" -> ""
        on -> ". #{on}"
      end

    "could not #{exception.action} from #{inspect(exception.source)} to " <>
      "#{inspect(exception.destination)}#{location}: #{formatted}"
  end
end

defmodule File.RenameError do
  @moduledoc """
  An exception that is raised when renaming a file fails.

  The following fields of this exception are public and can be accessed freely:

    * `:source` (`t:Path.t/0`) - the source path
    * `:destination` (`t:Path.t/0`) - the destination path
    * `:reason` (`t:File.posix/0`) - the reason why the file could not be renamed

  """

  defexception [:reason, :source, :destination, on: "", action: ""]

  @impl true
  def message(exception) do
    formatted = IO.iodata_to_binary(:file.format_error(exception.reason))

    location =
      case exception.on do
        "" -> ""
        on -> ". #{on}"
      end

    "could not #{exception.action} from #{inspect(exception.source)} to " <>
      "#{inspect(exception.destination)}#{location}: #{formatted}"
  end
end

defmodule File.LinkError do
  @moduledoc """
  An exception that is raised when linking a file fails.

  The following fields of this exception are public and can be accessed freely:

    * `:existing` (`t:Path.t/0`) - the existing file to link
    * `:new` (`t:Path.t/0`) - the link destination
    * `:reason` (`t:File.posix/0`) - the reason why the file could not be linked

  """

  defexception [:reason, :existing, :new, action: ""]

  @impl true
  def message(exception) do
    formatted = IO.iodata_to_binary(:file.format_error(exception.reason))

    "could not #{exception.action} from #{inspect(exception.existing)} to " <>
      "#{inspect(exception.new)}: #{formatted}"
  end
end

defmodule ErlangError do
  defexception [:original, :reason]

  @impl true
  def message(exception)

  def message(%__MODULE__{original: original, reason: nil}) do
    "Erlang error: #{inspect(original)}"
  end

  def message(%__MODULE__{original: original, reason: reason}) do
    IO.iodata_to_binary(["Erlang error: ", inspect(original), reason])
  end

  @doc false
  def normalize(:badarg, stacktrace) do
    case stacktrace do
      [{:erlang, :apply, [module, function, args], _} | _] when not is_atom(module) ->
        message =
          cond do
            is_map(module) and is_atom(function) and is_map_key(module, function) ->
              "you attempted to apply a function named #{inspect(function)} on a map/struct. " <>
                "If you are using Kernel.apply/3, make sure the module is an atom. " <>
                if is_function(module[function]) do
                  "If you are trying to invoke an anonymous function in a map/struct, " <>
                    "add a dot between the function name and the parenthesis: map.#{function}.()"
                else
                  "If you are using the dot syntax, ensure there are no parentheses " <>
                    "after the field name, such as map.#{function}"
                end

            is_atom(function) and args == [] ->
              "you attempted to apply a function named #{inspect(function)} on #{inspect(module)}. " <>
                "If you are using Kernel.apply/3, make sure the module is an atom. " <>
                "If you are using the dot syntax, such as module.function(), " <>
                "make sure the left-hand side of the dot is an atom representing a module"

            true ->
              "you attempted to apply a function on #{inspect(module)}. " <>
                "Modules (the first argument of apply) must always be an atom"
          end

        %ArgumentError{message: message}

      _ ->
        case error_info(:badarg, stacktrace, "errors were found at the given arguments") do
          {:ok, reason, details} -> %ArgumentError{message: reason <> details}
          :error -> %ArgumentError{}
        end
    end
  end

  def normalize(:badarith, _stacktrace) do
    %ArithmeticError{}
  end

  def normalize(:system_limit, stacktrace) do
    default_reason = "a system limit has been reached due to errors at the given arguments"

    case error_info(:system_limit, stacktrace, default_reason) do
      {:ok, reason, details} -> %SystemLimitError{message: reason <> details}
      :error -> %SystemLimitError{}
    end
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

  def normalize({:badkey, key, map}, _stacktrace) when is_map(map) do
    %KeyError{key: key, term: map}
  end

  def normalize({:badkey, key, term}, _stacktrace) do
    message =
      "key #{inspect(key)} not found in: #{inspect(term, pretty: true, limit: :infinity)}\n\n" <>
        "If you are using the dot syntax, such as map.field, " <>
        "make sure the left-hand side of the dot is a map"

    %KeyError{key: key, term: term, message: message}
  end

  def normalize({:case_clause, term}, _stacktrace) do
    %CaseClauseError{term: term}
  end

  # :else_clause is aligned on what Erlang returns for `maybe`
  def normalize({:else_clause, term}, _stacktrace) do
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

  def normalize(other, stacktrace) do
    case error_info(other, stacktrace, "") do
      {:ok, _reason, details} -> %ErlangError{original: other, reason: details}
      :error -> %ErlangError{original: other}
    end
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

  defp error_info(erl_exception, stacktrace, default_reason) do
    with [{module, fun, args_or_arity, opts} | tail] <- stacktrace,
         %{} = error_info <- opts[:error_info] do
      error_module = Map.get(error_info, :module, module)
      error_fun = Map.get(error_info, :function, :format_error)

      error_info = Map.put(error_info, :pretty_printer, &inspect/1)
      head = {module, fun, args_or_arity, Keyword.put(opts, :error_info, error_info)}

      extra =
        try do
          apply(error_module, error_fun, [erl_exception, [head | tail]])
        rescue
          _ -> %{}
        end

      arity = if is_integer(args_or_arity), do: args_or_arity, else: length(args_or_arity)
      args_errors = Map.take(extra, Enum.to_list(1..arity//1))
      reason = Map.get(extra, :reason, default_reason)

      cond do
        map_size(args_errors) > 0 ->
          {:ok, reason, IO.iodata_to_binary([":\n\n" | Enum.map(args_errors, &arg_error/1)])}

        general = extra[:general] ->
          {:ok, reason, ": " <> general}

        true ->
          :error
      end
    else
      _ -> :error
    end
  end

  defp arg_error({n, message}), do: "  * #{nth(n)} argument: #{message}\n"

  defp nth(1), do: "1st"
  defp nth(2), do: "2nd"
  defp nth(3), do: "3rd"
  defp nth(n), do: "#{n}th"
end

defmodule Inspect.Error do
  @moduledoc """
  Raised when a struct cannot be inspected.
  """
  @enforce_keys [:exception_module, :exception_message, :stacktrace, :inspected_struct]
  defexception @enforce_keys

  @impl true
  def exception(arguments) when is_list(arguments) do
    exception = Keyword.fetch!(arguments, :exception)
    exception_module = exception.__struct__
    exception_message = Exception.message(exception) |> String.trim_trailing("\n")
    stacktrace = Keyword.fetch!(arguments, :stacktrace)
    inspected_struct = Keyword.fetch!(arguments, :inspected_struct)

    %Inspect.Error{
      exception_module: exception_module,
      exception_message: exception_message,
      stacktrace: stacktrace,
      inspected_struct: inspected_struct
    }
  end

  @impl true
  def message(%__MODULE__{
        exception_module: exception_module,
        exception_message: exception_message,
        inspected_struct: inspected_struct
      }) do
    ~s'''
    got #{inspect(exception_module)} with message:

        """
    #{pad(exception_message, 4)}
        """

    while inspecting:

    #{pad(inspected_struct, 4)}
    '''
  end

  @doc false
  def pad(message, padding_length)
      when is_binary(message) and is_integer(padding_length) and padding_length >= 0 do
    padding = String.duplicate(" ", padding_length)

    message
    |> String.split("\n")
    |> Enum.map(fn
      "" -> "\n"
      line -> [padding, line, ?\n]
    end)
    |> IO.iodata_to_binary()
    |> String.trim_trailing("\n")
  end
end
