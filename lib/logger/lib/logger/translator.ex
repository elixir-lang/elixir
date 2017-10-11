defmodule Logger.Translator do
  @moduledoc """
  Default translation for Erlang log messages.

  Logger allows developers to rewrite log messages provided by
  Erlang applications into a format more compatible with Elixir
  log messages by providing a translator.

  A translator is simply a tuple containing a module and a function
  that can be added and removed via the `Logger.add_translator/1` and
  `Logger.remove_translator/1` functions and is invoked for every Erlang
  message above the minimum log level with four arguments:

    * `min_level` - the current Logger level
    * `level` - the level of the message being translated
    * `kind` - if the message is a report or a format
    * `message` - the message to format. If it is a report, it is a tuple
      with `{report_type, report_data}`, if it is a format, it is a
      tuple with `{format_message, format_args}`

  The function must return:

    * `{:ok, chardata}` - if the message was translated with its translation
    * `:skip` - if the message is not meant to be translated nor logged
    * `:none` - if there is no translation, which triggers the next translator

  See the function `translate/4` in this module for an example implementation
  and the default messages translated by Logger.
  """

  # The name_or_id checks are required to support old OTP projects.

  def translate(min_level, level, kind, message)

  def translate(min_level, :error, :format, message) do
    opts = Application.get_env(:logger, :translator_inspect_opts)

    case message do
      {'** Generic server ' ++ _, [name, last, state, reason | client]} ->
        msg =
          ["GenServer #{inspect(name)} terminating", format_stop(reason)] ++
            ["\nLast message#{format_from(client)}: #{inspect(last, opts)}"]

        if min_level == :debug do
          {:ok, [msg, "\nState: #{inspect(state, opts)}" | format_client(client)]}
        else
          {:ok, msg}
        end

      {'** gen_event handler ' ++ _, [name, manager, last, state, reason]} ->
        msg =
          ["GenEvent handler #{inspect(name)} installed in #{inspect(manager)} terminating"] ++
            [format_stop(reason), "\nLast message: #{inspect(last, opts)}"]

        if min_level == :debug do
          {:ok, [msg | "\nState: #{inspect(state, opts)}"]}
        else
          {:ok, msg}
        end

      {'** Task ' ++ _, [name, starter, function, args, reason]} ->
        msg =
          ["Task #{inspect(name)} started from #{inspect(starter)} terminating"] ++
            [format_stop(reason), "\nFunction: #{inspect(function, opts)}"] ++
            ["\n    Args: #{inspect(args, opts)}"]

        {:ok, msg}

      {'Error in process ' ++ _, [pid, {reason, stack}]} ->
        msg = ["Process ", inspect(pid), " raised an exception" | format(:error, reason, stack)]

        {:ok, msg}

      _ ->
        :none
    end
  end

  def translate(_min_level, :info, :report, {
        :std_info,
        [application: app, exited: reason, type: _type]
      }) do
    {:ok, "Application #{app} exited: #{Application.format_error(reason)}"}
  end

  def translate(min_level, :error, :report, {:supervisor_report, data}) do
    translate_supervisor(min_level, data)
  end

  def translate(min_level, :error, :report, {:crash_report, data}) do
    translate_crash(min_level, data)
  end

  def translate(min_level, :info, :report, {:progress, data}) do
    translate_progress(min_level, data)
  end

  def translate(_min_level, _level, _kind, _message) do
    :none
  end

  defp translate_supervisor(
         min_level,
         supervisor: sup,
         errorContext: context,
         reason: reason,
         offender: [{:pid, pid}, {name_or_id, name} | offender]
       )
       when is_pid(pid) and context !== :shutdown and name_or_id in [:name, :id] do
    msg =
      ["Child ", inspect(name), " of Supervisor ", sup_name(sup)] ++
        [?\s, sup_context(context), "\n** (exit) "] ++
        [offender_reason(reason, context), "\nPid: ", inspect(pid)] ++
        child_info(min_level, offender)

    {:ok, msg}
  end

  defp translate_supervisor(
         min_level,
         supervisor: sup,
         errorContext: context,
         reason: reason,
         offender: [{:pid, _pid}, {name_or_id, name} | offender]
       )
       when name_or_id in [:name, :id] do
    msg =
      ["Child ", inspect(name), " of Supervisor ", sup_name(sup)] ++
        [?\s, sup_context(context), "\n** (exit) ", offender_reason(reason, context)] ++
        child_info(min_level, offender)

    {:ok, msg}
  end

  defp translate_supervisor(
         min_level,
         supervisor: sup,
         errorContext: context,
         reason: reason,
         offender: [{:pid, pid} | offender]
       ) do
    msg =
      ["Child of Supervisor ", sup_name(sup), ?\s, sup_context(context), "\n** (exit) "] ++
        [offender_reason(reason, context), "\nPid: ", inspect(pid)] ++
        child_info(min_level, offender)

    {:ok, msg}
  end

  defp translate_supervisor(
         min_level,
         supervisor: sup,
         errorContext: context,
         reason: reason,
         offender: [{:nb_children, n}, {name_or_id, name} | offender]
       )
       when name_or_id in [:name, :id] do
    msg =
      ["Children ", inspect(name), " of Supervisor ", sup_name(sup), ?\s, sup_context(context)] ++
        ["\n** (exit) ", offender_reason(reason, context), "\nNumber: ", inspect(n)] ++
        child_info(min_level, offender)

    {:ok, msg}
  end

  defp translate_supervisor(_min_level, _other), do: :none

  defp translate_progress(_min_level, application: app, started_at: node_name) do
    {:ok, ["Application ", to_string(app), " started at " | inspect(node_name)]}
  end

  defp translate_progress(
         min_level,
         supervisor: sup,
         started: [{:pid, pid}, {name_or_id, name} | started]
       )
       when name_or_id in [:name, :id] do
    msg =
      ["Child ", inspect(name), " of Supervisor ", sup_name(sup)] ++
        [" started", "\nPid: ", inspect(pid)] ++ child_info(min_level, started)

    {:ok, msg}
  end

  defp translate_progress(
         min_level,
         supervisor: sup,
         started: [{:pid, pid} | started]
       ) do
    msg =
      ["Child of Supervisor ", sup_name(sup), " started", "\nPid: ", inspect(pid)] ++
        child_info(min_level, started)

    {:ok, msg}
  end

  defp translate_progress(_min_level, _other), do: :none

  defp sup_name({:local, name}), do: inspect(name)
  defp sup_name({:global, name}), do: inspect(name)
  defp sup_name({:via, _mod, name}), do: inspect(name)
  defp sup_name({pid, mod}), do: [inspect(pid), " (", inspect(mod), ?)]

  defp sup_context(:start_error), do: "failed to start"
  defp sup_context(:child_terminated), do: "terminated"
  defp sup_context(:shutdown), do: "caused shutdown"
  defp sup_context(:shutdown_error), do: "shutdown abnormally"

  defp child_info(min_level, [{:mfargs, {mod, fun, args}} | debug]) do
    ["\nStart Call: ", format_mfa(mod, fun, args) | child_debug(min_level, debug)]
  end

  defp child_info(min_level, [{:mfa, {mod, fun, args}} | debug]) do
    ["\nStart Call: ", format_mfa(mod, fun, args) | child_debug(min_level, debug)]
  end

  defp child_info(min_level, [{:mod, mod} | debug]) do
    ["\nStart Module: ", inspect(mod) | child_debug(min_level, debug)]
  end

  defp child_debug(:debug, restart_type: restart, shutdown: shutdown, child_type: type) do
    ["\nRestart: ", inspect(restart), "\nShutdown: ", inspect(shutdown)] ++
      ["\nType: ", inspect(type)]
  end

  defp child_debug(_min_level, _child) do
    []
  end

  # If start call raises reason will be of form {:EXIT, reason}
  defp offender_reason({:EXIT, reason}, :start_error) do
    Exception.format_exit(reason)
  end

  defp offender_reason(reason, _context) do
    Exception.format_exit(reason)
  end

  defp translate_crash(min_level, [
         [
           {:initial_call, _} = initial_call,
           {:pid, pid},
           {:registered_name, name},
           {:error_info, {kind, exception, stack}} | crashed
         ],
         linked
       ]) do
    msg =
      ["Process ", crash_name(pid, name), " terminating", format(kind, exception, stack)] ++
        [crash_info(min_level, [initial_call | crashed])] ++ crash_linked(min_level, linked)

    {:ok, msg}
  end

  defp translate_crash(min_level, [
         [
           {:pid, pid},
           {:registered_name, name},
           {:error_info, {kind, exception, stack}} | crashed
         ],
         linked
       ]) do
    msg =
      ["Process ", crash_name(pid, name), " terminating", format(kind, exception, stack)] ++
        [crash_info(min_level, crashed), crash_linked(min_level, linked)]

    {:ok, msg}
  end

  defp crash_name(pid, []), do: inspect(pid)
  defp crash_name(pid, name), do: [inspect(name), " (", inspect(pid), ?)]

  defp crash_info(min_level, info, prefix \\ [?\n])

  defp crash_info(min_level, [{:initial_call, {mod, fun, args}} | info], prefix) do
    [prefix, "Initial Call: ", crash_call(mod, fun, args) | crash_info(min_level, info, prefix)]
  end

  defp crash_info(min_level, [{:current_function, {mod, fun, args}} | info], prefix) do
    [prefix, "Current Call: ", crash_call(mod, fun, args) | crash_info(min_level, info, prefix)]
  end

  defp crash_info(min_level, [{:current_function, []} | info], prefix) do
    crash_info(min_level, info, prefix)
  end

  defp crash_info(min_level, [{:ancestors, ancestors} | debug], prefix) do
    [prefix, "Ancestors: ", inspect(ancestors) | crash_info(min_level, debug, prefix)]
  end

  defp crash_info(:debug, debug, prefix) do
    for {key, value} <- debug do
      crash_debug(key, value, prefix)
    end
  end

  defp crash_info(_, _, _) do
    []
  end

  defp crash_call(mod, fun, arity) when is_integer(arity) do
    format_mfa(mod, fun, arity)
  end

  defp crash_call(mod, fun, args) do
    format_mfa(mod, fun, length(args))
  end

  defp crash_debug(:current_stacktrace, stack, prefix) do
    stack_prefix = [prefix | "    "]
    stacktrace = Enum.map(stack, &[stack_prefix | Exception.format_stacktrace_entry(&1)])

    [prefix, "Current Stacktrace:" | stacktrace]
  end

  defp crash_debug(key, value, prefix) do
    [prefix, crash_debug_key(key), ?:, ?\s, inspect(value)]
  end

  defp crash_debug_key(key) do
    case key do
      :message_queue_len -> "Message Queue Length"
      :messages -> "Messages"
      :links -> "Links"
      :dictionary -> "Dictionary"
      :trap_exit -> "Trapping Exits"
      :status -> "Status"
      :heap_size -> "Heap Size"
      :stack_size -> "Stack Size"
      :reductions -> "Reductions"
    end
  end

  defp crash_linked(_min_level, []), do: []

  defp crash_linked(min_level, neighbours) do
    Enum.reduce(neighbours, "\nNeighbours:", fn {:neighbour, info}, acc ->
      [acc | crash_neighbour(min_level, info)]
    end)
  end

  defp crash_neighbour(min_level, [{:pid, pid}, {:registered_name, []} | info]) do
    indent = "    "

    [?\n, indent, inspect(pid) | crash_info(min_level, info, [?\n, indent | indent])]
  end

  defp crash_neighbour(min_level, [{:pid, pid}, {:registered_name, name} | info]) do
    indent = "    "

    [?\n, indent, inspect(name), " (", inspect(pid), ")"] ++
      crash_info(min_level, info, [?\n, indent | indent])
  end

  defp format_stop({maybe_exception, [_ | _] = maybe_stacktrace} = reason) do
    try do
      format_stacktrace(maybe_stacktrace)
    catch
      :error, _ ->
        format_stop_banner(reason)
    else
      formatted_stacktrace ->
        [format_stop_banner(maybe_exception, maybe_stacktrace) | formatted_stacktrace]
    end
  end

  defp format_stop(reason) do
    format_stop_banner(reason)
  end

  defp format_stop_banner(reason) do
    ["\n** (stop) " | Exception.format_exit(reason)]
  end

  # OTP processes rewrite the :undef error to these reasons when logging
  @gen_undef [:"module could not be loaded", :"function not exported"]

  defp format_stop_banner(undef, [{mod, fun, args, _info} | _] = stacktrace)
       when undef in @gen_undef and is_atom(mod) and is_atom(fun) do
    cond do
      is_list(args) ->
        format_undef(mod, fun, length(args), undef, stacktrace)

      is_integer(args) ->
        format_undef(mod, fun, args, undef, stacktrace)

      true ->
        format_stop_banner(undef)
    end
  end

  defp format_stop_banner(reason, stacktrace) do
    # If this is already an exception (even an ErlangError), we format it as an
    # exception. Otherwise, we try to normalize it, and if it's normalized as an
    # ErlangError we instead format it as an exit.
    if Exception.exception?(reason) do
      [?\n | Exception.format_banner(:error, reason, stacktrace)]
    else
      case Exception.normalize(:error, reason, stacktrace) do
        %ErlangError{} ->
          format_stop_banner(reason)

        exception ->
          [?\n | Exception.format_banner(:error, exception, stacktrace)]
      end
    end
  end

  defp format_undef(mod, fun, arity, undef, stacktrace) do
    opts = [module: mod, function: fun, arity: arity, reason: undef]
    exception = UndefinedFunctionError.exception(opts)
    [?\n | Exception.format_banner(:error, exception, stacktrace)]
  end

  defp format(kind, payload, stacktrace) do
    [?\n, Exception.format_banner(kind, payload, stacktrace) | format_stacktrace(stacktrace)]
  end

  defp format_stacktrace(stacktrace) do
    for entry <- stacktrace do
      [<<"\n    ">> | Exception.format_stacktrace_entry(entry)]
    end
  end

  defp format_mfa(mod, fun, :undefined),
    do: [inspect(mod), ?., Code.Identifier.inspect_as_function(fun) | "/?"]

  defp format_mfa(mod, fun, args), do: Exception.format_mfa(mod, fun, args)

  defp format_from([]), do: ""
  defp format_from([from]), do: " (from #{inspect(from)})"
  defp format_from([from, stacktrace]) when is_list(stacktrace), do: " (from #{inspect(from)})"

  defp format_from([from, node_name]) when is_atom(node_name),
    do: " (from #{inspect(from)} on #{inspect(node_name)})"

  defp format_client([from]) do
    "\nClient #{inspect(from)} is dead"
  end

  defp format_client([from, stacktrace]) when is_list(stacktrace) do
    ["\nClient #{inspect(from)} is alive\n" | Exception.format_stacktrace(stacktrace)]
  end

  defp format_client(_) do
    []
  end
end
