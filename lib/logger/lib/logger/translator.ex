defmodule Logger.Translator do
  @moduledoc """
  Default translation for Erlang log messages.

  Logger allows developers to rewrite log messages provided by
  OTP applications into a format more compatible with Elixir
  log messages by providing a translator.

  A translator is simply a tuple containing a module and a function
  that can be added and removed via the `Logger.add_translator/1` and
  `Logger.remove_translator/1` functions and is invoked for every Erlang
  message above the minimum log level with four arguments:

    * `min_level` - the current Logger level
    * `level` - the level of the message being translated
    * `kind` - if the message is a `:report` or `:format`
    * `message` - the message to format. If it is `:report`, it is a tuple
      with `{report_type, report_data}`, if it is `:format`, it is a
      tuple with `{format_message, format_args}`.

  The function must return:

    * `{:ok, chardata, metadata}` - if the message translation with its metadata
    * `{:ok, chardata}` - the translated message
    * `:skip` - if the message is not meant to be translated nor logged
    * `:none` - if there is no translation, which triggers the next translator

  See the function `translate/4` in this module for an example implementation
  and the default messages translated by Logger.
  """

  @doc """
  Built-in translation function.
  """
  def translate(min_level, level, kind, message)

  ## Erlang/OTP 21 and after

  def translate(min_level, _level, :report, {:logger, %{label: label} = report}) do
    case label do
      {:gen_server, :terminate} ->
        report_gen_server_terminate(min_level, report)

      {:gen_event, :terminate} ->
        report_gen_event_terminate(min_level, report)

      _ ->
        :skip
    end
  end

  def translate(min_level, _level, :report, {{:proc_lib, :crash}, data}) do
    report_crash(min_level, data)
  end

  def translate(min_level, _level, :report, {{:supervisor, :progress}, data}) do
    report_supervisor_progress(min_level, data)
  end

  def translate(min_level, _level, :report, {{:supervisor, _}, data}) do
    report_supervisor(min_level, data)
  end

  def translate(
        _min_level,
        _level,
        :report,
        {{:application_controller, :progress}, [application: app, started_at: node]}
      ) do
    {:ok, ["Application ", Atom.to_string(app), " started at " | inspect(node)]}
  end

  def translate(
        _min_level,
        _level,
        :report,
        {{:application_controller, :exit}, [application: app, exited: reason, type: _type]}
      ) do
    {:ok, ["Application ", Atom.to_string(app), " exited: " | Application.format_error(reason)]}
  end

  ## Erlang/OTP 20 and before
  # TODO: This clauses can be removed when we support only Erlang/OTP 21+.

  def translate(min_level, :error, :format, message) do
    opts = Application.get_env(:logger, :translator_inspect_opts)

    case message do
      {'** Generic server ' ++ _, [name, last, state, reason | client]} ->
        {formatted, reason} = format_reason(reason)
        metadata = [crash_reason: reason] ++ registered_name(name)

        msg =
          ["GenServer #{inspect(name)} terminating", formatted] ++
            ["\nLast message#{format_from(client)}: #{inspect(last, opts)}"]

        if min_level == :debug do
          msg = [msg, "\nState: #{inspect(state, opts)}" | format_client(client)]
          {:ok, msg, metadata}
        else
          {:ok, msg, metadata}
        end

      {'** gen_event handler ' ++ _, [name, manager, last, state, reason]} ->
        {formatted, reason} = format_reason(reason)
        metadata = [crash_reason: reason] ++ registered_name(manager)

        msg =
          [":gen_event handler #{inspect(name)} installed in #{inspect(manager)} terminating"] ++
            [formatted, "\nLast message: #{inspect(last, opts)}"]

        if min_level == :debug do
          {:ok, [msg | "\nState: #{inspect(state, opts)}"], metadata}
        else
          {:ok, msg, metadata}
        end

      {'** Task ' ++ _, [name, starter, function, args, reason]} ->
        {formatted, reason} = format_reason(reason)
        metadata = [crash_reason: reason] ++ registered_name(name)

        msg =
          ["Task #{inspect(name)} started from #{inspect(starter)} terminating"] ++
            [formatted, "\nFunction: #{inspect(function, opts)}"] ++
            ["\n    Args: #{inspect(args, opts)}"]

        {:ok, msg, metadata}

      {'Error in process ' ++ _, [pid, {reason, stack}]} ->
        reason = Exception.normalize(:error, reason, stack)
        msg = ["Process ", inspect(pid), " raised an exception" | format(:error, reason, stack)]
        {:ok, msg, [crash_reason: exit_reason(:error, reason, stack)]}

      _ ->
        :none
    end
  end

  def translate(_min_level, :info, :report, {
        :std_info,
        [application: app, exited: reason, type: _type]
      }) do
    {:ok, ["Application ", Atom.to_string(app), " exited: " | Application.format_error(reason)]}
  end

  def translate(min_level, :error, :report, {{:error_logger, :error_report}, data}) do
    report_supervisor(min_level, data)
  end

  def translate(min_level, :error, :report, {:supervisor_report, data}) do
    report_supervisor(min_level, data)
  end

  def translate(min_level, :error, :report, {:crash_report, data}) do
    report_crash(min_level, data)
  end

  def translate(min_level, :info, :report, {:progress, [{:supervisor, _} | _] = data}) do
    report_supervisor_progress(min_level, data)
  end

  def translate(_min_level, :info, :report, {:progress, [application: app, started_at: node]}) do
    {:ok, ["Application ", Atom.to_string(app), " started at " | inspect(node)]}
  end

  ## Helpers

  def translate(_min_level, _level, _kind, _message) do
    :none
  end

  defp report_gen_server_terminate(min_level, report) do
    inspect_opts = Application.get_env(:logger, :translator_inspect_opts)

    %{
      client_info: client,
      last_message: last,
      name: name,
      reason: reason,
      state: state
    } = report

    {formatted, reason} = format_reason(reason)
    metadata = [crash_reason: reason] ++ registered_name(name)

    msg =
      ["GenServer ", inspect(name), " terminating", formatted] ++
        ["\nLast message", format_last_message_from(client), ": ", inspect(last, inspect_opts)]

    if min_level == :debug do
      msg = [msg, "\nState: ", inspect(state, inspect_opts) | format_client_info(client)]
      {:ok, msg, metadata}
    else
      {:ok, msg, metadata}
    end
  end

  defp report_gen_event_terminate(min_level, report) do
    inspect_opts = Application.get_env(:logger, :translator_inspect_opts)

    %{
      handler: handler,
      last_message: last,
      name: name,
      reason: reason,
      state: state
    } = report

    reason =
      case reason do
        {:EXIT, why} -> why
        _ -> reason
      end

    {formatted, reason} = format_reason(reason)
    metadata = [crash_reason: reason] ++ registered_name(name)

    msg =
      [":gen_event handler ", inspect(handler), " installed in ", inspect(name), " terminating"] ++
        [formatted, "\nLast message: ", inspect(last, inspect_opts)]

    if min_level == :debug do
      {:ok, [msg, "\nState: ", inspect(state, inspect_opts)], metadata}
    else
      {:ok, msg, metadata}
    end
  end

  defp report_supervisor_progress(
         min_level,
         supervisor: sup,
         started: [{:pid, pid}, {:id, id} | started]
       ) do
    msg =
      ["Child ", inspect(id), " of Supervisor ", sup_name(sup), " started"] ++
        ["\nPid: ", inspect(pid)] ++ child_info(min_level, started)

    {:ok, msg}
  end

  defp report_supervisor_progress(
         min_level,
         supervisor: sup,
         started: [{:pid, pid} | started]
       ) do
    msg =
      ["Child of Supervisor ", sup_name(sup), " started", "\nPid: ", inspect(pid)] ++
        child_info(min_level, started)

    {:ok, msg}
  end

  defp report_supervisor_progress(_min_level, _other), do: :none

  defp report_supervisor(
         min_level,
         supervisor: sup,
         errorContext: context,
         reason: reason,
         offender: [{:pid, pid}, {:id, id} | offender]
       ) do
    pid_info =
      if is_pid(pid) and context != :shutdown do
        ["\nPid: ", inspect(pid)]
      else
        []
      end

    msg =
      ["Child ", inspect(id), " of Supervisor ", sup_name(sup)] ++
        [?\s, sup_context(context), "\n** (exit) ", offender_reason(reason, context)] ++
        pid_info ++ child_info(min_level, offender)

    {:ok, msg}
  end

  defp report_supervisor(
         min_level,
         supervisor: sup,
         errorContext: context,
         reason: reason,
         offender: [{:nb_children, n}, {:id, id} | offender]
       ) do
    msg =
      ["Children ", inspect(id), " of Supervisor ", sup_name(sup), ?\s, sup_context(context)] ++
        ["\n** (exit) ", offender_reason(reason, context), "\nNumber: ", Integer.to_string(n)] ++
        child_info(min_level, offender)

    {:ok, msg}
  end

  defp report_supervisor(
         min_level,
         supervisor: sup,
         errorContext: context,
         reason: reason,
         offender: [{:pid, pid} | offender]
       ) do
    msg =
      ["Child of Supervisor ", sup_name(sup), ?\s, sup_context(context)] ++
        ["\n** (exit) ", offender_reason(reason, context), "\nPid: ", inspect(pid)] ++
        child_info(min_level, offender)

    {:ok, msg}
  end

  defp report_supervisor(_min_level, _other), do: :none

  # If start call raises reason will be of form {:EXIT, reason}
  defp offender_reason({:EXIT, reason}, :start_error) do
    Exception.format_exit(reason)
  end

  defp offender_reason(reason, _context) do
    Exception.format_exit(reason)
  end

  defp sup_name({:local, name}), do: inspect(name)
  defp sup_name({:global, name}), do: inspect(name)
  defp sup_name({:via, _mod, name}), do: inspect(name)
  defp sup_name({pid, mod}), do: [inspect(pid), " (", inspect(mod), ?)]

  defp sup_context(:start_error), do: "failed to start"
  defp sup_context(:child_terminated), do: "terminated"
  defp sup_context(:shutdown), do: "caused shutdown"
  defp sup_context(:shutdown_error), do: "shut down abnormally"

  defp child_info(min_level, [{:mfargs, {mod, fun, args}} | debug]) do
    ["\nStart Call: ", format_mfa(mod, fun, args) | child_debug(min_level, debug)]
  end

  # Comes from bridge with MFA
  defp child_info(min_level, [{:mfa, {mod, fun, args}} | debug]) do
    ["\nStart Call: ", format_mfa(mod, fun, args) | child_debug(min_level, debug)]
  end

  # Comes from bridge with Mod
  defp child_info(min_level, [{:mod, mod} | debug]) do
    ["\nStart Module: ", inspect(mod) | child_debug(min_level, debug)]
  end

  defp child_info(_min_level, _child) do
    []
  end

  defp child_debug(:debug, restart_type: restart, shutdown: shutdown, child_type: type) do
    ["\nRestart: ", inspect(restart), "\nShutdown: ", inspect(shutdown)] ++
      ["\nType: ", inspect(type)]
  end

  defp child_debug(_min_level, _child) do
    []
  end

  defp report_crash(min_level, [[{:initial_call, initial_call} | crashed], linked]) do
    mfa = initial_call_to_mfa(initial_call)
    report_crash(min_level, crashed, [{:initial_call, mfa}], linked)
  end

  defp report_crash(min_level, [crashed, linked]) do
    report_crash(min_level, crashed, [], linked)
  end

  defp report_crash(min_level, crashed, extra, linked) do
    [
      {:pid, pid},
      {:registered_name, name},
      {:error_info, {kind, reason, stack}} | crashed
    ] = crashed

    dictionary = crashed[:dictionary]
    reason = Exception.normalize(kind, reason, stack)

    case Keyword.get(dictionary, :logger_metadata, {true, []}) do
      {false, _} ->
        :skip

      {true, user_metadata} ->
        msg =
          ["Process ", crash_name(pid, name), " terminating", format(kind, reason, stack)] ++
            [crash_info(min_level, extra ++ crashed, [?\n]), crash_linked(min_level, linked)]

        extra =
          if ancestors = crashed[:ancestors], do: [{:ancestors, ancestors} | extra], else: extra

        extra =
          if callers = dictionary[:"$callers"], do: [{:callers, callers} | extra], else: extra

        extra = [{:crash_reason, exit_reason(kind, reason, stack)} | extra]
        {:ok, msg, registered_name(name) ++ extra ++ user_metadata}
    end
  end

  defp initial_call_to_mfa({:supervisor, module, _}), do: {module, :init, 1}
  defp initial_call_to_mfa({:supervisor_bridge, module, _}), do: {module, :init, 1}
  defp initial_call_to_mfa({mod, fun, args}) when is_list(args), do: {mod, fun, length(args)}
  defp initial_call_to_mfa(mfa), do: mfa

  defp crash_name(pid, []), do: inspect(pid)
  defp crash_name(pid, name), do: [inspect(name), " (", inspect(pid), ?)]

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

  @indent "    "

  defp crash_neighbour(min_level, [{:pid, pid}, {:registered_name, []} | info]) do
    [?\n, @indent, inspect(pid) | crash_info(min_level, info, [?\n, @indent | @indent])]
  end

  defp crash_neighbour(min_level, [{:pid, pid}, {:registered_name, name} | info]) do
    [?\n, @indent, inspect(name), " (", inspect(pid), ")"] ++
      crash_info(min_level, info, [?\n, @indent | @indent])
  end

  defp format_last_message_from({_, {name, _}}), do: [" (from ", inspect(name), ")"]
  defp format_last_message_from({from, _}), do: [" (from ", inspect(from), ")"]
  defp format_last_message_from(_), do: []

  defp format_client_info({from, :dead}),
    do: ["\nClient ", inspect(from), " is dead"]

  defp format_client_info({from, :remote}),
    do: ["\nClient ", inspect(from), " is remote on node ", inspect(node(from))]

  defp format_client_info({_, {name, stacktrace}}),
    do: ["\nClient ", inspect(name), " is alive\n" | format_stacktrace(stacktrace)]

  defp format_client_info(_),
    do: []

  defp format_reason({maybe_exception, [_ | _] = maybe_stacktrace} = reason) do
    try do
      format_stacktrace(maybe_stacktrace)
    catch
      :error, _ ->
        {format_stop(reason), {reason, []}}
    else
      formatted_stacktrace ->
        {formatted, reason} = maybe_normalize(maybe_exception, maybe_stacktrace)
        {[formatted | formatted_stacktrace], {reason, maybe_stacktrace}}
    end
  end

  defp format_reason(reason) do
    {format_stop(reason), {reason, []}}
  end

  defp format_stop(reason) do
    ["\n** (stop) " | Exception.format_exit(reason)]
  end

  # Erlang processes rewrite the :undef error to these reasons when logging
  @gen_undef [:"module could not be loaded", :"function not exported"]

  defp maybe_normalize(undef, [{mod, fun, args, _info} | _] = stacktrace)
       when undef in @gen_undef and is_atom(mod) and is_atom(fun) do
    cond do
      is_list(args) ->
        format_undef(mod, fun, length(args), undef, stacktrace)

      is_integer(args) ->
        format_undef(mod, fun, args, undef, stacktrace)

      true ->
        {format_stop(undef), undef}
    end
  end

  defp maybe_normalize(reason, stacktrace) do
    # If this is already an exception (even an ErlangError), we format it as an
    # exception. Otherwise, we try to normalize it, and if it's normalized as an
    # ErlangError we instead format it as an exit.
    if Exception.exception?(reason) do
      {[?\n | Exception.format_banner(:error, reason, stacktrace)], reason}
    else
      case Exception.normalize(:error, reason, stacktrace) do
        %ErlangError{} ->
          {format_stop(reason), reason}

        exception ->
          {[?\n | Exception.format_banner(:error, exception, stacktrace)], exception}
      end
    end
  end

  defp format(kind, payload, stacktrace) do
    [?\n, Exception.format_banner(kind, payload, stacktrace) | format_stacktrace(stacktrace)]
  end

  defp format_stacktrace(stacktrace) do
    for entry <- stacktrace do
      ["\n    " | Exception.format_stacktrace_entry(entry)]
    end
  end

  defp registered_name(name) when is_atom(name), do: [registered_name: name]
  defp registered_name(_name), do: []

  defp format_mfa(mod, fun, :undefined),
    do: [inspect(mod), ?., Code.Identifier.inspect_as_function(fun) | "/?"]

  defp format_mfa(mod, fun, args),
    do: Exception.format_mfa(mod, fun, args)

  defp exit_reason(:exit, reason, stack), do: {reason, stack}
  defp exit_reason(:error, reason, stack), do: {reason, stack}
  defp exit_reason(:throw, value, stack), do: {{:nocatch, value}, stack}

  ## Deprecated helpers

  defp format_from([]), do: ""
  defp format_from([from]), do: " (from #{inspect(from)})"
  defp format_from([from, stacktrace]) when is_list(stacktrace), do: " (from #{inspect(from)})"

  defp format_from([from, node_name]) when is_atom(node_name),
    do: " (from #{inspect(from)} on #{inspect(node_name)})"

  defp format_client([from]) do
    "\nClient #{inspect(from)} is dead"
  end

  defp format_client([from, stacktrace]) when is_list(stacktrace) do
    ["\nClient #{inspect(from)} is alive\n" | format_stacktrace(stacktrace)]
  end

  defp format_client(_) do
    []
  end

  defp format_undef(mod, fun, arity, undef, stacktrace) do
    opts = [module: mod, function: fun, arity: arity, reason: undef]
    exception = UndefinedFunctionError.exception(opts)
    {[?\n | Exception.format_banner(:error, exception, stacktrace)], exception}
  end
end
