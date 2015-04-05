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
    * `level` - the level of the message being translator
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

  # TODO: Remove name_or_id checks once we support only OTP >18.0

  def translate(min_level, level, kind, message)

  def translate(min_level, :error, :format, message) do
    case message do
      {'** Generic server ' ++ _, [name, last, state, reason]} ->
        msg = "GenServer #{inspect name} terminating\n"
        if min_level == :debug do
          msg = msg <> "Last message: #{inspect last}\n"
                    <> "State: #{inspect state}\n"
        end
        {:ok, [msg | format_stop(reason)]}

      {'** gen_event handler ' ++ _, [name, manager, last, state, reason]} ->
        msg = "GenEvent handler #{inspect name} installed in #{inspect manager} terminating\n"
        if min_level == :debug do
          msg = msg <> "Last message: #{inspect last}\n"
                    <> "State: #{inspect state}\n"
        end
        {:ok, [msg | format_stop(reason)]}

      {'** Task ' ++ _, [name, starter, function, args, reason]} ->
        msg = "Task #{inspect name} started from #{inspect starter} terminating\n" <>
              "Function: #{inspect function}\n" <>
              "    Args: #{inspect args}\n"
        {:ok, [msg | format_stop(reason)]}

      _ ->
        :none
    end
  end

  def translate(_min_level, :info, :report,
                {:std_info, [application: app, exited: reason, type: _type]}) do
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

  defp translate_supervisor(min_level,
                           [supervisor: sup, errorContext: context,
                             reason: reason,
                             offender: [{:pid, pid}, {name_or_id, name} | offender]])
                           when is_pid(pid) and context !== :shutdown and name_or_id in [:name, :id] do
    {:ok, ["Child ", inspect(name), " of Supervisor ",
            sup_name(sup), ?\s, sup_context(context), ?\n,
            "Pid: ", inspect(pid), ?\n,
            child_info(min_level, offender), ?\n,
            "** (exit) " | offender_reason(reason, context)]}
  end

  defp translate_supervisor(min_level,
                           [supervisor: sup, errorContext: context,
                             reason: reason,
                             offender: [{:pid, _pid},
                                        {name_or_id, name} | offender]]) when name_or_id in [:name, :id] do
    {:ok, ["Child ", inspect(name), " of Supervisor ",
            sup_name(sup), ?\s, sup_context(context), ?\n,
            child_info(min_level, offender), ?\n,
            "** (exit) " | offender_reason(reason, context)]}
  end

  defp translate_supervisor(min_level,
                           [supervisor: sup, errorContext: context,
                             reason: reason,
                             offender: [{:pid, pid} | offender]]) do
    {:ok, ["Child of Supervisor ",
            sup_name(sup), ?\s, sup_context(context), ?\n,
            "Pid: ", inspect(pid), ?\n,
            child_info(min_level, offender), ?\n,
            "** (exit) " | offender_reason(reason, context)]}
  end

  defp translate_supervisor(min_level,
                           [supervisor: sup, errorContext: context,
                             reason: reason,
                             offender: [{:nb_children, n},
                                        {name_or_id, name} | offender]]) when name_or_id in [:name, :id] do
    {:ok, ["Children ", inspect(name), " of Supervisor ",
            sup_name(sup), ?\s, sup_context(context), ?\n,
            "Number: ", inspect(n), ?\n,
            child_info(min_level, offender), ?\n,
            "** (exit) " | offender_reason(reason, context)]}
  end

  defp translate_supervisor(_min_level, _other), do: :none

  defp translate_progress(_min_level,
                          [application: app, started_at: node_name]) do
    {:ok, ["Application ", to_string(app), " started at " | inspect(node_name)]}
  end

  defp translate_progress(min_level,
                          [supervisor: sup,
                            started: [{:pid, pid}, {name_or_id, name} | started]]) when name_or_id in [:name, :id] do
    {:ok, ["Child ", inspect(name), " of Supervisor ",
            sup_name(sup), " started\n",
            "Pid: ", inspect(pid), ?\n |
            child_info(min_level, started)]}
  end

  defp translate_progress(min_level,
                          [supervisor: sup,
                            started: [{:pid, pid} | started]]) do
    {:ok, ["Child of Supervisor ", sup_name(sup), " started\n",
            "Pid: ", inspect(pid), ?\n |
            child_info(min_level, started)]}
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
    ["Start Call: ", Exception.format_mfa(mod, fun, args) |
      child_debug(min_level, debug)]
  end

  defp child_info(min_level, [{:mfa, {mod, fun, args}} | debug]) do
    ["Start Call: ", Exception.format_mfa(mod, fun, args) |
      child_debug(min_level, debug)]
  end

  defp child_info(min_level, [{:mod, mod} | debug]) do
    ["Start Module: ", inspect(mod) |
      child_debug(min_level, debug)]
  end

  defp child_debug(:debug,
                   [restart_type: restart, shutdown: shutdown, child_type: type]) do
    [?\n,
      "Restart: ", inspect(restart), ?\n,
      "Shutdown: ", inspect(shutdown), ?\n,
      "Type: ", inspect(type)]
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

  defp translate_crash(min_level,
                       [[{:initial_call, _} = initial_call,
                         {:pid, pid},
                         {:registered_name, name},
                         {:error_info, {kind, exception, stack}} | crashed],
                        linked]) do
    {:ok, ["Process ", crash_name(pid, name), " terminating\n",
            crash_info(min_level, [initial_call | crashed]),
            crash_linked(min_level, linked) |
            Exception.format(kind, exception, stack)]}
  end

  defp translate_crash(min_level,
                       [[{:pid, pid},
                         {:registered_name, name},
                         {:error_info, {kind, exception, stack}} | crashed],
                        linked]) do
    {:ok, ["Process ", crash_name(pid, name), " terminating\n",
            crash_info(min_level, crashed),
            crash_linked(min_level, linked) |
            Exception.format(kind, exception, stack)]}
  end

  defp crash_name(pid, []), do: inspect(pid)
  defp crash_name(pid, name), do: [inspect(name), " (", inspect(pid), ?)]

  defp crash_info(min_level, info, prefix \\ [])

  defp crash_info(min_level,
                  [{:initial_call, {mod, fun, args}} | info], prefix) do
    [prefix, "Initial Call: ", crash_call(mod, fun, args), ?\n |
      crash_info(min_level, info, prefix)]
  end

  defp crash_info(min_level,
                  [{:current_function, {mod, fun, args}} | info], prefix) do
    [prefix, "Current Call: ", crash_call(mod, fun, args), ?\n |
      crash_info(min_level, info, prefix)]
  end

  defp crash_info(min_level, [{:current_function, []} | info], prefix) do
    crash_info(min_level, info, prefix)
  end

  defp crash_info(min_level,
                  [{:ancestors, ancestors} | debug], prefix) do
    [prefix, "Ancestors: ", inspect(ancestors), ?\n |
      crash_debug(min_level, debug, prefix)]
  end

  defp crash_call(mod, fun, arity) when is_integer(arity) do
    Exception.format_mfa(mod, fun, arity)
  end

  defp crash_call(mod, fun, args) do
    Exception.format_mfa(mod, fun, length(args))
  end

  defp crash_debug(:debug,
                   [messages: msgs, links: links, dictionary: dict,
                     trap_exit: trap, status: status, heap_size: heap_size,
                     stack_size: stack_size, reductions: reductions], prefix) do
    [prefix, "Messages: ", inspect(msgs), ?\n,
      prefix, "Links: ", inspect(links), ?\n,
      prefix, "Dictionary: ", inspect(dict), ?\n,
      prefix, "Trapping Exits: ", inspect(trap), ?\n,
      prefix, "Status: ", inspect(status), ?\n,
      prefix, "Heap Size: ", inspect(heap_size), ?\n,
      prefix, "Stack Size: ", inspect(stack_size), ?\n,
      prefix, "Reductions: ", inspect(reductions), ?\n]
  end

  defp crash_debug(_min_level, _info, _prefix) do
    []
  end

  defp crash_linked(_min_level, []), do: []

  defp crash_linked(min_level, neighbours) do
    Enum.reduce(neighbours, "Neighbours:\n", fn({:neighbour, info}, acc) ->
      [acc | crash_neighbour(min_level, info)]
    end)
  end

  defp crash_neighbour(min_level,
                       [{:pid, pid}, {:registered_name, []} | info]) do
    prefix = "    "
    [prefix, inspect(pid), ?\n |
      crash_info(min_level, info, [prefix | prefix])]
  end

  defp crash_neighbour(min_level,
                       [{:pid, pid}, {:registered_name, name} | info]) do
    prefix = "    "
    [prefix, inspect(name), " (", inspect(pid), ")\n" |
      crash_info(min_level, info, [prefix | prefix])]
  end

  defp format_stop({maybe_exception, [_ | _ ] = maybe_stacktrace} = reason) do
    try do
      for maybe_entry <- maybe_stacktrace do
        [<<"\n    ">> | Exception.format_stacktrace_entry(maybe_entry)]
      end
    catch
      :error, _ ->
        format_stop_banner(reason)
    else
      formatted_stacktrace ->
        [format_stop_banner(maybe_exception, maybe_stacktrace) |
          formatted_stacktrace]
    end
  end

  defp format_stop(reason) do
    format_stop_banner(reason)
  end

  defp format_stop_banner(reason) do
    ["** (stop) " | Exception.format_exit(reason)]
  end

  # OTP process rewrite the :undef error to these reasons when logging
  @gen_undef [:"module could not be loaded", :"function not exported"]

  defp format_stop_banner(undef, [{mod, fun, args, _info} | _ ]  = stacktrace)
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
    if Exception.exception?(reason) do
        Exception.format_banner(:error, reason, stacktrace)
    else
      case Exception.normalize(:error, reason, stacktrace) do
        %ErlangError{} ->
          format_stop_banner(reason)
        exception ->
          Exception.format_banner(:error, exception, stacktrace)
      end
    end
  end

  defp format_undef(mod, fun, arity, undef, stacktrace) do
    opts = [module: mod, function: fun, arity: arity, reason: undef]
    exception = UndefinedFunctionError.exception(opts)
    Exception.format_banner(:error, exception, stacktrace)
  end
end
