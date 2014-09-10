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
    * `data` - the data to format. If it is a report, it is a tuple
      with `{report_type, report_data}`, if it is a format, it is a
      tuple with `{format_message, format_args}`

  The function must return:

    * `{:ok, iodata}` - if the message was translated with its translation
    * `:skip` - if the message is not meant to be translated nor logged
    * `:none` - if there is no translation, which triggers the next translator

  See the function `translate/4` in this module for an example implementation
  and the default messages translated by Logger.
  """

  def translate(min_level, :error, :format, message) do
    case message do
      {'** Generic server ' ++ _,
        [name, last, %Agent.Server{state: state, initial_call: initial_call}, reason]} ->
        translate_agent(min_level, name, last, state, initial_call, reason)
      {'** Generic server ' ++ _, [name, last, state, reason]} ->
        msg = "GenServer #{inspect name} terminating\n"
        if min_level == :debug do
          msg = msg <> "Last message: #{inspect last}\n"
                    <> "State: #{inspect state}\n"
        end
        {:ok, msg <> "** (exit) " <> Exception.format_exit(reason)}

      {'** gen_event handler ' ++ _, [name, manager, last, state, reason]} ->
        msg = "GenEvent handler #{inspect name} installed in #{inspect manager} terminating\n"
        if min_level == :debug do
          msg = msg <> "Last message: #{inspect last}\n"
                    <> "State: #{inspect state}\n"
        end
        {:ok, msg <> "** (exit) " <> Exception.format_exit(reason)}

      {'** Task ' ++ _, [name, starter, function, args, reason]} ->
        msg = "Task #{inspect name} started from #{inspect starter} terminating\n" <>
              "Function: #{inspect function}\n" <>
              "    Args: #{inspect args}\n" <>
              "** (exit) " <> Exception.format_exit(reason)
        {:ok, msg}

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

  defp translate_agent(min_level, name, last, state, initial_call, reason) do
    {:ok,
      ["Agent ", inspect(name), " terminating\n",
        agent_info(min_level, last, state, initial_call),
        "** (exit) " | Exception.format_exit(reason)]}
  end

  @agent_tag [:get, :get_and_update, :update]

  defp agent_info(min_level, {tag, fun}, state, initial_call)
  when is_function(fun, 1) and tag in @agent_tag do
    agent_info(min_level, tag, fun, [state], initial_call)
  end

  defp agent_info(min_level, {:"$gen_cast", {:cast, fun}}, state, initial_call)
  when is_function(fun, 1) do
    agent_info(min_level, :cast, fun, [state], initial_call)
  end

  defp agent_info(min_level, {tag, {mod, fun, args}}, state, initial_call)
  when is_atom(mod) and is_atom(fun) and length(args) < 255 and tag in @agent_tag do
    arity = length(args) + 1
    fun  = :erlang.make_fun(mod, fun, arity)
    agent_info(min_level, tag, fun, [state | args], initial_call)
  end

  defp agent_info(min_level, {:"$gen_cast", {:cast, {mod, fun, args}}}, state, initial_call)
  when is_atom(mod) and length(args) < 255 do
    arity = length(args) + 1
    fun  = :erlang.make_fun(mod, fun, arity)
    agent_info(min_level, :cast, fun, [state | args], initial_call)
  end

  defp agent_info(:debug, message, state, {mod, fun, arity}) do
    ["Last Message: ", inspect(message), ?\n,
      "       State: ", inspect(state), ?\n,
      "        Init: ", Exception.format_mfa(mod, fun, arity), ?\n]
  end

  defp agent_info(_min_level, _message, _state, _initial_call) do
    []
  end

  defp agent_info(:debug, tag, fun, args, {mod, init_fun, arity}) do
    ["Function: ", inspect(fun), ?\n,
      "    Args: ", inspect(args), ?\n,
      "  Action: ", inspect(tag), ?\n,
      "    Init: ", Exception.format_mfa(mod, init_fun, arity), ?\n]
  end

  defp agent_info(_min_level, _tag, fun, args, _initial_call) do
    ["Function: ", inspect(fun), ?\n,
      "    Args: ", inspect(args), ?\n]
  end

  defp translate_supervisor(min_level,
                           [supervisor: sup, errorContext: context,
                             reason: reason,
                             offender: [{:pid, pid}, {:name, name} | offender]])
                           when is_pid(pid) and context !== :shutdown do
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
                                        {:name, name} | offender]]) do
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
                                        {:name, name} | offender]]) do
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
                            started: [{:pid, pid}, {:name, name} | started]]) do
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
    {:ok, ["Process ", crash_name(pid, name) , " terminating\n",
            crash_info(min_level, [initial_call | crashed]),
            crash_linked(min_level, linked) |
            Exception.format(kind, exception, stack)]}
  end

  defp translate_crash(min_level,
                       [[{:pid, pid},
                         {:registered_name, name},
                         {:error_info, {kind, exception, stack}} | crashed],
                        linked]) do
    {:ok, ["Process ", crash_name(pid, name) , " terminating\n",
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

end
