defmodule Logger.Translator do
  @moduledoc """
  Default translation for Erlang log messages.

  Logger allows developers to rewrite log messages provided by
  Erlang applications into a format more compatible to Elixir
  log messages by providing translator.

  A translator is simply a tuple containing a module and a function
  that can be added and removed via the `add_translator/1` and
  `remove_translator/1` functions and is invoked for every Erlang
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
    {:ok, "Application #{app} exited with reason #{Exception.format_exit(reason)}"}
  end

  def translate(min_level, :error, :report, {:supervisor_report, data}) do
    {:ok, translate_supervisor(min_level, data)}
  end

  def translate(min_level, :error, :report, {:crash_report, data}) do
    {:ok, translate_crash(min_level, data)}
  end

  def translate(min_level, :info, :report, {:progress, data}) do
    {:ok, translate_progress(min_level, data)}
  end

  def translate(_min_level, _level, _kind, _message) do
    :none
  end

  def translate_supervisor(min_level, data) do
    sup = Keyword.fetch!(data, :supervisor)
    context = Keyword.fetch!(data, :errorContext)
    offender = Keyword.fetch!(data, :offender)
    reason = Keyword.fetch!(data, :reason)
    case Keyword.fetch(offender, :pid) do
      {:ok, pid} when is_pid(pid) and context !== :shutdown ->
        ["Child ", child_name(offender), " of Supervisor ",
          sup_name(sup), ?\s, sup_context(context), ?\n,
          "Pid: ", inspect(pid), ?\n,
          child_info(min_level, offender), ?\n,
          "** (exit) " | offender_reason(reason, context)]
      {:ok, _} ->
        ["Child ", child_name(offender), " of Supervisor ",
          sup_name(sup), ?\s, sup_context(context), ?\n,
          child_info(min_level, offender), ?\n,
          "** (exit) " | offender_reason(reason, context)]
      :error ->
        number = Keyword.fetch!(offender, :nb_children)
        ["Children ", child_name(offender), " of Supervisor ",
          sup_name(sup), ?\s, sup_context(context), ?\n,
          "Number: ", inspect(number), ?\n,
          child_info(min_level, offender), ?\n,
          "** (exit) " | offender_reason(reason, context)]
    end
  end

  defp translate_progress(min_level, data) do
    case Keyword.fetch(data, :application) do
      {:ok, app} ->
        node_name = Keyword.fetch!(data, :started_at)
        ["Application ", to_string(app), " started at " | inspect(node_name)]
      :error ->
        translate_sup_progress(min_level, data)
    end
  end

  defp translate_sup_progress(min_level, data) do
    sup = Keyword.fetch!(data, :supervisor)
    started = Keyword.fetch!(data, :started)
    pid = Keyword.fetch!(started, :pid)
    ["Child ", child_name(started), " of Supervisor ",
      sup_name(sup), " started\n",
      "Pid: ", inspect(pid), ?\n |
      child_info(min_level, started)]
  end

  defp sup_name({:local, name}), do: inspect(name)
  defp sup_name({:global, name}), do: inspect(name)
  defp sup_name({:via, _mod, name}), do: inspect(name)
  defp sup_name({pid, mod}), do: [inspect(pid), " (", inspect(mod), ?)]

  defp sup_context(:start_error), do: "failed to start"
  defp sup_context(:child_terminated), do: "terminated"
  defp sup_context(:shutdown), do: "caused shutdown"
  defp sup_context(:shutdown_error), do: "shutdown abnormally"

  defp child_name(offender) do
    inspect(Keyword.fetch!(offender, :name))
  end

  defp child_info(min_level, child) do
    {mod, fun, args} = Keyword.fetch!(child, :mfargs)
    ["Start Call: ", Exception.format_mfa(mod, fun, args) |
      child_debug(min_level, child)]
  end

  defp child_debug(:debug, child) do
    restart = Keyword.fetch!(child, :restart_type)
    shutdown = Keyword.fetch!(child, :shutdown)
    type = Keyword.fetch!(child, :child_type)
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

  defp translate_crash(min_level, [crashed, linked]) do
    pid = Keyword.fetch!(crashed, :pid)
    {kind, exception, stack} = Keyword.fetch!(crashed, :error_info)
    ["Process ", inspect(pid) , " terminating\n",
      crash_info(min_level, crashed),
      crash_linked(min_level, linked),
      Exception.format_banner(kind, exception, stack)]
  end

  defp crash_info(min_level, info, prefix \\ []) do
    ancestors = Keyword.fetch!(info, :ancestors)
    [crash_name(info, prefix),
      prefix, "Initial Call: ", initial_call(info), ?\n,
      crash_current(info, prefix),
      prefix, "Ancestors: ", inspect(ancestors), ?\n |
      crash_debug(min_level, info, prefix)]
  end

  defp crash_name(info, prefix) do
    case Keyword.fetch!(info, :registered_name) do
      [] -> []
      name -> [prefix, "Name: ", inspect(name), ?\n]
    end
  end

  defp initial_call(info) do
    case Keyword.fetch!(info, :initial_call) do
      {mod, fun, arity} when is_integer(arity) ->
        Exception.format_mfa(mod, fun, arity)
      {mod, fun, args} ->
        # args are fake list
        Exception.format_mfa(mod, fun, length(args))
    end
  end

  defp crash_current(info, prefix) do
    case Keyword.fetch(info, :current_function) do
      {:ok, {mod, fun, arity}} ->
        [prefix, "Current Call: ", Exception.format_mfa(mod, fun, arity), ?\n]
      :error ->
        []
    end
  end

  defp crash_debug(:debug, info, prefix) do
    [messages: "Messages", links: "Links", dictionary: "Dictionary",
      trap_exit: "Trapping Exits", status: "Status", heap_size: "Heap Size",
      stack_size: "Stack Size", reductions: "Reductions"]
    |> Enum.reduce([], fn({key, text}, acc) ->
      [acc, prefix, text, ": ", inspect(Keyword.fetch!(info, key)), ?\n]
    end)
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

  defp crash_neighbour(min_level, info) do
    pid = Keyword.fetch!(info, :pid)
    prefix = "    "
    [prefix, inspect(pid), ?\n |
      crash_info(min_level, info, [prefix | prefix])]
  end

end
