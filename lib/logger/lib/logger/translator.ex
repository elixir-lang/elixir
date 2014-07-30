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

  def translate(_min_level, _level, _kind, _message) do
    :none
  end
end
