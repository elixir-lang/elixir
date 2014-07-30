import Kernel, except: [inspect: 2]

defmodule Logger.Formatter do
  @moduledoc ~S"""
  Conveniences for formatting data for logs.

  This module allows developers to specify a string that
  serves as template for log messages, for example:

      $time $metadata[$level] $message\n

  Will print error messages as:

      18:43:12.439 user_id=13 [error] Hello\n

  The valid parameters you can use are:

    * `$time` - time the log message was sent
    * `$date` - date the log message was sent
    * `$message` - the log message
    * `$level` - the log level
    * `$node` - the node that prints the message
    * `$metadata` - user controled data presented in "key=val key2=val2" format

  Backends typically allow developers to supply such control
  strings via configuration files. This module provides `compile/1`,
  which compiles the string into a format for fast operations at
  runtime and `format/5` to format the compiled pattern into an
  actual IO data.

  ## Metadata

  Metadata to be sent to the Logger can be read and written with
  the `Logger.metadata/0` and `Logger.metadata/1` functions. For example,
  you can set `Logger.metadata([user_id: 13])` to add user_id metadata
  to the current process. The user can configure the backend to chose
  which metadata it wants to print and it will replace the $metadata
  value.
  """

  @valid_patterns [:time, :date, :message, :level, :node, :metadata]
  @default_pattern "$time $metadata[$level] $message\n"

  @doc ~S"""
  Compiles a format string into an array that the `format/5` can handle.

  The valid parameters you can use are:

  * $time
  * $date
  * $message
  * $level
  * $node
  * $metadata - metadata is presented in key=val key2=val2 format.

  If you pass nil into compile it will use the default
  format of `$time $metadata [$level] $message`

  If you would like to make your own custom formatter simply pass
  `{module, function}` to compile and the rest is handled.

      iex> Logger.Formatter.compile("$time $metadata [$level] $message\n")
      [:time, " ", :metadata, " [", :level, "] ", :message, "\n"]
  """
  @spec compile(binary | nil) :: list()
  @spec compile({atom, atom}) :: {atom, atom}

  def compile(nil), do: compile(@default_pattern)
  def compile({mod, fun}) when is_atom(mod) and is_atom(fun), do: {mod, fun}

  def compile(str) do
    for part <- Regex.split(~r/(?<head>)\$[a-z]+(?<tail>)/, str, on: [:head, :tail], trim: true) do
      case part do
        "$" <> code -> compile_code(String.to_atom(code))
        _           -> part
      end
    end
  end

  defp compile_code(key) when key in @valid_patterns, do: key
  defp compile_code(key) when is_atom(key) do
    raise(ArgumentError, message: "$#{key} is an invalid format pattern.")
  end

  @doc """
  Takes a compiled format and injects the, level, timestamp, message and
  metadata listdict and returns a properly formatted string.
  """

  def format({mod, fun}, level, msg, ts, md) do
    Module.function(mod, fun, 4).(level, msg, ts, md)
  end

  def format(config, level, msg, ts, md) do
    for c <- config do
      output(c, level, msg, ts, md)
    end
  end

  defp output(:message, _, msg, _, _),        do: msg
  defp output(:date, _, _, {date, _time}, _), do: Logger.Utils.format_date(date)
  defp output(:time, _, _, {_date, time}, _), do: Logger.Utils.format_time(time)
  defp output(:level, level, _, _, _),        do: Atom.to_string(level)
  defp output(:node, _, _, _, _),             do: Atom.to_string(node())
  defp output(:metadata, _, _, _, []),        do: ""
  defp output(:metadata, _, _, _, meta) do
    Enum.map(meta, fn {key, val} ->
      [to_string(key), ?=, to_string(val), ?\s]
    end)
  end
  defp output(other, _, _, _, _), do: other
end
