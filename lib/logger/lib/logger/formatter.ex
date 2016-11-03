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

    * `$time`     - time the log message was sent
    * `$date`     - date the log message was sent
    * `$message`  - the log message
    * `$level`    - the log level
    * `$node`     - the node that prints the message
    * `$metadata` - user controlled data presented in `"key=val key2=val2"` format
    * `$levelpad` - sets to a single space if level is 4 characters long,
      otherwise set to the empty space. Used to align the message after level.

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
  which metadata it wants to print and it will replace the `$metadata`
  value.
  """

  @type time :: {{1970..10000, 1..12, 1..31}, {0..23, 0..59, 0..59, 0..999}}
  @type pattern :: :date | :level | :levelpad | :message | :metadata | :node | :time
  @valid_patterns [:time, :date, :message, :level, :node, :metadata, :levelpad]
  @default_pattern "\n$time $metadata[$level] $levelpad$message\n"
  @replacement "�"

  @doc """
  Prune non-valid UTF-8 codepoints.

  Typically called after formatting when the data cannot be printed.
  """
  @spec prune(IO.chardata) :: IO.chardata
  def prune(binary) when is_binary(binary), do: prune_binary(binary, "")
  def prune([h | t]) when h in 0..1114111, do: [h | prune(t)]
  def prune([h | t]), do: [prune(h) | prune(t)]
  def prune([]), do: []
  def prune(_), do: @replacement

  defp prune_binary(<<h::utf8, t::binary>>, acc),
    do: prune_binary(t, <<acc::binary, h::utf8>>)
  defp prune_binary(<<_, t::binary>>, acc),
    do: prune_binary(t, <<acc::binary, @replacement>>)
  defp prune_binary(<<>>, acc),
    do: acc

  @doc ~S"""
  Compiles a format string into a data structure that the `format/5` can handle.

  Check the module doc for documentation on the valid parameters. If you
  pass `nil`, it defaults to: `$time $metadata [$level] $levelpad$message\n`

  If you would like to make your own custom formatter simply pass
  `{module, function}` to `compile/1` and the rest is handled.

      iex> Logger.Formatter.compile("$time $metadata [$level] $message\n")
      [:time, " ", :metadata, " [", :level, "] ", :message, "\n"]
  """
  @spec compile(binary | nil) :: [pattern | binary]
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

  @spec format({atom, atom} | [pattern | binary], Logger.level, Logger.message, time, Keyword.t) ::
    IO.chardata
  def format({mod, fun}, level, msg, ts, md) do
    apply(mod, fun, [level, msg, ts, md])
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
      [to_string(key), ?=, metadata(val), ?\s]
    end)
  end

  defp output(:levelpad, level, _, _, _) do
    levelpad(level)
  end

  defp output(other, _, _, _, _), do: other

  defp levelpad(:debug), do: ""
  defp levelpad(:info), do: " "
  defp levelpad(:warn), do: " "
  defp levelpad(:error), do: ""

  defp metadata(pid) when is_pid(pid) do
    :erlang.pid_to_list(pid)
  end
  defp metadata(ref) when is_reference(ref) do
    '#Ref' ++ rest = :erlang.ref_to_list(ref)
    rest
  end
  defp metadata(atom) when is_atom(atom) do
    case Atom.to_string(atom) do
      "Elixir." <> rest -> rest
      "nil" -> ""
      binary -> binary
    end
  end
  defp metadata(other), do: to_string(other)
end
