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

    * `$time`     - the time the log message was sent
    * `$date`     - the date the log message was sent
    * `$message`  - the log message
    * `$level`    - the log level
    * `$node`     - the node that prints the message
    * `$metadata` - user controlled data presented in `"key=val key2=val2 "` format
    * `$levelpad` - sets to a single space if level is 4 characters long,
      otherwise set to the empty space. Used to align the message after level.

  Backends typically allow developers to supply such control
  strings via configuration files. This module provides `compile/1`,
  which compiles the string into a format for fast operations at
  runtime and `format/5` to format the compiled pattern into an
  actual IO data.

  ## Metadata

  Metadata to be sent to the logger can be read and written with
  the `Logger.metadata/0` and `Logger.metadata/1` functions. For example,
  you can set `Logger.metadata([user_id: 13])` to add user_id metadata
  to the current process. The user can configure the backend to choose
  which metadata it wants to print and it will replace the `$metadata`
  value.
  """

  @type time :: {{1970..10000, 1..12, 1..31}, {0..23, 0..59, 0..59, 0..999}}
  @type pattern :: :date | :level | :levelpad | :message | :metadata | :node | :time
  @valid_patterns [:time, :date, :message, :level, :node, :metadata, :levelpad]
  @default_pattern "\n$time $metadata[$level] $levelpad$message\n"
  @replacement "�"

  @doc """
  Prunes non-valid UTF-8 code points.

  Typically called after formatting when the data cannot be printed.
  """
  @spec prune(IO.chardata()) :: IO.chardata()
  def prune(binary) when is_binary(binary), do: prune_binary(binary, "")
  def prune([h | t]) when h in 0..1_114_111, do: [h | prune(t)]
  def prune([h | t]), do: [prune(h) | prune(t)]
  def prune([]), do: []
  def prune(_), do: @replacement

  defp prune_binary(<<h::utf8, t::binary>>, acc), do: prune_binary(t, <<acc::binary, h::utf8>>)
  defp prune_binary(<<_, t::binary>>, acc), do: prune_binary(t, <<acc::binary, @replacement>>)
  defp prune_binary(<<>>, acc), do: acc

  @doc """
  Compiles a format string into a data structure that `format/5` can handle.

  Check the module doc for documentation on the valid parameters that
  will be interpolated in the pattern. If you pass `nil` as the pattern,
  the pattern defaults to:

      #{inspect(@default_pattern)}

  If you want to customize formatting through a custom formatter, you can
  pass a `{module, function}` tuple as the `pattern`.

      iex> Logger.Formatter.compile("$time $metadata [$level] $message\\n")
      [:time, " ", :metadata, " [", :level, "] ", :message, "\\n"]

      iex> Logger.Formatter.compile({MyLoggerFormatter, :format})
      {MyLoggerFormatter, :format}

  """
  @spec compile(binary | nil) :: [pattern | binary]
  @spec compile(pattern) :: pattern when pattern: {module, function :: atom}
  def compile(pattern)

  def compile(nil), do: compile(@default_pattern)
  def compile({mod, fun}) when is_atom(mod) and is_atom(fun), do: {mod, fun}

  def compile(str) when is_binary(str) do
    regex = Regex.recompile!(~r/(?<head>)\$[a-z]+(?<tail>)/)

    for part <- Regex.split(regex, str, on: [:head, :tail], trim: true) do
      case part do
        "$" <> code -> compile_code(String.to_atom(code))
        _ -> part
      end
    end
  end

  defp compile_code(key) when key in @valid_patterns, do: key

  defp compile_code(key) when is_atom(key) do
    raise ArgumentError, "$#{key} is an invalid format pattern"
  end

  @doc """
  Formats time as chardata.
  """
  @spec format_time({0..23, 0..59, 0..59, 0..999}) :: IO.chardata()
  def format_time({hh, mi, ss, ms}) do
    [pad2(hh), ?:, pad2(mi), ?:, pad2(ss), ?., pad3(ms)]
  end

  @doc """
  Formats date as chardata.
  """
  @spec format_date({1970..10000, 1..12, 1..31}) :: IO.chardata()
  def format_date({yy, mm, dd}) do
    [Integer.to_string(yy), ?-, pad2(mm), ?-, pad2(dd)]
  end

  defp pad3(int) when int < 10, do: [?0, ?0, Integer.to_string(int)]
  defp pad3(int) when int < 100, do: [?0, Integer.to_string(int)]
  defp pad3(int), do: Integer.to_string(int)

  defp pad2(int) when int < 10, do: [?0, Integer.to_string(int)]
  defp pad2(int), do: Integer.to_string(int)

  @doc """
  Takes a compiled format and injects the level, timestamp, message, and
  metadata keyword list and returns a properly formatted string.

  ## Examples

      iex> pattern = Logger.Formatter.compile("[$level] $message")
      iex> timestamp = {{1977, 01, 28}, {13, 29, 00, 000}}
      iex> formatted = Logger.Formatter.format(pattern, :info, "hello", timestamp, [])
      iex> IO.chardata_to_string(formatted)
      "[info] hello"

  """
  @spec format({atom, atom} | [pattern | binary], Logger.level(), Logger.message(), time, keyword) ::
          IO.chardata()
  def format({mod, fun}, level, msg, timestamp, metadata) do
    apply(mod, fun, [level, msg, timestamp, metadata])
  end

  def format(config, level, msg, timestamp, metadata) do
    for config_option <- config do
      output(config_option, level, msg, timestamp, metadata)
    end
  end

  defp output(:message, _, msg, _, _), do: msg
  defp output(:date, _, _, {date, _time}, _), do: format_date(date)
  defp output(:time, _, _, {_date, time}, _), do: format_time(time)
  defp output(:level, level, _, _, _), do: Atom.to_string(level)
  defp output(:node, _, _, _, _), do: Atom.to_string(node())

  defp output(:metadata, _, _, _, []), do: ""

  defp output(:metadata, _, _, _, meta) do
    Enum.map(meta, fn {key, val} ->
      [to_string(key), ?=, metadata(key, val), ?\s]
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

  defp metadata(:initial_call, {mod, fun, arity})
       when is_atom(mod) and is_atom(fun) and is_integer(arity) do
    Exception.format_mfa(mod, fun, arity)
  end

  defp metadata(_, pid) when is_pid(pid) do
    :erlang.pid_to_list(pid)
  end

  defp metadata(_, ref) when is_reference(ref) do
    '#Ref' ++ rest = :erlang.ref_to_list(ref)
    rest
  end

  defp metadata(_, atom) when is_atom(atom) do
    case Atom.to_string(atom) do
      "Elixir." <> rest -> rest
      "nil" -> ""
      binary -> binary
    end
  end

  defp metadata(_, other), do: to_string(other)
end
