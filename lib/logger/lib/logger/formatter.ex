import Kernel, except: [inspect: 2]

defmodule Logger.Formatter do
  @moduledoc ~S"""
  Conveniences for formatting data for logs.

  This module allows developers to specify a `{module, function}`
  or a string that serves as template for log messages.

  ## Formatting string

  The log messages can be controlled by a formatting string.
  Here is an example of how to configure the `:console` backend
  in a `config/config.exs` file:

  For example:

      config :logger, :console,
        format: "$time $metadata[$level] $message\n"

  The above will print error messages as:

      18:43:12.439 user_id=13 [error] Hello\n

  The valid parameters you can use are:

    * `$time`     - the time the log message was sent
    * `$date`     - the date the log message was sent
    * `$message`  - the log message
    * `$level`    - the log level
    * `$node`     - the node that prints the message
    * `$metadata` - user controlled data presented in `"key=val key2=val2 "` format

  Backends typically allow developers to supply such control
  strings via configuration files. This module provides `compile/1`,
  which compiles the string into a format for fast operations at
  runtime and `format/5` to format the compiled pattern into an
  actual IO data.

  ## Formatting function

  You can also customize the format of your log messages to a
  `{module, function}` tuple if you wish to provide your own
  format function. Here is an example of how to configure the
  `:console` backend in a `config/config.exs` file:

      config :logger, :console,
        format: {MyConsoleLogger, :format}

  And here is an example of how you can define `MyConsoleLogger.format/4`
  from the above configuration:

      defmodule MyConsoleLogger do
        @spec format(atom, term, Logger.Formatter.time(), keyword()) :: IO.chardata()
        def format(level, message, timestamp, metadata) do
          # Custom formatting logic that must return chardata.
          # ...
        end
      end

  **The `format/4` function must not fail**. If it does, it will bring
  that particular logger instance down, causing your system to temporarily
  lose log messages. If necessary, wrap the function in a `rescue` and
  log a default message instead:

      defmodule MyConsoleLogger do
        def format(level, message, timestamp, metadata) do
          # Custom formatting logic
        rescue
          _ -> "could not format: #{inspect({level, message, metadata})}"
        end
      end

  The `{module, function}` will be invoked with four arguments:

    * the log level: an atom (`t:atom/0`)
    * the message: this is usually `t:IO.chardata/0`, but in some cases it
      may contain invalid data. Since the formatting function must
      *never* fail, you need to prepare for the message being anything
    * the current timestamp: a term of type `t:Logger.Formatter.time/0`
    * the metadata: a keyword list (`t:keyword/0`)

  The `{module, function}` must return a term of type `t:IO.chardata/0`.

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
  @default_pattern "\n$time $metadata[$level] $message\n"
  @replacement "�"

  @doc """
  Prunes invalid Unicode code points from lists and invalid UTF-8 bytes.

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
    regex = ~r/(?<head>)\$[a-z]+(?<tail>)/

    for part <- Regex.split(regex, str, on: [:head, :tail], trim: true) do
      case part do
        "$" <> code -> compile_code(String.to_atom(code))
        _ -> part
      end
    end
  end

  defp compile_code(:levelpad) do
    IO.warn("$levelpad in Logger message format is deprecated, please remove it")
    :levelpad
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

  If `pattern_or_function` is a `{module, function_name}` tuple,
  then `module.function_name(level, message, timestamp, metadata)` is
  invoked to get the message. See `Logger.Backends.Console` for more
  information on this.

  ## Examples

      iex> pattern = Logger.Formatter.compile("[$level] $message")
      iex> timestamp = {{1977, 01, 28}, {13, 29, 00, 000}}
      iex> formatted = Logger.Formatter.format(pattern, :info, "hello", timestamp, [])
      iex> IO.chardata_to_string(formatted)
      "[info] hello"

  """
  @spec format(mod_and_fun | [pattern | binary], Logger.level(), Logger.message(), time, keyword) ::
          IO.chardata()
        when mod_and_fun: {atom, atom}
  def format(pattern_or_function, level, message, timestamp, metadata)

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
  defp output(:metadata, _, _, _, meta), do: metadata(meta)
  defp output(:levelpad, level, _, _, _), do: levelpad(level)
  defp output(other, _, _, _, _), do: other

  defp levelpad(:info), do: " "
  defp levelpad(:warn), do: " "
  defp levelpad(_), do: ""

  defp metadata([{key, value} | metadata]) do
    if formatted = metadata(key, value) do
      [to_string(key), ?=, formatted, ?\s | metadata(metadata)]
    else
      metadata(metadata)
    end
  end

  defp metadata([]) do
    []
  end

  defp metadata(:time, _), do: nil
  defp metadata(:gl, _), do: nil
  defp metadata(:report_cb, _), do: nil

  defp metadata(_, nil), do: nil
  defp metadata(_, string) when is_binary(string), do: string
  defp metadata(_, integer) when is_integer(integer), do: Integer.to_string(integer)
  defp metadata(_, float) when is_float(float), do: Float.to_string(float)
  defp metadata(_, pid) when is_pid(pid), do: :erlang.pid_to_list(pid)

  defp metadata(_, atom) when is_atom(atom) do
    case Atom.to_string(atom) do
      "Elixir." <> rest -> rest
      "nil" -> ""
      binary -> binary
    end
  end

  defp metadata(_, ref) when is_reference(ref) do
    '#Ref' ++ rest = :erlang.ref_to_list(ref)
    rest
  end

  defp metadata(:file, file) when is_list(file), do: file

  defp metadata(:domain, [head | tail]) when is_atom(head) do
    Enum.map_intersperse([head | tail], ?., &Atom.to_string/1)
  end

  defp metadata(:mfa, {mod, fun, arity})
       when is_atom(mod) and is_atom(fun) and is_integer(arity) do
    Exception.format_mfa(mod, fun, arity)
  end

  defp metadata(:initial_call, {mod, fun, arity})
       when is_atom(mod) and is_atom(fun) and is_integer(arity) do
    Exception.format_mfa(mod, fun, arity)
  end

  defp metadata(_, list) when is_list(list), do: nil

  defp metadata(_, other) do
    case String.Chars.impl_for(other) do
      nil -> nil
      impl -> impl.to_string(other)
    end
  end
end
