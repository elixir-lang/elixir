import Kernel, except: [inspect: 2]

defmodule Logger.Formatter do
  @moduledoc ~S"""
  Conveniences and built-in formatter for logs.

  This modules defines a suitable `:logger` formatter which formats
  messages and reports as Elixir terms and also provides additional
  functionality, such as timezone conversion, truncation, and coloring.
  This formatter is used by default by `Logger` and you can configure it
  using:

      config :logger, :default_formatter,
        format: "\n$time $metadata[$level] $message\n",
        metadata: [:user_id]

  See `Logger.Formatter.new/1` for all configuration options.

  You can also build your own instances of this formatter by calling
  `new/1` and setting at the formatter of any `:logger` handler by
  settings its `:formatter` key to `Logger.Formatter.new(options)`.

  This module also provides several conveniences for those who wish
  to [write their custom logger formatters](https://www.erlang.org/doc/apps/kernel/logger_chapter.html#formatters).

  ## Formatting

  The log messages can be controlled by a formatting string. Here is
  an example:

      config :logger, :default_formatter,
        format: "\n$time $metadata[$level] $message\n",
        metadata: [:user_id]

  The above will print error messages as:

      18:43:12.439 user_id=13 [error] Hello\n

  The valid parameters you can use are:

    * `$time`     - the time the log message was sent
    * `$date`     - the date the log message was sent
    * `$message`  - the log message
    * `$level`    - the log level
    * `$node`     - the node that prints the message
    * `$metadata` - user controlled data presented in `"key=val key2=val2 "` format

  ### Formatting function

  You can also customize the format of your log messages with
  a `{module, function_name}` tuple if you wish to change how messages
  are formatted but keep all other features provided by `Logger.Formatter`
  such as truncation and coloring. However, if you want to get full
  control of formatting, consider writing a custom
  [`:logger` formatter](https://www.erlang.org/doc/apps/kernel/logger_chapter.html#formatters)
  instead, which has complete access to all events and metadata.

  When using a `{module, function_name}`, the function will be invoked
  with the level, the message, the timestamp, and metadata, like this:

      defmodule MyConsoleLogger do
        @spec format(atom, chardata, Logger.Formatter.date_time_ms(), keyword()) :: IO.chardata()
        def format(level, message, timestamp, metadata) do
          # Custom formatting logic that must return chardata.
          # ...
        end
      end

  ### Metadata

  Metadata to be sent to the logger can be read and written with
  the `Logger.metadata/0` and `Logger.metadata/1` functions. For example,
  you can set `Logger.metadata([user_id: 13])` to add user_id metadata
  to the current process. The user can configure the backend to choose
  which metadata it wants to print and it will replace the `$metadata`
  value.

  > #### When is user metadata printed? {: .warning}
  >
  > The default Logger formatter requires the user's metadata to meet
  > one of the following conditions to be printed:
  >
  >   * Be a string (`is_binary/1`)
  >   * Be a number (either `is_integer/1` or `is_float/1`)
  >   * Be a PID
  >   * Be an atom
  >   * Be a reference
  >   * Be a port
  >   * Implement the `String.Chars` protocol
  >
  > If none of the conditions above are `true`, the given metadata get
  > discarded.
  """

  @type date :: {1970..10000, 1..12, 1..31}
  @type time_ms :: {0..23, 0..59, 0..59, 0..999}
  @type date_time_ms :: {date, time_ms}

  @type pattern :: :date | :level | :levelpad | :message | :metadata | :node | :time
  @valid_patterns [:time, :date, :message, :level, :node, :metadata, :levelpad]
  @default_pattern "\n$time $metadata[$level] $message\n"
  @replacement "�"

  ## Formatter API

  defstruct [:template, :truncate, :metadata, :colors, :utc_log?]

  @doc ~S"""
  Initializes a formatter for `:logger` handlers.

  The supported options are:

    * `:colors` - a keyword list of coloring options.

    * `:format` - the format message used to print logs.
      Defaults to: `"\n$time $metadata[$level] $message\n"`.
      It may also be a `{module, function_name}` tuple that is invoked
      with the log level, the message, the current timestamp and
      the metadata and must return `t:IO.chardata/0`.
      See the module docs for more information on `:format`.

    * `:metadata` - a list of metadata keys to be printed by
      `$metadata`. Defaults to an empty list (no metadata).
      Setting `:metadata` to `:all` prints all metadata. See
      the "Metadata" section in the `Logger` documentation for
      more information.

    * `:truncate` - the maximum message size to be logged (in bytes).
      Defaults to 8192 bytes. Note this configuration is approximate.
      Truncated messages will have `" (truncated)"` at the end.
      The atom `:infinity` can be passed to disable this behavior.

    * `:utc_log` - when `true`, uses UTC in logs. By default it uses
      local time (as it defaults to `false`).

  The supported keys in the `:colors` keyword list are:

    * `:enabled` - boolean value that allows for switching the
      coloring on and off. Defaults to: `IO.ANSI.enabled?/0`

    * `:debug` - color for debug messages. Defaults to: `:cyan`

    * `:info` - color for info and notice messages. Defaults to: `:normal`

    * `:warning` - color for warning messages. Defaults to: `:yellow`

    * `:error` - color for error and higher messages. Defaults to: `:red`

  See the `IO.ANSI` module for a list of colors and attributes.
  The color of the message can also be configured per message via
  the `:ansi_color` metadata.
  """
  @spec new(keyword) :: formatter when formatter: term
  def new(options \\ []) do
    template = compile(options[:format])
    colors = colors(options[:colors] || [])
    truncate = options[:truncate] || Application.fetch_env!(:logger, :truncate)
    metadata = options[:metadata] || []
    utc_log? = Keyword.get(options, :utc_log, Application.fetch_env!(:logger, :utc_log))

    {__MODULE__,
     %__MODULE__{
       template: template,
       truncate: truncate,
       metadata: metadata,
       colors: colors,
       utc_log?: utc_log?
     }}
  end

  defp colors(colors) do
    warning =
      Keyword.get_lazy(colors, :warning, fn ->
        # TODO: Deprecate :warn option on Elixir v1.19
        if warn = Keyword.get(colors, :warn) do
          warn
        else
          :yellow
        end
      end)

    %{
      emergency: Keyword.get(colors, :error, :red),
      alert: Keyword.get(colors, :error, :red),
      critical: Keyword.get(colors, :error, :red),
      error: Keyword.get(colors, :error, :red),
      warning: warning,
      notice: Keyword.get(colors, :info, :normal),
      info: Keyword.get(colors, :info, :normal),
      debug: Keyword.get(colors, :debug, :cyan),
      enabled: Keyword.get(colors, :enabled, IO.ANSI.enabled?())
    }
  end

  @doc false
  def format(%{level: level, meta: meta} = event, %__MODULE__{} = config) do
    %{
      utc_log?: utc_log?,
      metadata: metadata_keys,
      template: template,
      colors: colors,
      truncate: truncate
    } = config

    system_time =
      case meta do
        %{time: time} when is_integer(time) and time >= 0 -> time
        _ -> :os.system_time(:microsecond)
      end

    date_time_ms = system_time_to_date_time_ms(system_time, utc_log?)

    meta_list =
      case metadata_keys do
        :all -> Map.to_list(meta)
        keys -> for key <- keys, value = compute_meta(key, meta), do: {key, value}
      end

    chardata = format_event(event, truncate)

    template
    |> format(level, chardata, date_time_ms, meta_list)
    |> colorize(level, colors, meta)
  end

  def format(_event, _config) do
    raise "invalid configuration for Logger.Formatter. " <>
            "Use Logger.Formatter.new/1 to define a formatter"
  end

  defp compute_meta(:module, %{mfa: {mod, _, _}}), do: mod
  defp compute_meta(:function, %{mfa: {_, fun, arity}}), do: format_fa(fun, arity)
  defp compute_meta(key, meta), do: meta[key]

  defp format_fa(fun, arity), do: [Atom.to_string(fun), "/", Integer.to_string(arity)]

  defp colorize(data, _level, %{enabled: false}, _md), do: data

  defp colorize(data, level, %{enabled: true} = colors, md) do
    color = md[:ansi_color] || Map.fetch!(colors, level)
    [IO.ANSI.format_fragment(color, true), data | IO.ANSI.reset()]
  end

  @doc """
  Formats the message of a log event.
  """
  @spec format_event(:logger.log_event(), pos_integer | :infinity) :: IO.chardata()
  def format_event(%{msg: msg, meta: meta} = _log_event, truncate) do
    format_message(msg, meta, truncate)
  end

  defp format_message({:string, message}, _metadata, truncate) do
    wrapped_truncate(message, truncate)
  end

  defp format_message({:report, data}, %{report_cb: callback} = meta, truncate) do
    cond do
      is_function(callback, 1) and callback != (&:logger.format_otp_report/1) ->
        format_message(callback.(data), meta, truncate)

      is_function(callback, 2) ->
        callback.(data, %{depth: :unlimited, chars_limit: truncate, single_line: false})

      true ->
        format_report(data, truncate)
    end
  end

  defp format_message({:report, data}, _meta, truncate) do
    format_report(data, truncate)
  end

  defp format_message({format, args}, _meta, truncate) do
    format
    |> Logger.Utils.scan_inspect(args, truncate)
    |> :io_lib.build_text()
    |> wrapped_truncate(truncate)
  end

  defp format_report(%{} = data, truncate) do
    wrapped_truncate(Kernel.inspect(Map.to_list(data), translator_inspect_opts()), truncate)
  end

  defp format_report(data, truncate) do
    wrapped_truncate(Kernel.inspect(data, translator_inspect_opts()), truncate)
  end

  defp translator_inspect_opts() do
    Application.fetch_env!(:logger, :translator_inspect_opts)
  end

  defp wrapped_truncate(data, n) when is_binary(data), do: truncate(data, n)

  defp wrapped_truncate(data, n) when is_list(data) do
    truncate(data, n)
  rescue
    msg in ArgumentError -> Exception.message(msg)
  end

  @doc """
  Truncates a `chardata` into `n` bytes.

  There is a chance we truncate in the middle of a grapheme
  cluster but we never truncate in the middle of a binary
  code point. For this reason, truncation is not exact.
  """
  @spec truncate(IO.chardata(), non_neg_integer | :infinity) :: IO.chardata()
  def truncate(chardata, :infinity) when is_binary(chardata) or is_list(chardata) do
    chardata
  end

  def truncate(chardata, n) when n >= 0 do
    {chardata, n} = Logger.Utils.truncate_n(chardata, n)
    if n >= 0, do: chardata, else: [chardata, " (truncated)"]
  end

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
  Formats time as chardata.
  """
  @spec format_time(time_ms) :: IO.chardata()
  def format_time({hh, mi, ss, ms} = _time_ms_tuple) do
    [pad2(hh), ?:, pad2(mi), ?:, pad2(ss), ?., pad3(ms)]
  end

  @doc """
  Formats date as chardata.
  """
  @spec format_date(date) :: IO.chardata()
  def format_date({yy, mm, dd} = _date_tuple) do
    [Integer.to_string(yy), ?-, pad2(mm), ?-, pad2(dd)]
  end

  defp pad3(int) when int < 10, do: [?0, ?0, Integer.to_string(int)]
  defp pad3(int) when int < 100, do: [?0, Integer.to_string(int)]
  defp pad3(int), do: Integer.to_string(int)

  defp pad2(int) when int < 10, do: [?0, Integer.to_string(int)]
  defp pad2(int), do: Integer.to_string(int)

  @doc """
  Converts the system time (in microseconds) from metadata into a `date_time_ms` tuple.
  """
  @spec system_time_to_date_time_ms(integer(), boolean()) :: date_time_ms()
  def system_time_to_date_time_ms(system_time, utc_log? \\ false) do
    micro = rem(system_time, 1_000_000)

    {date, {hours, minutes, seconds}} =
      case utc_log? do
        true -> :calendar.system_time_to_universal_time(system_time, :microsecond)
        false -> :calendar.system_time_to_local_time(system_time, :microsecond)
      end

    {date, {hours, minutes, seconds, div(micro, 1000)}}
  end

  @doc """
  Compiles a pattern or function into a data structure that `format/5` can handle.

  Check the module doc for documentation on the valid parameters that
  will be interpolated in the pattern. If you pass `nil` as the pattern,
  the pattern defaults to:

      #{inspect(@default_pattern)}

  If you want to customize formatting with a custom function, you can
  pass a `{module, function_name}` tuple.

  This function, alongside `format/5`, is the main building block used
  by `Logger.Formatter.new/1` for formatting messages. It can also be used
  by those interested in building custom formatters.

  ## Examples

      iex> Logger.Formatter.compile("$time $metadata [$level] $message\\n")
      [:time, " ", :metadata, " [", :level, "] ", :message, "\\n"]

      iex> Logger.Formatter.compile({MyLoggerFormatter, :format})
      {MyLoggerFormatter, :format}

  """
  @spec compile(binary | nil) :: [pattern | binary]
  @spec compile(pattern) :: pattern when pattern: {module, function :: atom}
  def compile(pattern_or_function)

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
  Formats a `pattern_or_function` returned by `compile/1`.

  It takes a compiled format and injects the level, timestamp, message,
  and metadata keyword list and returns a properly formatted string.

  If `pattern_or_function` is a `{module, function_name}` tuple,
  then `module.function_name(level, message, timestamp, metadata)` is
  invoked to get the message.

  This function, alongside `compile/1`, is the main building block used
  by `Logger.Formatter.new/1` for formatting messages. It can also be used
  by those interested in building custom formatters.

  ## Examples

      iex> pattern = Logger.Formatter.compile("[$level] $message")
      iex> timestamp = {{1977, 01, 28}, {13, 29, 00, 000}}
      iex> formatted = Logger.Formatter.format(pattern, :info, "hello", timestamp, [])
      iex> IO.chardata_to_string(formatted)
      "[info] hello"

  """
  @spec format(
          mod_and_fun | [pattern | binary],
          Logger.level(),
          Logger.message(),
          date_time_ms(),
          keyword
        ) ::
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
      binary -> binary
    end
  end

  defp metadata(_, ref) when is_reference(ref) do
    ~c"#Ref" ++ rest = :erlang.ref_to_list(ref)
    rest
  end

  defp metadata(_, port) when is_port(port) do
    ~c"#Port" ++ rest = :erlang.port_to_list(port)
    rest
  end

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

  defp metadata(:function, function) when is_list(function), do: function
  defp metadata(:file, file) when is_list(file), do: file
  defp metadata(_, list) when is_list(list), do: nil

  defp metadata(_, other) do
    case String.Chars.impl_for(other) do
      nil -> nil
      impl -> impl.to_string(other)
    end
  end
end
