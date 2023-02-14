import Kernel, except: [inspect: 2]

defmodule Logger.Formatter do
  # TODO: Rewrite docs

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

  @type date :: {1970..10000, 1..12, 1..31}
  @type time_ms :: {0..23, 0..59, 0..59, 0..999}
  @type date_time_ms :: {date, time_ms}

  @type pattern :: :date | :level | :levelpad | :message | :metadata | :node | :time
  @valid_patterns [:time, :date, :message, :level, :node, :metadata, :levelpad]
  @default_pattern "\n$time $metadata[$level] $message\n"
  @replacement "�"

  defstruct [:template, :truncate, :metadata, :colors, :utc_log?]

  @doc """
  Initializes a formatter.

  TODO options
  """
  def new(options \\ []) do
    # TODO: deprecate passing a two-element tuple
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

  def format(%{level: level, msg: msg, meta: meta}, %__MODULE__{} = config) do
    %{
      utc_log?: utc_log?,
      metadata: metadata_keys,
      template: template,
      colors: colors,
      truncate: truncate
    } = config

    system_time = Map.get_lazy(meta, :time, fn -> :os.system_time(:microsecond) end)
    date_time_ms = system_time_to_date_time_ms(system_time, utc_log?)

    meta_list =
      case metadata_keys do
        :all -> Map.to_list(meta)
        keys -> for key <- keys, value = compute_meta(key, meta), do: {key, value}
      end

    msg = format_message(msg, meta, truncate)

    template
    # TODO: Move format/5 and compile/1 to Logger.Backends and,
    # in this function, check specifically for pairs and delegate
    # there.
    # TODO: Introduce format_metadata and format_template as building blocs here
    |> format(level, msg, date_time_ms, meta_list)
    |> color_event(level, colors, meta)
  end

  def format(_event, _config) do
    raise "invalid configuration for Logger.Formatter. " <>
            "Use Logger.Formatter.init/1 to define a formatter"
  end

  defp compute_meta(:module, %{mfa: {mod, _, _}}), do: mod
  defp compute_meta(:function, %{mfa: {_, fun, arity}}), do: format_fa(fun, arity)
  defp compute_meta(key, meta), do: meta[key]

  defp format_fa(fun, arity), do: [Atom.to_string(fun), "/", Integer.to_string(arity)]

  defp color_event(data, _level, %{enabled: false}, _md), do: data

  defp color_event(data, level, %{enabled: true} = colors, md) do
    color = md[:ansi_color] || Map.fetch!(colors, level)
    [IO.ANSI.format_fragment(color, true), data | IO.ANSI.reset()]
  end

  @doc """
  Formats the `:msg` key of a log event.

  It also requires its `:meta` key and a truncate value.
  """
  def format_message(msg, meta, truncate)

  def format_message({:string, message}, _metadata, truncate) do
    wrapped_truncate(message, truncate)
  end

  def format_message({:report, data}, %{report_cb: callback} = meta, truncate) do
    cond do
      is_function(callback, 1) and callback != (&:logger.format_otp_report/1) ->
        format_message(callback.(data), meta, truncate)

      is_function(callback, 2) ->
        callback.(data, %{depth: :unlimited, chars_limit: truncate, single_line: false})

      true ->
        format_report(data, truncate)
    end
  end

  def format_message({:report, data}, _meta, truncate) do
    format_report(data, truncate)
  end

  def format_message({format, args}, _meta, truncate) do
    format
    |> scan_inspect(args, truncate)
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
  @spec truncate(IO.chardata(), non_neg_integer) :: IO.chardata()
  def truncate(chardata, :infinity) when is_binary(chardata) or is_list(chardata) do
    chardata
  end

  def truncate(chardata, n) when n >= 0 do
    {chardata, n} = truncate_n(chardata, n)
    if n >= 0, do: chardata, else: [chardata, " (truncated)"]
  end

  defp truncate_n(_, n) when n < 0 do
    {"", n}
  end

  defp truncate_n(binary, n) when is_binary(binary) do
    remaining = n - byte_size(binary)

    if remaining < 0 do
      # There is a chance we are cutting at the wrong
      # place so we need to fix the binary.
      {fix_binary(binary_part(binary, 0, n)), remaining}
    else
      {binary, remaining}
    end
  end

  defp truncate_n(int, n) when int in 0..127, do: {int, n - 1}
  defp truncate_n(int, n) when int in 127..0x07FF, do: {int, n - 2}
  defp truncate_n(int, n) when int in 0x800..0xFFFF, do: {int, n - 3}
  defp truncate_n(int, n) when int >= 0x10000 and is_integer(int), do: {int, n - 4}

  defp truncate_n(list, n) when is_list(list) do
    truncate_n_list(list, n, [])
  end

  defp truncate_n(other, _n) do
    raise ArgumentError,
          "cannot truncate chardata because it contains something that is not " <>
            "valid chardata: #{inspect(other)}"
  end

  defp truncate_n_list(_, n, acc) when n < 0 do
    {:lists.reverse(acc), n}
  end

  defp truncate_n_list([h | t], n, acc) do
    {h, n} = truncate_n(h, n)
    truncate_n_list(t, n, [h | acc])
  end

  defp truncate_n_list([], n, acc) do
    {:lists.reverse(acc), n}
  end

  defp truncate_n_list(t, n, acc) do
    {t, n} = truncate_n(t, n)
    {:lists.reverse(acc, t), n}
  end

  defp fix_binary(binary) do
    # Use a thirteen-bytes offset to look back in the binary.
    # This should allow at least two code points of 6 bytes.
    suffix_size = min(byte_size(binary), 13)
    prefix_size = byte_size(binary) - suffix_size
    <<prefix::binary-size(prefix_size), suffix::binary-size(suffix_size)>> = binary
    prefix <> fix_binary(suffix, "")
  end

  defp fix_binary(<<h::utf8, t::binary>>, acc) do
    acc <> <<h::utf8>> <> fix_binary(t, "")
  end

  defp fix_binary(<<h, t::binary>>, acc) do
    fix_binary(t, <<acc::binary, h>>)
  end

  defp fix_binary(<<>>, _acc) do
    <<>>
  end

  @doc """
  Receives a format string and arguments, scans them, and then replace `~p`,
  `~P`, `~w` and `~W` by its inspected variants.

  For information about format scanning and how to consume them,
  check `:io_lib.scan_format/2`.
  """
  def scan_inspect(format, args, truncate, opts \\ %Inspect.Opts{})

  def scan_inspect(format, args, truncate, opts) when is_atom(format) do
    scan_inspect(Atom.to_charlist(format), args, truncate, opts)
  end

  def scan_inspect(format, args, truncate, opts) when is_binary(format) do
    scan_inspect(:binary.bin_to_list(format), args, truncate, opts)
  end

  def scan_inspect(format, [], _truncate, _opts) when is_list(format) do
    :io_lib.scan_format(format, [])
  end

  def scan_inspect(format, args, truncate, opts) when is_list(format) do
    # A pre-pass that removes binaries from
    # arguments according to the truncate limit.
    {args, _} =
      Enum.map_reduce(args, truncate, fn arg, acc ->
        if is_binary(arg) and acc != :infinity do
          truncate_n(arg, acc)
        else
          {arg, acc}
        end
      end)

    format
    |> :io_lib.scan_format(args)
    |> Enum.map(&handle_format_spec(&1, opts))
  end

  @inspected_format_spec %{
    adjust: :right,
    args: [],
    control_char: ?s,
    encoding: :unicode,
    pad_char: ?\s,
    precision: :none,
    strings: true,
    width: :none
  }

  defp handle_format_spec(%{control_char: char} = spec, opts) when char in ~c"wWpP" do
    %{args: args, width: width, strings: strings?} = spec

    opts = %{
      opts
      | charlists: inspect_charlists(strings?, opts),
        limit: inspect_limit(char, args, opts),
        width: inspect_width(char, width)
    }

    %{@inspected_format_spec | args: [inspect_data(args, opts)]}
  end

  defp handle_format_spec(spec, _opts), do: spec

  defp inspect_charlists(false, _), do: :as_lists
  defp inspect_charlists(_, opts), do: opts.charlists

  defp inspect_limit(char, [_, limit], _) when char in ~c"WP", do: limit
  defp inspect_limit(_, _, opts), do: opts.limit

  defp inspect_width(char, _) when char in ~c"wW", do: :infinity
  defp inspect_width(_, width), do: width

  defp inspect_data([data | _], opts) do
    width = if opts.width == :none, do: 80, else: opts.width

    data
    |> Inspect.Algebra.to_doc(opts)
    |> Inspect.Algebra.format(width)
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
  def format_time({hh, mi, ss, ms}) do
    [pad2(hh), ?:, pad2(mi), ?:, pad2(ss), ?., pad3(ms)]
  end

  @doc """
  Formats date as chardata.
  """
  @spec format_date(date) :: IO.chardata()
  def format_date({yy, mm, dd}) do
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

  ## OLD API.

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
