defmodule Logger do
  @moduledoc ~S"""
  A logger for Elixir applications.

  It includes many features:

    * Provides debug, info, warn, and error levels.

    * Supports multiple backends which are automatically
      supervised when plugged into `Logger`.

    * Formats and truncates messages on the client
      to avoid clogging `Logger` backends.

    * Alternates between sync and async modes to remain
      performant when required but also apply backpressure
      when under stress.

    * Plugs into Erlang's [`:logger`](http://erlang.org/doc/man/logger.html)
      (from Erlang/OTP 21) to convert terms to Elixir syntax or wraps
      Erlang's [`:error_logger`](http://erlang.org/doc/man/error_logger.html)
      in earlier Erlang/OTP versions to prevent it from overflowing.

  Logging is useful for tracking when an event of interest happens in your
  system. For example, it may be helpful to log whenever a user is deleted.

      def delete_user(user) do
        Logger.info("Deleting user from the system: #{inspect(user)}")
        # ...
      end

  The `Logger.info/2` macro emits the provided message at the `:info`
  level. Note the arguments given to `info/2` will only be evaluated
  if a message is logged. For instance, if the Logger level is
  set to `:warn`, `:info` messages are never logged and therefore the
  arguments given above won't even be executed.

  There are additional macros for other levels.

  Logger also allows log commands to be removed altogether via the
  `:compile_time_purge_matching` option (see below).

  For dynamically logging messages, see `bare_log/3`. But note that
  `bare_log/3` always evaluates its arguments (unless the argument
  is an anonymous function).

  ## Levels

  The supported levels, ordered by precedence, are:

    * `:debug` - for debug-related messages
    * `:info` - for information of any kind
    * `:warn` - for warnings
    * `:error` - for errors

  For example, `:info` takes precedence over `:debug`. If your log
  level is set to `:info`, `:info`, `:warn`, and `:error` will be
  printed to the console. If your log level is set to `:warn`, only
  `:warn` and `:error` will be printed.

  ## Configuration

  `Logger` supports a wide range of configurations.

  This configuration is split in three categories:

    * Application configuration - must be set before the `:logger`
      application is started

    * Runtime configuration - can be set before the `:logger`
      application is started, but may be changed during runtime

    * Erlang configuration - options that handle integration with
      Erlang's logging facilities

  ### Application configuration

  The following configuration must be set via config files (such as
  `config/config.exs`) before the `:logger` application is started.

    * `:backends` - the backends to be used. Defaults to `[:console]`.
      See the "Backends" section for more information.

    * `:compile_time_application` - sets the `:application` metadata value
      to the configured value at compilation time. This configuration is
      usually only useful for build tools to automatically add the
      application to the metadata for `Logger.debug/2`, `Logger.info/2`, etc.
      style of calls.

    * `:compile_time_purge_matching` - purges *at compilation time* all calls
      that match the given conditions. This means that `Logger` calls with
      level lower than this option will be completely removed at compile time,
      accruing no overhead at runtime. This configuration expects a list of
      keyword lists. Each keyword list contains a metadata key and the matching
      value that should be purged. A special key named `:level_lower_than` can
      be used to purge all messages with a lower logger level. Remember that
      if you want to purge log calls from a dependency, the dependency must be
      recompiled.

    * `:start_options` - passes start options to Logger's main process, such
      as `:spawn_opt` and `:hibernate_after`. All options in `t:GenServer.option`
      are accepted, except by `:name`.

  For example, to configure the `:backends` and purge all calls that happen
  at compile time with level lower than `:info` in a `config/config.exs` file:

      config :logger,
        backends: [:console],
        compile_time_purge_matching: [
          [level_lower_than: :info]
        ]

  If you want to purge all log calls from an application named `:foo` and only
  keep errors from `Bar.foo/3`, you can set up two different matches:

      config :logger,
        compile_time_purge_matching: [
          [application: :foo],
          [module: Bar, function: "foo/3", level_lower_than: :error]
        ]

  ### Runtime Configuration

  All configuration below can be set via config files (such as
  `config/config.exs`) but also changed dynamically during runtime via
  `Logger.configure/1`.

    * `:level` - the logging level. Attempting to log any message
      with severity less than the configured level will simply
      cause the message to be ignored. Keep in mind that each backend
      may have its specific level, too.

    * `:utc_log` - when `true`, uses UTC in logs. By default it uses
      local time (i.e., it defaults to `false`).

    * `:truncate` - the maximum message size to be logged (in bytes).
      Defaults to 8192 bytes. Note this configuration is approximate.
      Truncated messages will have `" (truncated)"` at the end.
      The atom `:infinity` can be passed to disable this behavior.

    * `:sync_threshold` - if the `Logger` manager has more than
      `:sync_threshold` messages in its queue, `Logger` will change
      to *sync mode*, to apply backpressure to the clients.
      `Logger` will return to *async mode* once the number of messages
      in the queue is reduced to one below the `sync_threshold`.
      Defaults to 20 messages. `:sync_threshold` can be set to `0` to
      force *sync mode*.

    * `:discard_threshold` - if the `Logger` manager has more than
      `:discard_threshold` messages in its queue, `Logger` will change
      to *discard mode* and messages will be discarded directly in the
      clients. `Logger` will return to *sync mode* once the number of
      messages in the queue is reduced to one below the `discard_threshold`.
      Defaults to 500 messages.

    * `:discard_threshold_periodic_check` - a periodic check that
      checks and reports if logger is discarding messages. It logs a warn
      message whenever the system is (or continues) in discard mode and
      it logs a warn message whenever if the system was discarding messages
      but stopped doing so after the previous check. By default it runs
      every `30_000` milliseconds.

    * `:translator_inspect_opts` - when translating OTP reports and
      errors, the last message and state must be inspected in the
      error reports. This configuration allow developers to change
      how much and how the data should be inspected.

  For example, to configure the `:level` and `:truncate` options in a
  `config/config.exs` file:

      config :logger,
        level: :warn,
        truncate: 4096

  ### Error logger configuration

  The following configuration applies to `Logger`'s wrapper around
  Erlang's logging functionalities. All the configurations below must
  be set before the `:logger` application starts.

    * `:handle_otp_reports` - redirects OTP reports to `Logger` so
      they are formatted in Elixir terms. This effectively disables
      Erlang standard logger. Defaults to `true`.

    * `:handle_sasl_reports` - redirects supervisor, crash and
      progress reports to `Logger` so they are formatted in Elixir
      terms. Your application must guarantee `:sasl` is started before
      `:logger`. This means you may see some initial reports written
      in Erlang syntax until the Logger application kicks in.
      Defaults to `false`.

  From Erlang/OTP 21, `:handle_sasl_reports` only has an effect if
  `:handle_otp_reports` is true.

  The following configurations apply only for Erlang/OTP 20 and earlier:

    * `:discard_threshold_for_error_logger` - if `:error_logger` has more than
      `discard_threshold` messages in its inbox, messages will be dropped
      until the message queue goes down to `discard_threshold * 0.75`
      entries. The threshold will be checked once again after 10% of threshold
      messages are processed, to avoid messages from being constantly dropped.
      For example, if the threshold is 500 (the default) and the inbox has
      600 messages, 225 messages will dropped, bringing the inbox down to
      375 (0.75 * threshold) entries and 50 (0.1 * threshold) messages will
      be processed before the threshold is checked once again.

  For example, to configure `Logger` to redirect all Erlang messages using a
  `config/config.exs` file:

      config :logger,
        handle_otp_reports: true,
        handle_sasl_reports: true

  Furthermore, `Logger` allows messages sent by Erlang to be translated
  into an Elixir format via translators. Translators can be added at any
  time with the `add_translator/1` and `remove_translator/1` APIs. Check
  `Logger.Translator` for more information.

  ## Backends

  `Logger` supports different backends where log messages are written to.

  The available backends by default are:

    * `:console` - logs messages to the console (enabled by default)

  Developers may also implement their own backends, an option that
  is explored in more detail below.

  The initial backends are loaded via the `:backends` configuration,
  which must be set before the `:logger` application is started.
  Backends can also be added dynamically through `add_backend/2`.

  ### Console backend

  The console backend logs messages by printing them to the console. It supports
  the following options:

    * `:level` - the level to be logged by this backend.
      Note that messages are filtered by the general
      `:level` configuration for the `:logger` application first.

    * `:format` - the format message used to print logs.
      Defaults to: `"\n$time $metadata[$level] $levelpad$message\n"`.
      It may also be a `{module, function}` tuple that is invoked
      with the log level, the message, the current timestamp and
      the metadata.

    * `:metadata` - the metadata to be printed by `$metadata`.
      Defaults to an empty list (no metadata).
      Setting `:metadata` to `:all` prints all metadata. See
      the "Metadata" section for more information.

    * `:colors` - a keyword list of coloring options.

    * `:device` - the device to log error messages to. Defaults to
      `:user` but can be changed to something else such as `:standard_error`.

    * `:max_buffer` - maximum events to buffer while waiting
      for a confirmation from the IO device (default: 32).
      Once the buffer is full, the backend will block until
      a confirmation is received.

  The supported keys in the `:colors` keyword list are:

    * `:enabled` - boolean value that allows for switching the
      coloring on and off. Defaults to: `IO.ANSI.enabled?/0`

    * `:debug` - color for debug messages. Defaults to: `:cyan`

    * `:info` - color for info messages. Defaults to: `:normal`

    * `:warn` - color for warn messages. Defaults to: `:yellow`

    * `:error` - color for error messages. Defaults to: `:red`

  See the `IO.ANSI` module for a list of colors and attributes.

  Here is an example of how to configure the `:console` backend in a
  `config/config.exs` file:

      config :logger, :console,
        format: "\n$time $metadata[$level] $levelpad$message\n",
        metadata: [:user_id]

  ### Custom formatting

  The console backend allows you to customize the format of your log messages
  with the `:format` option.

  You may set `:format` to either a string or a `{module, function}` tuple if
  you wish to provide your own format function. Here is an example of how to
  configure the `:console` backend in a `config/config.exs` file:

      config :logger, :console,
        format: {MyConsoleLogger, :format}

  And here is an example of how you can define `MyConsoleLogger.format/4` from the
  above configuration:

      defmodule MyConsoleLogger do
        def format(level, message, timestamp, metadata) do
          # Custom formatting logic...
        end
      end

  It is extremely important that **the formatting function does not fail**, as
  it will bring that particular logger instance down, causing your system to
  temporarily lose messages. If necessary, wrap the function in a `rescue` and
  log a default message instead:

      defmodule MyConsoleLogger do
        def format(level, message, timestamp, metadata) do
          # Custom formatting logic...
        rescue
          _ -> "could not format: #{inspect({level, message, metadata})}"
        end
      end

  The `{module, function}` will be invoked with four arguments:

    * the log level: an atom
    * the message: this is usually chardata, but in some cases it may not be.
      Since the formatting function should *never* fail, you need to prepare for
      the message being anything (and do something like the `rescue` in the example
      above)
    * the current timestamp: a term of type `t:Logger.Formatter.time/0`
    * the metadata: a keyword list

  You can read more about formatting in `Logger.Formatter`, especially if you
  want to support custom formatting in a custom backend.

  ## Metadata

  In addition to the keys provided by the user via `Logger.metadata/1`,
  the following extra keys are available to the `:metadata` list:

    * `:application` - the current application

    * `:module` - the current module

    * `:function` - the current function

    * `:file` - the current file

    * `:line` - the current line

    * `:pid` - the current process identifier

    * `:crash_reason` - a two-element tuple with the throw/error/exit reason
      as first argument and the stacktrace as second. A throw will always be
      `{:nocatch, term}`. An error is always an `Exception` struct. All other
      entries are exits. The console backend ignores this metadata by default
      but it can be useful to other backends, such as the ones that report
      errors to third-party services

    * `:initial_call` - the initial call that started the process

    * `:registered_name` - the process registered name as an atom

  Note that all metadata is optional and may not always be available.
  The `:module`, `:function`, `:line`, and similar metadata are automatically
  included when using `Logger` macros. `Logger.bare_log/3` does not include
  any metadata beyond the `:pid` by default. Other metadata, such as
  `:crash_reason`, `:initial_call`, and `:registered_name` are extracted
  from Erlang/OTP crash reports and available only in those cases.

  ## Custom backends

  Any developer can create their own `Logger` backend.
  Since `Logger` is an event manager powered by `:gen_event`,
  writing a new backend is a matter of creating an event
  handler, as described in the [`:gen_event`](http://erlang.org/doc/man/gen_event.html)
  documentation.

  From now on, we will be using the term "event handler" to refer
  to your custom backend, as we head into implementation details.

  Once the `:logger` application starts, it installs all event handlers listed under
  the `:backends` configuration into the `Logger` event manager. The event
  manager and all added event handlers are automatically supervised by `Logger`.

  Note that if a backend fails to start by returning `{:error, :ignore}` from its
  `init/1` callback, then it's not added to the backends but nothing fails. If a
  backend fails to start by returning `{:error, reason}` from its `init/1` callback,
  the `:logger` application will fail to start.

  Once initialized, the handler should be designed to handle the following
  events:

    * `{level, group_leader, {Logger, message, timestamp, metadata}}` where:
      * `level` is one of `:debug`, `:info`, `:warn`, or `:error`, as previously
        described
      * `group_leader` is the group leader of the process which logged the message
      * `{Logger, message, timestamp, metadata}` is a tuple containing information
        about the logged message:
        * the first element is always the atom `Logger`
        * `message` is the actual message (as chardata)
        * `timestamp` is the timestamp for when the message was logged, as a
          `{{year, month, day}, {hour, minute, second, millisecond}}` tuple
        * `metadata` is a keyword list of metadata used when logging the message

    * `:flush`

  It is recommended that handlers ignore messages where
  the group leader is in a different node than the one where
  the handler is installed. For example:

      def handle_event({_level, gl, {Logger, _, _, _}}, state)
          when node(gl) != node() do
        {:ok, state}
      end

  In the case of the event `:flush` handlers should flush any pending data. This
  event is triggered by `flush/0`.

  Furthermore, backends can be configured via the
  `configure_backend/2` function which requires event handlers
  to handle calls of the following format:

      {:configure, options}

  where `options` is a keyword list. The result of the call is
  the result returned by `configure_backend/2`. The recommended
  return value for successful configuration is `:ok`. For example

      def handle_call({:configure, options}, state) do
        new_state = reconfigure_state(state, options)
        {:ok, :ok, new_state}
      end

  It is recommended that backends support at least the following
  configuration options:

    * `:level` - the logging level for that backend
    * `:format` - the logging format for that backend
    * `:metadata` - the metadata to include in that backend

  Check the implementation for `Logger.Backends.Console`, for
  examples on how to handle the recommendations in this section
  and how to process the existing options.
  """

  @type backend :: :gen_event.handler()
  @type message :: IO.chardata() | String.Chars.t()
  @type level :: :error | :info | :warn | :debug
  @type metadata :: keyword()
  @levels [:error, :info, :warn, :debug]

  @metadata :logger_metadata
  @compile {:inline, __metadata__: 0}

  defp __metadata__ do
    Process.get(@metadata) || {true, []}
  end

  @doc """
  Alters the current process metadata according the given keyword list.

  This function will merge the given keyword list into the existing metadata,
  with the exception of setting a key to `nil`, which will remove that key
  from the metadata.
  """
  @spec metadata(metadata) :: :ok
  def metadata(keyword) do
    {enabled?, metadata} = __metadata__()
    Process.put(@metadata, {enabled?, into_metadata(keyword, metadata)})
    :ok
  end

  defp into_metadata([], metadata), do: metadata
  defp into_metadata(keyword, metadata), do: into_metadata(keyword, [], metadata)

  defp into_metadata([{key, nil} | keyword], prepend, metadata) do
    into_metadata(keyword, prepend, :lists.keydelete(key, 1, metadata))
  end

  defp into_metadata([{key, _} = pair | keyword], prepend, metadata) do
    into_metadata(keyword, [pair | prepend], :lists.keydelete(key, 1, metadata))
  end

  defp into_metadata([], prepend, metadata) do
    prepend ++ metadata
  end

  @doc """
  Reads the current process metadata.
  """
  @spec metadata() :: metadata
  def metadata() do
    __metadata__() |> elem(1)
  end

  @doc """
  Resets the current process metadata to the given keyword list.
  """
  @spec reset_metadata(metadata) :: :ok
  def reset_metadata(keywords \\ []) do
    {enabled?, _metadata} = __metadata__()
    Process.put(@metadata, {enabled?, []})
    metadata(keywords)
  end

  @doc """
  Enables logging for the current process.

  Currently the only accepted PID is `self()`.
  """
  @spec enable(pid) :: :ok
  def enable(pid) when pid == self() do
    Process.put(@metadata, {true, metadata()})
    :ok
  end

  @doc """
  Disables logging for the current process.

  Currently the only accepted PID is `self()`.
  """
  @spec disable(pid) :: :ok
  def disable(pid) when pid == self() do
    Process.put(@metadata, {false, metadata()})
    :ok
  end

  @doc """
  Retrieves the `Logger` level.

  The `Logger` level can be changed via `configure/1`.
  """
  @spec level() :: level
  defdelegate level(), to: Logger.Config

  @doc """
  Compares log levels.

  Receives two log levels and compares the `left` level
  against the `right` level and returns:

    * `:lt` if `left` is less than `right`
    * `:eq` if `left` and `right` are equal
    * `:gt` if `left` is greater than `right`

  ## Examples

      iex> Logger.compare_levels(:debug, :warn)
      :lt
      iex> Logger.compare_levels(:error, :info)
      :gt

  """
  @spec compare_levels(level, level) :: :lt | :eq | :gt
  defdelegate compare_levels(left, right), to: Logger.Config

  @doc """
  Configures the logger.

  See the "Runtime Configuration" section in the `Logger` module
  documentation for the available options. The changes done here
  are automatically persisted to the `:logger` application
  environment.
  """
  @valid_options [
    :compile_time_application,
    :compile_time_purge_level,
    :compile_time_purge_matching,
    :sync_threshold,
    :truncate,
    :level,
    :utc_log,
    :discard_threshold,
    :translator_inspect_opts
  ]
  @spec configure(keyword) :: :ok
  def configure(options) do
    Logger.Config.configure(Keyword.take(options, @valid_options))
  end

  @doc """
  Flushes the logger.

  This guarantees all messages sent to `Logger` prior to this call will
  be processed. This is useful for testing and it should not be called
  in production code.
  """
  @spec flush :: :ok
  def flush do
    _ = Process.whereis(:error_logger) && :gen_event.which_handlers(:error_logger)
    :gen_event.sync_notify(Logger, :flush)
  end

  @doc """
  Adds a new backend.

  Adding a backend calls the `init/1` function in that backend
  with the name of the backend as its argument. For example,
  calling

      Logger.add_backend(MyBackend)

  will call `MyBackend.init(MyBackend)` to initialize the new
  backend. If the backend's `init/1` callback returns `{:ok, _}`,
  then this function returns `{:ok, pid}`. If the handler returns
  `{:error, :ignore}` from `init/1`, this function still returns
  `{:ok, pid}` but the handler is not started. If the handler
  returns `{:error, reason}` from `init/1`, this function returns
  `{:error, {reason, info}}` where `info` is more information on
  the backend that failed to start.

  Backends added by this function are not persisted. Therefore
  if the Logger application or supervision tree is restarted,
  the backend won't be available. If you need this guarantee,
  then configure the backend via the application environment:

      config :logger, :backends, [MyBackend]

  ## Options

    * `:flush` - when `true`, guarantees all messages currently sent
      to `Logger` are processed before the backend is added

  ## Examples

      {:ok, _pid} = Logger.add_backend(MyBackend, flush: true)

  """
  @spec add_backend(backend, keyword) :: Supervisor.on_start_child()
  def add_backend(backend, opts \\ []) do
    _ = if opts[:flush], do: flush()

    case Logger.BackendSupervisor.watch(backend) do
      {:ok, _} = ok ->
        ok

      {:error, {:already_started, _pid}} ->
        {:error, :already_present}

      {:error, _} = error ->
        error
    end
  end

  @doc """
  Removes a backend.

  ## Options

    * `:flush` - when `true`, guarantees all messages currently sent
      to `Logger` are processed before the backend is removed

  """
  @spec remove_backend(backend, keyword) :: :ok | {:error, term}
  def remove_backend(backend, opts \\ []) do
    _ = if opts[:flush], do: flush()
    Logger.BackendSupervisor.unwatch(backend)
  end

  @doc """
  Adds a new translator.
  """
  @spec add_translator({module, function :: atom}) :: :ok
  def add_translator({mod, fun} = translator) when is_atom(mod) and is_atom(fun) do
    Logger.Config.add_translator(translator)
  end

  @doc """
  Removes a translator.
  """
  @spec remove_translator({module, function :: atom}) :: :ok
  def remove_translator({mod, fun} = translator) when is_atom(mod) and is_atom(fun) do
    Logger.Config.remove_translator(translator)
  end

  @doc """
  Configures the given backend.

  The backend needs to be started and running in order to
  be configured at runtime.
  """
  @spec configure_backend(backend, keyword) :: term
  def configure_backend(backend, options) when is_list(options) do
    backend = Logger.BackendSupervisor.translate_backend(backend)
    :gen_event.call(Logger, backend, {:configure, options})
  end

  @doc """
  Logs a message dynamically.

  Opposite to `log/3`, `debug/2`, `info/2`, and friends, the arguments
  given to `bare_log/3` are always evaluated. However, you can pass
  anonymous functions to `bare_log/3` and they will only be evaluated
  if there is something to be logged.
  """
  @spec bare_log(level, message | (() -> message | {message, keyword}), keyword) ::
          :ok | {:error, :noproc} | {:error, term}
  def bare_log(level, chardata_or_fun, metadata \\ []) do
    case __should_log__(level) do
      :error -> :ok
      info -> __do_log__(info, chardata_or_fun, metadata)
    end
  end

  @doc false
  def __should_log__(level) when level in @levels do
    case __metadata__() do
      {true, pdict} ->
        case Logger.Config.log_data(level) do
          {:discard, _config} -> :error
          {mode, config} -> {level, mode, config, pdict}
        end

      {false, _} ->
        :error
    end
  end

  @doc false
  def __do_log__({level, mode, config, pdict}, chardata_or_fun, metadata)
      when is_list(metadata) do
    %{utc_log: utc_log?, truncate: truncate} = config
    metadata = [pid: self()] ++ into_metadata(metadata, pdict)

    case normalize_message(chardata_or_fun, metadata) do
      {message, metadata} ->
        tuple = {Logger, truncate(message, truncate), Logger.Utils.timestamp(utc_log?), metadata}

        try do
          notify(mode, {level, Process.group_leader(), tuple})
          :ok
        rescue
          ArgumentError -> {:error, :noproc}
        catch
          :exit, reason -> {:error, reason}
        end

      :skip ->
        :ok
    end
  end

  @doc """
  Logs a warning message.

  Returns `:ok` or an `{:error, reason}` tuple.

  ## Examples

      Logger.warn("knob turned too far to the right")
      Logger.warn(fn -> "dynamically calculated warning" end)
      Logger.warn(fn -> {"dynamically calculated warning", [additional: :metadata]} end)

  """
  defmacro warn(chardata_or_fun, metadata \\ []) do
    maybe_log(:warn, chardata_or_fun, metadata, __CALLER__)
  end

  @doc """
  Logs an info message.

  Returns `:ok` or an `{:error, reason}` tuple.

  ## Examples

      Logger.info("mission accomplished")
      Logger.info(fn -> "dynamically calculated info" end)
      Logger.info(fn -> {"dynamically calculated info", [additional: :metadata]} end)

  """
  defmacro info(chardata_or_fun, metadata \\ []) do
    maybe_log(:info, chardata_or_fun, metadata, __CALLER__)
  end

  @doc """
  Logs an error message.

  Returns `:ok` or an `{:error, reason}` tuple.

  ## Examples

      Logger.error("oops")
      Logger.error(fn -> "dynamically calculated error" end)
      Logger.error(fn -> {"dynamically calculated error", [additional: :metadata]} end)

  """
  defmacro error(chardata_or_fun, metadata \\ []) do
    maybe_log(:error, chardata_or_fun, metadata, __CALLER__)
  end

  @doc """
  Logs a debug message.

  Returns `:ok` or an `{:error, reason}` tuple.

  ## Examples

      Logger.debug("hello?")
      Logger.debug(fn -> "dynamically calculated debug" end)
      Logger.debug(fn -> {"dynamically calculated debug", [additional: :metadata]} end)

  """
  defmacro debug(chardata_or_fun, metadata \\ []) do
    maybe_log(:debug, chardata_or_fun, metadata, __CALLER__)
  end

  @doc """
  Logs a message with the given `level`.

  Returns `:ok` or an `{:error, reason}` tuple.

  The macros `debug/2`, `warn/2`, `info/2`, and `error/2` are
  preferred over this macro as they can automatically eliminate
  the call to `Logger` altogether at compile time if desired
  (see the documentation for the `Logger` module).
  """
  defmacro log(level, chardata_or_fun, metadata \\ []) do
    macro_log(level, chardata_or_fun, metadata, __CALLER__)
  end

  defp macro_log(level, data, metadata, caller) do
    %{module: module, function: fun, file: file, line: line} = caller

    caller =
      compile_time_application_and_file(file) ++
        [module: module, function: form_fa(fun), line: line]

    {compile_metadata, quoted_metadata} =
      if Keyword.keyword?(metadata) do
        metadata = Keyword.merge(caller, metadata)
        {metadata, metadata}
      else
        {[],
         quote do
           Keyword.merge(unquote(caller), unquote(metadata))
         end}
      end

    compile_level = if is_atom(level), do: level, else: :error

    if compile_time_purge_matching?(compile_level, compile_metadata) do
      no_log(data, quoted_metadata)
    else
      quote do
        case Logger.__should_log__(unquote(level)) do
          :error -> :ok
          info -> Logger.__do_log__(info, unquote(data), unquote(quoted_metadata))
        end
      end
    end
  end

  defp compile_time_application_and_file(file) do
    if app = Application.get_env(:logger, :compile_time_application) do
      [application: app, file: Path.relative_to_cwd(file)]
    else
      [file: file]
    end
  end

  defp compile_time_purge_matching?(level, compile_metadata) do
    matching = Application.get_env(:logger, :compile_time_purge_matching, [])

    Enum.any?(matching, fn filter ->
      Enum.all?(filter, fn
        {:level_lower_than, min_level} ->
          Logger.Config.compare_levels(level, min_level) == :lt

        {k, v} when is_atom(k) ->
          Keyword.fetch(compile_metadata, k) == {:ok, v}

        _ ->
          raise "expected :compile_time_purge_matching to be a list of keyword lists, " <>
                  "got: #{inspect(matching)}"
      end)
    end)
  end

  defp maybe_log(level, data, metadata, caller) do
    min_level =
      if env_level = Application.get_env(:logger, :compile_time_purge_level) do
        IO.warn(
          ":compile_time_purge_level option for the :logger application is deprecated, " <>
            "use :compile_time_purge_matching instead",
          Macro.Env.stacktrace(caller)
        )

        env_level
      else
        :debug
      end

    if Logger.Config.compare_levels(level, min_level) != :lt do
      macro_log(level, data, metadata, caller)
    else
      no_log(data, metadata)
    end
  end

  defp no_log(data, metadata) do
    # We wrap the contents in an anonymous function
    # to avoid unused variable warnings.
    quote do
      _ = fn -> {unquote(data), unquote(metadata)} end
      :ok
    end
  end

  defp normalize_message(fun, metadata) when is_function(fun, 0) do
    case fun.() do
      {message, fun_metadata} -> {message, into_metadata(fun_metadata, metadata)}
      :skip -> :skip
      message -> {message, metadata}
    end
  end

  defp normalize_message(message, metadata) do
    {message, metadata}
  end

  defp truncate(data, n) when is_list(data) or is_binary(data), do: Logger.Utils.truncate(data, n)
  defp truncate(data, n), do: Logger.Utils.truncate(to_string(data), n)

  defp form_fa({name, arity}) do
    Atom.to_string(name) <> "/" <> Integer.to_string(arity)
  end

  defp form_fa(nil), do: nil

  defp notify(:sync, msg), do: :gen_event.sync_notify(Logger, msg)
  defp notify(:async, msg), do: :gen_event.notify(Logger, msg)
end
