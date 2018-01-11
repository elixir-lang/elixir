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

    * Wraps OTP's [`:error_logger`](http://erlang.org/doc/man/error_logger.html)
      to prevent it from overflowing.

  Logging is useful for tracking when an event of interest happens in your
  system. For example, it may be helpful to log whenever a user is deleted.

      def delete_user(user) do
        Logger.info fn ->
          "Deleting user from the system: #{inspect(user)}"
        end
        # ...
      end

  The `Logger.info/2` macro emits the provided message at the `:info`
  level. There are additional macros for other levels. Notice the argument
  passed to `Logger.info/2` in the above example is a zero argument function.

  The `Logger` macros also accept messages as strings, but keep in mind that
  strings are **always** evaluated regardless of log-level. As such, it is
  recommended to use a function whenever the message is expensive to compute.

  Another option that does not depend on the message type is to purge the log
  calls at compile-time using the `:compile_time_purge_level` option (see
  below).

  ## Levels

  The supported levels are:

    * `:debug` - for debug-related messages
    * `:info` - for information of any kind
    * `:warn` - for warnings
    * `:error` - for errors

  ## Configuration

  `Logger` supports a wide range of configurations.

  This configuration is split in three categories:

    * Application configuration - must be set before the `:logger`
      application is started

    * Runtime configuration - can be set before the `:logger`
      application is started, but may be changed during runtime

    * Error logger configuration - configuration for the
      wrapper around OTP's [`:error_logger`](http://erlang.org/doc/man/error_logger.html)

  ### Application configuration

  The following configuration must be set via config files (such as
  `config/config.exs`) before the `:logger` application is started.

    * `:backends` - the backends to be used. Defaults to `[:console]`.
      See the "Backends" section for more information.

    * `:compile_time_purge_level` - purges *at compilation time* all calls that
      have log level lower than the value of this option. This means that
      `Logger` calls with level lower than this option will be completely
      removed at compile time, accruing no overhead at runtime. Defaults to
      `:debug` and only applies to the `Logger.debug/2`, `Logger.info/2`,
      `Logger.warn/2`, and `Logger.error/2` macros (for example, it doesn't apply to
      `Logger.log/3`). Note that arguments passed to `Logger` calls that are
      removed from the AST at compilation time are never evaluated, thus any
      function call that occurs in these arguments is never executed. As a
      consequence, avoid code that looks like `Logger.debug("Cleanup:
      #{perform_cleanup()}")` as in the example `perform_cleanup/0` won't be
      executed if the `:compile_time_purge_level` is `:info` or higher.

    * `:compile_time_application` - sets the `:application` metadata value
      to the configured value at compilation time. This configuration is
      usually only useful for build tools to automatically add the
      application to the metadata for `Logger.debug/2`, `Logger.info/2`, etc.
      style of calls.

  For example, to configure the `:backends` and `compile_time_purge_level`
  options in a `config/config.exs` file:

      config :logger,
        backends: [:console],
        compile_time_purge_level: :info

  ### Runtime Configuration

  All configuration below can be set via config files (such as
  `config/config.exs`) but also changed dynamically during runtime via
  `Logger.configure/1`.

    * `:level` - the logging level. Attempting to log any message
      with severity less than the configured level will simply
      cause the message to be ignored. Keep in mind that each backend
      may have its specific level, too. Note that, unlike what happens with the
      `:compile_time_purge_level` option, the argument passed to `Logger` calls
      is evaluated even if the level of the call is lower than
      `:level`. For this reason, messages that are expensive to
      compute should be wrapped in 0-arity anonymous functions that are
      evaluated only when the `:level` option demands it.

    * `:utc_log` - when `true`, uses UTC in logs. By default it uses
      local time (i.e., it defaults to `false`).

    * `:truncate` - the maximum message size to be logged (in bytes). Defaults
      to 8192 bytes. Note this configuration is approximate. Truncated messages
      will have `" (truncated)"` at the end.  The atom `:infinity` can be passed
      to disable this behavior.

    * `:sync_threshold` - if the `Logger` manager has more than
      `:sync_threshold` messages in its queue, `Logger` will change
      to *sync mode*, to apply backpressure to the clients.
      `Logger` will return to *async mode* once the number of messages
      in the queue is reduced to `sync_threshold * 0.75` messages.
      Defaults to 20 messages.

    * `:discard_threshold` - if the `Logger` manager has more than
      `:discard_threshold` messages in its queue, `Logger` will change
      to *discard mode* and messages will be discarded directly in the
      clients. `Logger` will return to *sync mode* once the number of
      messages in the queue is reduced to `discard_threshold * 0.75`
      messages. Defaults to 500 messages.

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
  OTP's [`:error_logger`](http://erlang.org/doc/man/error_logger.html).
  All the configurations below must be set before the `:logger` application starts.

    * `:handle_otp_reports` - redirects OTP reports to `Logger` so
      they are formatted in Elixir terms. This uninstalls OTP's
      logger that prints terms to terminal. Defaults to `true`.

    * `:handle_sasl_reports` - redirects supervisor, crash and
      progress reports to `Logger` so they are formatted in Elixir
      terms. Your application must guarantee `:sasl` is started before
      `:logger`. This means you may see some initial reports written
      in Erlang syntax until the Logger application kicks in and
      uninstalls SASL's logger in favor of its own. Defaults to `false`.

    * `:discard_threshold_for_error_logger` - if `:error_logger` has more than
      `discard_threshold` messages in its inbox, messages will be dropped
      until the message queue goes down to `discard_threshold * 0.75`
      entries. The threshold will be checked once again after 10% of thereshold
      messages are processed, to avoid messages from being constantly dropped.
      For exmaple, if the thereshold is 500 (the default) and the inbox has
      600 messages, 250 messages will dropped, bringing the inbox down to
      350 (0.75 * threshold) entries and 50 (0.1 * theshold) messages will
      be processed before the threshold is checked once again.

  For example, to configure `Logger` to redirect all
  [`:error_logger`](http://erlang.org/doc/man/error_logger.html) messages
  using a `config/config.exs` file:

      config :logger,
        handle_otp_reports: true,
        handle_sasl_reports: true

  Furthermore, `Logger` allows messages sent by OTP's `:error_logger`
  to be translated into an Elixir format via translators. Translators
  can be dynamically added at any time with the `add_translator/1`
  and `remove_translator/1` APIs. Check `Logger.Translator` for more
  information.

  ## Backends

  `Logger` supports different backends where log messages are written to.

  The available backends by default are:

    * `:console` - logs messages to the console (enabled by default)

  Developers may also implement their own backends, an option that
  is explored in more detail below.

  The initial backends are loaded via the `:backends` configuration,
  which must be set before the `:logger` application is started.

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
      Setting `:metadata` to `:all` prints all metadata.

    * `:colors` - a keyword list of coloring options.

    * `:device` - the device to log error messages to. Defaults to
      `:user` but can be changed to something else such as `:standard_error`.

    * `:max_buffer` - maximum events to buffer while waiting
      for a confirmation from the IO device (default: 32).
      Once the buffer is full, the backend will block until
      a confirmation is received.

  In addition to the keys provided by the user via `Logger.metadata/1`,
  the following extra keys are available to the `:metadata` list:

    * `:application` - the current application

    * `:module` - the current module

    * `:function` - the current function

    * `:file` - the current file

    * `:line` - the current line

    * `:pid` - the current process ID

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
  you wish to provide your own format function. The `{module, function}` will be
  invoked with the log level, the message, the current timestamp and the
  metadata.

  Here is an example of how to configure the `:console` backend in a
  `config/config.exs` file:

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
  temporarily lose messages. If necessary, wrap the function in a "rescue" and
  log a default message instead:

      defmodule MyConsoleLogger do
        def format(level, message, timestamp, metadata) do
          # Custom formatting logic...
        rescue
          _ -> "could not format: #{inspect {level, message, metadata}}"
        end
      end

  You can read more about formatting in `Logger.Formatter`.

  ### Custom backends

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

  Once initialized, the handler should be designed to handle events
  in the following format:

      {level, group_leader, {Logger, message, timestamp, metadata}} | :flush

  where:

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
  return value for successful configuration is `:ok`.

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
  @type metadata :: keyword(String.Chars.t())
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

    metadata =
      Enum.reduce(keyword, metadata, fn
        {key, nil}, acc -> Keyword.delete(acc, key)
        {key, val}, acc -> Keyword.put(acc, key, val)
      end)

    Process.put(@metadata, {enabled?, metadata})
    :ok
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
  def level() do
    %{level: level} = Logger.Config.__data__()
    level
  end

  @doc """
  Compares log levels.

  Receives two log levels and compares the `left` level
  against the `right` level and returns

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
  def compare_levels(level, level) do
    :eq
  end

  def compare_levels(left, right) do
    if level_to_number(left) > level_to_number(right), do: :gt, else: :lt
  end

  defp level_to_number(:debug), do: 0
  defp level_to_number(:info), do: 1
  defp level_to_number(:warn), do: 2
  defp level_to_number(:error), do: 3

  @doc """
  Configures the logger.

  See the "Runtime Configuration" section in the `Logger` module
  documentation for the available options.
  """
  @valid_options [
    :compile_time_purge_level,
    :compile_time_application,
    :sync_threshold,
    :truncate,
    :level,
    :utc_log
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
    _ = :gen_event.which_handlers(:error_logger)
    :gen_event.sync_notify(Logger, :flush)
  end

  @doc """
  Adds a new backend.

  ## Options

    * `:flush` - when `true`, guarantees all messages currently sent
      to both Logger and OTP's [`:error_logger`](http://erlang.org/doc/man/error_logger.html)
      are processed before the backend is added

  """
  @spec add_backend(atom, keyword) :: Supervisor.on_start_child()
  def add_backend(backend, opts \\ []) do
    _ = if opts[:flush], do: flush()

    case Logger.WatcherSupervisor.watch(Logger, Logger.Config.translate_backend(backend), backend) do
      {:ok, _} = ok ->
        Logger.Config.add_backend(backend)
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
      to both Logger and OTP's [`:error_logger`](http://erlang.org/doc/man/error_logger.html)
      are processed before the backend is removed

  """
  @spec remove_backend(atom, keyword) :: :ok | {:error, term}
  def remove_backend(backend, opts \\ []) do
    _ = if opts[:flush], do: flush()
    Logger.Config.remove_backend(backend)
    Logger.WatcherSupervisor.unwatch(Logger, Logger.Config.translate_backend(backend))
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
    :gen_event.call(Logger, Logger.Config.translate_backend(backend), {:configure, options})
  end

  @doc """
  Logs a message dynamically.

  Use this function only when there is a need to
  explicitly avoid embedding metadata.
  """
  @spec bare_log(level, message | (() -> message | {message, keyword}), keyword) ::
          :ok | {:error, :noproc} | {:error, term}
  def bare_log(level, chardata_or_fun, metadata \\ []) when level in @levels and is_list(metadata) do
    case __metadata__() do
      {true, pdict} ->
        %{mode: mode, truncate: truncate, level: min_level, utc_log: utc_log?} =
          Logger.Config.__data__()

        if compare_levels(level, min_level) != :lt and mode != :discard do
          metadata = [pid: self()] ++ Keyword.merge(pdict, metadata)
          {message, metadata} = normalize_message(chardata_or_fun, metadata)
          truncated = truncate(message, truncate)

          tuple = {Logger, truncated, Logger.Utils.timestamp(utc_log?), metadata}

          try do
            notify(mode, {level, Process.group_leader(), tuple})
            :ok
          rescue
            ArgumentError -> {:error, :noproc}
          catch
            :exit, reason -> {:error, reason}
          end
        else
          :ok
        end

      {false, _} ->
        :ok
    end
  end

  @doc """
  Logs a warning message.

  Returns `:ok` or an `{:error, reason}` tuple.

  ## Examples

      Logger.warn "knob turned too far to the right"
      Logger.warn fn -> "expensive to calculate warning" end
      Logger.warn fn -> {"expensive to calculate warning", [additional: :metadata]} end

  """
  defmacro warn(chardata_or_fun, metadata \\ []) do
    maybe_log(:warn, chardata_or_fun, metadata, __CALLER__)
  end

  @doc """
  Logs an info message.

  Returns `:ok` or an `{:error, reason}` tuple.

  ## Examples

      Logger.info "mission accomplished"
      Logger.info fn -> "expensive to calculate info" end
      Logger.info fn -> {"expensive to calculate info", [additional: :metadata]} end

  """
  defmacro info(chardata_or_fun, metadata \\ []) do
    maybe_log(:info, chardata_or_fun, metadata, __CALLER__)
  end

  @doc """
  Logs an error message.

  Returns `:ok` or an `{:error, reason}` tuple.

  ## Examples

      Logger.error "oops"
      Logger.error fn -> "expensive to calculate error" end
      Logger.error fn -> {"expensive to calculate error", [additional: :metadata]} end

  """
  defmacro error(chardata_or_fun, metadata \\ []) do
    maybe_log(:error, chardata_or_fun, metadata, __CALLER__)
  end

  @doc """
  Logs a debug message.

  Returns `:ok` or an `{:error, reason}` tuple.

  ## Examples

      Logger.debug "hello?"
      Logger.debug fn -> "expensive to calculate debug" end
      Logger.debug fn -> {"expensive to calculate debug", [additional: :metadata]} end

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
      compile_time_application() ++
        [module: module, function: form_fa(fun), file: file, line: line]

    metadata =
      if Keyword.keyword?(metadata) do
        Keyword.merge(caller, metadata)
      else
        quote do
          Keyword.merge(unquote(caller), unquote(metadata))
        end
      end

    quote do
      Logger.bare_log(unquote(level), unquote(data), unquote(metadata))
    end
  end

  defp compile_time_application do
    if app = Application.get_env(:logger, :compile_time_application) do
      [application: app]
    else
      []
    end
  end

  defp maybe_log(level, data, metadata, caller) do
    min_level = Application.get_env(:logger, :compile_time_purge_level, :debug)

    if compare_levels(level, min_level) != :lt do
      macro_log(level, data, metadata, caller)
    else
      handle_unused_variable_warnings(data, caller)
    end
  end

  defp normalize_message(fun, metadata) when is_function(fun, 0) do
    normalize_message(fun.(), metadata)
  end

  defp normalize_message({message, fun_metadata}, metadata) when is_list(fun_metadata) do
    {message, Keyword.merge(metadata, fun_metadata)}
  end

  defp normalize_message(message, metadata), do: {message, metadata}

  defp truncate(data, n) when is_list(data) or is_binary(data), do: Logger.Utils.truncate(data, n)
  defp truncate(data, n), do: Logger.Utils.truncate(to_string(data), n)

  defp form_fa({name, arity}) do
    Atom.to_string(name) <> "/" <> Integer.to_string(arity)
  end

  defp form_fa(nil), do: nil

  defp notify(:sync, msg), do: :gen_event.sync_notify(Logger, msg)
  defp notify(:async, msg), do: :gen_event.notify(Logger, msg)

  defp handle_unused_variable_warnings(data, caller) do
    # We collect all the names of variables (leaving `data` unchanged) with a
    # scope of `nil` (as we don't warn for variables with a different scope
    # anyways). We only want the variables that figure in `caller.vars`, as the
    # AST for calls to local 0-arity functions without parens is the same as the
    # AST for variables.
    {^data, logged_vars} =
      Macro.postwalk(data, [], fn
        {name, _meta, nil} = var, acc when is_atom(name) ->
          if {name, nil} in caller.vars, do: {var, [name | acc]}, else: {var, acc}

        ast, acc ->
          {ast, acc}
      end)

    assignments =
      logged_vars
      |> Enum.reverse()
      |> Enum.uniq()
      |> Enum.map(&quote(do: _ = unquote(Macro.var(&1, nil))))

    quote do
      unquote_splicing(assignments)
      :ok
    end
  end
end
