defmodule Logger do
  @moduledoc ~S"""
  A logger for Elixir applications.

  It includes many features:

    * Provides debug, info, warn and error levels.

    * Supports multiple backends which are automatically
      supervised when plugged into Logger.

    * Formats and truncates messages on the client
      to avoid clogging logger backends.

    * Alternates between sync and async modes to remain
      performant when required but also apply backpressure
      when under stress.

    * Wraps OTP's `error_logger` to prevent it from
      overflowing.

  ## Levels

  The supported levels are:

    * `:debug` - for debug-related messages
    * `:info` - for information of any kind
    * `:warn` - for warnings
    * `:error` - for errors

  ## Configuration

  Logger supports a wide range of configurations.

  This configuration is split in three categories:

    * Application configuration - must be set before the logger
      application is started

    * Runtime configuration - can be set before the logger
      application is started, but may be changed during runtime

    * Error logger configuration - configuration for the
      wrapper around OTP's `error_logger`

  ### Application configuration

  The following configuration must be set via config files
  before the logger application is started.

    * `:backends` - the backends to be used. Defaults to `[:console]`.
      See the "Backends" section for more information.

    * `:compile_time_purge_level` - purge all calls that have log level
      lower than the configured value at compilation time. This means the
      Logger call will be completely removed at compile time, accruing
      no overhead at runtime. Defaults to `:debug` and only
      applies to the `Logger.debug`, `Logger.info`, etc style of calls.

    * `:compile_time_application` - sets the `:application` metadata value
      to the configured value at compilation time. This configuration is
      usually only useful for build tools to automatically add the
      application to the metadata for `Logger.debug`, `Logger.info`, etc
      style of calls.

  For example, to configure the `:backends` and `compile_time_purge_level`
  in a `config/config.exs` file:

      config :logger,
        backends: [:console],
        compile_time_purge_level: :info

  ### Runtime Configuration

  All configuration below can be set via config files but also
  changed dynamically during runtime via `Logger.configure/1`.

    * `:level` - the logging level. Attempting to log any message
      with severity less than the configured level will simply
      cause the message to be ignored. Keep in mind that each backend
      may have its specific level, too.

    * `:utc_log` - when `true`, uses UTC in logs. By default it uses
      local time (i.e. it defaults to `false`).

    * `:truncate` - the maximum message size to be logged. Defaults
      to 8192 bytes. Note this configuration is approximate. Truncated
      messages will have `" (truncated)"` at the end.

    * `:sync_threshold` - if the logger manager has more than
      `sync_threshold` messages in its queue, Logger will change
      to sync mode, to apply backpressure to the clients.
      Logger will return to async mode once the number of messages
      in the queue is reduced to `sync_threshold * 0.75` messages.
      Defaults to 20 messages.

    * `:translator_inspect_opts` - when translating OTP reports and
      errors, the last message and state must be inspected in the
      error reports. This configuration allow developers to change
      how much and how the data should be inspected.

  For example, to configure the `:level` and `:truncate` in a
  `config/config.exs` file:

      config :logger,
        level: :warn,
        truncate: 4096

  ### Error logger configuration

  The following configuration applies to the Logger wrapper around
  Erlang's `error_logger`. All the configurations below must be set
  before the logger application starts.

    * `:handle_otp_reports` - redirects OTP reports to Logger so
      they are formatted in Elixir terms. This uninstalls Erlang's
      logger that prints terms to terminal. Defaults to `true`.

    * `:handle_sasl_reports` - redirects supervisor, crash and
      progress reports to Logger so they are formatted in Elixir
      terms. This uninstalls `sasl`'s logger that prints these
      reports to the terminal. Defaults to `false`.

    * `:discard_threshold_for_error_logger` - a value that, when
      reached, triggers the error logger to discard messages. This
      value must be a positive number that represents the maximum
      number of messages accepted per second. Once above this
      threshold, the `error_logger` enters discard mode for the
      remainder of that second. Defaults to 500 messages.

  For example, to configure Logger to redirect all `error_logger` messages
  using a `config/config.exs` file:

      config :logger,
        handle_otp_reports: true,
        handle_sasl_reports: true

  Furthermore, Logger allows messages sent by Erlang's `error_logger`
  to be translated into an Elixir format via translators. Translators
  can be dynamically added at any time with the `add_translator/1`
  and `remove_translator/1` APIs. Check `Logger.Translator` for more
  information.

  ## Backends

  Logger supports different backends where log messages are written to.

  The available backends by default are:

    * `:console` - logs messages to the console (enabled by default)

  Developers may also implement their own backends, an option that
  is explored with detail below.

  The initial backends are loaded via the `:backends` configuration,
  which must be set before the logger application is started.

  ### Console backend

  The console backend logs message to the console. It supports the
  following options:

    * `:level` - the level to be logged by this backend.
      Note that messages are first filtered by the general
      `:level` configuration in `:logger`

    * `:format` - the format message used to print logs.
      Defaults to: `"$time $metadata[$level] $levelpad$message\n"`

    * `:metadata` - the metadata to be printed by `$metadata`.
      Defaults to an empty list (no metadata)

    * `:colors` - a keyword list of coloring options.

  The supported keys in the `:colors` keyword list are:

    * `:enabled` - boolean value that allows for switching the
      coloring on and off. Defaults to: `IO.ANSI.enabled?`

    * `:debug` - color for debug messages. Defaults to: `:cyan`

    * `:info` - color for info messages. Defaults to: `:normal`

    * `:warn` - color for warn messages. Defaults to: `:yellow`

    * `:error` - color for error messages. Defaults to: `:red`

  See the `IO.ANSI` module for a list of colors and attributes.

  Here is an example of how to configure the `:console` backend in a
  `config/config.exs` file:

      config :logger, :console,
        format: "\n$time $metadata[$level] $levelpad$message\n"
        metadata: [:user_id]

  You can read more about formatting in `Logger.Formatter`.

  ### Custom backends

  Any developer can create their own backend for Logger.
  Since Logger is an event manager powered by `GenEvent`,
  writing a new backend is a matter of creating an event
  handler, as described in the `GenEvent` module.

  From now on, we will be using the term "event handler" to refer
  to your custom backend, as we head into implementation details.

  Once Logger starts, it installs all event handlers under
  the `:backends` configuration into the Logger event manager.
  The event manager and all added event handlers are
  automatically supervised by Logger.

  Once initialized, the handler should be designed to handle events
  in the following format:

      {level, group_leader,
        {Logger, message, timestamp, metadata}}

  The level is one of `:debug`, `:info`, `:warn` or `:error`,
  as previously described, the group leader is the group
  leader of the process who logged the message, followed by
  a tuple starting with the atom `Logger`, the message as
  chardata, the timestamp and a keyword list of metadata.

  It is recommended that handlers ignore messages where
  the group leader is in a different node than the one
  the handler is installed.

  Furthermore, backends can be configured via the
  `configure_backend/2` function which requires event handlers
  to handle calls of the following format:

      {:configure, options}

  where options is a keyword list. The result of the call is
  the result returned by `configure_backend/2`. The recommended
  return value for successful configuration is `:ok`.

  It is recommended that backends support at least the following
  configuration values:

    * `level` - the logging level for that backend
    * `format` - the logging format for that backend
    * `metadata` - the metadata to include the backend

  Check the implementation for `Logger.Backends.Console` for
  examples on how to handle the recommendations in this section
  and how to process the existing options.
  """

  @type backend :: GenEvent.handler
  @type message :: IO.chardata | String.Chars.t
  @type level :: :error | :info | :warn | :debug
  @levels [:error, :info, :warn, :debug]

  @metadata :logger_metadata
  @compile {:inline, __metadata__: 0}

  defp __metadata__ do
    Process.get(@metadata) || {true, []}
  end

  @doc """
  Adds the given keyword list to the current process metadata.
  """
  def metadata(dict) do
    {enabled, metadata} = __metadata__()
    metadata =
      Enum.reduce(dict, metadata, fn
        {key, nil}, acc -> Keyword.delete(acc, key)
        {key, val}, acc -> Keyword.put(acc, key, val)
      end)
    Process.put(@metadata, {enabled, metadata})
    :ok
  end

  @doc """
  Reads the current process metadata.
  """
  def metadata() do
    __metadata__() |> elem(1)
  end

  @doc """
  Enables logging for the current process.

  Currently the only accepted process is self().
  """
  def enable(pid) when pid == self() do
    Process.put(@metadata, {true, metadata()})
    :ok
  end

  @doc """
  Disables logging for the current process.

  Currently the only accepted process is self().
  """
  def disable(pid) when pid == self() do
    Process.put(@metadata, {false, metadata()})
    :ok
  end

  @doc """
  Retrieves the logger level.

  The logger level can be changed via `configure/1`.
  """
  @spec level() :: level
  def level() do
    %{level: level} = Logger.Config.__data__
    level
  end

  @doc """
  Compare log levels.

  Receives two log levels and compares the `left`
  against `right` and returns `:lt`, `:eq` or `:gt`.
  """
  @spec compare_levels(level, level) :: :lt | :eq | :gt
  def compare_levels(level, level), do:
    :eq
  def compare_levels(left, right), do:
    if(level_to_number(left) > level_to_number(right), do: :gt, else: :lt)

  defp level_to_number(:debug), do: 0
  defp level_to_number(:info),  do: 1
  defp level_to_number(:warn),  do: 2
  defp level_to_number(:error), do: 3

  @doc """
  Configures the logger.

  See the "Runtime Configuration" section in `Logger` module
  documentation for the available options.
  """
  @valid_options [:compile_time_purge_level, :compile_time_application, :sync_threshold, :truncate, :level, :utc_log]

  def configure(options) do
    Logger.Config.configure(Dict.take(options, @valid_options))
  end

  @doc """
  Flushes the Logger.

  This basically guarantees all messages sent to the
  Logger prior to this call will be processed. This is useful
  for testing and it should not be called in production code.
  """
  @spec flush :: :ok
  def flush do
    _ = GenEvent.which_handlers(:error_logger)
    _ = GenEvent.which_handlers(Logger)
    :ok
  end

  @doc """
  Adds a new backend.

  ## Options

    * `:flush` - when `true`, guarantees all messages currently sent
      to both Logger and Erlang's `error_logger` are processed before
      the backend is added

  """
  def add_backend(backend, opts \\ []) do
    _ = if opts[:flush], do: GenEvent.which_handlers(:error_logger)
    case Logger.Watcher.watch(Logger, Logger.Config.translate_backend(backend), backend) do
      {:ok, _} = ok ->
        Logger.Config.add_backend(backend)
        ok
      {:error, _} = error ->
        error
    end
  end

  @doc """
  Removes a backend.

  ## Options

    * `:flush` - when `true`, guarantees all messages currently sent
      to both Logger and Erlang's `error_logger` are processed before
      the backend is removed
  """
  def remove_backend(backend, opts \\ []) do
    _ = if opts[:flush], do: GenEvent.which_handlers(:error_logger)
    Logger.Config.remove_backend(backend)
    Logger.Watcher.unwatch(Logger, Logger.Config.translate_backend(backend))
  end

  @doc """
  Adds a new translator.
  """
  def add_translator({mod, fun} = translator) when is_atom(mod) and is_atom(fun) do
    Logger.Config.add_translator(translator)
  end

  @doc """
  Removes a translator.
  """
  def remove_translator({mod, fun} = translator) when is_atom(mod) and is_atom(fun) do
    Logger.Config.remove_translator(translator)
  end

  @doc """
  Configures the given backend.

  The backends needs to be started and running in order to
  be configured at runtime.
  """
  @spec configure_backend(backend, Keyword.t) :: term
  def configure_backend(backend, options) when is_list(options) do
    GenEvent.call(Logger, Logger.Config.translate_backend(backend), {:configure, options})
  end

  @doc """
  Logs a message dynamically.

  Use this function only when there is a need to
  explicitly avoid embedding metadata.
  """
  @spec bare_log(level, message | (() -> message), Keyword.t) ::
        :ok | {:error, :noproc} | {:error, term}
  def bare_log(level, chardata_or_fn, metadata \\ [])
      when level in @levels and is_list(metadata) do
    case __metadata__() do
      {true, pdict} ->
        %{mode: mode, truncate: truncate,
          level: min_level, utc_log: utc_log?} = Logger.Config.__data__

        if compare_levels(level, min_level) != :lt do
          metadata = [pid: self()] ++ Keyword.merge(pdict, metadata)
          tuple = {Logger, truncate(chardata_or_fn, truncate),
                   Logger.Utils.timestamp(utc_log?), metadata}
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
  Logs a warning.

  ## Examples

      Logger.warn "knob turned too far to the right"
      Logger.warn fn -> "expensive to calculate warning" end

  """
  defmacro warn(chardata_or_fn, metadata \\ []) do
    maybe_log(:warn, chardata_or_fn, metadata, __CALLER__)
  end

  @doc """
  Logs some info.

  ## Examples

      Logger.info "mission accomplished"
      Logger.info fn -> "expensive to calculate info" end

  """
  defmacro info(chardata_or_fn, metadata \\ []) do
    maybe_log(:info, chardata_or_fn, metadata, __CALLER__)
  end

  @doc """
  Logs an error.

  ## Examples

      Logger.error "oops"
      Logger.error fn -> "expensive to calculate error" end

  """
  defmacro error(chardata_or_fn, metadata \\ []) do
    maybe_log(:error, chardata_or_fn, metadata, __CALLER__)
  end

  @doc """
  Logs a debug message.

  ## Examples

      Logger.debug "hello?"
      Logger.debug fn -> "expensive to calculate debug" end

  """
  defmacro debug(chardata_or_fn, metadata \\ []) do
    maybe_log(:debug, chardata_or_fn, metadata, __CALLER__)
  end

  @doc """
  Logs a message.

  Developers should rather use the macros `Logger.debug/2`,
  `Logger.warn/2`, `Logger.info/2` or `Logger.error/2` instead
  of this macro as they can automatically eliminate
  the Logger call altogether at compile time if desired.
  """
  defmacro log(level, chardata_or_fn, metadata \\ []) do
    macro_log(level, chardata_or_fn, metadata, __CALLER__)
  end

  defp macro_log(level, data, metadata, caller) do
    %{module: module, function: fun, line: line} = caller
    app = Application.get_env(:logger, :compile_time_application)
    caller = [application: app, module: module, function: form_fa(fun), line: line]
    quote do
      Logger.bare_log(unquote(level), unquote(data), unquote(caller) ++ unquote(metadata))
    end
  end

  defp maybe_log(level, data, metadata, caller) do
    min_level = Application.get_env(:logger, :compile_time_purge_level, :debug)
    if compare_levels(level, min_level) != :lt do
      macro_log(level, data, metadata, caller)
    else
      :ok
    end
  end

  defp truncate(data, n) when is_function(data, 0),
    do: Logger.Utils.truncate(data.(), n)
  defp truncate(data, n) when is_list(data) or is_binary(data),
    do: Logger.Utils.truncate(data, n)
  defp truncate(data, n),
    do: Logger.Utils.truncate(to_string(data), n)

  defp form_fa({name, arity}) do
    Atom.to_string(name) <> "/" <> Integer.to_string(arity)
  end

  defp form_fa(nil), do: nil

  defp notify(:sync, msg),  do: GenEvent.sync_notify(Logger, msg)
  defp notify(:async, msg), do: GenEvent.notify(Logger, msg)
end
