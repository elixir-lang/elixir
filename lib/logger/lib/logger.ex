defmodule Logger do
  use Application

  @moduledoc ~S"""
  A logger for Elixir applications.

  It includes many features:

    * Provides debug, info, warn and error levels.

    * Supports multiple backends which are automatically
      supervised when plugged into Logger.

    * Formats and truncates messages on the client
      to avoid clogging logger backends.

    * Alternates between sync and async modes to keep
      it performant when required but also apply back-
      pressure when under stress.

    * Wraps OTP's error_logger to avoid it from
      overflowing.

  ## Levels

  The supported levels are:

    * `:debug` - for debug-related messages
    * `:info` - for information of any kind
    * `:warn` - for warnings
    * `:error` - for errors

  ## Configuration

  Logger supports a wide range of configuration.

  This configuration is split in three categories:

    * Application configuration - must be set before the logger
      application is started

    * Runtime configuration - can be set before the logger
      application is started but changed during runtime

    * Error logger configuration - configuration for the
      wrapper around OTP's error_logger

  ### Application configuration

  The following configuration must be set via config files
  before the logger application is started.

    * `:backends` - the backends to be used. Defaults to `[:console]`.
      See the "Backends" section for more information.

    * `:compile_time_purge_level` - purge all calls that have log level
      lower than the configured value at compilation time. This means the
      Logger call will be completely removed at compile time, occuring
      no overhead at runtime. By default, defaults to `:debug` and only
      applies to the `Logger.debug`, `Logger.info`, etc style of calls.

  ### Runtime Configuration

  All configuration below can be set via the config files but also
  changed dynamically during runtime via `Logger.configure/1`.

    * `:level` - the logging level. Attempting to log any message
      with severity less than the configured level will simply
      cause the message to be ignored. Keep in mind that each backend
      may have its specific level too.

    * `:utc_log` - when true, uses UTC in logs. By default it uses
      local time (i.e. it defaults to false).

    * `:truncate` - the maximum message size to be logged. Defaults
      to 8192 bytes. Note this configuration is approximate. Truncated
      messages will have " (truncated)" at the end.

    * `:sync_threshold` - if the logger manager has more than
      `sync_threshold` messages in its queue, logger will change
      to sync mode, to apply back-pressure to the clients.
      Logger will return to sync mode once the number of messages
      in the queue reduce to `sync_threshold * 0.75` messages.
      Defaults to 20 messages.

  ### Error logger configuration

  The following configuration applies to the Logger wrapper around
  Erlang's error_logger. All the configurations below must be set
  before the logger application starts.

    * `:handle_otp_reports` - redirects OTP reports to Logger so
      they are formatted in Elixir terms. This uninstalls Erlang's
      logger that prints terms to terminal.

    * `:discard_threshold_for_error_logger` - a value that, when
      reached, triggers the error logger to discard messages. This
      value must be a positive number that represents the maximum
      number of messages accepted per second. Once above this
      threshold, the error_logger enters in discard mode for the
      remaining of that second. Defaults to 500 messages.

  Furthermore, Logger allows messages sent by Erlang's `error_logger`
  to be translated into an Elixir format via translators. Translator
  can be dynamically added at any time with the `add_translator/1`
  and `remove_translator/1` APIs. Check `Logger.Translator` for more
  information.

  ## Backends

  Logger supports different backends where log messages are written to.

  The available backends by default are:

    * `:console` - Logs messages to the console (enabled by default)

  Developers may also implement their own backends, an option that
  is explored with detail below.

  The initial backends are loaded via the `:backends` configuration,
  which must be set before the logger application is started. However,
  backends can be added or removed dynamically via the `add_backend/2`,
  `remove_backend/1` and `configure_backend/2` functions. Note though
  that dynamically added backends are not restarded in case of crashes.

  ### Console backend

  The console backend logs message to the console. It supports the
  following options:

    * `:level` - the level to be logged by this backend.
      Note though messages are first filtered by the general
      `:level` configuration in `:logger`

    * `:format` - the format message used to print logs.
      Defaults to: "$time $metadata[$level] $message\n"

    * `:metadata` - the metadata to be printed by `$metadata`.
      Defaults to an empty list (no metadata)

  Here is an example on how to configure the `:console` in a
  `config/config.exs` file:

      config :logger, :console,
        format: "$date $time [$level] $metadata$message\n",
        metadata: [:user_id]

  You can read more about formatting in `Logger.Formatter`.

  ### Custom backends

  Any developer can create their own backend for Logger.
  Since Logger is an event manager powered by `GenEvent`,
  writing a new backend is a matter of creating an event
  handler, as described in the `GenEvent` module.

  From now on, we will be using event handler to refer to
  your custom backend, as we head into implementation details.

  The `add_backend/1` function is used to start a new
  backend, which installs the given event handler to the
  Logger event manager. This event handler is automatically
  supervised by Logger.

  Once added, the handler should be able to handle events
  in the following format:

      {level, group_leader,
        {Logger, message, timestamp, metadata}}

  The level is one of `:error`, `:info`, `:warn` or `:error`,
  as previously described, the group leader is the group
  leader of the process who logged the message, followed by
  a tuple starting with the atom `Logger`, the message as
  iodata, the timestamp and a keyword list of metadata.

  It is recommended that handlers ignore messages where
  the group leader is in a different node than the one
  the handler is installed.

  Furthermore, backends can be configured via the `configure_backend/2`
  function which requires event handlers to handle calls of
  the following format:

      {:configure, options}

  where options is a keyword list. The result of the call is
  the result returned by `configure_backend/2`. You may simply
  return `:ok` if you don't perform any kind of validation.

  It is recommended that backends support at least the following
  configuration values:

    * level - the logging level for that backend
    * format - the logging format for that backend
    * metadata - the metadata to include the backend

  Check the implementation for `Logger.Backends.Console` for
  examples on how to handle the recommendations in this section
  and how to process the existing options.
  """

  @type backend :: GenEvent.handler
  @type level :: :error | :info | :warn | :debug
  @levels [:error, :info, :warn, :debug]

  @doc false
  def start(_type, _args) do
    import Supervisor.Spec

    otp_reports? = Application.get_env(:logger, :handle_otp_reports)
    threshold    = Application.get_env(:logger, :discard_threshold_for_error_logger)

    handlers =
      for backend <- Application.get_env(:logger, :backends) do
        {Logger, translate_backend(backend), []}
      end

    options  = [strategy: :rest_for_one, name: Logger.Supervisor]
    children = [worker(GenEvent, [[name: Logger]]),
                worker(Logger.Watcher, [Logger, Logger.Config, []],
                  [id: Logger.Config, function: :watcher]),
                supervisor(Logger.Watcher, [handlers]),
                worker(Logger.Watcher,
                  [:error_logger, Logger.ErrorHandler, {otp_reports?, threshold}],
                  [id: Logger.ErrorHandler, function: :watcher])]

    case Supervisor.start_link(children, options) do
      {:ok, _} = ok ->
        deleted = delete_error_logger_handler(otp_reports?, :error_logger_tty_h, [])
        store_deleted_handlers(deleted)
        ok
      {:error, _} = error ->
        error
    end
  end

  @doc false
  def stop(_) do
    Application.get_env(:logger, :deleted_handlers)
    |> Enum.each(&:error_logger.add_report_handler/1)

    # We need to do this in another process as the Application
    # Controller is currently blocked shutting down this app.
    spawn_link(fn -> Logger.Config.clear_data end)

    :ok
  end

  defp store_deleted_handlers(list) do
    Application.put_env(:logger, :deleted_handlers, Enum.into(list, HashSet.new))
  end

  defp delete_error_logger_handler(should_delete?, handler, deleted) do
    if should_delete? and
         :error_logger.delete_report_handler(handler) != {:error, :module_not_found} do
      [handler|deleted]
    else
      deleted
    end
  end

  @metadata :logger_metadata

  @doc """
  Adds the given keyword list to the current process metadata.
  """
  def metadata(dict) do
    Process.put(@metadata, dict ++ metadata)
  end

  @doc """
  Reads the current process metadata.
  """
  def metadata() do
    Process.get(@metadata) || []
  end

  @doc """
  Retrieves the logger level.

  The logger level can be changed via `configure/1`.
  """
  @spec level() :: level
  def level() do
    check_logger!
    %{level: level} = Logger.Config.__data__
    level
  end

  @doc """
  Compare log levels.

  Receives to log levels and compares the `left`
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
  @valid_options [:compile_time_purge_level, :sync_threshold, :truncate, :level, :utc_log]

  def configure(options) do
    Logger.Config.configure(Dict.take(options, @valid_options))
  end

  @doc """
  Adds a new backend.
  """
  def add_backend(backend) do
    Logger.Watcher.watch(Logger, translate_backend(backend), [])
  end

  @doc """
  Removes a backend.
  """
  def remove_backend(backend) do
    Logger.Watcher.unwatch(Logger, translate_backend(backend))
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
  """
  @spec configure_backend(backend, Keywowrd.t) :: term
  def configure_backend(backend, options) when is_list(options) do
    GenEvent.call(Logger, translate_backend(backend), {:configure, options})
  end

  defp translate_backend(:console), do: Logger.Backends.Console
  defp translate_backend(other),    do: other

  @doc """
  Logs a message.

  Developers should rather use the macros `Logger.debug/2`,
  `Logger.warn/2`, `Logger.info/2` or `Logger.error/2` instead
  of this function as they automatically include caller metadata
  and can eliminate the Logger call altogether at compile time if
  desired.

  Use this function only when there is a need to log dynamically
  or you want to explicitly avoid embedding metadata.
  """
  @spec log(level, IO.chardata | (() -> IO.chardata), Keyword.t) :: :ok
  def log(level, chardata, metadata \\ []) when level in @levels and is_list(metadata) do
    check_logger!
    %{mode: mode, truncate: truncate,
      level: min_level, utc_log: utc_log?} = Logger.Config.__data__

    if compare_levels(level, min_level) != :lt do
      tuple = {Logger, truncate(chardata, truncate), Logger.Utils.timestamp(utc_log?),
               [pid: self()] ++ metadata() ++ metadata}
      notify(mode, {level, Process.group_leader(), tuple})
    end

    :ok
  end

  @doc """
  Logs a warning.

  ## Examples

      Logger.warn "knob turned too much to the right"
      Logger.warn fn -> "expensive to calculate warning" end

  """
  defmacro warn(chardata, metadata \\ []) do
    macro_log(:warn, chardata, metadata, __CALLER__)
  end

  @doc """
  Logs some info.

  ## Examples

      Logger.info "mission accomplished"
      Logger.info fn -> "expensive to calculate info" end

  """
  defmacro info(chardata, metadata \\ []) do
    macro_log(:info, chardata, metadata, __CALLER__)
  end

  @doc """
  Logs an error.

  ## Examples

      Logger.error "oops"
      Logger.error fn -> "expensive to calculate error" end

  """
  defmacro error(chardata, metadata \\ []) do
    macro_log(:error, chardata, metadata, __CALLER__)
  end

  @doc """
  Logs a debug message.

  ## Examples

      Logger.debug "hello?"
      Logger.debug fn -> "expensive to calculate debug" end

  """
  defmacro debug(chardata, metadata \\ []) do
    macro_log(:debug, chardata, metadata, __CALLER__)
  end

  defp macro_log(level, chardata, metadata, caller) do
    min_level = Application.get_env(:logger, :compile_time_purge_level, :debug)
    if compare_levels(level, min_level) != :lt do
      %{module: module, function: function, line: line} = caller
      caller = [module: module, function: function, line: line]
      quote do
        Logger.log(unquote(level), unquote(chardata), unquote(caller) ++ unquote(metadata))
      end
    else
      :ok
    end
  end

  defp truncate(data, n) when is_function(data, 0),
    do: Logger.Utils.truncate(data.(), n)
  defp truncate(data, n) when is_list(data) or is_binary(data),
    do: Logger.Utils.truncate(data, n)

  defp notify(:sync, msg),  do: GenEvent.sync_notify(Logger, msg)
  defp notify(:async, msg), do: GenEvent.notify(Logger, msg)

  defp check_logger! do
    unless Process.whereis(Logger) do
      raise "Cannot log messages, the :logger application is not running"
    end
  end
end
