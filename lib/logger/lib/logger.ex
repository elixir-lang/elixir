defmodule Logger do
  @moduledoc ~S"""
  A logger for Elixir applications.

  This application is mostly a wrapper around Erlang's
  [`:logger`](`:logger`) functionality, to provide message
  translation and formatting to Elixir terms.

  Overall, you will find that `Logger`:

    * Provides all 7 syslog levels
      (although debug, info, warning, and error are the most commonly used).

    * Supports both message-based and structural logging.

    * Integrate with Erlang's [`:logger`](`:logger`) and
      support custom filters and handlers.

    * Formats and truncates messages on the client
      to avoid clogging `Logger` handlers.

    * Provides multiple forms of [overload protection](https://www.erlang.org/doc/apps/kernel/logger_chapter.html#protecting-the-handler-from-overload):
      * keeps track of its message queue and switches to sync mode to apply
        back pressure or even drop messages
      * limits the number of logs emitted defaulting to 500 per second
      * optionally allows to terminate and restart it if the message queue length
        or memory thresholds are exceeded

    * Allows overriding the logging level for a specific module,
      application or process.

  Logging is useful for tracking when an event of interest happens in your
  system. For example, it may be helpful to log whenever a user is deleted.

      def delete_user(user) do
        Logger.info("Deleting user from the system: #{inspect(user)}")
        # ...
      end

  The `Logger.info/2` macro emits the provided message at the `:info`
  level. Note the arguments given to `info/2` will only be evaluated
  if a message is logged. For instance, if the Logger level is
  set to `:warning`, `:info` messages are never logged and therefore
  the arguments given above won't even be executed.

  There are additional macros for other levels.

  Logger also allows log commands to be removed altogether via the
  `:compile_time_purge_matching` option (see below).

  For dynamically logging messages, see `bare_log/3`. But note that
  `bare_log/3` always evaluates its arguments (unless the argument
  is an anonymous function).

  ## Levels

  The supported levels, ordered by importance, are:

    * `:emergency` - when system is unusable, panics
    * `:alert` - for alerts, actions that must be taken immediately,
      ex. corrupted database
    * `:critical` - for critical conditions
    * `:error` - for errors
    * `:warning` - for warnings
    * `:notice` - for normal, but significant, messages
    * `:info` - for information of any kind
    * `:debug` - for debug-related messages

  For example, `:info` takes precedence over `:debug`. If your log
  level is set to `:info`, then all `:info`, `:notice` and above will
  be passed to handlers. If your log level is set to `:alert`, only
  `:alert` and `:emergency` will be printed.

  ## Message

  Logger can be used for logging both unstructured and structured data.

  Unstructured data is a string or a list of strings:

      Logger.info("hello world!")
      Logger.info(["hello ", "world!"])

  Structured data, also known as reports, are keyword lists and maps:

      Logger.info([new_user: user.id, account_type: :admin])
      Logger.info(%{new_user: user.id, account_type: :admin})

  Log functions also accept a zero-arity anonymous function as a message:

      Logger.info(fn -> "hello world!" end)

  The anonymous function can return a message or a tuple containing
  the message and additional metadata (to be described in the next
  section).

  In all cases, the arguments given to the `Logger` macros are only
  evaluated if required by the current log level. The exception is
  the `bare_log/3` function, which is the raw mechanism for logging.

  ## Metadata

  Whenever a message is logged, additional information can be given
  via metadata. Each log operation, such as `Logger.info/2`, allows
  metadata to be given as an argument.

  Furthermore, metadata can be set per process with `Logger.metadata/1`.

  Some metadata, however, may be added automatically by Logger whenever
  possible. Those are:

    * `:application` - the current application

    * `:mfa` - the current module, function and arity

    * `:file` - the current file

    * `:line` - the current line

    * `:pid` - the current process identifier

    * `:initial_call` - the initial call that started the process

    * `:registered_name` - the process registered name as an atom

    * `:process_label` - (available from Erlang/OTP 27+) an arbitrary term
      which can be added to a process with `Process.set_label/1` for
      debugging purposes

    * `:domain` - a list of domains for the logged message. For example,
      all Elixir reports default to `[:elixir]`. Erlang reports may start
      with `[:otp]` or `[:sasl]`

    * `:crash_reason` - a two-element tuple with the throw/error/exit reason
      as first argument and the stacktrace as second. A throw will always be
      `{:nocatch, term}`. An error is always an `Exception` struct. All other
      entries are exits. The default formatter ignores this metadata by default
      but it can be useful to certain handlers, such as the ones that report
      errors to third-party services

  There are two special metadata keys, `:module` and `:function`, which
  extract the relevant bits from `:mfa`.

  The metadata keys above may not always be available. The `:mfa`, `:file`,
  `:line`, and similar metadata are automatically included when using `Logger`
  macros, but not when using `Logger.bare_log/3`. Other metadata, such as
  `:crash_reason`, `:initial_call`, and `:registered_name` are available
  only inside behaviours such as GenServer, Supervisor, and others.

  It is also possible to pass metadata on a particular Logger invocation.
  For example, you might wish to include a custom `:error_code` metadata in
  your logs:

      Logger.error("We have a problem", [error_code: :pc_load_letter])

  By default, no metadata is logged. We will learn how to enable that
  over the next sections.

  ## Configuration

  `Logger` supports a wide range of configurations.

  This configuration is split in three categories:

    * Boot configuration - this configuration is read when logger
      starts and configures how Elixir hooks into Erlang's own logger

    * Compile configuration - this must be set before your code
      is compiled

    * Runtime configuration - can be set before the `:logger`
      application is started, but may be changed during runtime

  ### Boot configuration

  When `Logger` starts, it configures the `:default` log handler from
  Erlang to translate and format Elixir terms. As a developer, you
  are able to customize the default handler, the default formatter,
  and many other options.

  The following configuration must be set via config files (such as
  `config/config.exs`), under the `:logger` key, before your application
  is started:

    * `:default_formatter` - a keyword list which configures the
      default formatter used by the default handler. See `Logger.Formatter`
      for the full list of configuration.

    * `:default_handler` - this option configures the default handler
      used for logging. The default handler is a [`:logger_std_h`](`:logger_std_h`)
      instance which also supports file logging and log rotation.
      You can set it to `false` to disable the default logging altogether.
      See the examples below for more information.

    * `:handle_otp_reports` - if Erlang/OTP message should be logged.
      Defaults to `true`.

    * `:handle_sasl_reports` - if supervisor, crash, and progress reports
      should be logged. Defaults to `false`. This option only has an effect
      if `:handle_otp_reports` is true.

    * `:metadata` - key-value pairs of global primary metadata to be included
      in all log messages. Defaults to `[]`. The default formatter writes to
      standard out and therefore cannot print all metadata. See
      [`Logger.Formatter`'s documentation](`m:Logger.Formatter#module-metadata`)
      for more information.

  For example, to configure `Logger` to redirect all Erlang messages using a
  `config/config.exs` file:

      config :logger,
        handle_otp_reports: true,
        handle_sasl_reports: true

  To configure the default formatter, for example, to use a different format
  and include some metadata:

      config :logger, :default_formatter,
        format: "[$level] $message $metadata\n",
        metadata: [:error_code, :file]

  Or to configure default handler, for instance, to log into a file with
  built-in support for log rotation and compression:

      config :logger, :default_handler,
        config: [
          file: ~c"system.log",
          filesync_repeat_interval: 5000,
          file_check: 5000,
          max_no_bytes: 10_000_000,
          max_no_files: 5,
          compress_on_rotate: true
        ]

  You can find a complete reference on all handler options
  [on Erlang/OTP docs](`t::logger_handler.config/0`). Here is Elixir's
  default configuration for the default handler:

      [
        # Do not log messages from other nodes
        filters: [{&:logger_filters.remote_gl/2, :stop}],
        filter_default: :log,
        formatter: &Logger.default_formatter/0,
        level: :all,
        module: :logger_std_h
      ]

  The `:config` customizes a specific handler module. The default handler
  is [`:logger_std_h`](`:logger_std_h`), which logs to standard IO, and you
  call find all relevant configuration in its module documentation, including
  information overload protection.

  You may also set `:default_handler` to false to disable the default logging
  altogether:

      config :logger, :default_handler, false

  How to add more handlers besides the default one is covered in later sections.

  > #### Keywords or maps {: .tip}
  >
  > While Erlang's logger expects `:config` to be a map, Elixir's Logger
  > allows the default handler configuration to be set with keyword lists.
  > For example, this allows your `config/*.exs` files, such as `config/dev.exs`,
  > to override individual keys defined in `config/config.exs`.
  >
  > When reading the handler configuration using Erlang's APIs,
  > the configuration will always be read (and written) as a map.

  ### Compile configuration

  The following configuration must be set via config files (such as
  `config/config.exs`) under the `:logger` application before your code
  is compiled:

    * `:always_evaluate_messages` - if messages should be *evaluated* even if
      the log level is lower than the minimum configured level. Defaults to `false`.
      This is useful for cases where the log level in your *test environment*
      is high (such as `:error`), which is common in order to avoid logs mixed
      with the test output. In such, cases, you might discover log messages
      that contain runtime errors only when your code is deployed to production,
      where the log level is lower (such as `:info`). These runtime errors could
      be caused by, for example, interpolating something that doesn't implement
      the `String.Chars` protocol in the log message, such as `"PID: #{self()}"`
      (since PIDs cannot be converted to strings with `String.Chars`).

    * `:compile_time_application` - sets the `:application` metadata value
      to the configured value at compilation time. This configuration is
      automatically set by Mix and made available as metadata when logging.

    * `:compile_time_purge_matching` - purges *at compilation time* all calls
      that match the given conditions. This means that `Logger` calls with
      level lower than this option will be completely removed at compile time,
      accruing no overhead at runtime. This configuration expects a list of
      keyword lists. Each keyword list contains a metadata key and the matching
      value that should be purged. Some special keys are supported:

        * `:level_lower_than` - purges all messages with a lower logger level
        * `:module` - purges all messages with the matching module
        * `:function` - purges all messages with the "function/arity"

      Remember that if you want to purge log calls from a dependency, the
      dependency must be recompiled.

  For example, to purge all calls that happen at compile time with level
  lower than `:info` in a `config/config.exs` file:

      config :logger,
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
      cause the message to be ignored. Keep in mind that each handler
      may have its specific level, too. In addition to levels mentioned
      above it also supports 2 "meta-levels":

        - `:all` - all messages will be logged, conceptually identical to
          `:debug`
        - `:none` - no messages will be logged at all

    * `:translator_inspect_opts` - when translating OTP reports and
      errors, the last message and state must be inspected in the
      error reports. This configuration allow developers to change
      how much and how the data should be inspected.

  For example, to configure the `:level` options in a `config/config.exs`
  file:

      config :logger, level: :warning

  Furthermore, `Logger` allows messages sent by Erlang to be translated
  into an Elixir format via translators. Translators can be added at any
  time with the `add_translator/1` and `remove_translator/1` APIs. Check
  `Logger.Translator` for more information.

  ## Erlang/OTP handlers

  Handlers represent the ability to integrate into the logging system to
  handle each logged message/event. Elixir's Logger automatically sets a
  default handler based on Erlang's `:logger_std_h`, which you can configure
  using the `:default_handler` boot configuration outlined above. You may
  also attach additional handlers when you boot your application.

  To do so, you must list a series of handlers under the `:logger` key
  of your application configuration. For example, to setup an additional
  handler that writes to a file:

      config :my_app, :logger, [
        {:handler, :file_log, :logger_std_h, %{
           config: %{
             file: ~c"system.log",
             filesync_repeat_interval: 5000,
             file_check: 5000,
             max_no_bytes: 10_000_000,
             max_no_files: 5,
             compress_on_rotate: true
           },
           formatter: Logger.Formatter.new()
         }}
      ]

  Each handler has the shape `{:handler, name, handler_module, config_map}`.
  Once defined, a handler can be explicitly attached in your
  `c:Application.start/2` callback with `add_handlers/1`:

      Logger.add_handlers(:my_app)

  You can also add, remove, and update handlers at runtime with the help
  of the Erlang's [`:logger`](`:logger`) module.

  You may also develop your own handlers. Handlers run in the same
  process as the process logging the message/event. This gives developers
  flexibility but they should avoid performing any long running action in
  such handlers, as it may slow down the action being executed considerably.
  At the moment, there is no built-in overload protection for Erlang handlers,
  so it is your responsibility to implement it.

  Alternatively, you can use the
  [`:logger_backends`](https://github.com/elixir-lang/logger_backends) project.
  It sets up a log handler with overload protection and allows incoming events
  to be dispatched to multiple backends.

  ### Filtering

  You can add filters to any handler. For example, to filter out logs
  that contain a particular string, you could create a module:

      defmodule LogFilter do
        def filter(log_event, _opts) do
          case log_event do
            %{msg: msg} when is_binary(msg) ->
              if msg =~ "password" do
                :stop
              else
                :ignore
              end

            _ ->
              :ignore
          end
        end
      end

  It may return `:log` (to log the message), `:stop` (to not log the
  message), or `:ignore` (to ignore the filter).

  Then you can attach the filter, either as a primary filter (which
  applies to all handlers), or to a specific handler, when you start
  your application, such as in the `c:Application.start/2` callback:

      :logger.add_primary_filter(:word_filter, {&LogFilter.filter/2, []})

  ## Backends and backwards compatibility

  Prior to Elixir v1.15, custom logging could be achieved with Logger
  backends. The main API for writing Logger backends have been moved to
  the [`:logger_backends`](https://github.com/elixir-lang/logger_backends)
  project. However, the backends API is still part of Elixir for backwards
  compatibility.

  Important remarks:

    * If the `:backends` key is set and it doesn't have the `:console` entry,
      we assume that you want to disable the built-in logging. You can force
      logging by setting `config :logger, :default_handler, []`

    * The `:console` backend configuration is automatically mapped to the default
      handler and default formatter. Previously, you would set:

          config :logger, :console,
            level: :error,
            format: "$time $message $metadata"

      This is now equivalent to:

          config :logger, :default_handler,
            level: :error

          config :logger, :default_formatter,
            format: "$time $message $metadata"

      All previous console configuration, except for `:level`, now go under
      `:default_formatter`.

    * If you want to use the previous `:console` implementation based on Logger
      Backends, you can still set `backends: [Logger.Backends.Console]` and place
      the configuration under `config :logger, Logger.Backends.Console`. Although
      consider using the [`:logger_backends`](https://github.com/elixir-lang/logger_backends)
      project in such cases, as `Logger.Backends.Console` itself will be deprecated
      in future releases

    * `Logger.Backends` only receive `:debug`, `:info`, `:warning`, and `:error`
      messages. `:notice` maps to `:info`. `:warn` maps to `:warnings`.
      All others map to `:error`
  """

  @type level ::
          :emergency | :alert | :critical | :error | :warning | :warn | :notice | :info | :debug
  @type report :: map() | keyword()
  @type message :: :unicode.chardata() | String.Chars.t() | report()
  @type metadata :: keyword()
  @new_erlang_levels [:emergency, :alert, :critical, :warning, :notice]
  @levels [:error, :info, :debug] ++ @new_erlang_levels
  @metadata :logger_level

  @doc ~S"""
  Returns all the available levels.
  """
  @doc since: "1.16.0"
  @spec levels() :: [level(), ...]
  def levels(), do: @levels

  @doc ~S"""
  Returns the default formatter used by Logger.

  It returns a `Logger.Formatter` built on the `:default_formatter` configuration:

      config :logger, :default_formatter,
        format: "\n$time $metadata[$level] $message\n",
        metadata: [:user_id]

  In case of a list, a set of `overrides` can be given to merge into the list.
  See `Logger.Formatter.new/1` for all options.

  ## Examples

  `Logger` will automatically load a default formatter into the default handler
  on boot. However, you can use this function if you wish to programmatically replace
  a handler formatter. For example, inside tests, you might want to change the formatter
  settings:

      setup tags do
        formatter = Logger.default_formatter(colors: [enabled: false])
        :logger.update_handler_config(:default, :formatter, formatter)

        on_exit(fn ->
          :logger.update_handler_config(:default, :formatter, Logger.default_formatter())
        end)
      end

  However, note you should not invoke this function inside `config` files,
  as this function expects `Logger` to already be configured and started.
  To start a brand new handler with this formatter, use `Logger.Formatter.new/1`
  instead.
  """
  @doc since: "1.15.0"
  @spec default_formatter(keyword) :: {module, :logger.formatter_config()}
  def default_formatter(overrides \\ []) when is_list(overrides) do
    Application.get_env(:logger, :default_formatter, [])
    |> Keyword.merge(overrides)
    |> Logger.Formatter.new()
  end

  @doc """
  Alters the current process metadata according to the given keyword list.

  This function will merge the given keyword list into the existing metadata,
  with the exception of setting a key to `nil`, which will remove that key
  from the metadata.

  Note some metadata keys are reserved and cannot be overridden. See
  [the module documentation](#module-metadata) for more information.
  """
  @spec metadata(metadata) :: :ok
  def metadata(keyword) do
    case :logger.get_process_metadata() do
      :undefined ->
        reset_metadata(keyword)

      map when is_map(map) ->
        metadata =
          Enum.reduce(keyword, map, fn
            {k, nil}, acc -> Map.delete(acc, k)
            {k, v}, acc -> Map.put(acc, k, v)
          end)

        :ok = :logger.set_process_metadata(metadata)
    end
  end

  @doc """
  Reads the current process metadata.

  This does not return the "global" logger metadata (set via the `:metadata` key in the
  `:logger` application config), but only the process metadata.
  """
  @spec metadata() :: metadata
  def metadata() do
    case :logger.get_process_metadata() do
      :undefined -> []
      map when is_map(map) -> Map.to_list(map)
    end
  end

  @doc """
  Resets the current process metadata to the given keyword list.
  """
  @spec reset_metadata(metadata) :: :ok
  def reset_metadata(keyword \\ []) do
    :ok = :logger.set_process_metadata(filter_out_nils(keyword))
  end

  defp filter_out_nils(keyword) do
    for {_k, v} = elem <- keyword, v != nil, into: %{}, do: elem
  end

  @doc """
  Enables logging for the current process.

  Currently the only accepted PID is `self()`.

  Equivalent of:

      delete_process_level(pid)
  """
  # TODO: Deprecate me on v1.19
  @doc deprecated: "Use Logger.delete_process_level(pid) instead"
  @spec enable(pid) :: :ok
  def enable(pid) when pid == self() do
    delete_process_level(pid)
  end

  @doc """
  Disables logging for the current process.

  Currently the only accepted PID is `self()`.

  Equivalent of:

      put_process_level(pid, :none)
  """
  # TODO: Deprecate me on v1.20
  @doc deprecated: "Use Logger.put_process_level(pid, :none) instead"
  @spec disable(pid) :: :ok
  def disable(pid) when pid == self() do
    put_process_level(pid, :none)
  end

  @doc """
  Returns whether the logging is enabled for a given process.

  Currently the only accepted PID is `self()`.
  """
  # TODO: Deprecate me on v1.20
  @doc deprecated: "Use Logger.get_process_level(pid) instead"
  @spec enabled?(pid) :: boolean
  def enabled?(pid) when pid == self() do
    get_process_level(pid) != :none
  end

  @doc """
  Retrieves the `Logger` level.

  The `Logger` level can be changed via `configure/1`.
  """
  @spec level() :: level() | :all | :none
  def level() do
    %{level: level} = :logger.get_primary_config()

    level
  end

  @doc """
  Compares log levels.

  Receives two log levels and compares the `left` level
  against the `right` level and returns:

    * `:lt` if `left` is less than `right`
    * `:eq` if `left` and `right` are equal
    * `:gt` if `left` is greater than `right`

  ## Examples

      iex> Logger.compare_levels(:debug, :warning)
      :lt
      iex> Logger.compare_levels(:error, :info)
      :gt

  """
  @spec compare_levels(level, level) :: :lt | :eq | :gt
  def compare_levels(left, right) do
    :logger.compare_levels(
      elixir_level_to_erlang_level(left),
      elixir_level_to_erlang_level(right)
    )
  end

  @doc """
  Configures the logger.

  See the "Runtime Configuration" section in the `Logger` module
  documentation for the available options. The changes done here
  are automatically persisted to the `:logger` application
  environment.
  """
  @valid_options [
    :always_evaluate_messages,
    :compile_time_application,
    :compile_time_purge_level,
    :compile_time_purge_matching,
    :truncate,
    :utc_log,
    :translator_inspect_opts
  ]
  @backend_options [:sync_threshold, :discard_threshold, :truncate, :utc_log]
  @spec configure(keyword) :: :ok
  def configure(options) do
    for {k, v} <- options do
      cond do
        k == :level -> :logger.set_primary_config(:level, elixir_level_to_erlang_level(v))
        k in @valid_options -> Application.put_env(:logger, k, v)
        true -> :ok
      end
    end

    # TODO: Deprecate passing backend options to configure on Elixir v1.19
    case Keyword.take(options, @backend_options) do
      [] -> :ok
      backend_options -> Logger.Backends.Internal.configure(backend_options)
    end

    :ok
  end

  @doc """
  Flushes the logger.

  This guarantees all log handlers are flushed. This is useful
  for testing and it should not be called in production code.
  """
  @spec flush :: :ok
  def flush do
    for %{id: id, module: module} <- :logger.get_handler_config(),
        function_exported?(module, :filesync, 1) do
      try do
        module.filesync(id)
      catch
        _, _ -> :ok
      end
    end

    :ok
  end

  @doc """
  Puts logging level for given module.

  This will take priority over the primary level set, so it can be
  used to increase or decrease verbosity of some parts of the project.

  ## Example

      defmodule Foo do
        require Logger

        def log, do: Logger.debug("foo")
      end

      Logger.configure(level: :error)
      Logger.put_module_level(Foo, :all)

      Foo.log()
      # This will print the message even if global level is :error

  """
  @doc since: "1.11.0"
  @spec put_module_level(module() | [module()], level() | :all | :none) :: :ok | {:error, term()}
  defdelegate put_module_level(mod, level), to: :logger, as: :set_module_level

  @doc """
  Gets logging level for given module.

  The returned value will be the effective value used. If no value
  was set for a given module, then it will not be present in
  the returned list.
  """
  @doc since: "1.11.0"
  @spec get_module_level(module() | [module()]) :: [{module(), level() | :all | :none}]
  defdelegate get_module_level(mod), to: :logger

  @doc """
  Resets the logging level for a given module to the primary level.
  """
  @doc since: "1.11.0"
  @spec delete_module_level(module() | [module()]) :: :ok
  defdelegate delete_module_level(module), to: :logger, as: :unset_module_level

  @doc """
  Resets the logging level for all modules to the primary level.
  """
  @doc since: "1.11.0"
  @spec delete_all_module_levels() :: :ok
  defdelegate delete_all_module_levels(), to: :logger, as: :unset_module_level

  @doc """
  Puts logging level for modules in a given application.

  This will take priority over the primary level set, so it can be
  used to increase or decrease verbosity of some parts of the project.

  Equivalent of:

      appname |> Application.spec(:modules) |> Logger.put_module_level(level)
  """
  @doc since: "1.13.0"
  @spec put_application_level(atom(), level() | :all | :none) :: :ok | {:error, :not_loaded}
  defdelegate put_application_level(appname, level), to: :logger, as: :set_application_level

  @doc """
  Resets logging level for all modules in the given application to the primary level.

  Equivalent of:

      appname |> Application.spec(:modules) |> Logger.delete_module_level()
  """
  @doc since: "1.13.0"
  @spec delete_application_level(application) :: :ok | {:error, {:not_loaded, application}}
        when application: atom()
  defdelegate delete_application_level(appname), to: :logger, as: :unset_application_level

  @doc """
  Puts logging level for the current process.

  Currently the only accepted PID is `self()`.

  This will take priority over the primary level set, so it can be
  used to increase or decrease verbosity of some parts of the running system.
  """
  @doc since: "1.15.0"
  @spec put_process_level(pid(), level() | :all | :none) :: :ok
  def put_process_level(pid, level) when pid == self() do
    Process.put(@metadata, elixir_level_to_erlang_level(level))
    :ok
  end

  @doc """
  Gets logging level for the current process.

  Currently the only accepted PID is `self()`.

  The returned value will be the effective value used. If no value
  was set for a given process, then `nil` is returned.
  """
  @doc since: "1.15.0"
  @spec get_process_level(pid) :: level() | :all | :none | nil
  def get_process_level(pid) when pid == self() do
    Process.get(@metadata, nil)
  end

  @doc """
  Resets logging level for the current process to the primary level.

  Currently the only accepted PID is `self()`.
  """
  @doc since: "1.15.0"
  @spec delete_process_level(pid()) :: :ok
  def delete_process_level(pid) when pid == self() do
    Process.delete(@metadata)
    :ok
  end

  @doc """
  Adds the handlers configured in the `:logger` application parameter
  of the given `app`.

  This is used to register new handlers into the logging system.
  See [the module documentation](#module-erlang-otp-handlers) for
  more information.
  """
  @doc since: "1.15.0"
  @spec add_handlers(atom()) :: :ok | {:error, term}
  def add_handlers(app) when is_atom(app) do
    :logger.add_handlers(app)
  end

  @doc """
  Adds a new backend.
  """
  # TODO: Deprecate this on Elixir v1.20
  @doc deprecated: "Use LoggerBackends.add/2 from :logger_backends dependency"
  def add_backend(backend, opts \\ []) do
    Logger.Backends.Internal.add(backend, opts)
  end

  @doc """
  Removes a backend.
  """
  # TODO: Deprecate this on Elixir v1.20
  @doc deprecated: "Use LoggerBackends.remove/2 from :logger_backends dependency"
  def remove_backend(backend, opts \\ []) do
    Logger.Backends.Internal.remove(backend, opts)
  end

  @doc """
  Configures the given backend.
  """
  # TODO: Deprecate this on Elixir v1.20
  @doc deprecated: "Use LoggerBackends.configure/2 from :logger_backends dependency"
  def configure_backend(:console, options) when is_list(options) do
    options = Keyword.merge(Application.get_env(:logger, :console, []), options)
    Application.put_env(:logger, :console, options)
    {with_level, without_level} = Keyword.split(options, [:level])
    config = Map.new(with_level ++ [formatter: Logger.Formatter.new(without_level)])
    :logger.update_handler_config(:default, config)
  end

  def configure_backend(backend, options) do
    Logger.Backends.Internal.configure(backend, options)
  end

  @doc """
  Adds a new translator.
  """
  @spec add_translator({module, function :: atom}) :: :ok
  def add_translator({mod, fun} = translator) when is_atom(mod) and is_atom(fun) do
    update_translators(&[translator | List.delete(&1, translator)])
  end

  @doc """
  Removes a translator.
  """
  @spec remove_translator({module, function :: atom}) :: :ok
  def remove_translator({mod, fun} = translator) when is_atom(mod) and is_atom(fun) do
    update_translators(&List.delete(&1, translator))
  end

  defp update_translators(updater) do
    :elixir_config.serial(fn ->
      translators = updater.(Application.fetch_env!(:logger, :translators))
      Application.put_env(:logger, :translators, translators)

      with %{filters: filters} <- :logger.get_primary_config(),
           {{_, {fun, config}}, filters} <- List.keytake(filters, :logger_translator, 0) do
        config = %{config | translators: translators}
        :ok = :logger.set_primary_config(:filters, filters ++ [logger_translator: {fun, config}])
      end
    end)

    :ok
  end

  @doc """
  Logs a message dynamically.

  Opposite to `log/3`, `debug/2`, `info/2`, and friends, the arguments
  given to `bare_log/3` are always evaluated. However, you can pass
  anonymous functions to `bare_log/3` and they will only be evaluated
  if there is something to be logged.
  """
  @spec bare_log(level, message | (-> message | {message, keyword}), keyword) :: :ok
  def bare_log(level, message_or_fun, metadata \\ []) do
    case __should_log__(level, nil) do
      nil -> :ok
      level -> __do_log__(level, message_or_fun, %{}, Map.new(metadata))
    end
  end

  @doc false
  def __should_log__(level, module) do
    level = elixir_level_to_erlang_level(level)

    if :logger.allow(level, module) do
      level
    end
  end

  @doc false
  def __evaluate_log__(data) when is_function(data, 0) do
    data.()
  end

  def __evaluate_log__(data) do
    data
  end

  @doc false
  def __do_log__(level, fun, location, metadata)
      when is_function(fun, 0) and is_map(location) and is_map(metadata) do
    case fun.() do
      {msg, meta} ->
        __do_log__(level, msg, location, Enum.into(meta, metadata))

      msg ->
        __do_log__(level, msg, location, metadata)
    end
  end

  def __do_log__(level, msg, location, metadata)
      when level in @levels and is_map(location) and is_map(metadata) do
    if is_binary(msg) or is_list(msg) or is_map(msg) do
      :logger.macro_log(location, level, msg, add_elixir_domain(metadata))
    else
      IO.warn(
        "passing #{inspect(msg)} to Logger is deprecated, expected a map, a keyword list, " <>
          "a string, a list of strings, or a zero-arity anonymous function"
      )

      :logger.macro_log(location, level, to_string(msg), add_elixir_domain(metadata))
    end
  end

  defp add_elixir_domain(%{domain: domain} = metadata) when is_list(domain) do
    %{metadata | domain: [:elixir | domain]}
  end

  defp add_elixir_domain(metadata), do: Map.put(metadata, :domain, [:elixir])

  for level <- @levels do
    report = [something: :reported, this: level]
    metadata = [user_id: 42, request_id: "xU32kFa"]
    article = if level in [:info, :error, :alert, :emergency], do: "an", else: "a"

    @doc """
    Logs #{article} #{level} message.

    Returns `:ok`.

    ## Examples

    Logging a message (string or iodata):

        Logger.#{level}("this is #{article} #{level} message")

    Report message (maps or keywords):

        # as keyword list
        Logger.#{level}(#{inspect(report)})

        # as map
        Logger.#{level}(#{inspect(Map.new(report))})

    Report message with metadata (maps or keywords):

        # as a keyword list
        Logger.#{level}("this is #{article} #{level} message", #{inspect(metadata)})

        # as map
        Logger.#{level}("this is #{article} #{level} message", #{inspect(Map.new(metadata))})
    """

    # Only macros generated for the "new" Erlang levels are available since 1.11.0. Other
    # ones, such as :info or :debug, are available since the early days of Elixir.
    if level in @new_erlang_levels do
      @doc since: "1.11.0"
    end

    defmacro unquote(level)(message_or_fun, metadata \\ []) do
      maybe_log(unquote(level), message_or_fun, metadata, __CALLER__)
    end
  end

  @deprecated "Use Logger.warning/2 instead"
  defmacro warn(message_or_fun, metadata \\ []) do
    maybe_log(:warning, message_or_fun, metadata, __CALLER__)
  end

  @doc """
  Logs a message with the given `level`.

  Returns `:ok`.

  The macros `debug/2`, `info/2`, `notice/2`, `warning/2`,
  `error/2`, `critical/2`, `alert/2`, and `emergency/2` are
  preferred over this macro as they can automatically eliminate
  the call to `Logger` altogether at compile time if desired
  (see the documentation for the `Logger` module).
  """
  defmacro log(level, message_or_fun, metadata \\ []) do
    macro_log(level, message_or_fun, metadata, __CALLER__)
  end

  defp macro_log(level, data, metadata, caller) do
    {maybe_application, file} = compile_time_application_and_file(caller)

    location =
      case caller do
        %{module: module, function: {fun, arity}, line: line} ->
          %{mfa: {module, fun, arity}, file: file, line: line}

        _ ->
          %{}
      end

    {compile_metadata, quoted_metadata} =
      if Keyword.keyword?(metadata) do
        metadata = Keyword.merge(maybe_application, metadata)
        {Map.merge(location, Map.new(metadata)), escape_metadata(metadata)}
      else
        {%{},
         quote do
           Enum.into(unquote(metadata), unquote(escape_metadata(maybe_application)))
         end}
      end

    compile_level = if is_atom(level), do: level, else: :error

    cond do
      compile_time_purge_matching?(compile_level, compile_metadata) ->
        no_log(data, quoted_metadata)

      Application.get_env(:logger, :always_evaluate_messages, false) ->
        quote do
          data = Logger.__evaluate_log__(unquote(data))
          metadata = unquote(quoted_metadata)

          case Logger.__should_log__(unquote(level), __MODULE__) do
            nil -> :ok
            level -> Logger.__do_log__(level, data, unquote(Macro.escape(location)), metadata)
          end
        end

      true ->
        quote do
          case Logger.__should_log__(unquote(level), __MODULE__) do
            nil ->
              :ok

            level ->
              Logger.__do_log__(
                level,
                unquote(data),
                unquote(Macro.escape(location)),
                unquote(quoted_metadata)
              )
          end
        end
    end
  end

  defp escape_metadata(metadata) do
    {_, metadata} =
      Keyword.get_and_update(metadata, :mfa, fn
        nil -> :pop
        mfa -> {mfa, Macro.escape(mfa)}
      end)

    {:%{}, [], metadata}
  end

  defp compile_time_application_and_file(%{file: file}) do
    if app = Application.get_env(:logger, :compile_time_application) do
      {[application: app], file |> Path.relative_to_cwd() |> String.to_charlist()}
    else
      {[], String.to_charlist(file)}
    end
  end

  defp compile_time_purge_matching?(level, compile_metadata) do
    matching = Application.get_env(:logger, :compile_time_purge_matching, [])

    if not is_list(matching) do
      bad_compile_time_purge_matching!(matching)
    end

    Enum.any?(matching, fn filter ->
      if not is_list(filter) do
        bad_compile_time_purge_matching!(matching)
      end

      Enum.all?(filter, fn
        {:level_lower_than, min_level} ->
          compare_levels(level, min_level) == :lt

        {:module, module} ->
          match?({:ok, {^module, _, _}}, Map.fetch(compile_metadata, :mfa))

        {:function, func} ->
          case Map.fetch(compile_metadata, :mfa) do
            {:ok, {_, f, a}} -> "#{f}/#{a}" == func
            _ -> false
          end

        {k, v} when is_atom(k) ->
          Map.fetch(compile_metadata, k) == {:ok, v}

        _ ->
          bad_compile_time_purge_matching!(matching)
      end)
    end)
  end

  defp bad_compile_time_purge_matching!(matching) do
    raise "expected :compile_time_purge_matching to be a list of keyword lists, " <>
            "got: #{inspect(matching)}"
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

    if compare_levels(level, min_level) != :lt do
      macro_log(level, data, metadata, caller)
    else
      no_log(data, metadata)
    end
  end

  defp no_log(data, metadata) do
    if Application.get_env(:logger, :always_evaluate_messages, false) do
      quote do
        Logger.__evaluate_log__(unquote(data))
        unquote(metadata)
        :ok
      end
    else
      # We wrap the contents in an anonymous function
      # to avoid unused variable warnings.
      quote do
        _ = fn -> {unquote(data), unquote(metadata)} end
        :ok
      end
    end
  end

  defp elixir_level_to_erlang_level(:warn) do
    IO.warn("the log level :warn is deprecated, use :warning instead")
    :warning
  end

  defp elixir_level_to_erlang_level(other), do: other
end
