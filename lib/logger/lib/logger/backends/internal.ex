defmodule Logger.Backends.Internal do
  # TODO: Move the docs to a separate package

  @moduledoc """
  `:gen_event`-based backends with overload protection.

  This module provides backends for Elixir's Logger with
  built-in overload protection. This was the default
  mechanism for hooking into Elixir's Logger until Elixir v1.15.

  Elixir backends run in a separate process which comes with overload
  protection. All backends run in this same process as a unified front
  for handling log events.

  The available backends by default are:

    * `Logger.Backends.Console` - logs messages to the console
      (see its documentation for more information)

  Developers may also implement their own backends, an option that
  is explored in more detail later.

  The initial backends are loaded via the `:backends` configuration.
  However, by the time this application starts, the code for your
  own and third-party backends may not yet be available. For this reason,
  it is preferred to add and remove backends via `add/2` and
  `remove/2` functions. This is often done in your `c:Application.start/2`
  callback:

      @impl true
      def start(_type, _args) do
        Logger.Backends.add(MyCustomBackend)

  The backend can be configured either on the `add_backend/2` call:

      @impl true
      def start(_type, _args) do
        Logger.Backends.add(MyCustomBackend, some_config: ...)

  Or in your config files:

      config :logger, MyCustomBackend,
        some_config: ...

  ## Application configuration

  Application configuration goes under the `:logger` application for
  backwards compatibility. The following keys must be set before
  the `:logger` application (and this application) are started.

    * `:backends` - the backends to be used. Defaults to `[]`.
      See the "Backends" section for more information.

    * `:start_options` - passes start options to Logger's main process, such
      as `:spawn_opt` and `:hibernate_after`. All options in `t:GenServer.option/0`
      are accepted, except `:name`.

  ## Runtime configuration

  The following keys can be set at runtime via the `configure/1` function.
  In your config files, they also go under the `:logger` application
  for backwards compatibility.

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
      checks and reports if logger is discarding messages. It logs a warning
      message whenever the system is (or continues) in discard mode and
      it logs a warning message whenever if the system was discarding messages
      but stopped doing so after the previous check. By default it runs
      every `30_000` milliseconds.

  ## Custom backends

  Any developer can create their own backend. Since `Logger` is an
  event manager powered by `:gen_event`, writing a new backend
  is a matter of creating an event handler, as described in the
  [`:gen_event`](`:gen_event`) documentation.

  From now on, we will be using the term "event handler" to refer
  to your custom backend, as we head into implementation details.

  Once Logger starts, it installs all event handlers listed under
  the `:backends` configuration into the `Logger` event manager.
  The event manager and all added event handlers are automatically
  supervised by `Logger`.

  Note that if a backend fails to start by returning `{:error, :ignore}`
  from its `init/1` callback, then it's not added to the backends but
  nothing fails. If a backend fails to start by returning `{:error, reason}`
  from its `init/1` callback, the system will fail to start.

  Once initialized, the handler should be designed to handle the
  following events:

    * `{level, group_leader, {Logger, message, timestamp, metadata}}` where:
      * `level` is one of `:debug`, `:info`, `:warn`, or `:error`, as previously
        described (for compatibility with pre 1.10 backends the `:notice` will
        be translated to `:info` and all messages above `:error` will be translated
        to `:error`)
      * `group_leader` is the group leader of the process which logged the message
      * `{Logger, message, timestamp, metadata}` is a tuple containing information
        about the logged message:
        * the first element is always the atom `Logger`
        * `message` is the actual message (as chardata)
        * `timestamp` is the timestamp for when the message was logged, as a
          `{{year, month, day}, {hour, minute, second, millisecond}}` tuple
        * `metadata` is a keyword list of metadata used when logging the message

    * `:flush`

  It is recommended that handlers ignore messages where the group
  leader is in a different node than the one where the handler is
  installed. For example:

      def handle_event({_level, gl, {Logger, _, _, _}}, state)
          when node(gl) != node() do
        {:ok, state}
      end

  In the case of the event `:flush` handlers should flush any pending
  data. This event is triggered by `Logger.flush/0`.

  Furthermore, backends can be configured via the `configure_backend/2`
  function which requires event handlers to handle calls of the
  following format:

      {:configure, options}

  where `options` is a keyword list. The result of the call is the result
  returned by `configure_backend/2`. The recommended return value for
  successful configuration is `:ok`. For example:

      def handle_call({:configure, options}, state) do
        new_state = reconfigure_state(state, options)
        {:ok, :ok, new_state}
      end

  It is recommended that backends support at least the following configuration
  options:

    * `:level` - the logging level for that backend
    * `:format` - the logging format for that backend
    * `:metadata` - the metadata to include in that backend

  Check the `Logger.Backends.Console` implementation in Elixir's codebase
  for examples on how to handle the recommendations in this section and
  how to process the existing options.
  """

  use Supervisor

  @name __MODULE__
  @type backend :: :gen_event.handler()

  @doc """
  Apply runtime configuration to all backends.

  See the module doc for more information.
  """
  @spec configure(keyword) :: :ok
  def configure(options) do
    ensure_started()
    Logger.Backends.Config.configure(options)
    :ok = :logger.update_handler_config(Logger, :config, :refresh)
  end

  @doc """
  Configures a given backend.
  """
  @spec configure(backend, keyword) :: term
  def configure(backend, options) when is_list(options) do
    ensure_started()
    :gen_event.call(Logger, backend, {:configure, options})
  end

  @doc """
  Adds a new backend.

  Adding a backend calls the `init/1` function in that backend
  with the name of the backend as its argument. For example,
  calling

      Logger.Backends.add(MyBackend)

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
  @spec add(backend, keyword) :: Supervisor.on_start_child()
  def add(backend, opts \\ []) do
    ensure_started()
    _ = if opts[:flush], do: Logger.flush()

    case Logger.Backends.Supervisor.add(backend) do
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
  @spec remove(backend, keyword) :: :ok | {:error, term}
  def remove(backend, opts \\ []) do
    ensure_started()
    _ = if opts[:flush], do: Logger.flush()
    Logger.Backends.Supervisor.remove(backend)
  end

  ## Supervisor callbacks

  defp ensure_started() do
    unless Process.whereis(@name) do
      Supervisor.start_child(Logger.Supervisor, __MODULE__)
    end

    :ok
  end

  @doc false
  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, :ok, name: @name)
  end

  @impl true
  def init(:ok) do
    backends = Application.get_env(:logger, :backends, []) -- [:console]
    start_options = Application.fetch_env!(:logger, :start_options)
    counter = :counters.new(1, [:atomics])

    children = [
      %{
        id: :gen_event,
        start: {:gen_event, :start_link, [{:local, Logger}, start_options]},
        modules: :dynamic
      },
      {Logger.Backends.Watcher, {Logger.Backends.Config, counter}},
      {Logger.Backends.Supervisor, backends}
    ]

    :ok =
      :logger.add_handler(Logger, Logger.Backends.Handler, %{
        level: :all,
        config: %{counter: counter},
        filter_default: :log,
        filters: []
      })

    Supervisor.init(children, strategy: :rest_for_one)
  end
end
