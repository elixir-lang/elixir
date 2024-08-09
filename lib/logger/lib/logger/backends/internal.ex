defmodule Logger.Backends.Internal do
  @moduledoc false

  use Supervisor
  @name __MODULE__
  @type backend :: :gen_event.handler()

  @doc """
  Apply runtime configuration to all backends.

  See the module doc for more information.
  """
  @backend_options [:sync_threshold, :discard_threshold, :truncate, :utc_log]
  @spec configure(keyword) :: :ok
  def configure(options) do
    ensure_started()
    Logger.Backends.Config.configure(Keyword.take(options, @backend_options))
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
    if !Process.whereis(@name) do
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
