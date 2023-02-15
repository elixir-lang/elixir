defmodule Logger.Backends do
  # TODO: Make this public
  @moduledoc false
  use Supervisor

  @name Logger.Backends
  @type backend :: :gen_event.handler()

  @doc """
  Configures all backends.

  The supported options are:

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

    case watch(backend) do
      {:ok, _} = ok ->
        ok

      {:error, {:already_started, _pid}} ->
        {:error, :already_present}

      {:error, _} = error ->
        error
    end
  end

  defp watch(backend) do
    spec = %{
      id: backend,
      start: {Logger.Backends.Watcher, :start_link, [{backend, backend}]},
      restart: :transient
    }

    case Supervisor.start_child(@name, spec) do
      {:error, :already_present} ->
        _ = Supervisor.delete_child(@name, backend)
        watch(backend)

      other ->
        other
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

    case Supervisor.terminate_child(@name, backend) do
      :ok ->
        _ = Supervisor.delete_child(@name, backend)
        :ok

      {:error, _} = error ->
        error
    end
  end

  ## Supervisor callbacks

  defp ensure_started() do
    unless Process.whereis(@name) do
      # TODO: Warn if :console is set on Elixir v1.19+
      backends = Application.get_env(:logger, :backends, []) -- [:console]
      Supervisor.start_child(Logger.Supervisor, {Logger.Backends.RootSupervisor, backends})
    end

    :ok
  end

  @doc false
  def start_link(backends) do
    case Supervisor.start_link(__MODULE__, [], name: @name) do
      {:ok, _} = ok ->
        for backend <- backends do
          case watch(backend) do
            {:ok, _} ->
              :ok

            {:error, {{:EXIT, exit}, _spec}} ->
              raise "EXIT when installing backend #{inspect(backend)}: " <>
                      Exception.format_exit(exit)

            {:error, error} ->
              raise "ERROR when installing backend #{inspect(backend)}: " <>
                      Exception.format_exit(error)
          end
        end

        ok

      {:error, _} = error ->
        error
    end
  end

  @impl true
  def init(children) do
    Supervisor.init(children, strategy: :one_for_one, max_restarts: 30, max_seconds: 3)
  end
end
