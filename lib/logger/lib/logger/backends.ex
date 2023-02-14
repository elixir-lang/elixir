defmodule Logger.Backends do
  # TODO: Make this public
  @moduledoc false
  use Supervisor

  @name Logger.Backends

  @doc """
  Configures all backends.
  """
  def configure(options) do
    Logger.Backends.Config.configure(options)
    :ok = :logger.update_handler_config(Logger, :config, :refresh)
  end

  @doc """
  Removes the given `backend`.
  """
  def unwatch(backend) do
    handler = translate_backend(backend)

    case Supervisor.terminate_child(@name, handler) do
      :ok ->
        _ = Supervisor.delete_child(@name, handler)
        :ok

      {:error, _} = error ->
        error
    end
  end

  @doc """
  Watches the given `backend`.
  """
  def watch(backend) do
    handler = translate_backend(backend)

    spec = %{
      id: handler,
      start: {Logger.Backends.Watcher, :start_link, [{handler, backend}]},
      restart: :transient
    }

    case Supervisor.start_child(@name, spec) do
      {:error, :already_present} ->
        _ = Supervisor.delete_child(@name, handler)
        watch(backend)

      other ->
        other
    end
  end

  @doc """
  Translates the shortcut backend name into its handler.
  """
  def translate_backend(:console), do: Logger.Backends.Console
  def translate_backend(other), do: other

  ## Supervisor callbacks

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
