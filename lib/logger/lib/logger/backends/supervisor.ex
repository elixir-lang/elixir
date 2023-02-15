defmodule Logger.Backends.Supervisor do
  @moduledoc false
  use Supervisor

  @name __MODULE__

  def add(backend) do
    spec = %{
      id: backend,
      start: {Logger.Backends.Watcher, :start_link, [{backend, backend}]},
      restart: :transient
    }

    case Supervisor.start_child(@name, spec) do
      {:error, :already_present} ->
        _ = Supervisor.delete_child(@name, backend)
        add(backend)

      other ->
        other
    end
  end

  def remove(backend) do
    case Supervisor.terminate_child(@name, backend) do
      :ok ->
        _ = Supervisor.delete_child(@name, backend)
        :ok

      {:error, _} = error ->
        error
    end
  end

  def start_link(backends) do
    case Supervisor.start_link(__MODULE__, [], name: @name) do
      {:ok, _} = ok ->
        for backend <- backends do
          case add(backend) do
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
