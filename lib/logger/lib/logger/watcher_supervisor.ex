defmodule Logger.WatcherSupervisor do
  @moduledoc false

  use Supervisor
  @name Logger.WatcherSupervisor

  @doc """
  Starts the watcher supervisor.
  """
  def start_link({m, f, a}) do
    case Supervisor.start_link(__MODULE__, [], name: @name) do
      {:ok, _} = ok ->
        _ = for {mod, handler, args} <- apply(m, f, a) do
          {:ok, _} = watch(mod, handler, args)
        end
        ok
      {:error, _} = error ->
        error
    end
  end

  @doc """
  Removes the given handler.
  """
  def unwatch(mod, handler) do
    child_id = {__MODULE__, {mod, handler}}
    case Supervisor.terminate_child(@name, child_id) do
      :ok ->
         _ = Supervisor.delete_child(@name, child_id)
        :ok
      {:error, _} = error ->
        error
    end
  end

  @doc """
  Watches the given handler as part of the watcher supervision tree.
  """
  def watch(mod, handler, args) do
    id = {__MODULE__, {mod, handler}}

    spec = %{
      id: id,
      start: {Logger.Watcher, :start_link, [{mod, handler, args}]},
      restart: :transient
    }

    case Supervisor.start_child(@name, spec) do
      {:error, :already_present} ->
        _ = Supervisor.delete_child(@name, id)
        watch(mod, handler, args)
      other ->
        other
    end
  end

  @doc false
  def init(children) do
    Supervisor.init(children, strategy: :one_for_one, max_restarts: 30, max_seconds: 3)
  end
end
