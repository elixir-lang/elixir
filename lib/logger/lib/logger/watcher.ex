defmodule Logger.Watcher do
  @moduledoc false

  require Logger
  use GenServer
  @name Logger.Watcher

  @doc """
  Starts the watcher supervisor.
  """
  def start_link(m, f, a) do
    options = [strategy: :one_for_one, name: @name, max_restarts: 30, max_seconds: 3]
    case Supervisor.start_link([], options) do
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
    import Supervisor.Spec
    id = {__MODULE__, {mod, handler}}
    child = worker(__MODULE__, [mod, handler, args], id: id, function: :watcher, restart: :transient)
    case Supervisor.start_child(@name, child) do
      {:error, :already_present} ->
        _ = Supervisor.delete_child(@name, id)
        watch(mod, handler, args)
      other ->
        other
    end
  end

  @doc """
  Starts a watcher server.

  This is useful when there is a need to start a handler
  outside of the handler supervision tree.
  """
  def watcher(mod, handler, args) do
    GenServer.start_link(__MODULE__, {mod, handler, args})
  end

  ## Callbacks

  @doc false
  def init({mod, handler, args}) do
    case :gen_event.delete_handler(mod, handler, :ok) do
      {:error, :module_not_found} ->
        res = :gen_event.add_sup_handler(mod, handler, args)
        do_init(res, mod, handler)
      _ ->
        init({mod, handler, args})
    end
  end

  defp do_init(res, mod, handler) do
    case res do
      :ok ->
        {:ok, {mod, handler}}
      {:error, :ignore} ->
        # Can't return :ignore as a transient child under a one_for_one.
        # Instead return ok and then immediately exit normally - using a fake
        # message.
        send(self(), {:gen_event_EXIT, handler, :normal})
        {:ok, {mod, handler}}
      {:error, reason}  ->
        {:stop, reason}
    end
  end

  @doc false
  def handle_info({:gen_event_EXIT, handler, reason}, {_, handler} = state)
      when reason in [:normal, :shutdown] do
    {:stop, reason, state}
  end

  def handle_info({:gen_event_EXIT, handler, reason}, {mod, handler} = state) do
    _ = Logger.error ":gen_event handler #{inspect handler} installed at #{inspect mod}\n" <>
                 "** (exit) #{format_exit(reason)}"
    {:stop, reason, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  defp format_exit({:EXIT, reason}), do: Exception.format_exit(reason)
  defp format_exit(reason), do: Exception.format_exit(reason)
end
