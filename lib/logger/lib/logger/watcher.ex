defmodule Logger.Watcher do
  @moduledoc false

  require Logger
  use GenServer
  @name Logger.Watcher

  @doc """
  Starts the watcher supervisor.
  """
  def start_link(m, f, a) do
    import Supervisor.Spec
    child = worker(__MODULE__, [],
      [function: :watcher, restart: :transient])
    options  = [strategy: :simple_one_for_one, name: @name]
    case Supervisor.start_link([child], options) do
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
    GenEvent.remove_handler(mod, handler, :ok)
  end

  @doc """
  Watches the given handler as part of the watcher supervision tree.
  """
  def watch(mod, handler, args) do
    case Supervisor.start_child(@name, [mod, handler, args]) do
      {:ok, _pid} = result ->
        result
      {:error, _reason} = error ->
        error
    end
  end

  @doc """
  Starts a watcher server.

  This is useful when there is a need to start a handler
  outside of the handler supervision tree.
  """
  def watcher(mod, handler, args, style \\ :monitor) do
    GenServer.start_link(__MODULE__, {mod, handler, args, style})
  end

  ## Callbacks

  @doc false
  def init({mod, handler, args, :monitor}) do
    ref = Process.monitor(mod)
    res = GenEvent.add_mon_handler(mod, handler, args)
    do_init(res, mod, handler, ref)
  end

  def init({mod, handler, args, :link}) do
    res = :gen_event.add_sup_handler(mod, handler, args)
    do_init(res, mod, handler, nil)
  end

  defp do_init(res, mod, handler, ref) do
    case res do
      :ok ->
        {:ok, {mod, handler, ref}}
      {:error, :ignore} ->
        # Can't return :ignore as a transient child under a simple_one_for_one.
        # Instead return ok and then immediately exit normally - using a fake
        # message.
        send(self(), {:gen_event_EXIT, handler, :normal})
        {:ok, {mod, handler, ref}}
      {:error, reason}  ->
        {:stop, reason}
    end
  end

  @doc false
  def handle_info({:gen_event_EXIT, handler, reason}, {_, handler, _} = state)
      when reason in [:normal, :shutdown] do
    {:stop, reason, state}
  end

  def handle_info({:gen_event_EXIT, handler, reason}, {mod, handler, _} = state) do
    _ = Logger.error "GenEvent handler #{inspect handler} installed at #{inspect mod}\n" <>
                 "** (exit) #{format_exit(reason)}"
    {:stop, reason, state}
  end

  def handle_info({:DOWN, ref, _, _, reason}, {mod, handler, ref} = state) do
    _ = Logger.error "GenEvent handler #{inspect handler} installed at #{inspect mod}\n" <>
                 "** (exit) #{format_exit(reason)}"
    {:stop, reason, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  defp format_exit({:EXIT, reason}), do: Exception.format_exit(reason)
  defp format_exit(reason), do: Exception.format_exit(reason)
end
