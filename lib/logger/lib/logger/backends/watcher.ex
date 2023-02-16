defmodule Logger.Backends.Watcher do
  @moduledoc false
  require Logger
  use GenServer

  @doc false
  def start_link(tuple) do
    GenServer.start_link(__MODULE__, tuple)
  end

  @impl true
  def init({handler, args}) do
    Process.flag(:trap_exit, true)

    case :gen_event.delete_handler(Logger, handler, :ok) do
      {:error, :module_not_found} ->
        case :gen_event.add_sup_handler(Logger, handler, args) do
          :ok ->
            {:ok, handler}

          {:error, :ignore} ->
            # Can't return :ignore as a transient child under a one_for_one.
            # Instead return ok and then immediately exit normally - using a fake
            # message.
            send(self(), {:gen_event_EXIT, handler, :normal})
            {:ok, handler}

          {:error, reason} ->
            {:stop, reason}

          {:EXIT, _} = exit ->
            {:stop, exit}
        end

      _ ->
        init({handler, args})
    end
  end

  @impl true
  def handle_info({:gen_event_EXIT, handler, reason}, handler)
      when reason in [:normal, :shutdown] do
    {:stop, reason, handler}
  end

  def handle_info({:gen_event_EXIT, handler, reason}, handler) do
    message = [
      ":gen_event handler ",
      inspect(handler),
      " installed in Logger terminating\n",
      "** (exit) ",
      format_exit(reason)
    ]

    cond do
      logger_has_backends?() -> :ok
      true -> IO.puts(:stderr, message)
    end

    {:stop, reason, handler}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  defp logger_has_backends?() do
    try do
      :gen_event.which_handlers(Logger) != [Logger.Backends.Config]
    catch
      _, _ -> false
    end
  end

  @impl true
  def terminate(_reason, handler) do
    # On terminate we remove the handler, this makes the
    # process sync, allowing existing messages to be flushed
    :gen_event.delete_handler(Logger, handler, :ok)
    :ok
  end

  defp format_exit({:EXIT, reason}), do: Exception.format_exit(reason)
  defp format_exit(reason), do: Exception.format_exit(reason)
end
