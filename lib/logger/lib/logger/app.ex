defmodule Logger.App do
  @moduledoc false

  use Application

  @doc false
  def start(_type, _args) do
    otp_reports?  = Application.get_env(:logger, :handle_otp_reports)
    sasl_reports? = Application.get_env(:logger, :handle_sasl_reports)
    threshold     = Application.get_env(:logger, :discard_threshold_for_error_logger)
    error_handler = {:error_logger, Logger.ErrorHandler, {otp_reports?, sasl_reports?, threshold}}

    children = [
      %{
        id: :gen_event,
        start: {:gen_event, :start_link, [{:local, Logger}]},
        modules: :dynamic
      },
      {Logger.Watcher, {Logger, Logger.Config, []}},
      {Logger.WatcherSupervisor, {Logger.Config, :handlers, []}},
      %{
        id: Logger.ErrorHandler,
        start: {Logger.Watcher, :start_link, [error_handler]}
      }
    ]

    config = Logger.Config.new()

    case Supervisor.start_link(children, strategy: :rest_for_one, name: Logger.Supervisor) do
      {:ok, sup} ->
        handlers = [error_logger_tty_h: otp_reports?,
                    sasl_report_tty_h: sasl_reports?]
        delete_handlers(handlers)
        {:ok, sup, config}
      {:error, _} = error ->
        Logger.Config.delete(config)
        error
    end
  end

  @doc false
  def start do
    Application.start(:logger)
  end

  @doc false
  def stop(config) do
    Logger.Config.deleted_handlers()
    |> add_handlers()
    Logger.Config.delete(config)
  end

  @doc false
  def config_change(_changed, _new, _removed) do
    Logger.Config.configure([])
  end

  @doc """
  Stops the application without sending messages to error logger.
  """
  def stop() do
    try do
      Logger.Config.deleted_handlers([])
    catch
      :exit, {:noproc, _} ->
        {:error, {:not_started, :logger}}
    else
      deleted_handlers ->
        result = Application.stop(:logger)
        add_handlers(deleted_handlers)
        result
    end
  end

  defp delete_handlers(handlers) do
    to_delete =
      for {handler, delete?} <- handlers,
          delete? && :error_logger.delete_report_handler(handler) != {:error, :module_not_found},
          do: handler
    [] = Logger.Config.deleted_handlers(to_delete)
    :ok
  end

  defp add_handlers(handlers) do
    Enum.each(handlers, &:error_logger.add_report_handler/1)
  end
end
