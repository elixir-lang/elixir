defmodule Logger.App do
  @moduledoc false

  use Application

  @doc false
  def start(_type, _args) do
    import Supervisor.Spec

    otp_reports?  = Application.get_env(:logger, :handle_otp_reports)
    sasl_reports? = Application.get_env(:logger, :handle_sasl_reports)
    threshold     = Application.get_env(:logger, :discard_threshold_for_error_logger)

    options  = [strategy: :rest_for_one, name: Logger.Supervisor]
    children = [worker(:gen_event, [{:local, Logger}]),
                worker(Logger.Watcher, [Logger, Logger.Config, []],
                  [id: Logger.Config, function: :watcher]),
                supervisor(Logger.Watcher, [Logger.Config, :handlers, []]),
                worker(Logger.Watcher,
                  [:error_logger, Logger.ErrorHandler,
                    {otp_reports?, sasl_reports?, threshold}],
                  [id: Logger.ErrorHandler, function: :watcher])]

    config = Logger.Config.new()

    case Supervisor.start_link(children, options) do
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
    deleted? = fn({handler, delete?}) ->
        delete? && :error_logger.delete_report_handler(handler) != {:error, :module_not_found}
      end
    [] = Enum.filter_map(handlers, deleted?, fn({handler, _}) -> handler end)
        |> Logger.Config.deleted_handlers()
    :ok
  end

  defp add_handlers(handlers) do
    Enum.each(handlers, &:error_logger.add_report_handler/1)
  end
end
