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
    children = [worker(GenEvent, [[name: Logger]]),
                worker(Logger.Watcher, [Logger, Logger.Config, []],
                  [id: Logger.Config, function: :watcher]),
                supervisor(Logger.Watcher, [Logger.Config, :handlers, []]),
                worker(Logger.Watcher,
                  [:error_logger, Logger.ErrorHandler,
                    {otp_reports?, sasl_reports?, threshold}, :link],
                  [id: Logger.ErrorHandler, function: :watcher])]

    case Supervisor.start_link(children, options) do
      {:ok, _} = ok ->
        deleted = delete_error_logger_handler(otp_reports?, :error_logger_tty_h, [])
        deleted = delete_error_logger_handler(sasl_reports?, :sasl_report_tty_h, deleted)
        store_deleted_handlers(deleted)
        ok
      {:error, _} = error ->
        error
    end
  end

  @doc false
  def stop(_) do
    Application.get_env(:logger, :deleted_handlers)
    |> Enum.each(&:error_logger.add_report_handler/1)

    # We need to do this in another process as the Application
    # Controller is currently blocked shutting down this app.
    spawn_link(fn -> Logger.Config.clear_data end)

    :ok
  end

  @doc """
  Stops the application without sending messages to error logger.
  """
  def stop() do
    set = Application.get_env(:logger, :deleted_handlers)
    Application.put_env(:logger, :deleted_handlers, HashSet.new)
    _ = Application.stop(:logger)
    Enum.each(set, &:error_logger.add_report_handler/1)
  end

  defp store_deleted_handlers(list) do
    Application.put_env(:logger, :deleted_handlers, Enum.into(list, HashSet.new))
  end

  defp delete_error_logger_handler(should_delete?, handler, deleted) do
    if should_delete? and
        :error_logger.delete_report_handler(handler) != {:error, :module_not_found} do
      [handler|deleted]
    else
      deleted
    end
  end
end
