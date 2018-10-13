defmodule Logger.App do
  @moduledoc false

  use Application

  @doc false
  def start(_type, _args) do
    otp_reports? = Application.get_env(:logger, :handle_otp_reports)
    sasl_reports? = Application.get_env(:logger, :handle_sasl_reports)

    otp_children =
      if otp_logger?() do
        []
      else
        threshold = Application.get_env(:logger, :discard_threshold_for_error_logger)
        arg = {:error_logger, Logger.ErrorHandler, {otp_reports?, sasl_reports?, threshold}}
        [%{id: Logger.ErrorHandler, start: {Logger.Watcher, :start_link, [arg]}}]
      end

    children = [
      %{
        id: :gen_event,
        start: {:gen_event, :start_link, [{:local, Logger}]},
        modules: :dynamic
      },
      {Logger.Watcher, {Logger, Logger.Config, []}},
      Logger.BackendSupervisor
      | otp_children
    ]

    config = Logger.Config.new()

    case Supervisor.start_link(children, strategy: :rest_for_one, name: Logger.Supervisor) do
      {:ok, sup} ->
        if otp_logger?() do
          if otp_reports? do
            add_elixir_handler(sasl_reports?)
            delete_erlang_handler()
          end
        else
          delete_old_handlers(otp_reports?, sasl_reports?)
        end

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
    if otp_logger?() do
      _ = :logger.remove_handler(Logger)
    end

    add_handlers(Logger.Config.deleted_handlers())
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

  # TODO: Remove conditional error_logger code once we require Erlang/OTP 21+.
  defp otp_logger? do
    is_pid(Process.whereis(:logger))
  end

  defp add_elixir_handler(sasl_reports?) do
    config = %{level: :debug, sasl_reports?: sasl_reports?}
    :logger.add_handler(Logger, Logger.ErlangHandler, config)
  end

  defp delete_erlang_handler() do
    with {:ok, %{module: module} = config} <- :logger.get_handler_config(:default),
         :ok <- :logger.remove_handler(:default) do
      %{level: level} = :logger.get_primary_config()
      :logger.update_primary_config(%{level: :debug})
      primary_config = {:primary, %{level: level}}
      handler_config = {:default, module, config}
      [] = Logger.Config.deleted_handlers([primary_config, handler_config])
      :ok
    else
      _ -> :ok
    end
  end

  defp delete_old_handlers(otp_reports?, sasl_reports?) do
    deleted =
      for {tty, true} <- [error_logger_tty_h: otp_reports?, sasl_report_tty_h: sasl_reports?],
          :error_logger.delete_report_handler(tty) != {:error, :module_not_found},
          do: tty

    [] = Logger.Config.deleted_handlers(deleted)
    :ok
  end

  defp add_handlers(handlers) do
    for handler <- handlers do
      case handler do
        {handler, module, config} ->
          :logger.add_handler(handler, module, config)

        {:primary, config} ->
          :logger.update_primary_config(config)

        handler ->
          :error_logger.add_report_handler(handler)
      end
    end

    :ok
  end
end
