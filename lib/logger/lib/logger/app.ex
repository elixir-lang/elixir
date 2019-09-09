defmodule Logger.App do
  @moduledoc false

  # TODO: Remove this once we support Erlang/OTP 21+ exclusively.
  @compile {:no_warn_undefined, :logger}

  use Application

  @doc false
  def start(_type, _args) do
    otp_reports? = Application.get_env(:logger, :handle_otp_reports)
    sasl_reports? = Application.get_env(:logger, :handle_sasl_reports)
    start_options = Application.get_env(:logger, :start_options)

    config = Logger.Config.new()

    children = [
      %{
        id: :gen_event,
        start: {:gen_event, :start_link, [{:local, Logger}, start_options]},
        modules: :dynamic
      },
      {Logger.Watcher, {Logger, Logger.Config, config}},
      Logger.BackendSupervisor
    ]

    case Supervisor.start_link(children, strategy: :rest_for_one, name: Logger.Supervisor) do
      {:ok, sup} ->
        if otp_reports? do
          add_elixir_handler(sasl_reports?)
          delete_erlang_handler()
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
    _ = :logger.remove_handler(Logger)

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

  defp add_elixir_handler(sasl_reports?) do
    data = %{
      utc_log: Application.fetch_env!(:logger, :utc_log),
      truncate: Application.fetch_env!(:logger, :truncate)
    }

    config = %{
      level: :all,
      config: data,
      filters: [
        sasl: Logger.Filter.sasl(sasl_reports?),
        process_disabled: Logger.Filter.process_disabled()
      ]
    }

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

  defp add_handlers(handlers) do
    for handler <- handlers do
      case handler do
        {handler, module, config} ->
          :logger.add_handler(handler, module, config)

        {:primary, config} ->
          :logger.update_primary_config(config)
      end
    end

    :ok
  end
end
