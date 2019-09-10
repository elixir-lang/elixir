defmodule Logger.App do
  @moduledoc false

  use Application

  @doc false
  def start(_type, _args) do
    # TODO: Deprecate this and make it false by defaut unless there are
    # registered custom backends
    otp_reports? = Application.get_env(:logger, :handle_otp_reports)
    # TODO: This probably should be changed to true, as right now there is no
    # other logger for sasl_reports
    sasl_reports? = Application.get_env(:logger, :handle_sasl_reports)
    start_options = Application.get_env(:logger, :start_options)

    config = Logger.Config.new()

    children = [
      Logger.LegacyHandler,
      {Logger.Watcher, {Logger.Config, config}},
      Logger.BackendSupervisor
    ]

    handlers =
      if otp_reports? do
        primary_config = add_elixir_handler(sasl_reports?, config)

        [primary_config | delete_erlang_handler()]
      end

    case Supervisor.start_link(children, strategy: :rest_for_one, name: Logger.Supervisor) do
      {:ok, sup} ->
        {:ok, sup, {config, handlers}}

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
  def stop({config, handlers}) do
    _ = :logger.remove_handler(Logger)

    add_handlers(handlers)
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
    primary_config = :logger.get_primary_config()
    :logger.update_primary_config(%{level: :none})

    result = Application.stop(:logger)

    :logger.set_primary_config(primary_config)

    result
  end

  defp add_elixir_handler(sasl_reports?, counter) do
    sync_threshold = Application.fetch_env!(:logger, :sync_threshold)
    discard_threshold = Application.fetch_env!(:logger, :discard_threshold)

    level =
      case Application.fetch_env!(:logger, :level) do
        :warn -> :warning
        other -> other
      end

    data = %{
      level: level,
      utc_log: Application.fetch_env!(:logger, :utc_log),
      truncate: Application.fetch_env!(:logger, :truncate),
      translators: Application.fetch_env!(:logger, :translators),
      thresholds: {sync_threshold, discard_threshold},
      counter: counter
    }

    config = %{
      level: :all,
      config: data,
      filters: [
        sasl: Logger.Filter.sasl(sasl_reports?)
      ]
    }

    primary_config = :logger.get_primary_config()

    :logger.add_primary_filter(:process_disabled, Logger.Filter.process_disabled())
    :logger.add_handler(Logger, Logger.LegacyHandler, config)

    :logger.set_primary_config(:level, level)

    primary_config
  end

  defp delete_erlang_handler() do
    with {:ok, %{module: module} = config} <- :logger.get_handler_config(:default),
         :ok <- :logger.remove_handler(:default) do
      handler_config = {:default, module, config}

      [handler_config]
    else
      _ -> []
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
