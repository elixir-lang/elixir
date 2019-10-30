defmodule Logger.App do
  @moduledoc false

  require Logger

  use Application

  @doc false
  def start(_type, _args) do
    start_options = Application.get_env(:logger, :start_options)
    otp_reports? = Application.fetch_env!(:logger, :handle_otp_reports)
    config = Logger.Counter.new()

    children = [
      %{
        id: :gen_event,
        start: {:gen_event, :start_link, [{:local, Logger}, start_options]},
        modules: :dynamic
      },
      {Logger.Watcher, {Logger.Config, config}},
      Logger.BackendSupervisor
    ]

    primary_config = add_elixir_handler(otp_reports?, config)

    default_handlers =
      if otp_reports? do
        delete_erlang_handler()
      else
        []
      end

    handlers = [primary_config | default_handlers]

    case Supervisor.start_link(children, strategy: :rest_for_one, name: Logger.Supervisor) do
      {:ok, sup} ->
        {:ok, sup, {config, handlers}}

      {:error, _} = error ->
        Logger.Counter.delete(config)
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
    _ = :logger.remove_primary_filter(:process_disabled)

    add_handlers(handlers)
    Logger.Counter.delete(config)
  end

  @doc false
  def config_change(_changed, _new, _removed) do
    Logger.configure([])
  end

  @doc """
  Stops the application without sending messages to error logger.
  """
  def stop() do
    Application.stop(:logger)
  end

  defp add_elixir_handler(otp_reports?, counter) do
    config = %{
      level: :all,
      config: %{counter: counter},
      filter_default: :log,
      filters:
        if not otp_reports? do
          [filter_elixir: {&Logger.Filter.elixir_domain/2, :ignore}]
        else
          []
        end
    }

    primary_config = :logger.get_primary_config()
    Logger.LegacyHandler.load_log_level()
    :ok = :logger.add_primary_filter(:process_disabled, {&Logger.Filter.process_disabled/2, []})
    :ok = :logger.add_handler(Logger, Logger.LegacyHandler, config)
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
          :logger.set_primary_config(config)
      end
    end

    :ok
  end
end
