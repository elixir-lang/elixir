defmodule Logger.App do
  @moduledoc false

  require Logger

  use Application

  @doc false
  def start(_type, _args) do
    config = Logger.Config.new()

    children = [
      Logger.LegacyHandler,
      {Logger.Watcher, {Logger.Config, config}},
      Logger.BackendSupervisor
    ]

    otp_reports? = Application.fetch_env!(:logger, :handle_otp_reports)

    primary_config = add_elixir_handler(otp_reports?, config)

    default_handlers =
      if otp_reports? do
        delete_erlang_handler()
      else
        []
      end

    handlers = [primary_config | default_handlers]

    case Supervisor.start_link(children, strategy: :rest_for_one, name: Logger.Supervisor) do
      # No deprecations before this point
      {:ok, sup} ->
        deprecations()

        {:ok, sup, {config, handlers}}

      {:error, _} = error ->
        # TODO: This will not be needed once we are OTP 22+
        Logger.Config.delete(config)
        error
    end
  end

  defp deprecations do
    # TODO: Uncomment this
    # if not Application.get_env(:logger, :handle_otp_reports) do
    #   Logger.warn("handle_otp_repors: false is deprecated")
    # end
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

    # TODO: This will not be needed once we are OTP 22+
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
    :ok = :logger.update_primary_config(%{level: :none})

    Application.stop(:logger)
  end

  defp add_elixir_handler(otp_reports?, counter) do
    level =
      case Application.fetch_env!(:logger, :level) do
        :warn -> :warning
        other -> other
      end

    # TODO: This will not be needed once we are OTP 22+
    data = %{counter: counter}

    config = %{
      level: :all,
      config: data,
      filter_default: :log,
      filters:
        if not otp_reports? do
          [
            filter_elixir: {&Logger.Filter.elixir_domain/2, :ignore}
          ]
        else
          []
        end
    }

    primary_config = :logger.get_primary_config()

    :ok = :logger.add_primary_filter(:process_disabled, Logger.Filter.process_disabled())
    :ok = :logger.add_handler(Logger, Logger.LegacyHandler, config)

    :ok = :logger.set_primary_config(:level, level)

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
