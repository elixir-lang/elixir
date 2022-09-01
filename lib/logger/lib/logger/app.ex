defmodule Logger.App do
  @moduledoc false

  require Logger

  use Application

  @doc false
  def start(_type, _args) do
    start_options = Application.get_env(:logger, :start_options)
    otp_reports? = Application.fetch_env!(:logger, :handle_otp_reports)
    counter = :counters.new(1, [:atomics])

    children = [
      %{
        id: :gen_event,
        start: {:gen_event, :start_link, [{:local, Logger}, start_options]},
        modules: :dynamic
      },
      {Logger.Watcher, {Logger.Config, counter}},
      Logger.BackendSupervisor
    ]

    case Supervisor.start_link(children, strategy: :rest_for_one, name: Logger.Supervisor) do
      {:ok, sup} ->
        primary_config = add_elixir_handler(otp_reports?, counter)

        default_handlers =
          if otp_reports? do
            delete_erlang_handler()
          else
            []
          end

        handlers = [{:primary, primary_config} | default_handlers]
        {:ok, sup, handlers}

      {:error, _} = error ->
        error
    end
  end

  @doc false
  def start do
    Application.start(:logger)
  end

  @doc false
  def stop(handlers) do
    _ = :logger.remove_handler(Logger)
    _ = :logger.remove_primary_filter(:process_level)
    add_handlers(handlers)

    :logger.add_primary_filter(
      :silence_logger_exit,
      {&Logger.Filter.silence_logger_exit/2, []}
    )
  end

  @doc false
  def config_change(changed, _new, _removed) do
    # All other config has already been persisted, we only need to
    # update the level and reload the logger state.
    Logger.configure(Keyword.take(changed, [:level]))
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
          [filter_elixir_domain: {&Logger.Filter.filter_elixir_domain/2, []}]
        else
          []
        end
    }

    primary_config = :logger.get_primary_config()
    level = Application.get_env(:logger, :level, :debug)

    level =
      if level == :warn do
        IO.warn(":logger has be set to :warn in config files, please use :warning instead")
        :warning
      else
        level
      end

    level = Logger.Handler.elixir_level_to_erlang_level(level)

    :ok = :logger.set_primary_config(:level, level)
    :ok = :logger.add_primary_filter(:process_level, {&Logger.Filter.process_level/2, []})
    :ok = :logger.add_handler(Logger, Logger.Handler, config)
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
