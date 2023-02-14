defmodule Logger.App do
  @moduledoc false

  require Logger

  use Application

  @doc false
  def start(_type, _args) do
    start_options = Application.get_env(:logger, :start_options)
    otp_reports? = Application.fetch_env!(:logger, :handle_otp_reports)
    sasl_reports? = Application.fetch_env!(:logger, :handle_sasl_reports)
    translators = Application.fetch_env!(:logger, :translators)
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
        primary_config = :logger.get_primary_config()
        :ok = :logger.set_primary_config(:level, default_level())

        process_level_filter = {&Logger.Filter.process_level/2, []}
        :ok = :logger.add_primary_filter(:logger_process_level, process_level_filter)

        config = %{translators: translators, otp: otp_reports?, sasl: sasl_reports?}
        translator_filter = {&Logger.Filter.translator/2, config}
        :ok = :logger.add_primary_filter(:logger_translator, translator_filter)

        add_elixir_handler(counter)
        handlers = [{:primary, primary_config} | delete_erlang_handler()]
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
    _ = :logger.remove_primary_filter(:logger_process_level)
    _ = :logger.remove_primary_filter(:logger_translator)
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

  ## Helpers

  defp default_level() do
    case Application.get_env(:logger, :level, :debug) do
      :warn ->
        IO.warn(":logger has be set to :warn in config files, please use :warning instead")
        :warning

      level ->
        level
    end
  end

  defp add_elixir_handler(counter) do
    :ok =
      :logger.add_handler(Logger, Logger.Handler, %{
        level: :all,
        config: %{counter: counter},
        filter_default: :log,
        filters: []
      })
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
