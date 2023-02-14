defmodule Logger.App do
  @moduledoc false

  require Logger

  use Application

  @doc false
  def start(_type, _args) do
    otp_reports? = Application.fetch_env!(:logger, :handle_otp_reports)
    sasl_reports? = Application.fetch_env!(:logger, :handle_sasl_reports)
    translators = Application.fetch_env!(:logger, :translators)

    {backends, console?} =
      case Application.fetch_env(:logger, :backends) do
        {:ok, backends} -> {List.delete(backends, :console), :console in backends}
        :error -> {[], true}
      end

    primary_config = :logger.get_primary_config()
    :ok = :logger.set_primary_config(:level, default_level())

    process_level_filter = {&Logger.Filter.process_level/2, []}
    :ok = :logger.add_primary_filter(:logger_process_level, process_level_filter)

    config = %{translators: translators, otp: otp_reports?, sasl: sasl_reports?}
    translator_filter = {&Logger.Filter.translator/2, config}
    :ok = :logger.add_primary_filter(:logger_translator, translator_filter)

    revert = [{:set_primary_config, [primary_config]} | update_default_handler(console?)]
    children = [{Logger.Backends.RootSupervisor, backends}]

    case Supervisor.start_link(children, strategy: :one_for_one, name: Logger.Supervisor) do
      {:ok, sup} ->
        {:ok, sup, revert}

      {:error, _} = error ->
        redo(revert)
        error
    end
  end

  @doc false
  def start do
    Application.start(:logger)
  end

  @doc false
  def stop(revert) do
    _ = :logger.remove_handler(Logger)
    _ = :logger.remove_primary_filter(:logger_process_level)
    _ = :logger.remove_primary_filter(:logger_translator)

    redo(revert)

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

  defp update_default_handler(console?) do
    case :logger.get_handler_config(:default) do
      {:ok, %{module: module} = config} ->
        case Application.fetch_env(:logger, :default) do
          # If default was explicitly disabled or backends was listed
          # without console, we won't add any handler.
          result when result == {:ok, false} or console? == false ->
            case :logger.remove_handler(:default) do
              :ok -> [{:add_handler, [:default, module, config]}]
              _ -> []
            end

          # Otherwise use the default configuration if provided
          {:ok, default} when is_list(default) ->
            update_default_handler(default, config)

          # Otherwise translate console to the new configuration
          # TODO: Deprecate if config(:logger, :console) is set
          :error ->
            default =
              if console = Application.get_env(:logger, :console) do
                {with_level, without_level} = Keyword.split(console, [:level])
                with_level ++ [formatter: Logger.Formatter.new(without_level)]
              else
                []
              end

            update_default_handler(default, config)
        end

      _ ->
        []
    end
  end

  defp update_default_handler(default, config) do
    new_config =
      default
      |> Keyword.put_new_lazy(:formatter, &Logger.Formatter.new/0)
      |> Keyword.put_new(:filters, [remote_gl: {&:logger_filters.remote_gl/2, :stop}])
      |> Keyword.put_new(:filter_default, :log)
      |> Map.new()

    case :logger.update_handler_config(:default, new_config) do
      :ok -> [{:update_handler_config, [:default, config]}]
      _ -> []
    end
  end

  defp redo(handlers) do
    for {fun, args} <- handlers do
      apply(:logger, fun, args)
    end
  end
end
