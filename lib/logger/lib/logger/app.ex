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

    # TODO: Warn if :backends is set on Elixir v1.19
    # TODO: Warn if :console is set on Elixir v1.19
    default_handler =
      if console = Application.get_env(:logger, :console) do
        {handler, formatter} = Keyword.split(console, [:level])

        with :error <- Application.fetch_env(:logger, :default_formatter) do
          Application.put_env(:logger, :default_formatter, formatter)
        end

        handler
      else
        []
      end

    primary_config = :logger.get_primary_config()
    :ok = :logger.set_primary_config(:level, default_level())

    # If there is additional metadata in the :logger config, we merge it into
    # the primary :logger metadata.
    with [_ | _] = metadata <- Application.fetch_env!(:logger, :metadata) do
      :ok = :logger.set_primary_config(:metadata, Enum.into(metadata, primary_config.metadata))
    end

    process_level_filter = {&Logger.Utils.process_level/2, []}
    :ok = :logger.add_primary_filter(:logger_process_level, process_level_filter)

    translator_config = %{translators: translators, otp: otp_reports?, sasl: sasl_reports?}
    translator_filter = {&Logger.Utils.translator/2, translator_config}
    :ok = :logger.add_primary_filter(:logger_translator, translator_filter)

    revert = [{:set_primary_config, [primary_config]} | remove_erlang_handler()]
    children = if backends != [], do: [Logger.Backends.Internal], else: []

    case Supervisor.start_link(children, strategy: :one_for_one, name: Logger.Supervisor) do
      {:ok, sup} ->
        {:ok, sup, [add_elixir_handler(console? && default_handler) | revert]}

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
    # TODO: Remove this line and all of Logger.Backends.* on Elixir v2.0+
    _ = :logger.remove_handler(Logger)
    _ = :logger.remove_primary_filter(:logger_process_level)
    _ = :logger.remove_primary_filter(:logger_translator)

    redo(revert)

    :logger.add_primary_filter(
      :silence_logger_exit,
      {&Logger.Utils.silence_logger_exit/2, []}
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
        IO.warn(
          ":logger :level has been set to :warn in config files, please use :warning instead"
        )

        :warning

      level ->
        level
    end
  end

  defp remove_erlang_handler() do
    with {:ok, %{module: module} = config} <- :logger.get_handler_config(:default),
         :ok <- :logger.remove_handler(:default) do
      [{:add_handler, [:default, module, config]}]
    else
      _ -> []
    end
  end

  defp add_elixir_handler(default_handler) do
    if handler = Application.get_env(:logger, :default_handler, default_handler) do
      config =
        handler
        |> Keyword.put_new(:filters, remote_gl: {&:logger_filters.remote_gl/2, :stop})
        |> Keyword.put_new(:filter_default, :log)
        |> Keyword.put_new(:module, :logger_std_h)
        |> Keyword.put_new_lazy(:formatter, &Logger.default_formatter/0)
        |> Keyword.replace_lazy(:config, &Map.new/1)
        |> Map.new()

      case :logger.add_handler(:default, config.module, config) do
        :ok ->
          [{:remove_handler, [:default]}]

        {:error, error} ->
          IO.puts(:stderr, "Could not attach default Logger handler: #{inspect(error)}")
      end
    else
      []
    end
  end

  defp redo(handlers) do
    for {fun, args} <- handlers do
      apply(:logger, fun, args)
    end
  end
end
