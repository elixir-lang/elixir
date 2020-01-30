defmodule Logger.Config do
  @moduledoc false

  @behaviour :gen_event
  @name __MODULE__
  @update_counter_message {__MODULE__, :update_counter}

  alias Logger.Counter

  def configure(options) do
    :gen_event.call(Logger, @name, {:configure, options})
  end

  def add_translator(translator) do
    :gen_event.call(Logger, @name, {:add_translator, translator})
  end

  def remove_translator(translator) do
    :gen_event.call(Logger, @name, {:remove_translator, translator})
  end

  ## Callbacks

  def init(counter) do
    state = load_state(counter)
    state = update_counter(state, false)
    {:ok, state}
  end

  defp load_state(counter) do
    {counter, :log, Application.fetch_env!(:logger, :discard_threshold),
     Application.fetch_env!(:logger, :discard_threshold_periodic_check)}
  end

  def handle_event(_event, state) do
    {:ok, update_counter(state, false)}
  end

  def handle_call({:configure, options}, {counter, _, _, _}) do
    Enum.each(options, fn
      {:level, level} ->
        :logger.set_primary_config(:level, Logger.Handler.elixir_level_to_erlang_level(level))

      {key, value} ->
        Application.put_env(:logger, key, value)
    end)

    {:ok, :ok, load_state(counter)}
  end

  def handle_call({:add_translator, translator}, state) do
    update_translators(fn t -> [translator | List.delete(t, translator)] end)
    {:ok, :ok, state}
  end

  def handle_call({:remove_translator, translator}, state) do
    update_translators(&List.delete(&1, translator))
    {:ok, :ok, state}
  end

  def handle_info(@update_counter_message, state) do
    state = update_counter(state, true)
    schedule_update_counter(state)
    {:ok, state}
  end

  def handle_info(_, state) do
    {:ok, state}
  end

  def terminate(_reason, _state) do
    :ok
  end

  def code_change(_old, state, _extra) do
    {:ok, state}
  end

  defp update_counter({counter, log, discard_threshold, discard_period}, periodic_check?) do
    # If length is more than the total, it means the counter is behind,
    # due to non-log messages, so we need to increase the counter.
    #
    # If length is less than the total, then we either have a spike or
    # the counter drifted due to failures.
    #
    # Because we always bump the counter and then we send the message,
    # there is a chance clients have bumped the counter but they did not
    # deliver the message yet. Those bumps will be lost. At the same time,
    # we are careful to read the counter first here, so if the counter is
    # bumped after we read from it, those bumps won't be lost.
    total = Counter.read(counter)
    {:message_queue_len, length} = Process.info(self(), :message_queue_len)
    Counter.add(counter, length - total)

    # In case we are logging but we reached the threshold, we log that we
    # started discarding messages. This can only be reverted by the periodic
    # discard check.
    cond do
      total >= discard_threshold ->
        if log == :log or periodic_check? do
          warn("Attempted to log #{total} messages, which is above :discard_threshold")
        end

        {counter, :discard, discard_threshold, discard_period}

      log == :discard and periodic_check? ->
        warn("Attempted to log #{total} messages, which is below :discard_threshold")
        {counter, :log, discard_threshold, discard_period}

      true ->
        {counter, log, discard_threshold, discard_period}
    end
  end

  defp warn(message) do
    utc_log = Application.fetch_env!(:logger, :utc_log)
    event = {Logger, message, Logger.Utils.timestamp(utc_log), pid: self()}
    :gen_event.notify(self(), {:warn, Process.group_leader(), event})
  end

  defp schedule_update_counter({_, _, _, discard_period}) do
    Process.send_after(self(), @update_counter_message, discard_period)
  end

  ## Data helpers

  defp update_translators(fun) do
    {:ok, %{config: data}} = :logger.get_handler_config(Logger)
    translators = fun.(data.translators)
    Application.put_env(:logger, :translators, translators)
    # TODO: Use update_handler_config on Erlang/OTP 22+.
    :ok = :logger.set_handler_config(Logger, :config, %{data | translators: translators})
  end
end
