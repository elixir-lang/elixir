defmodule Logger.Config do
  @moduledoc false

  # TODO Remove this once we support Erlang/OTP 22+ exclusively.
  @compile {:no_warn_undefined, [:persistent_term, :counters]}

  @behaviour :gen_event
  @name __MODULE__
  @table __MODULE__
  @counter_pos 1
  @update_counter_message {__MODULE__, :update_counter}

  def configure(options) do
    :gen_event.call(Logger, @name, {:configure, options})
  end

  def default_config do
    sync_threshold = Application.fetch_env!(:logger, :sync_threshold)
    discard_threshold = Application.fetch_env!(:logger, :discard_threshold)
    level = Application.fetch_env!(:logger, :level)
    sasl_reports? = Application.get_env(:logger, :handle_sasl_reports)

    %{
      level: level,
      utc_log: Application.fetch_env!(:logger, :utc_log),
      truncate: Application.fetch_env!(:logger, :truncate),
      translators: Application.fetch_env!(:logger, :translators),
      thresholds: {sync_threshold, discard_threshold},
      sasl: sasl_reports?
    }
  end

  def add_translator(translator) do
    update_translators(fn t -> [translator | List.delete(t, translator)] end)
  end

  def remove_translator(translator) do
    update_translators(&List.delete(&1, translator))
  end

  def level do
    %{level: level} = :logger.get_primary_config()

    level
  end

  def compare_levels(level, level), do: :eq
  def compare_levels(:all, _), do: :gt
  def compare_levels(_, :all), do: :lt
  def compare_levels(:none, _), do: :lt
  def compare_levels(_, :none), do: :gt

  def compare_levels(left, right) do
    :logger.compare_levels(normalise(left), normalise(right))
  end

  defp normalise(:warn), do: :warning
  defp normalise(level), do: level

  # TODO: Use counters exclusively when we require Erlang/OTP 22+.
  def new() do
    if Code.ensure_loaded?(:counters) do
      {:counters, :counters.new(1, [:atomics])}
    else
      table = :ets.new(@table.Counter, [:public])
      :ets.insert(table, [{@counter_pos, 0}])
      {:ets, table}
    end
  end

  def delete({:ets, counter}), do: :ets.delete(counter)
  def delete({:counters, _}), do: :ok

  ## Callbacks

  def init(counter) do
    state =
      {counter, :log, Application.fetch_env!(:logger, :discard_threshold),
       Application.fetch_env!(:logger, :discard_threshold_periodic_check)}

    compute_data(state)
    state = update_counter(state, false)
    {:ok, state}
  end

  def handle_event({_type, gl, _msg} = event, state) when node(gl) != node() do
    # Cross node messages are always async which also
    # means this handler won't crash in case Logger
    # is not installed in the other node.
    :gen_event.notify({Logger, node(gl)}, event)
    {:ok, update_counter(state, false)}
  end

  def handle_event(_event, state) do
    {:ok, update_counter(state, false)}
  end

  def handle_call({:configure, options}, state) do
    Enum.each(options, fn {key, value} ->
      Application.put_env(:logger, key, value)
    end)

    {:ok, :ok, compute_data(state)}
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
    total = read_counter(counter)
    {:message_queue_len, length} = Process.info(self(), :message_queue_len)
    add_counter(counter, length - total)

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

  ## Counter Helpers

  defp read_counter({:ets, counter}), do: :ets.lookup_element(counter, @counter_pos, 2)
  defp read_counter({:counters, counter}), do: :counters.get(counter, @counter_pos)

  defp add_counter({:ets, counter}, value),
    do: :ets.update_counter(counter, @counter_pos, {2, value})

  defp add_counter({:counters, counter}, value),
    do: :counters.add(counter, @counter_pos, value)

  ## Data helpers

  defp update_translators(fun) do
    {:ok, %{config: data}} = :logger.get_handler_config(Logger)
    translators = fun.(data.translators)
    Application.put_env(:logger, :translators, translators)
    :ok = :logger.set_handler_config(Logger, :config, %{data | translators: translators})
  end

  defp compute_data({counter, _mode, _discard_threshold, _discard_period}) do
    sync_threshold = Application.fetch_env!(:logger, :sync_threshold)
    discard_threshold = Application.fetch_env!(:logger, :discard_threshold)
    discard_period = Application.fetch_env!(:logger, :discard_threshold_periodic_check)

    data = %{
      level: Application.fetch_env!(:logger, :level),
      utc_log: Application.fetch_env!(:logger, :utc_log),
      truncate: Application.fetch_env!(:logger, :truncate),
      translators: Application.fetch_env!(:logger, :translators),
      thresholds: {sync_threshold, discard_threshold}
    }

    :ok = :logger.update_handler_config(Logger, :config, data)
    :ok = :logger.set_primary_config(:level, normalise(data.level))

    {counter, :log, discard_threshold, discard_period}
  end
end
