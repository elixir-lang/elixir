defmodule Logger.Backends.Config do
  @moduledoc false

  @behaviour :gen_event
  @name __MODULE__
  @update_counter_message {__MODULE__, :update_counter}

  def configure(options) do
    :gen_event.call(Logger, @name, {:configure, options})
  end

  ## Callbacks

  def init(counter) do
    state = load_state(counter)
    state = update_counter(state, false)
    schedule_update_counter(state)
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
    Enum.each(options, fn {key, value} ->
      Application.put_env(:logger, key, value)
    end)

    {:ok, :ok, load_state(counter)}
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

  @counter_pos 1

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
    total = :counters.get(counter, @counter_pos)
    {:message_queue_len, length} = Process.info(self(), :message_queue_len)
    :counters.add(counter, @counter_pos, length - total)

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
    system_time = :os.system_time(:microsecond)
    utc_log = Application.fetch_env!(:logger, :utc_log)
    date_time_ms = Logger.Formatter.system_time_to_date_time_ms(system_time, utc_log)
    event = {Logger, message, date_time_ms, pid: self()}
    :gen_event.notify(self(), {:warning, Process.group_leader(), event})
  end

  defp schedule_update_counter({_, _, _, discard_period}) do
    Process.send_after(self(), @update_counter_message, discard_period)
  end
end
