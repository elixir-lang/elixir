defmodule Logger.Config do
  @moduledoc false

  @behaviour :gen_event
  @name __MODULE__
  @table __MODULE__
  @counter_pos 1
  @update_counter_message {__MODULE__, :update_counter}

  def configure(options) do
    :gen_event.call(Logger, @name, {:configure, options})
  end

  def add_translator(translator) do
    :gen_event.call(Logger, @name, {:add_translator, translator})
  end

  def remove_translator(translator) do
    :gen_event.call(Logger, @name, {:remove_translator, translator})
  end

  def level do
    %{level: level} = read_data!(:translation_data)
    level
  end

  def compare_levels(level, level), do: :eq

  def compare_levels(left, right) do
    if level_to_number(left) > level_to_number(right), do: :gt, else: :lt
  end

  defp level_to_number(:debug), do: 0
  defp level_to_number(:info), do: 1
  defp level_to_number(:warn), do: 2
  defp level_to_number(:error), do: 3

  def translation_data do
    read_data!(:translation_data)
  end

  def log_data(level) do
    %{level: min_level} = config = read_data!(:log_data)

    if compare_levels(level, min_level) != :lt do
      %{thresholds: {counter, sync, discard}} = config
      value = bump_counter(counter)

      cond do
        value >= discard -> {:discard, config}
        value >= sync -> {:sync, config}
        true -> {:async, config}
      end
    else
      {:discard, config}
    end
  end

  def deleted_handlers do
    try do
      :ets.lookup_element(@table, :deleted_handlers, 2)
    rescue
      ArgumentError -> []
    end
  end

  def deleted_handlers(handlers) do
    :gen_event.call(Logger, @name, {:deleted_handlers, handlers})
  end

  def new do
    {new_data(), new_counter()}
  end

  def delete({data, counter}) do
    delete_data(data)
    delete_counter(counter)
  end

  ## Callbacks

  def init({@table, counter}) do
    state =
      {counter, :log, Application.fetch_env!(:logger, :discard_threshold),
       Application.fetch_env!(:logger, :discard_threshold_periodic_check)}

    read_data(:log_data) || compute_data(state)
    state = update_counter(state, false)
    {:ok, state}
  end

  def handle_event({_type, gl, _msg} = event, state) when node(gl) != node() do
    # Cross node messages are always async which also
    # means this handler won't crash in case Logger
    # is not installed in the other node.
    :gen_event.notify({Logger, node(gl)}, event)
    {:ok, state}
  end

  def handle_event(_event, state) do
    state = update_counter(state, false)
    {:ok, state}
  end

  def handle_call({:configure, options}, state) do
    Enum.each(options, fn {key, value} ->
      Application.put_env(:logger, key, value)
    end)

    {:ok, :ok, compute_data(state)}
  end

  def handle_call({:add_translator, translator}, state) do
    update_translators(fn t -> [translator | List.delete(t, translator)] end)
    {:ok, :ok, state}
  end

  def handle_call({:remove_translator, translator}, state) do
    update_translators(&List.delete(&1, translator))
    {:ok, :ok, state}
  end

  def handle_call({:deleted_handlers, new}, state) do
    old = deleted_handlers()
    update_data(:deleted_handlers, new)
    {:ok, old, state}
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

  # TODO: Use counters exclusively when we require Erlang/OTP 22+.
  defp new_counter() do
    if Code.ensure_loaded?(:counters) do
      {:counters, :counters.new(1, [:atomics])}
    else
      table = :ets.new(@table.Counter, [:public])
      :ets.insert(table, [{@counter_pos, 0}])
      {:ets, table}
    end
  end

  defp delete_counter({:ets, counter}), do: :ets.delete(counter)
  defp delete_counter({:counters, _}), do: :ok

  defp read_counter({:ets, counter}), do: :ets.lookup_element(counter, @counter_pos, 2)
  defp read_counter({:counters, counter}), do: :counters.get(counter, @counter_pos)

  defp add_counter({:ets, counter}, value),
    do: :ets.update_counter(counter, @counter_pos, {2, value})

  defp add_counter({:counters, counter}, value),
    do: :counters.add(counter, @counter_pos, value)

  defp bump_counter({:ets, counter}),
    do: :ets.update_counter(counter, @counter_pos, {2, 1})

  defp bump_counter({:counters, counter}) do
    :counters.add(counter, @counter_pos, 1)
    :counters.get(counter, @counter_pos)
  end

  ## Data helpers

  # TODO: Use persistent_term exclusively when we require Erlang/OTP 22+.
  # Once we do this, we can also:
  #
  # * Remove the initialization of deleted_handlers (the default value can be given on read)
  # * Merge translation_data and log_data into a single map
  #
  defp new_data do
    if Code.ensure_loaded?(:persistent_term) do
      :persistent_term.put({@table, :deleted_handlers}, [])
    else
      entries = [
        {:log_data, nil},
        {:translation_data, nil},
        {:deleted_handlers, []}
      ]

      _ = :ets.new(@table, [:named_table, :public, {:read_concurrency, true}])
      true = :ets.insert_new(@table, entries)
    end

    @table
  end

  defp delete_data(@table) do
    if :erlang.module_loaded(:persistent_term) do
      :persistent_term.erase({@table, :log_data})
      :persistent_term.erase({@table, :translation_data})
      :persistent_term.erase({@table, :deleted_handlers})
    else
      :ets.delete(@table)
    end
  end

  defp update_translators(fun) do
    translation_data = read_data!(:translation_data)
    translators = fun.(translation_data.translators)
    Application.put_env(:logger, :translators, translators)
    update_data(:translation_data, %{translation_data | translators: translators})
  end

  defp compute_data({counter, _mode, _discard_threshold, _discard_period}) do
    sync_threshold = Application.fetch_env!(:logger, :sync_threshold)
    discard_threshold = Application.fetch_env!(:logger, :discard_threshold)
    discard_period = Application.fetch_env!(:logger, :discard_threshold_periodic_check)

    log_data = %{
      level: Application.fetch_env!(:logger, :level),
      utc_log: Application.fetch_env!(:logger, :utc_log),
      truncate: Application.fetch_env!(:logger, :truncate),
      thresholds: {counter, sync_threshold, discard_threshold}
    }

    translation_data = %{
      level: Application.fetch_env!(:logger, :level),
      translators: Application.fetch_env!(:logger, :translators),
      truncate: Application.fetch_env!(:logger, :truncate)
    }

    update_data(:log_data, log_data)
    update_data(:translation_data, translation_data)
    {counter, :log, discard_threshold, discard_period}
  end

  defp read_data!(key) do
    try do
      read_data(key)
    rescue
      ArgumentError ->
        raise "cannot use Logger, the :logger application is not running"
    else
      nil ->
        raise "cannot use Logger, the :logger application is not running"

      data ->
        data
    end
  end

  defp read_data(key) do
    if :erlang.module_loaded(:persistent_term) do
      :persistent_term.get({@table, key}, nil)
    else
      :ets.lookup_element(@table, key, 2)
    end
  end

  defp update_data(key, value) do
    if :erlang.module_loaded(:persistent_term) do
      :persistent_term.put({@table, key}, value)
    else
      :ets.insert(@table, {key, value})
    end
  end
end
