defmodule Logger.Config do
  @moduledoc false

  @behaviour :gen_event
  @name __MODULE__
  @table __MODULE__
  @message_queue_len_pos 1
  @counter_pos 2

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
      counter = read_message_queue_len(counter) + bump_counter(counter)

      cond do
        counter >= discard -> {:discard, config}
        counter >= sync -> {:sync, config}
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
    :ets.lookup_element(@table, :log_data, 2) || compute_state(counter)
    reset_counter_and_update_message_queue_len(counter)
    {:ok, counter}
  end

  def handle_event({_type, gl, _msg} = event, counter) when node(gl) != node() do
    # Cross node messages are always async which also
    # means this handler won't crash in case Logger
    # is not installed in the other node.
    :gen_event.notify({Logger, node(gl)}, event)
    {:ok, counter}
  end

  def handle_event(_event, counter) do
    reset_counter_and_update_message_queue_len(counter)
    {:ok, counter}
  end

  def handle_call({:configure, options}, counter) do
    Enum.each(options, fn {key, value} ->
      Application.put_env(:logger, key, value)
    end)

    _ = compute_state(counter)
    {:ok, :ok, counter}
  end

  def handle_call({:add_translator, translator}, counter) do
    update_translators(fn t -> [translator | List.delete(t, translator)] end)
    {:ok, :ok, counter}
  end

  def handle_call({:remove_translator, translator}, counter) do
    update_translators(&List.delete(&1, translator))
    {:ok, :ok, counter}
  end

  def handle_call({:deleted_handlers, new}, counter) do
    old = deleted_handlers()
    true = :ets.update_element(@table, :deleted_handlers, {2, new})
    {:ok, old, counter}
  end

  def handle_info(_msg, counter) do
    {:ok, counter}
  end

  def terminate(_reason, _counter) do
    :ok
  end

  def code_change(_old, counter, _extra) do
    {:ok, counter}
  end

  # It is very important to reset the counter first and then update
  # the message queue length because that's the pessimistic approach.
  # If the system is approaching overloaded, then we will overestimate
  # the overall size, which is better than the inverse. However, once
  # we reach overload, we will underestimate the value, as the counter
  # is increased by cleaned. Therefore some messages may make it back
  # to the logger as we circle around the discard threshold value.
  defp reset_counter_and_update_message_queue_len(counter) do
    reset_counter(counter)
    update_message_queue_len(counter)
  end

  ## Counter Helpers

  # TODO: Use counters exclusively when we require Erlang/OTP 22+.
  defp new_counter() do
    if Code.ensure_loaded?(:counters) do
      {:counters, :counters.new(2, [:atomics])}
    else
      table = :ets.new(@table.Counter, [:public, write_concurrency: true])
      :ets.insert(table, [{:counter, 0}, {:message_queue_len, 0}])
      {:ets, table}
    end
  end

  defp reset_counter({:ets, counter}), do: :ets.update_element(counter, :counter, {2, 0})
  defp reset_counter({:counters, counter}), do: :counters.put(counter, @counter_pos, 0)

  defp delete_counter({:ets, counter}), do: :ets.delete(counter)
  defp delete_counter({:counters, _}), do: :ok

  defp bump_counter({:ets, counter}),
    do: :ets.update_counter(counter, :counter, {2, 1})

  defp bump_counter({:counters, counter}) do
    :counters.add(counter, @counter_pos, 1)
    :counters.get(counter, @counter_pos)
  end

  defp read_message_queue_len({:ets, counter}),
    do: :ets.lookup_element(counter, :message_queue_len, 2)

  defp read_message_queue_len({:counters, counter}),
    do: :counters.get(counter, @message_queue_len_pos)

  defp update_message_queue_len({:ets, counter}),
    do: :ets.insert(counter, Process.info(self(), :message_queue_len))

  defp update_message_queue_len({:counters, counter}) do
    {:message_queue_len, length} = Process.info(self(), :message_queue_len)
    :counters.put(counter, @counter_pos, length)
  end

  ## Data helpers

  defp new_data do
    entries = [
      {:log_data, nil},
      {:translation_data, nil},
      {:deleted_handlers, []}
    ]

    table = :ets.new(@table, [:named_table, :public, {:read_concurrency, true}])
    true = :ets.insert_new(@table, entries)
    table
  end

  defp delete_data(@table), do: :ets.delete(@table)

  defp update_translators(fun) do
    translation_data = read_data!(:translation_data)
    translators = fun.(translation_data.translators)
    Application.put_env(:logger, :translators, translators)
    update_data(:translation_data, %{translation_data | translators: translators})
  end

  defp compute_state(counter) do
    sync_threshold = Application.get_env(:logger, :sync_threshold)
    discard_threshold = Application.get_env(:logger, :discard_threshold)

    log_data = %{
      level: Application.get_env(:logger, :level),
      utc_log: Application.get_env(:logger, :utc_log),
      truncate: Application.get_env(:logger, :truncate),
      thresholds: {counter, sync_threshold, discard_threshold}
    }

    translation_data = %{
      level: Application.get_env(:logger, :level),
      translators: Application.get_env(:logger, :translators),
      truncate: Application.get_env(:logger, :truncate)
    }

    update_data(:log_data, log_data)
    update_data(:translation_data, translation_data)
    :ok
  end

  defp read_data!(key) do
    try do
      :ets.lookup_element(@table, key, 2)
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

  defp update_data(key, value) do
    :ets.update_element(@table, key, {2, value})
  end
end
