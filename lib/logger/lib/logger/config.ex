defmodule Logger.Config do
  @moduledoc false

  @behaviour :gen_event
  @name __MODULE__
  @table __MODULE__

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
    %{level: level} = read_translation_data!()
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
    read_translation_data!()
  end

  def log_data(level) do
    {:log_data, %{level: min_level} = config, message_queue_length} = read_log_data!()

    if compare_levels(level, min_level) != :lt do
      %{thresholds: {counter, sync, discard}} = config
      counter = message_queue_length + bump_counter(counter)

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
    reset_counter_and_update_message_queue_length(counter)
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
    reset_counter_and_update_message_queue_length(counter)
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
  # If the system is overloaded, then we will overestimate the overall
  # size this is better than the inverse.
  defp reset_counter_and_update_message_queue_length(counter) do
    reset_counter(counter)
    update_message_queue_length()
  end

  ## Counter Helpers

  # TODO: Use counters exclusively when we require Erlang/OTP 22+.
  defp new_counter() do
    if Code.ensure_loaded?(:counters) do
      {:counters, :counters.new(1, [:atomics])}
    else
      table = :ets.new(@table.Counter, [:public, write_concurrency: true])
      :ets.insert(table, {:counter, 0})
      {:ets, table}
    end
  end

  defp reset_counter({:ets, counter}), do: :ets.update_element(counter, :counter, {2, 0})
  defp reset_counter({:counters, counter}), do: :counters.put(counter, 1, 0)

  defp bump_counter({:ets, counter}), do: :ets.update_counter(counter, :counter, {2, 1})

  defp bump_counter({:counters, counter}) do
    :counters.add(counter, 1, 1)
    :counters.get(counter, 1)
  end

  defp delete_counter({:ets, counter}), do: :ets.delete(counter)
  defp delete_counter({:counters, _}), do: :ok

  ## Data helpers

  defp new_data do
    entries = [
      {:log_data, nil, 0},
      {:translation_data, nil},
      {:deleted_handlers, []}
    ]

    table = :ets.new(@table, [:named_table, :public, {:read_concurrency, true}])
    true = :ets.insert_new(@table, entries)
    table
  end

  defp delete_data(@table), do: :ets.delete(@table)

  defp update_message_queue_length do
    {:message_queue_len, length} = Process.info(self(), :message_queue_len)
    :ets.update_element(@table, :log_data, {3, length})
  end

  defp update_translators(fun) do
    translation_data = read_translation_data!()
    translators = fun.(translation_data.translators)
    Application.put_env(:logger, :translators, translators)
    update_translation_data(%{translation_data | translators: translators})
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

    update_log_data(log_data)
    update_translation_data(translation_data)
    :ok
  end

  defp read_log_data! do
    try do
      :ets.lookup(@table, :log_data)
    rescue
      ArgumentError ->
        raise "cannot use Logger, the :logger application is not running"
    else
      [log_data] -> log_data
      [] -> raise "cannot use Logger, the :logger application is not running"
    end
  end

  defp update_log_data(log_data) do
    :ets.update_element(@table, :log_data, {2, log_data})
  end

  defp read_translation_data! do
    try do
      :ets.lookup_element(@table, :translation_data, 2)
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

  defp update_translation_data(translation_data) do
    :ets.update_element(@table, :translation_data, {2, translation_data})
  end
end
