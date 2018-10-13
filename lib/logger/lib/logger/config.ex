defmodule Logger.Config do
  @moduledoc false

  @behaviour :gen_event
  @name __MODULE__
  @table __MODULE__

  def start_link do
    GenServer.start_link(__MODULE__, :ok, name: @name)
  end

  def configure(options) do
    :gen_event.call(Logger, @name, {:configure, options})
  end

  def add_translator(translator) do
    :gen_event.call(Logger, @name, {:add_translator, translator})
  end

  def remove_translator(translator) do
    :gen_event.call(Logger, @name, {:remove_translator, translator})
  end

  def __data__() do
    try do
      :ets.lookup_element(@table, :data, 2)
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

  def deleted_handlers() do
    try do
      :ets.lookup_element(@table, :deleted_handlers, 2)
    rescue
      ArgumentError ->
        []
    end
  end

  def deleted_handlers(handlers) do
    :gen_event.call(Logger, @name, {:deleted_handlers, handlers})
  end

  def new() do
    tab = :ets.new(@table, [:named_table, :public, {:read_concurrency, true}])
    true = :ets.insert_new(@table, [{:data, nil}, {:deleted_handlers, []}])
    tab
  end

  def delete(@table) do
    :ets.delete(@table)
  end

  ## Callbacks

  def init(_) do
    thresholds = compute_thresholds()
    state = :ets.lookup_element(@table, :data, 2) || compute_state(:async, thresholds)
    {:ok, {state, thresholds}}
  end

  def handle_event({_type, gl, _msg} = event, state) when node(gl) != node() do
    # Cross node messages are always async which also
    # means this handler won't crash in case Logger
    # is not installed in the other node.
    :gen_event.notify({Logger, node(gl)}, event)
    {:ok, state}
  end

  def handle_event(_event, {state, thresholds}) do
    %{mode: mode} = state

    case compute_mode(mode, thresholds) do
      ^mode ->
        {:ok, {state, thresholds}}

      new_mode ->
        if new_mode == :discard do
          message =
            "Logger has #{message_queue_length()} messages in its queue, " <>
              "which is above :discard_threshold. Messages will be discarded " <>
              "until the message queue goes back to 75% of the threshold size"

          log(:warn, message, state)
        end

        if mode == :discard do
          log(:warn, "Logger has stopped discarding messages", state)
        end

        state = persist(%{state | mode: new_mode})
        {:ok, {state, thresholds}}
    end
  end

  def handle_call({:configure, options}, {%{mode: mode}, _}) do
    Enum.each(options, fn {key, value} ->
      Application.put_env(:logger, key, value)
    end)

    thresholds = compute_thresholds()
    state = compute_state(mode, thresholds)
    {:ok, :ok, {state, thresholds}}
  end

  def handle_call({:add_translator, translator}, {state, thresholds}) do
    state = update_translators(state, fn t -> [translator | List.delete(t, translator)] end)
    {:ok, :ok, {state, thresholds}}
  end

  def handle_call({:remove_translator, translator}, {state, thresholds}) do
    state = update_translators(state, &List.delete(&1, translator))
    {:ok, :ok, {state, thresholds}}
  end

  def handle_call({:deleted_handlers, new}, state) do
    old = deleted_handlers()
    true = :ets.update_element(@table, :deleted_handlers, {2, new})
    {:ok, old, state}
  end

  def handle_info(_msg, state) do
    {:ok, state}
  end

  def terminate(_reason, _state) do
    :ok
  end

  def code_change(_old, state, _extra) do
    {:ok, state}
  end

  ## Helpers

  defp log(level, message, state) do
    event = {Logger, message, Logger.Utils.timestamp(state.utc_log), pid: self()}
    :gen_event.notify(self(), {level, Process.group_leader(), event})
  end

  defp message_queue_length() do
    {:message_queue_len, messages} = Process.info(self(), :message_queue_len)
    messages
  end

  defp update_translators(%{translators: translators} = state, fun) do
    translators = fun.(translators)
    Application.put_env(:logger, :translators, translators)
    persist(%{state | translators: translators})
  end

  defp compute_state(mode, thresholds) do
    persist(%{
      mode: compute_mode(mode, thresholds),
      level: Application.get_env(:logger, :level),
      translators: Application.get_env(:logger, :translators),
      truncate: Application.get_env(:logger, :truncate),
      utc_log: Application.get_env(:logger, :utc_log)
    })
  end

  defp compute_mode(mode, thresholds) do
    %{
      async_threshold: async_threshold,
      sync_threshold: sync_threshold,
      keep_threshold: keep_threshold,
      discard_threshold: discard_threshold
    } = thresholds

    Logger.Utils.compute_mode(
      mode,
      message_queue_length(),
      async_threshold,
      sync_threshold,
      keep_threshold,
      discard_threshold
    )
  end

  defp compute_thresholds() do
    sync_threshold = Application.get_env(:logger, :sync_threshold)
    async_threshold = trunc(sync_threshold * 0.75)

    discard_threshold = Application.get_env(:logger, :discard_threshold)
    keep_threshold = trunc(discard_threshold * 0.75)

    %{
      async_threshold: async_threshold,
      sync_threshold: sync_threshold,
      keep_threshold: keep_threshold,
      discard_threshold: discard_threshold
    }
  end

  defp persist(state) do
    :ets.update_element(@table, :data, {2, state})
    state
  end
end
