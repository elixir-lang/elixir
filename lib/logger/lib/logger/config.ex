defmodule Logger.Config do
  @moduledoc false

  @behaviour :gen_event

  @name __MODULE__
  @table __MODULE__
  @data :__data__
  @deleted_handlers :__deleted_handlers__

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

  def handlers() do
    for backend <- backends() do
      {Logger, translate_backend(backend), backend}
    end
  end

  def backends() do
    :gen_event.call(Logger, @name, :backends)
  end

  def add_backend(backend) do
    :gen_event.call(Logger, @name, {:add_backend, backend})
  end

  def remove_backend(backend) do
    :gen_event.call(Logger, @name, {:remove_backend, backend})
  end

  def translate_backend(:console), do: Logger.Backends.Console
  def translate_backend(other),    do: other

  def __data__() do
    try do
      :ets.lookup_element(@table, @data, 2)
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
      :ets.lookup_element(@table, @deleted_handlers, 2)
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
    true = :ets.insert_new(@table, [{@data, nil}, {@deleted_handlers, []}])
    tab
  end

  def delete(@table) do
    :ets.delete(@table)
  end

  ## Callbacks

  def init(_) do
    # Use previous data if available in case this handler crashed.
    state = :ets.lookup_element(@table, @data, 2) || compute_state(:async)
    {:ok, state}
  end

  def handle_event({_type, gl, _msg} = event, state) when node(gl) != node() do
    # Cross node messages are always async which also
    # means this handler won't crash in case there is
    # no logger installed in the other node.
    :gen_event.notify({Logger, node(gl)}, event)
    {:ok, state}
  end

  def handle_event(_event, %{mode: mode} = state) do
    case compute_mode(state) do
      ^mode ->
        {:ok, state}
      new_mode ->
        {:ok, persist(%{state | mode: new_mode})}
    end
  end

  def handle_call(:backends, state) do
    {:ok, Application.get_env(:logger, :backends), state}
  end

  def handle_call({:configure, options}, state) do
    Enum.each options, fn {key, value} ->
      Application.put_env(:logger, key, value)
    end
    {:ok, :ok, compute_state(state.mode)}
  end

  def handle_call({:add_translator, translator}, state) do
    state = update_translators(state, fn t -> [translator | List.delete(t, translator)] end)
    {:ok, :ok, state}
  end

  def handle_call({:remove_translator, translator}, state) do
    state = update_translators(state, &List.delete(&1, translator))
    {:ok, :ok, state}
  end

  def handle_call({:add_backend, backend}, state) do
    update_backends(&[backend | List.delete(&1, backend)])
    {:ok, :ok, state}
  end

  def handle_call({:remove_backend, backend}, state) do
    update_backends(&List.delete(&1, backend))
    {:ok, :ok, state}
  end

  def handle_call({:deleted_handlers, new}, state) do
    old = deleted_handlers()
    true = :ets.update_element(@table, @deleted_handlers, {2, new})
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

  defp compute_mode(state) do
    {:message_queue_len, len} = Process.info(self(), :message_queue_len)

    cond do
      len > state.sync_threshold and state.mode == :async ->
        :sync
      len < state.async_threshold and state.mode == :sync ->
        :async
      true ->
        state.mode
    end
  end

  defp update_backends(fun) do
    backends = fun.(Application.get_env(:logger, :backends, []))
    Application.put_env(:logger, :backends, backends)
  end

  defp update_translators(%{translators: translators} = state, fun) do
    translators = fun.(translators)
    Application.put_env(:logger, :translators, translators)
    persist %{state | translators: translators}
  end

  defp compute_state(mode) do
    level       = Application.get_env(:logger, :level)
    utc_log     = Application.get_env(:logger, :utc_log)
    truncate    = Application.get_env(:logger, :truncate)
    translators = Application.get_env(:logger, :translators)

    sync_threshold  = Application.get_env(:logger, :sync_threshold)
    async_threshold = trunc(sync_threshold * 0.75)

    state = %{level: level, mode: mode, truncate: truncate,
              utc_log: utc_log, sync_threshold: sync_threshold,
              async_threshold: async_threshold, translators: translators}

    case compute_mode(state) do
      ^mode ->
        persist(state)
      new_mode ->
        persist(%{state | mode: new_mode})
    end
  end

  defp persist(state) do
    :ets.update_element(@table, @data, {2, state})
    state
  end
end
