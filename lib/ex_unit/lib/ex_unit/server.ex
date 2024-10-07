defmodule ExUnit.Server do
  @moduledoc false
  @name __MODULE__
  @timeout :infinity

  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: @name)
  end

  def add_module(name, config) do
    %{
      async?: async?,
      async_partition_key: async_partition_key,
      parameterize: parameterize
    } = config

    modules =
      if parameterize do
        Enum.map(parameterize, &{name, &1})
      else
        [{name, %{}}]
      end

    case GenServer.call(@name, {:add, {async?, async_partition_key}, modules}, @timeout) do
      :ok ->
        :ok

      :already_running ->
        raise "cannot add module named #{inspect(name)} to test suite after the suite starts running"
    end
  end

  def modules_loaded(uniq?) do
    GenServer.call(@name, {:modules_loaded, uniq?}, @timeout)
  end

  def take_async_partitions(count) do
    GenServer.call(@name, {:take_async_partitions, count}, @timeout)
  end

  def take_sync_modules() do
    GenServer.call(@name, :take_sync_modules, @timeout)
  end

  def restore_modules(async_partitions, sync_modules) do
    GenServer.call(@name, {:restore_modules, async_partitions, sync_modules}, @timeout)
  end

  ## Callbacks

  def init(:ok) do
    # Table used by OnExitHandler
    :ets.new(__MODULE__, [:public, :named_table, read_concurrency: true, write_concurrency: true])

    state = %{
      loaded: System.monotonic_time(),
      waiting: nil,
      async_partitions: %{},
      async_partition_keys: :queue.new(),
      sync_modules: :queue.new()
    }

    {:ok, state}
  end

  # Called on demand until we are signaled all modules are loaded.
  def handle_call({:take_async_partitions, count}, from, %{waiting: nil} = state) do
    {:noreply, take_module_partitions(%{state | waiting: {from, count}})}
  end

  # Called once after all async modules have been sent and reverts the state.
  def handle_call(:take_sync_modules, _from, state) do
    %{waiting: nil, loaded: :done, async_partition_keys: async_partition_keys} = state
    0 = :queue.len(async_partition_keys)

    {:reply, :queue.to_list(state.sync_modules),
     %{state | sync_modules: :queue.new(), loaded: System.monotonic_time()}}
  end

  # Called by the runner when --repeat-until-failure is used.
  def handle_call({:restore_modules, async_partitions, sync_modules}, _from, state) do
    async_partition_keys =
      Enum.map(async_partitions, fn {partition_key, _modules} -> partition_key end)

    {:reply, :ok,
     %{
       state
       | loaded: :done,
         async_partitions: Map.new(async_partitions),
         async_partition_keys: :queue.from_list(async_partition_keys),
         sync_modules: :queue.from_list(sync_modules)
     }}
  end

  def handle_call({:modules_loaded, _}, _from, %{loaded: :done} = state) do
    {:reply, 0, state}
  end

  def handle_call({:modules_loaded, uniq?}, _from, %{loaded: loaded} = state)
      when is_integer(loaded) do
    state =
      if uniq? do
        async_partitions =
          state.async_partitions
          |> Enum.map(fn {partition_key, modules} ->
            partition_modules = Enum.uniq(modules)
            {partition_key, partition_modules}
          end)
          |> Map.new()

        sync_modules = :queue.to_list(state.sync_modules) |> Enum.uniq() |> :queue.from_list()

        %{
          state
          | async_partitions: async_partitions,
            sync_modules: sync_modules
        }
      else
        state
      end

    diff = System.convert_time_unit(System.monotonic_time() - loaded, :native, :microsecond)
    {:reply, diff, take_module_partitions(%{state | loaded: :done})}
  end

  def handle_call({:add, {true, async_partition_key}, names}, _from, %{loaded: loaded} = state)
      when is_integer(loaded) do
    state =
      Enum.reduce(names, state, fn name, updated_state ->
        async_partition_key = async_partition_key || default_async_partition_key(name)
        partition_key_exists? = Map.has_key?(state.async_partitions, async_partition_key)

        updated_state
        |> update_in([:async_partitions], fn async_partitions ->
          {_, async_partitions} =
            Map.get_and_update(async_partitions, async_partition_key, fn
              nil ->
                {nil, [name]}

              modules ->
                {modules, [name | modules]}
            end)

          async_partitions
        end)
        |> update_in([:async_partition_keys], fn q ->
          if partition_key_exists? do
            q
          else
            :queue.in(async_partition_key, q)
          end
        end)
      end)

    {:reply, :ok, take_module_partitions(state)}
  end

  def handle_call({:add, {false, _async_partition_key}, names}, _from, %{loaded: loaded} = state)
      when is_integer(loaded) do
    state =
      update_in(state.sync_modules, &Enum.reduce(names, &1, fn name, q -> :queue.in(name, q) end))

    {:reply, :ok, state}
  end

  def handle_call({:add, {_async?, _async_partition_key}, _names}, _from, state),
    do: {:reply, :already_running, state}

  defp default_async_partition_key({module, params}) do
    if params == %{} do
      module
    else
      # if no async partition is specified, parameters run concurrently
      "#{module}_#{:erlang.phash2(params)}"
    end
  end

  defp take_module_partitions(%{waiting: nil} = state) do
    state
  end

  defp take_module_partitions(%{waiting: {from, count}} = state) do
    has_async_partitions? = not :queue.is_empty(state.async_partition_keys)

    cond do
      not has_async_partitions? and state.loaded == :done ->
        GenServer.reply(from, nil)
        %{state | waiting: nil}

      not has_async_partitions? ->
        state

      true ->
        {partition_keys, remaining_partition_keys} =
          take_until(count, state.async_partition_keys)

        {async_partitions, remaining_partitions} =
          Enum.map_reduce(partition_keys, state.async_partitions, fn partition_key,
                                                                     remaining_partitions ->
            {partition_modules, remaining_partitions} =
              Map.pop!(remaining_partitions, partition_key)

            {
              {partition_key, Enum.reverse(partition_modules)},
              remaining_partitions
            }
          end)

        GenServer.reply(from, async_partitions)

        %{
          state
          | async_partition_keys: remaining_partition_keys,
            async_partitions: remaining_partitions,
            waiting: nil
        }
    end
  end

  # :queue.split fails if the provided count is larger than the queue size;
  # as we also want to return the values as a list later, we directly
  # return {list, queue} instead of {queue, queue}
  defp take_until(n, queue), do: take_until(n, queue, [])

  defp take_until(0, queue, acc), do: {Enum.reverse(acc), queue}

  defp take_until(n, queue, acc) do
    case :queue.out(queue) do
      {{:value, item}, queue} -> take_until(n - 1, queue, [item | acc])
      {:empty, queue} -> {Enum.reverse(acc), queue}
    end
  end
end
