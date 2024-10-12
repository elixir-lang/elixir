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
      group: group,
      parameterize: parameterize
    } = config

    modules =
      if parameterize do
        Enum.map(parameterize, &{name, &1})
      else
        [{name, %{}}]
      end

    case GenServer.call(@name, {:add, {async?, group}, modules}, @timeout) do
      :ok ->
        :ok

      :already_running ->
        raise "cannot add module named #{inspect(name)} to test suite after the suite starts running"
    end
  end

  def modules_loaded(uniq?) do
    GenServer.call(@name, {:modules_loaded, uniq?}, @timeout)
  end

  def take_async_modules(count) do
    GenServer.call(@name, {:take_async_modules, count}, @timeout)
  end

  def take_sync_modules() do
    GenServer.call(@name, :take_sync_modules, @timeout)
  end

  def restore_modules(async_modules, sync_modules) do
    GenServer.call(@name, {:restore_modules, async_modules, sync_modules}, @timeout)
  end

  ## Callbacks

  def init(:ok) do
    # Table used by OnExitHandler
    :ets.new(__MODULE__, [:public, :named_table, read_concurrency: true, write_concurrency: true])

    state = %{
      loaded: System.monotonic_time(),
      waiting: nil,
      async_groups: MapSet.new(),
      async_modules: :queue.new(),
      sync_modules: :queue.new()
    }

    {:ok, state}
  end

  # Called on demand until we are signaled all modules are loaded.
  def handle_call({:take_async_modules, count}, from, %{waiting: nil} = state) do
    {:noreply, take_modules(%{state | waiting: {from, count}})}
  end

  # Called once after all async modules have been sent and reverts the state.
  def handle_call(:take_sync_modules, _from, state) do
    %{waiting: nil, loaded: :done, async_modules: async_modules} = state
    0 = :queue.len(async_modules)

    {:reply, :queue.to_list(state.sync_modules),
     %{state | sync_modules: :queue.new(), loaded: System.monotonic_time()}}
  end

  # Called by the runner when --repeat-until-failure is used.
  def handle_call({:restore_modules, async_modules, sync_modules}, _from, state) do
    {:reply, :ok,
     %{
       state
       | loaded: :done,
         async_groups: MapSet.new(async_modules, fn {group, _} -> group end),
         async_modules: :queue.from_list(async_modules),
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
        async_modules =
          state.async_modules
          |> :queue.to_list()
          |> Enum.uniq()
          |> Enum.map(fn {group, modules} ->
            {group, Enum.uniq(modules)}
          end)
          |> :queue.from_list()

        sync_modules = :queue.to_list(state.sync_modules) |> Enum.uniq() |> :queue.from_list()

        %{
          state
          | async_modules: async_modules,
            sync_modules: sync_modules
        }
      else
        state
      end

    diff = System.convert_time_unit(System.monotonic_time() - loaded, :native, :microsecond)
    {:reply, diff, take_modules(%{state | loaded: :done})}
  end

  def handle_call({:add, {true, group}, names}, _from, %{loaded: loaded} = state)
      when is_integer(loaded) do
    state =
      Enum.reduce(names, state, fn name, updated_state ->
        group = group || default_group(name)
        group_exists? = MapSet.member?(updated_state.async_groups, group)

        if group_exists? do
          update_in(updated_state.async_modules, fn q ->
            q
            |> :queue.to_list()
            |> Enum.map(fn
              {^group, modules} ->
                {group, [name | modules]}

              other ->
                other
            end)
            |> :queue.from_list()
          end)
        else
          updated_state
          |> update_in([:async_modules], &:queue.in({group, [name]}, &1))
          |> update_in([:async_groups], &MapSet.put(&1, group))
        end
      end)

    {:reply, :ok, take_modules(state)}
  end

  def handle_call({:add, {false, group}, names}, _from, %{loaded: loaded} = state)
      when is_integer(loaded) do
    state =
      update_in(
        state.sync_modules,
        &Enum.reduce(names, &1, fn name, q -> :queue.in({group, [name]}, q) end)
      )

    {:reply, :ok, state}
  end

  def handle_call({:add, {_async?, _group}, _names}, _from, state),
    do: {:reply, :already_running, state}

  defp default_group({module, params}) do
    if params == %{} do
      module
    else
      # if no group is specified, parameters need to run concurrently
      "#{module}_#{:erlang.phash2(params)}"
    end
  end

  defp take_modules(%{waiting: nil} = state) do
    state
  end

  defp take_modules(%{waiting: {from, count}} = state) do
    has_async_modules? = not :queue.is_empty(state.async_modules)

    cond do
      not has_async_modules? and state.loaded == :done ->
        GenServer.reply(from, nil)
        %{state | waiting: nil}

      not has_async_modules? ->
        state

      true ->
        {async_modules, remaining_modules} = take_until(count, state.async_modules)

        remaining_groups =
          Enum.reduce(async_modules, state.async_groups, fn {group, _modules}, groups ->
            MapSet.delete(groups, group)
          end)

        async_modules =
          Enum.map(async_modules, fn {group, modules} -> {group, Enum.reverse(modules)} end)

        GenServer.reply(from, async_modules)

        %{
          state
          | async_groups: remaining_groups,
            async_modules: remaining_modules,
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
