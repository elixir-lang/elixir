# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

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

    case GenServer.call(@name, {:add, async?, group, modules}, @timeout) do
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
      groups: %{},
      async_groups: [],
      async_modules: :queue.new(),
      sync_modules: []
    }

    {:ok, state}
  end

  # Called on demand until we are signaled all modules are loaded.
  def handle_call({:take_async_modules, count}, from, %{waiting: nil} = state) do
    {:noreply, take_modules(%{state | waiting: {from, count}})}
  end

  # Called once after all async modules have been sent and reverts the state.
  def handle_call(:take_sync_modules, _from, state) do
    %{waiting: nil, loaded: :done, async_groups: []} = state
    true = :queue.is_empty(state.async_modules)

    {:reply, state.sync_modules, %{state | sync_modules: [], loaded: System.monotonic_time()}}
  end

  # Called by the runner when --repeat-until-failure is used.
  def handle_call({:restore_modules, async_modules, sync_modules}, _from, state) do
    {async_modules, async_groups, groups} =
      Enum.reduce(async_modules, {[], [], []}, fn
        {nil, [module]}, {async_modules, async_groups, groups} ->
          {[module | async_modules], async_groups, groups}

        {group, group_modules}, {async_modules, async_groups, groups} ->
          {async_modules, [group | async_groups], [{group, group_modules} | groups]}
      end)

    {:reply, :ok,
     %{
       state
       | loaded: :done,
         groups: Map.new(groups),
         async_groups: async_groups,
         async_modules: :queue.from_list(async_modules),
         sync_modules: sync_modules
     }}
  end

  def handle_call({:modules_loaded, _}, _from, %{loaded: :done} = state) do
    {:reply, 0, state}
  end

  def handle_call({:modules_loaded, uniq?}, _from, %{loaded: loaded} = state)
      when is_integer(loaded) do
    state =
      if uniq? do
        groups = Map.new(state.groups, fn {group, modules} -> {group, Enum.uniq(modules)} end)
        async_groups = state.async_groups |> Enum.uniq() |> Enum.reverse()
        async_modules = :queue.to_list(state.async_modules) |> Enum.uniq() |> :queue.from_list()
        sync_modules = state.sync_modules |> Enum.uniq() |> Enum.reverse()

        %{
          state
          | groups: groups,
            async_groups: async_groups,
            async_modules: async_modules,
            sync_modules: sync_modules
        }
      else
        %{
          state
          | async_groups: Enum.reverse(state.async_groups),
            sync_modules: Enum.reverse(state.sync_modules)
        }
      end

    diff = System.convert_time_unit(System.monotonic_time() - loaded, :native, :microsecond)
    {:reply, diff, take_modules(%{state | loaded: :done})}
  end

  def handle_call({:add, false = _async, _group, names}, _from, %{loaded: loaded} = state)
      when is_integer(loaded) do
    state = update_in(state.sync_modules, &Enum.reverse(names, &1))
    {:reply, :ok, state}
  end

  def handle_call({:add, true = _async, nil = _group, names}, _from, %{loaded: loaded} = state)
      when is_integer(loaded) do
    state =
      update_in(
        state.async_modules,
        &Enum.reduce(names, &1, fn name, q -> :queue.in(name, q) end)
      )

    {:reply, :ok, take_modules(state)}
  end

  def handle_call({:add, true = _async, group, names}, _from, %{loaded: loaded} = state)
      when is_integer(loaded) do
    {groups, async_groups} =
      case state.groups do
        %{^group => entries} = groups ->
          {%{groups | group => Enum.reverse(names, entries)}, state.async_groups}

        %{} = groups ->
          {Map.put(groups, group, names), [group | state.async_groups]}
      end

    {:reply, :ok, take_modules(%{state | groups: groups, async_groups: async_groups})}
  end

  def handle_call({:add, _async?, _group, _names}, _from, state) do
    {:reply, :already_running, state}
  end

  defp take_modules(%{waiting: nil} = state) do
    state
  end

  defp take_modules(%{waiting: {from, count}} = state) do
    cond do
      not :queue.is_empty(state.async_modules) ->
        {reply, remaining_modules} = take_until(count, state.async_modules)
        GenServer.reply(from, reply)
        %{state | async_modules: remaining_modules, waiting: nil}

      state.async_groups != [] and state.loaded == :done ->
        {groups, remaining_groups} = Enum.split(state.async_groups, count)

        {reply, groups} =
          Enum.map_reduce(groups, state.groups, fn group, acc ->
            {entries, acc} = Map.pop!(acc, group)
            {{group, Enum.reverse(entries)}, acc}
          end)

        GenServer.reply(from, reply)
        %{state | groups: groups, async_groups: remaining_groups, waiting: nil}

      state.loaded == :done ->
        GenServer.reply(from, nil)
        %{state | waiting: nil}

      true ->
        state
    end
  end

  # :queue.split fails if the provided count is larger than the queue size.
  # We also want to return the values as tuples of shape {group, [modules]}.
  defp take_until(n, queue), do: take_until(n, queue, [])

  defp take_until(0, queue, acc), do: {Enum.reverse(acc), queue}

  defp take_until(n, queue, acc) do
    case :queue.out(queue) do
      {{:value, item}, queue} -> take_until(n - 1, queue, [{nil, [item]} | acc])
      {:empty, queue} -> {Enum.reverse(acc), queue}
    end
  end
end
