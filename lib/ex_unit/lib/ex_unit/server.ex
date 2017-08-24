defmodule ExUnit.Server do
  @moduledoc false
  @name __MODULE__

  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: @name)
  end

  def add_async_module(name) do
    GenServer.cast(@name, {:add_async_module, name})
  end

  def add_sync_module(name) do
    GenServer.cast(@name, {:add_sync_module, name})
  end

  def modules_loaded do
    GenServer.call(@name, :modules_loaded)
  end

  def take_async_modules(count) do
    timeout = Application.fetch_env!(:ex_unit, :module_load_timeout)
    GenServer.call(@name, {:take_async_modules, count}, timeout)
  end

  def take_sync_modules() do
    timeout = Application.fetch_env!(:ex_unit, :module_load_timeout)
    GenServer.call(@name, :take_sync_modules, timeout)
  end

  ## Callbacks

  def init(:ok) do
    {:ok, %{
      loaded: System.monotonic_time,
      waiting: nil,
      async_modules: [],
      sync_modules: [],
    }}
  end

  # Called on demand until we are signaled all modules are loaded.
  def handle_call({:take_async_modules, count}, from, %{waiting: nil} = state) do
    {:noreply, take_modules(%{state | waiting: {from, count}})}
  end

  # Called once after all async modules have been sent and reverts the state.
  def handle_call(:take_sync_modules, _from, %{waiting: nil, loaded: :done, async_modules: []} = state) do
    {:reply, state.sync_modules,
     %{state | sync_modules: [], loaded: System.monotonic_time}}
  end

  def handle_call(:modules_loaded, _from, %{loaded: loaded} = state) when is_integer(loaded) do
    diff = System.convert_time_unit(System.monotonic_time - loaded, :native, :microsecond)
    {:reply, diff, take_modules(%{state | loaded: :done})}
  end

  def handle_cast({:add_async_module, name}, %{async_modules: modules, loaded: loaded} = state)
      when is_integer(loaded) do
    {:noreply, take_modules(%{state | async_modules: [name | modules]})}
  end

  def handle_cast({:add_sync_module, name}, %{sync_modules: modules, loaded: loaded} = state)
      when is_integer(loaded) do
    {:noreply, %{state | sync_modules: [name | modules]}}
  end

  defp take_modules(%{waiting: nil} = state) do
    state
  end

  defp take_modules(%{waiting: {from, _count}, async_modules: [], loaded: :done} = state) do
    GenServer.reply(from, nil)
    %{state | waiting: nil}
  end

  defp take_modules(%{async_modules: []} = state) do
    state
  end

  defp take_modules(%{waiting: {from, count}, async_modules: modules} = state) do
    {reply, modules} = Enum.split(modules, count)
    GenServer.reply(from, reply)
    %{state | async_modules: modules, waiting: nil}
  end
end
