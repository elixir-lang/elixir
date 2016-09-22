defmodule ExUnit.Server do
  @moduledoc false

  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def add_async_case(name) do
    GenServer.cast(__MODULE__, {:add_async_case, name})
  end

  def add_sync_case(name) do
    GenServer.cast(__MODULE__, {:add_sync_case, name})
  end

  def cases_loaded do
    GenServer.call(__MODULE__, :cases_loaded)
  end

  def take_async_cases(count) do
    timeout = Application.fetch_env!(:ex_unit, :case_load_timeout)
    GenServer.call(__MODULE__, {:take_async_cases, count}, timeout)
  end

  def take_sync_cases() do
    timeout = Application.fetch_env!(:ex_unit, :case_load_timeout)
    GenServer.call(__MODULE__, :take_sync_cases, timeout)
  end

  ## Callbacks

  def init(:ok) do
    {:ok, %{
      loaded: System.monotonic_time,
      waiting: nil,
      async_cases: [],
      sync_cases: [],
    }}
  end

  # Called on demand until we are signaled all cases are loaded.
  def handle_call({:take_async_cases, count}, from, %{waiting: nil} = state) do
    {:noreply, take_cases(%{state | waiting: {from, count}})}
  end

  # Called once after all async cases have been sent and reverts the state.
  def handle_call(:take_sync_cases, _from, %{waiting: nil, loaded: :done, async_cases: []} = state) do
    {:reply, state.sync_cases,
     %{state | sync_cases: [], loaded: System.monotonic_time}}
  end

  def handle_call(:cases_loaded, _from, %{loaded: loaded} = state) when is_integer(loaded) do
    diff = System.convert_time_unit(System.monotonic_time - loaded, :native, :microsecond)
    {:reply, diff, take_cases(%{state | loaded: :done})}
  end

  def handle_cast({:add_async_case, name}, %{async_cases: cases, loaded: loaded} = state)
      when is_integer(loaded) do
    {:noreply, take_cases(%{state | async_cases: [name | cases]})}
  end

  def handle_cast({:add_sync_case, name}, %{sync_cases: cases, loaded: loaded} = state)
      when is_integer(loaded) do
    {:noreply, %{state | sync_cases: [name | cases]}}
  end

  defp take_cases(%{waiting: nil} = state) do
    state
  end

  defp take_cases(%{waiting: {from, _count}, async_cases: [], loaded: :done} = state) do
    GenServer.reply(from, nil)
    %{state | waiting: nil}
  end

  defp take_cases(%{async_cases: []} = state) do
    state
  end

  defp take_cases(%{waiting: {from, count}, async_cases: cases} = state) do
    {reply, cases} = Enum.split(cases, count)
    GenServer.reply(from, reply)
    %{state | async_cases: cases, waiting: nil}
  end
end
