defmodule ExUnit.Server do
  @moduledoc false
  @timeout 60_000

  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  ## Before run API

  def add_async_case(name) do
    GenServer.cast(__MODULE__, {:add_async_case, name})
  end

  def add_sync_case(name) do
    GenServer.cast(__MODULE__, {:add_sync_case, name})
  end

  def cases_loaded do
    GenServer.cast(__MODULE__, :cases_loaded)
  end

  ## Run API

  def take_async_cases(count) do
    GenServer.call(__MODULE__, {:take_async_cases, count}, @timeout)
  end

  def take_sync_cases() do
    GenServer.call(__MODULE__, :take_sync_cases, @timeout)
  end

  ## Callbacks

  def init(:ok) do
    {:ok, %{
      loaded: false,
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
  def handle_call(:take_sync_cases, _from, %{waiting: nil, loaded: true, async_cases: []} = state) do
    {:reply, state.sync_cases, %{state | sync_cases: [], loaded: false}}
  end

  def handle_cast({:add_async_case, name}, %{async_cases: cases} = state) do
    {:noreply, take_cases(%{state | async_cases: [name | cases]})}
  end

  def handle_cast({:add_sync_case, name}, %{sync_cases: cases} = state) do
    {:noreply, %{state | sync_cases: [name | cases]}}
  end

  def handle_cast(:cases_loaded, %{loaded: false} = state) do
    {:noreply, take_cases(%{state | loaded: true})}
  end

  defp take_cases(%{waiting: nil} = state) do
    state
  end

  defp take_cases(%{waiting: {from, _count}, async_cases: [], loaded: true} = state) do
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
