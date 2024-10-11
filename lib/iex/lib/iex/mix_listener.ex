defmodule IEx.MixListener do
  @moduledoc false

  use GenServer

  @name __MODULE__

  @spec start_link(keyword) :: GenServer.on_start()
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, {}, name: @name)
  end

  @doc """
  Unloads all modules invalidated by external compilations.
  """
  @spec purge :: :ok | :noop
  def purge do
    GenServer.call(@name, :purge, :infinity)
  end

  @impl true
  def init({}) do
    {:ok, %{to_purge: MapSet.new()}}
  end

  @impl true
  def handle_call(:purge, _from, state) do
    for module <- state.to_purge do
      :code.purge(module)
      :code.delete(module)
    end

    status = if Enum.empty?(state.to_purge), do: :noop, else: :ok

    {:reply, status, %{state | to_purge: MapSet.new()}}
  end

  @impl true
  def handle_info({:modules_compiled, info}, state) do
    %{changed: changed, removed: removed} = info.modules_diff
    state = update_in(state.to_purge, &Enum.into(changed, &1))
    state = update_in(state.to_purge, &Enum.into(removed, &1))
    {:noreply, state}
  end

  def handle_info(_message, state) do
    {:noreply, state}
  end
end
