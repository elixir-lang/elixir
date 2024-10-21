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
    purge_modules(state.to_purge)
    status = if Enum.empty?(state.to_purge), do: :noop, else: :ok
    {:reply, status, %{state | to_purge: MapSet.new()}}
  end

  @impl true
  def handle_info({:modules_compiled, info}, state) do
    if info.os_pid == System.pid() do
      # Ignore compilations from ourselves, because the modules are
      # already updated in memory
      {:noreply, state}
    else
      %{changed: changed, removed: removed} = info.modules_diff

      if IEx.Config.auto_reload?() do
        purge_modules(changed)
        purge_modules(removed)
        {:noreply, state}
      else
        state = update_in(state.to_purge, &Enum.into(changed, &1))
        state = update_in(state.to_purge, &Enum.into(removed, &1))
        {:noreply, state}
      end
    end
  end

  def handle_info(_message, state) do
    {:noreply, state}
  end

  defp purge_modules(modules) do
    for module <- modules do
      :code.purge(module)
      :code.delete(module)
    end
  end
end
