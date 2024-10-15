defmodule Reloader do
  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, {})
  end

  @impl true
  def init({}) do
    {:ok, {}}
  end

  @impl true
  def handle_info({:modules_compiled, info}, state) do
    %{
      modules_diff: %{added: added, changed: changed, removed: removed, timestamp: _timestamp},
      app: app,
      scm: scm,
      os_pid: os_pid
    } = info

    IO.write("""
    Received :modules_compiled with
      added: #{inspect(Enum.sort(added))}, changed: #{inspect(Enum.sort(changed))}, removed: #{inspect(Enum.sort(removed))}
      app: #{inspect(app)}
      scm: #{inspect(scm)}
      os_pid: #{inspect(os_pid)}
    """)

    {:noreply, state}
  end

  def handle_info({:dep_compiled, info}, state) do
    %{
      app: app,
      scm: scm,
      manager: manager,
      os_pid: os_pid
    } = info

    IO.write("""
    Received :dep_compiled with
      app: #{inspect(app)}
      scm: #{inspect(scm)}
      manager: #{inspect(manager)}
      os_pid: #{inspect(os_pid)}
    """)

    {:noreply, state}
  end
end
