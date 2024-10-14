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
      build_scm: build_scm,
      os_pid: os_pid
    } = info

    IO.write("""
    Received :modules_compiled with
      added: #{inspect(Enum.sort(added))}, changed: #{inspect(Enum.sort(changed))}, removed: #{inspect(Enum.sort(removed))}
      app: #{inspect(app)}
      build_scm: #{inspect(build_scm)}
      os_pid: #{inspect(os_pid)}
    """)

    {:noreply, state}
  end
end
