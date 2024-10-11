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
      self: self
    } = info

    IO.write("""
    Received :modules_compiled with
      added: #{inspect(added)}, changed: #{inspect(changed)}, removed: #{inspect(removed)}
      app: #{inspect(app)}
      build_scm: #{inspect(build_scm)}
      self: #{inspect(self)}
    """)

    {:noreply, state}
  end
end
