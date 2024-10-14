defmodule Mix.PubSub.Subscriber do
  @moduledoc false

  use GenServer

  @name __MODULE__

  @spec start_link(keyword) :: GenServer.on_start()
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, {}, name: @name)
  end

  @spec flush :: :ok
  def flush do
    GenServer.cast(@name, :flush)
  end

  @impl true
  def init({}) do
    build_path = Mix.Project.build_path()
    Mix.Sync.PubSub.subscribe(build_path)
    {:ok, %{acc: []}}
  end

  @impl true
  def handle_info(message, %{acc: nil} = state) do
    notify_listeners([message])
    {:noreply, state}
  end

  def handle_info(message, state) do
    # Accumulate messages until the flush
    {:noreply, update_in(state.acc, &[message | &1])}
  end

  @impl true
  def handle_cast(:flush, state) do
    notify_listeners(Enum.reverse(state.acc))
    {:noreply, %{state | acc: nil}}
  end

  defp notify_listeners(messages) do
    children = Supervisor.which_children(Mix.PubSub.ListenerSupervisor)

    for message <- messages do
      for {_, pid, _, _} <- children, is_pid(pid) do
        send(pid, message)
      end
    end
  end
end
