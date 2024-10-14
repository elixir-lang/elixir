defmodule Mix.PubSub do
  @moduledoc false

  # The Mix pub/sub is responsible for notifying other OS processes
  # about relevant concurrent events.
  #
  # The pub/sub consists of a local subscriber process that receives
  # events from other OS processes, and a listener supervisor, which
  # holds all listener processes that have been configured. Whenever
  # the subscriber receives an event, it sends a message to each of
  # the listeners.
  #
  # Inherently, a compilation may be required before the listener
  # modules are available, so we start the local subscriber process
  # separately with `start/0`, and then start the listeners later
  # with `start_listeners/0`. The subscriber is going to accumulate
  # events and reply them once the listenres are started.

  @spec start :: :ok
  def start do
    # Avoid calling the supervisor, if already started
    if Process.whereis(Mix.PubSub) do
      :ok
    else
      case Supervisor.start_child(Mix.Supervisor, Mix.PubSub) do
        {:ok, _pid} ->
          :ok

        {:error, {:already_started, _pid}} ->
          :ok

        {:error, reason} ->
          raise RuntimeError, "failed to start Mix.PubSub, reason: #{inspect(reason)}"
      end
    end
  end

  @spec child_spec(term) :: Supervisor.child_spec()
  def child_spec(_opts) do
    children = [
      Mix.PubSub.Subscriber
    ]

    opts = [strategy: :one_for_one, name: Mix.PubSub]

    %{
      id: Mix.PubSub,
      start: {Supervisor, :start_link, [children, opts]},
      type: :supervisor
    }
  end

  @spec start_listeners :: :ok
  def start_listeners do
    # Avoid calling the supervisor, if already started
    if Process.whereis(Mix.PubSub.ListenerSupervisor) do
      :ok
    else
      case Supervisor.start_child(Mix.PubSub, listener_supervisor()) do
        {:ok, _pid} ->
          Mix.PubSub.Subscriber.flush()
          :ok

        {:error, {:already_started, _pid}} ->
          :ok

        {:error, reason} ->
          raise RuntimeError,
                "failed to start Mix.PubSub.ListenerSupervisor, reason: #{inspect(reason)}"
      end
    end
  end

  defp listener_supervisor do
    children = configured_listeners() ++ built_in_listeners()

    children = Enum.map(children, &Supervisor.child_spec(&1, []))

    opts = [strategy: :one_for_one, name: Mix.PubSub.ListenerSupervisor]

    %{
      id: Mix.PubSub.ListenerSupervisor,
      start: {Supervisor, :start_link, [children, opts]},
      type: :supervisor
    }
  end

  defp configured_listeners do
    config = Mix.Project.config()

    listeners =
      Application.get_env(:mix, :listeners, []) ++
        Keyword.get(config, :listeners, [])

    Enum.uniq(listeners)
  end

  defp built_in_listeners do
    iex_started? = Process.whereis(IEx.Supervisor) != nil

    if iex_started? do
      [IEx.MixListener]
    else
      []
    end
  end
end
