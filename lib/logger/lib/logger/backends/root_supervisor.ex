defmodule Logger.Backends.RootSupervisor do
  @moduledoc false
  use Supervisor

  def start_link(backends) do
    Supervisor.start_link(__MODULE__, backends, name: __MODULE__)
  end

  @impl true
  def init(backends) do
    start_options = Application.fetch_env!(:logger, :start_options)
    counter = :counters.new(1, [:atomics])

    children = [
      %{
        id: :gen_event,
        start: {:gen_event, :start_link, [{:local, Logger}, start_options]},
        modules: :dynamic
      },
      {Logger.Backends.Watcher, {Logger.Backends.Config, counter}},
      {Logger.Backends, backends}
    ]

    :ok =
      :logger.add_handler(Logger, Logger.Backends.Handler, %{
        level: :all,
        config: %{counter: counter},
        filter_default: :log,
        filters: []
      })

    Supervisor.init(children, strategy: :rest_for_one)
  end
end
