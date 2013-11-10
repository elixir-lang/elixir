defmodule Mix.Sup do
  @moduledoc false
  use Supervisor.Behaviour

  def start_link() do
    :supervisor.start_link(__MODULE__, :ok)
  end

  def init(:ok) do
    tree = [
      worker(Mix.TasksServer, []),
      worker(Mix.ProjectStack, [])
    ]
    supervise(tree, strategy: :one_for_one)
  end
end
