defmodule Mix.Sup do
  @moduledoc false
  use Supervisor.Behaviour

  def start_link(env) do
    :supervisor.start_link(__MODULE__, env)
  end

  def init(env) do
    tree = [worker(Mix.Server, [env])]
    supervise(tree, strategy: :one_for_one)
  end
end
