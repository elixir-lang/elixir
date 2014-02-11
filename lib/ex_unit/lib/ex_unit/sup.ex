defmodule ExUnit.Sup do
  @moduledoc false

  use Supervisor.Behaviour

  def start_link() do
    :supervisor.start_link({ :local, __MODULE__ }, __MODULE__, :ok)
  end

  def init(:ok) do
    tree = [ worker(ExUnit.Server, []) ]
    supervise(tree, strategy: :one_for_all)
  end
end
