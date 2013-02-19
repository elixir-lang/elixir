defmodule ExUnit.Sup do
  @moduledoc false

  use Supervisor.Behaviour

  def start_link(options) do
    :supervisor.start_link({ :local, __MODULE__ }, __MODULE__, options)
  end

  def init(options) do
    tree = [ worker(ExUnit.Server, [options]) ]
    supervise(tree, strategy: :one_for_one)
  end
end
