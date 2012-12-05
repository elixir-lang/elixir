defmodule ExUnit.Sup do
  @moduledoc false

  use Supervisor.Behaviour

  def start_link(user_options) do
    :supervisor.start_link({ :local, __MODULE__ }, __MODULE__, user_options)
  end

  def init(user_options) do
    tree = [ worker(ExUnit.Server, [user_options]) ]
    supervise(tree, strategy: :one_for_one)
  end
end
