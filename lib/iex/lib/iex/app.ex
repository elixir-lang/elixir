defmodule IEx.App do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [IEx.Config, IEx.Broker, IEx.Pry]
    Supervisor.start_link(children, strategy: :one_for_one, name: IEx.Supervisor)
  end
end
