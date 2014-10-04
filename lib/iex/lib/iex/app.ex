defmodule IEx.App do
  @moduledoc false

  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    children = [worker(IEx.Config, [])]
    options = [strategy: :one_for_one, name: IEx.Supervisor]

    tab = IEx.Config.new()
    case Supervisor.start_link(children, options) do
      {:ok, pid} ->
        {:ok, pid, tab}
      {:error, _} = error ->
        IEx.Config.delete(tab)
        error
    end
  end

  def stop(tab) do
    IEx.Config.delete(tab)
  end
end
