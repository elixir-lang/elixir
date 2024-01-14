defmodule ExUnit.OnExitHandler.Supervisor do
  @moduledoc false
  use Supervisor

  def start_link(children) do
    Supervisor.start_link(__MODULE__, {children, get_callers(self())})
  end

  @impl true
  def init({children, callers}) do
    put_callers(callers)
    Supervisor.init(children, strategy: :one_for_one, max_restarts: 1_000_000, max_seconds: 1)
  end

  defp get_callers(owner) do
    case :erlang.get(:"$callers") do
      [_ | _] = list -> [owner | list]
      _ -> [owner]
    end
  end

  defp put_callers(callers) do
    :erlang.put(:"$callers", callers)
  end
end
