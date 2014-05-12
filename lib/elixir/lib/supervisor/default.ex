defmodule Supervisor.Default do
  @moduledoc false
  @behaviour :supervisor

  @doc """
  Supevisor callback that simply returns the given args.

  This is the supervisor used by `Supervisor.start_link/2`.
  """
  def init(args) do
    args
  end
end