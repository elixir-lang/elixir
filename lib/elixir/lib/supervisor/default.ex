defmodule Supervisor.Default do
  @moduledoc false

  @doc """
  Supervisor callback that simply returns the given args.

  This is the supervisor used by `Supervisor.start_link/2`
  and others.
  """
  def init(args) do
    args
  end
end
