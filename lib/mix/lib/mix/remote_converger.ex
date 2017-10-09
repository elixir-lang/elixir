defmodule Mix.RemoteConverger do
  @moduledoc false

  # A remote converger returns updated dependencies with
  # extra information that can be used during Mix's converging.
  # Useful for things like external package managers

  @doc """
  Returns `true` if given dependency is handled by
  remote converger.
  """
  @callback remote?(Mix.Dep.t()) :: boolean

  @doc """
  Runs the remote converger.

  Return updated lock.
  """
  @callback converge([Mix.Dep.t()], map) :: map

  @doc """
  Returns child dependencies the converger has for the
  dependency. This list should filter the loaded children.
  """
  @callback deps(Mix.Dep.t(), map) :: [atom]

  @doc """
  Called after all convergers have run so that the remote
  converger can perform cleanup.
  """
  @callback post_converge() :: any

  @doc """
  Gets registered remote converger.
  """
  def get do
    Mix.State.get(:remote_converger)
  end

  @doc """
  Registers a remote converger.
  """
  def register(mod) when is_atom(mod) do
    Mix.State.put(:remote_converger, mod)
  end
end
