defmodule Mix.RemoteConverger do
  @moduledoc false
  use Behaviour

  # A remote converger returns updated dependencies with
  # extra information that can be used during mix's converging.
  # Useful for things like external package managers

  @doc """
  Return `true` if given dependency is handled by
  remote converger.
  """
  defcallback remote?(Mix.Dep.t) :: boolean

  @doc """
  Run the remote converger.

  Return updated lock.
  """
  defcallback converge([Mix.Dep.t], map) :: map

  @doc """
  Returns child dependencies the converger has for the
  dependency. This list should filter the loaded children.
  """
  defcallback deps(Mix.Dep.t, map) :: [atom]


  @doc """
  Get registered remote converger.
  """
  def get do
    Application.get_env(:mix, :remote_converger)
  end

  @doc """
  Register a remote converger.
  """
  def register(mod) when is_atom(mod) do
    Application.put_env(:mix, :remote_converger, mod)
  end
end
