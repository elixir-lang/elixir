defmodule Mix.RemoteConverger do
  @moduledoc false
  use Behaviour

  # A remote converger returns updated dependencies with
  # extra information that can be used during mix's converging.
  # Useful for things like external package managers

  @doc """
  Return `true` if given dependency app name handled by
  remote converger.
  """
  defcallback remote?(atom) :: boolean

  @doc """
  Run the remote converger.

  Return the converged deps.
  """
  defcallback converge([Mix.Dep.t]) :: [Mix.Dep.t]

  @doc """
  Get registered remote converger.
  """
  def get do
    case :application.get_env(:mix, :remote_converger) do
      { :ok, converger } -> converger
      :undefined -> nil
    end
  end

  @doc """
  Register a remote converger.
  """
  def register(mod) when is_atom(mod) do
    :application.set_env(:mix, :remote_converger, mod)
  end
end
