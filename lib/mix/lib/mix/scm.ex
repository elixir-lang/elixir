defmodule Mix.SCM do
  @doc """
  Register required callbacks.
  """
  def behaviour_info(:callbacks) do
    [key: 0, consumes?: 1, available?: 1, get: 2, update: 2, clean: 1]
  end

  @doc """
  Returns all available SCM.
  """
  def available do
    Mix.Server.call(:scm)
  end

  @doc """
  Register the scm repository with the given `key` and `mod`.
  """
  def register(mod) when is_atom(mod) do
    Mix.Server.cast({ :add_scm, mod })
  end

  @doc """
  Register builtin SCMs.
  """
  def register_builtin do
    register Mix.SCM.Git
    register Mix.SCM.Raw
  end
end