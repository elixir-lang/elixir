defmodule Mix.SCM do
  @doc """
  This module provides helper functions and defines the
  behavior required by any SCM used by mix.
  """

  @doc """
  Register required callbacks.
  """
  def behaviour_info(:callbacks) do
    [key: 0, consumes?: 1, available?: 2, get: 2, check?: 2, update: 2, clean: 2]
  end

  @doc """
  This behavior function should retrieve an atom representing
  the SCM key. In the dependency opts, a value for the given
  must be found since it is used to print information about
  the requested dependency.
  """
  def key

  @doc """
  This behavior function receives a keywords list of `opts`
  and should return an updated list in case the SCM consumes
  the available options. For example, when a developer specifies
  a dependency:

      { "foo", "0.1.0", github: "foo/bar" }

  Each registered SCM will be asked if they consume this dependency,
  receiving [github: "foo/bar"] as argument. Since this option makes
  sense for the Git SCM, it will return an update list of options
  while other SCMs would simply return nil.
  """
  def consumes?(opts)

  @doc """
  This behavior function receives a `path`, `opts` and returns
  a boolean if the dependency is available.
  """
  def available?(path, opts)

  @doc """
  This behavior function gets unchecked dependencies.
  If the dependency is locked, it receives the lock under the
  `:lock` key in `opts`. In case no lock is given, it must
  return a new lock (if one exists). If a lock is given,
  it must preferably return the same lock, but can return
  a different one in case of failure.
  """
  def get(path, opts)

  @doc """
  This behavior function updates dependencies. It may be
  called either directly via `deps.update` or implicitly
  by `deps.get`. In the first scenario, no lock is received,
  while one is given in the second.
  """
  def update(path, opts)

  @doc """
  This behavior function checks if the dependency is locked and
  the current repository version matches the lock. Note that some
  SCMs do not require a lock, for such, this function can simply
  return true.
  """
  def check?(path, opts)

  @doc """
  This behavior function should clean the given dependency.
  """
  def clean(path, opts)

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