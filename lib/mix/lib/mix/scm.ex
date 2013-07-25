defmodule Mix.SCM do
  use Behaviour

  @typep opts :: [{ atom, any }]
  @typep lock

  @moduledoc """
  This module provides helper functions and defines the
  behavior required by any SCM used by mix.
  """

  @doc """
  Returns an Elixir term that contains relevant SCM
  information for printing.
  """
  defcallback format(opts) :: term

  @doc """
  Returns an Elixir term that contains relevant SCM
  lock information for printing.
  """
  defcallback format_lock(lock) :: term

  @doc """
  This behavior function receives a keyword list of `opts`
  and should return an updated list in case the SCM consumes
  the available options. For example, when a developer specifies
  a dependency:

      { :foo, "0.1.0", github: "foo/bar" }

  Each registered SCM will be asked if they consume this dependency,
  receiving [github: "foo/bar"] as argument. Since this option makes
  sense for the Git SCM, it will return an update list of options
  while other SCMs would simply return nil.
  """
  defcallback accepts_options(app :: atom, opts) :: opts | nil

  @doc """
  This behavior function returns a boolean if the
  dependency is available.
  """
  defcallback checked_out?(opts) :: boolean

  @doc """
  This behavior function checks out dependencies.

  If the dependency is locked, a lock is received in `opts`
  and the repository must be check out at the lock. Otherwise,
  no lock is given and the repository can be checked out
  to the latest version.
  """
  defcallback checkout(opts) :: any

  @doc """
  This behavior function updates dependencies. It may be
  called by `deps.get` or `deps.update`.

  In the first scenario, a lock is received in `opts` and
  the repository must be updated to the lock. In the second,
  no lock is given and the repository can be updated freely.

  It must return the current lock.
  """
  defcallback update(opts) :: any

  @doc """
  This behavior function checks if the dependency is locked and
  the current repository version matches the lock. Note that some
  SCMs do not require a lock, for such, this function can simply
  return true.
  """
  defcallback matches_lock?(opts) :: boolean

  @doc """
  Receives two options and must return true if the refer to the
  same repository. The options are guaranteed to belong to the
  same SCM.
  """
  defcallback equal?(opts1 :: opts, opts2 :: opts) :: boolean

  @doc """
  This behavior function should clean the given dependency.
  """
  defcallback clean(opts) :: any

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
    register Mix.SCM.Path
  end
end
