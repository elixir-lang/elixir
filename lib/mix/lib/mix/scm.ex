defmodule Mix.SCM do
  use Behaviour

  @typep opts :: [{ atom, any }]
  @typep lock

  @moduledoc """
  This module provides helper functions and defines the
  behavior required by any SCM used by mix.
  """

  @doc """
  Returns a string representing the SCM. This is used
  when printing the dependency and not for inspection,
  so the amount of information should be concise and
  easy to spot.
  """
  defcallback format(opts) :: String.t

  @doc """
  Returns a string representing the SCM. This is used
  when printing the dependency and not for inspection,
  so the amount of information should be concise and
  easy to spot.

  If nil is returned, it means no lock information is available.
  """
  defcallback format_lock(lock) :: String.t | nil

  @doc """
  This behavior function receives a keyword list of `opts`
  and should return an updated list in case the SCM consumes
  the available options. For example, when a developer specifies
  a dependency:

      { :foo, "0.1.0", github: "foo/bar" }

  Each registered SCM will be asked if they consume this dependency,
  receiving `[github: "foo/bar"]` as argument. Since this option makes
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

  It must return the current lock.
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
  This behavior function checks the status of the lock. In
  particular, it checks if the revision stored in the lock
  is the same as the repository is currently in. It may return:

  * `:mismatch` - if the lock doesn't match and we need to
    simply move to the latest lock
  * `:outdated` - the repository options are out of dated in the
    lock and we need to trigger a full update
  * `:ok` - everything is fine

  The lock is sent via `opts[:lock]` but it may not always be
  available. In such cases, if the SCM requires a lock, it must
  return `:lockmismatch`, otherwise simply `:ok`.

  Note the lock may also belong to another SCM and as such, an
  structural check is required. A structural mismatch should always
  return `:outdated`.
  """
  defcallback lock_status(opts) :: :mismatch | :outdated | :ok

  @doc """
  Receives two options and must return true if they refer to the
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
