defmodule Mix.SCM do
  @moduledoc """
  This module provides helper functions and defines the
  behaviour required by any source code manager (SCM) used by Mix.
  """

  @doc """
  A module implementing the `Mix.SCM` behaviour.
  """
  @type t :: module

  @type opts :: keyword

  @doc """
  Returns a boolean if the dependency can be fetched
  or it is meant to be previously available in the
  file system.

  Local dependencies (i.e. non-fetchable ones) are automatically
  recompiled every time the parent project is compiled.
  """
  @callback fetchable? :: boolean

  @doc """
  Returns a string representing the SCM. This is used
  when printing the dependency and not for inspection,
  so the amount of information should be concise and
  easy to spot.
  """
  @callback format(opts) :: String.t()

  @doc """
  Returns a string representing the SCM. This is used
  when printing the dependency and not for inspection,
  so the amount of information should be concise and
  easy to spot.

  If nil is returned, it means no lock information is available.
  """
  @callback format_lock(opts) :: String.t() | nil

  @doc """
  This behaviour function receives a keyword list of `opts`
  and should return an updated list in case the SCM consumes
  the available options. For example, when a developer specifies
  a dependency:

      {:foo, "0.1.0", github: "foo/bar"}

  Each registered SCM will be asked if they consume this dependency,
  receiving `[github: "foo/bar"]` as argument. Since this option makes
  sense for the Git SCM, it will return an update list of options
  while other SCMs would simply return `nil`.
  """
  @callback accepts_options(app :: atom, opts) :: opts | nil

  @doc """
  This behaviour function returns a boolean if the
  dependency is available.
  """
  @callback checked_out?(opts) :: boolean

  @doc """
  This behaviour function checks out dependencies.

  If the dependency is locked, a lock is received in `opts`
  and the repository must be check out at the lock. Otherwise,
  no lock is given and the repository can be checked out
  to the latest version.

  It must return the current lock.
  """
  @callback checkout(opts) :: any

  @doc """
  This behaviour function updates dependencies. It may be
  called by `deps.get` or `deps.update`.

  In the first scenario, a lock is received in `opts` and
  the repository must be updated to the lock. In the second,
  no lock is given and the repository can be updated freely.

  It must return the current lock.
  """
  @callback update(opts) :: any

  @doc """
  This behaviour function checks the status of the lock. In
  particular, it checks if the revision stored in the lock
  is the same as the repository it is currently in.

  It may return:

    * `:mismatch` - if the lock doesn't match and we need to
      simply move to the latest lock

    * `:outdated` - the repository options are outdated in the
      lock and we need to trigger a full update

    * `:ok` - everything is fine

  The lock is sent via `opts[:lock]` but it may not always be
  available. In such cases, if the SCM requires a lock, it must
  return `:mismatch`, otherwise simply `:ok`.

  Note the lock may also belong to another SCM and as such, an
  structural check is required. A structural mismatch should always
  return `:outdated`.
  """
  @callback lock_status(opts) :: :mismatch | :outdated | :ok

  @doc """
  Receives two options and must return `true` if they refer to the
  same repository. The options are guaranteed to belong to the
  same SCM.
  """
  @callback equal?(opts1 :: opts, opts2 :: opts) :: boolean

  @doc """
  Returns the usable managers for the dependency. This can be used
  if the SCM has extra knowledge of the dependency, otherwise it
  should return an empty list.
  """
  @callback managers(opts) :: [atom]

  @doc """
  Returns all available SCMs. Each SCM is tried in order
  until a matching one is found.
  """
  def available do
    {:ok, scm} = Mix.State.fetch(:scm)
    scm
  end

  @doc """
  Prepends the given SCM module to the list of available SCMs.
  """
  def prepend(mod) when is_atom(mod) do
    Mix.State.prepend(:scm, mod)
  end

  @doc """
  Appends the given SCM module to the list of available SCMs.
  """
  def append(mod) when is_atom(mod) do
    Mix.State.append(:scm, mod)
  end
end
