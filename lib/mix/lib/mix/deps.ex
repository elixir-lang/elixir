defrecord Mix.Dep, [ scm: nil, app: nil, requirement: nil, status: nil, opts: nil,
                     deps: [], source: nil, manager: nil, from: nil ] do
  @moduledoc """
  This is a record that keeps information about your project
  dependencies. It contains:

  * `scm` - a module representing the source code management tool (SCM) operations;
  * `app` - the application name as an atom;
  * `requirement` - a binary or regex with the dependency's requirement
  * `status` - the current status of the dependency, check `Mix.Deps.format_status/1` for more info;
  * `opts` - the options given by the developer
  * `deps` - dependencies of this dependency
  * `source` - any possible configuration associated with the `manager` field,
      `rebar.config` for rebar or the `Mix.Project` for Mix
  * `manager` - the project management, possible values: `:rebar` | `:mix` | `:make` | `nil`
  * `from` - path to the file where the dependency was defined

  """
end

defmodule Mix.Deps do
  @moduledoc %S"""
  A module with common functions to work with dependencies.

  Dependencies must be specified in the Mix application in the
  following format:

      { app :: atom, opts :: Keyword.t }
      { app :: atom, requirement :: String.t, opts :: Keyword.t }

  The application name must be an atom, the version requirement must
  be a string according to the specification outline in `Version`
  and opts is a keyword lists that may include options for the underlying
  SCM or options used by Mix. Each set of options is documented below.

  ## Mix options

  * `:app` - Do not try to read the app file for this dependency
  * `:env` - The environment to run the dependency on, defaults to :prod
  * `:compile` - A command to compile the dependency, defaults to a mix,
                 rebar or make command

  ## Git options (`:git`)

  * `:git`        - The git repository URI
  * `:github`     - A shortcut for specifying git repos from github, uses `git:`
  * `:ref`        - The reference to checkout (may be a branch, a commit sha or a tag)
  * `:branch`     - The git branch to checkout
  * `:tag`        - The git tag to checkout
  * `:submodules` - When true, initialize submodules for the repo

  ## Path options (`:path`)

  * `:path` - The path for the dependency
  * `:in_umbrella` - When true, sets a path dependency pointing to "../#{app}",
                     sharing the same environment as the current application

  ## Internal options

  Those options are set internally by Mix and they can't be
  overriden from the Mixfile:

  * `:dest` - The destination path for the dependency
  * `:lock` - The lock information retrieved from mix.lock

  """

  @doc """
  Returns all dependencies recursively as a `Mix.Dep` record.

  ## Exceptions

  This function raises an exception if any of the dependencies
  provided in the project are in the wrong format.
  """
  def all do
    { deps, _ } = Mix.Deps.Converger.all(nil, fn(dep, acc) -> { dep, acc } end)
    deps
  end

  @doc """
  Maps and reduces over all dependencies, one by one.

  This is useful in case you want to retrieve the dependency
  tree for a project but process and change them along the way.
  For example, `mix deps.get` uses it to get all dependencies
  by first fetching the parent and then updating the tree as it goes.

  The callback expects the current dependency and the accumulator
  as arguments. The accumulator is returned as result.
  """
  def all(acc, callback) do
    { _deps, acc } = Mix.Deps.Converger.all(acc, callback)
    acc
  end

  @doc """
  Returns all children dependencies for the current project.
  Umbrella projects have their apps treated as direct dependencies.

  ## Exceptions

  This function raises an exception if any of the dependencies
  provided in the project are in the wrong format.
  """
  defdelegate children(), to: Mix.Deps.Retriever

  @doc """
  Returns all given dependencies and their depending dependencies.
  """
  def with_depending(deps, all_deps // all) do
    deps ++ do_with_depending(deps, all_deps)
      |> Enum.uniq(&(&1.app))
  end

  defp do_with_depending([], _all_deps) do
    []
  end

  defp do_with_depending(deps, all_deps) do
    dep_names = Enum.map(deps, fn dep -> dep.app end)

    parents = Enum.filter all_deps, fn dep ->
      Enum.any?(dep.deps, fn child_dep -> child_dep.app in dep_names end)
    end

    do_with_depending(parents, all_deps) ++ parents
  end

  @doc """
  Receives a list of dependency names and returns dependency records.
  Logs a message if the dependency could not be found.
  """
  def by_name(given, all_deps // all) do
    # Ensure all apps are atoms
    apps = Enum.map given, fn(app) ->
      if is_binary(app), do: binary_to_atom(app), else: app
    end

    # We need to keep the order of all, which properly orders deps
    deps = Enum.filter all_deps, fn(dep) -> dep.app in apps end

    # Now we validate the given atoms
    index = Mix.Dep.__record__(:index, :app)
    Enum.each apps, fn(app) ->
      unless List.keyfind(deps, app, index) do
        Mix.shell.info "unknown dependency #{app} for env #{Mix.env}"
      end
    end

    deps
  end

  @doc """
  Run the given `fun` inside the given dependency project by
  changing the current working directory and loading the given
  project onto the project stack.

  In case the project is a rebar dependency, a `Mix.Rebar` project
  will be pushed into the stack to simulate the rebar configuration.
  """
  def in_dependency(dep, post_config // [], fun)

  def in_dependency(Mix.Dep[manager: :rebar, opts: opts], post_config, fun) do
    # Use post_config for rebar deps
    Mix.Project.post_config(post_config)
    Mix.Project.push(Mix.Rebar)
    try do
      File.cd!(opts[:dest], fn -> fun.(Mix.Rebar) end)
    after
      Mix.Project.pop
    end
  end

  def in_dependency(Mix.Dep[app: app, opts: opts], post_config, fun) do
    env     = opts[:env] || :prod
    old_env = Mix.env

    try do
      Mix.env(env)
      Mix.Project.in_project(app, opts[:dest], post_config, fun)
    after
      Mix.env(old_env)
    end
  end

  @doc """
  Formats the status of a dependency.
  """
  def format_status(Mix.Dep[status: { :ok, _vsn }]),
    do: "ok"

  def format_status(Mix.Dep[status: { :noappfile, path }]),
    do: "could not find an app file at #{Path.relative_to_cwd(path)}"

  def format_status(Mix.Dep[status: { :invalidapp, path }]),
    do: "the app file at #{Path.relative_to_cwd(path)} is invalid"

  def format_status(Mix.Dep[status: { :invalidvsn, vsn }]),
    do: "the app file contains an invalid version: #{inspect vsn}"

  def format_status(Mix.Dep[status: { :nomatchvsn, vsn }, requirement: req]),
    do: "the dependency does not match the requirement #{req}, got #{vsn}"

  def format_status(Mix.Dep[status: { :lockmismatch, _ }]),
    do: "lock mismatch: the dependency is out of date"

  def format_status(Mix.Dep[status: :lockoutdated]),
    do: "lock outdated: the lock is outdated compared to the options in your mixfile"

  def format_status(Mix.Dep[status: :nolock]),
    do: "the dependency is not locked"

  def format_status(Mix.Dep[app: app, status: { :diverged, other }] = dep) do
    "different specs were given for the #{inspect app} app:\n" <>
    "#{dep_status(dep)}#{dep_status(other)}" <>
    "\n  Ensure they match or specify one in your #{inspect Mix.Project.get} deps and set `override: true`"
  end

  def format_status(Mix.Dep[app: app, status: { :overriden, other }] = dep) do
    "the dependency #{app} in #{Path.relative_to_cwd(dep.from)} is overriding a child dependency:\n" <>
    "#{dep_status(dep)}#{dep_status(other)}" <>
    "\n  Specify one in your #{inspect Mix.Project.get} deps and set `override: true`"
  end

  def format_status(Mix.Dep[status: { :unavailable, _ }]),
    do: "the dependency is not available, run `mix deps.get`"

  def format_status(Mix.Dep[status: { :elixirlock, _ }]),
    do: "the dependency is built with an out-of-date elixir version, run `mix deps.get`"

  defp dep_status(Mix.Dep[app: app, requirement: req, opts: opts, from: from]) do
    info = { app, req, Dict.drop(opts, [:dest, :lock, :env]) }
    "\n  > In #{Path.relative_to_cwd(from)}:\n    #{inspect info}\n"
  end

  @doc """
  Check the lock for the given dependency and update its status accordingly.
  """
  def check_lock(Mix.Dep[scm: scm, app: app, opts: opts] = dep, lock) do
    if available?(dep) do
      if vsn = old_elixir_lock(dep) do
        dep.status({ :elixirlock, vsn })
      else
        rev  = lock[app]
        opts = Keyword.put(opts, :lock, rev)

        case scm.lock_status(opts) do
          :mismatch ->
            dep.status(if rev, do: { :lockmismatch, rev }, else: :nolock)
          :outdated ->
            dep.status :lockoutdated
          :ok ->
            dep
        end
      end
    else
      dep
    end
  end

  @doc """
  Updates the dependency.

  This function is useful when the given dependency changes
  (for example, it was just checked out) and you want to refetch
  its information.
  """
  defdelegate update(dep), to: Mix.Deps.Retriever

  @doc """
  Check if a dependency is ok.
  """
  def ok?(Mix.Dep[status: { :ok, _ }]), do: true
  def ok?(_), do: false

  @doc """
  Check if a dependency is available.
  """
  def available?(Mix.Dep[status: { :overriden, _ }]),    do: false
  def available?(Mix.Dep[status: { :diverged, _ }]),     do: false
  def available?(Mix.Dep[status: { :unavailable, _ }]),  do: false
  def available?(_), do: true

  @doc """
  Check if a dependency is out of date considering its
  lock status. Therefore, be sure to call `check_lock` before
  invoking this function.
  """
  def out_of_date?(Mix.Dep[status: { :lockmismatch, _ }]), do: true
  def out_of_date?(Mix.Dep[status: :lockoutdated]),        do: true
  def out_of_date?(Mix.Dep[status: :nolock]),              do: true
  def out_of_date?(Mix.Dep[status: { :elixirlock, _ }]),   do: true
  def out_of_date?(dep),                                   do: not available?(dep)

  @doc """
  Format a dependency for printing.
  """
  def format_dep(Mix.Dep[scm: scm, app: app, status: status, opts: opts]) do
    version =
      case status do
        { :ok, vsn } when vsn != nil -> "#{vsn} "
        _ -> ""
      end

    "#{app} #{version}(#{scm.format(opts)})"
  end

  @doc """
  Returns all compile paths for the dependency.
  """
  def compile_path(Mix.Dep[app: app, opts: opts, manager: manager]) do
    if manager == :mix do
      Mix.Project.in_project app, opts[:dest], fn _ ->
        Mix.Project.compile_path
      end
    else
      Path.join(opts[:dest], "ebin")
    end
  end

  @doc """
  Returns all load paths for the dependency.
  """
  def load_paths(Mix.Dep[manager: :mix, app: app, opts: opts]) do
    Mix.Project.in_project(app, opts[:dest], fn _ ->
      Mix.Project.load_paths
    end) |> Enum.uniq
  end

  def load_paths(Mix.Dep[manager: :rebar, opts: opts, source: source]) do
    # Add root dir and all sub dirs with ebin/ directory
    sub_dirs = Enum.map(source[:sub_dirs] || [], fn path ->
      Path.join(opts[:dest], path)
    end)

    [ opts[:dest] | sub_dirs ]
      |> Enum.map(&Path.wildcard(&1))
      |> Enum.concat
      |> Enum.map(&Path.join(&1, "ebin"))
      |> Enum.filter(&File.dir?(&1))
  end

  def load_paths(Mix.Dep[manager: manager, opts: opts]) when manager in [:make, nil] do
    [ Path.join(opts[:dest], "ebin") ]
  end

  @doc """
  Return `true` if dependency is a mix project.
  """
  def mix?(Mix.Dep[manager: manager]) do
    manager == :mix
  end

  @doc """
  Return `true` if dependency is a rebar project.
  """
  def rebar?(Mix.Dep[manager: manager]) do
    manager == :rebar
  end

  @doc """
  Return `true` if dependency is a make project.
  """
  def make?(Mix.Dep[manager: manager]) do
    manager == :make
  end

  ## Helpers

  # Returns the elixir lock version of the given dependency
  defp old_elixir_lock(dep) do
    in_dependency(dep, fn _ ->
      old_vsn = Mix.Deps.Lock.elixir_vsn
      if old_vsn && old_vsn != System.version do
        old_vsn
      end
    end)
  end
end
