defrecord Mix.Dep, [ scm: nil, app: nil, requirement: nil, status: nil, opts: nil,
                     deps: [], source: nil, manager: nil ] do
  @moduledoc """
  This is a record that keeps information about your project
  dependencies. It keeps:

  * scm - a module representing the source code management tool (SCM) operations;
  * app - the app name as an atom;
  * requirements - a binary or regexp with the deps requirement;
  * status - the current status of dependency, check `Mix.Deps.format_status/1` for more info;
  * opts - the options given by the developer
  * source - any possible configuration associated with the manager field,
             rebar.config for rebar or the Mix.Project for Mix
  * manager - the project management, possible values: :rebar / :mix / nil
  """
end

defmodule Mix.Deps do
  @moduledoc """
  A module with common functions to work with dependencies.
  """

  @doc """
  Returns all dependencies recursively as `Mix.Dep` record.

  ## Exceptions

  This function raises an exception in case the developer
  provides a dependency in the wrong format.
  """
  def all do
    { deps, _ } = Mix.Deps.Converger.all(nil, fn(dep, acc) -> { dep, acc } end)
    deps
  end

  @doc """
  Returns all dependencies but with a custom callback and
  accumulator.
  """
  def all(acc, callback) do
    { _deps, acc } = Mix.Deps.Converger.all(acc, callback)
    acc
  end

  @doc """
  Returns all direct child dependencies.
  """
  defdelegate children(), to: Mix.Deps.Retriever

  @doc """
  Returns all dependencies depending on given dependencies.
  """
  def depending(deps, all_deps // all)

  def depending([], _all_deps) do
    []
  end

  def depending(deps, all_deps) do
    dep_names = Enum.map(deps, fn dep -> dep.app end)

    parents = Enum.filter all_deps, fn dep ->
      Enum.any?(dep.deps, fn child_dep -> child_dep.app in dep_names end)
    end

    parents ++ depending(parents, all_deps)
  end

  @doc """
  Receives a list of deps names and returns deps records.
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
    index = Mix.Dep.__index__(:app)
    Enum.each apps, fn(app) ->
      unless List.keyfind(deps, app, index) do
        Mix.shell.info "unknown dependency #{app} for env #{Mix.env}"
      end
    end

    deps
  end

  @doc """
  Runs the given `fun` inside the given dependency project by
  changing the current working directory and loading the given
  project into the project stack.
  """
  def in_dependency(dep, post_config // [], fun)

  def in_dependency(Mix.Dep[manager: :rebar, opts: opts], post_config, fun) do
    # Use post_config for rebar deps
    Mix.Project.post_config(post_config)
    Mix.Project.push(Mix.Rebar)
    try do
      File.cd!(opts[:dest], fn -> fun.(nil) end)
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
    do: "could not find app file at #{Mix.Utils.relative_to_cwd(path)}"

  def format_status(Mix.Dep[status: { :invalidapp, path }]),
    do: "the app file at #{Mix.Utils.relative_to_cwd(path)} is invalid"

  def format_status(Mix.Dep[status: { :invalidvsn, vsn }]),
    do: "the dependency does not match the specified version, got #{vsn}"

  def format_status(Mix.Dep[status: { :lockmismatch, _ }]),
    do: "lock mismatch: the dependency is out of date"

  def format_status(Mix.Dep[status: :nolock]),
    do: "the dependency is not locked"

  def format_status(Mix.Dep[status: { :diverged, other }, opts: opts]),
    do: "different specs were given for this dependency, choose one in your deps:\n" <>
        "$ #{inspect_kw opts}\n$ #{inspect_kw other.opts}\n"

  def format_status(Mix.Dep[status: { :unavailable, _ }]),
    do: "the dependency is not available, run `mix deps.get`"

  defp inspect_kw(list) do
    middle = lc { key, value } inlist Enum.sort(list), do: "#{key}: #{inspect value, raw: true}"
    "[ " <> Enum.join(middle, ",\n  ") <> " ]"
  end

  @doc """
  Checks the lock for the given dependency and update its status accordingly.
  """
  def check_lock(Mix.Dep[scm: scm, app: app, opts: opts] = dep, lock) do
    if available?(dep) do
      rev  = lock[app]
      opts = Keyword.put(opts, :lock, rev)

      if scm.matches_lock?(opts) do
        dep
      else
        status = if rev, do: { :lockmismatch, rev }, else: :nolock
        dep.status(status)
      end
    else
      dep
    end
  end

  @doc """
  Updates the dependency inside the given project.
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
  def available?(Mix.Dep[status: { :diverged, _ }]),    do: false
  def available?(Mix.Dep[status: { :unavailable, _ }]), do: false
  def available?(_), do: true

  @doc """
  Check if a dependency is part of an umbrella project as a top level project.
  """
  def in_umbrella?(Mix.Dep[opts: opts], apps_path) do
    apps_path == Path.expand(Path.join(opts[:dest], ".."))
  end

  @doc """
  Check if a dependency is out of date or not, considering its
  lock status. Therefore, be sure to call `check_lock` before
  invoking this function.
  """
  def out_of_date?(Mix.Dep[status: { :lockmismatch, _ }]), do: true
  def out_of_date?(Mix.Dep[status: :nolock]),              do: true
  def out_of_date?(dep),                                   do: not available?(dep)

  @doc """
  Format the dependency for printing.
  """
  def format_dep(Mix.Dep[scm: scm, app: app, status: status, opts: opts]) do
    version =
      case status do
        { :ok, vsn } when vsn != nil -> "(#{vsn}) "
        _ -> ""
      end

    "#{app} #{version}#{inspect scm.format(opts)}"
  end

  @doc """
  Returns all compile paths for the dependency.
  """
  def compile_paths(Mix.Dep[app: app, opts: opts, manager: manager]) do
    if manager == :mix do
      Mix.Project.in_project app, opts[:dest], fn _ ->
        Mix.Project.compile_paths
      end
    else
      [ Path.join(opts[:dest], "ebin") ]
    end
  end

  @doc """
  Returns all load paths for the dependency.
  """
  def load_paths(Mix.Dep[manager: :mix, app: app, opts: opts]) do
    paths = Mix.Project.in_project app, opts[:dest], fn _ ->
      Mix.Project.load_paths
    end
    Enum.uniq paths
  end

  def load_paths(Mix.Dep[manager: :rebar, opts: opts, source: source]) do
    # Add root dir and all sub dirs with ebin/ directory
    [ opts[:dest] | (source[:sub_dirs] || []) ]
      |> Enum.map(Path.wildcard(&1))
      |> List.concat
      |> Enum.map(fn path -> Path.join([opts[:dest], path, "ebin"]) end)
      |> Enum.filter(File.dir?(&1))
  end

  def load_paths(Mix.Dep[manager: nil, opts: opts]) do
    [ Path.join(opts[:dest], "ebin") ]
  end

  @doc """
  Returns true if dependency is a mix project.
  """
  def mix?(Mix.Dep[manager: manager]) do
    manager == :mix
  end

  @doc """
  Returns true if dependency is a rebar project.
  """
  def rebar?(Mix.Dep[manager: manager]) do
    manager == :rebar
  end

  @doc """
  Returns true if dependency is a make project.
  """
  def make?(dep) do
    File.regular? Path.join(dep.opts[:dest], "Makefile")
  end
end
