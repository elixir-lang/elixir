defmodule Mix.Dep do
  @moduledoc false

  @doc """
  The Mix.Dep a struct keeps information about your project dependencies.
  It contains:

  * `scm` - a module representing the source code management tool (SCM) operations;
  * `app` - the application name as an atom;
  * `requirement` - a binary or regex with the dependency's requirement
  * `status` - the current status of the dependency, check `Mix.Dep.format_status/1` for more info;
  * `opts` - the options given by the developer
  * `deps` - dependencies of this dependency
  * `manager` - the project management, possible values: `:rebar` | `:mix` | `:make` | `nil`
  * `from` - path to the file where the dependency was defined
  * `extra` - a slot for adding extra configuration based on the scm.
              the information on this field is private to the scm and
              should not be relied on.

  A dependency is in two specific states: loaded and unloaded.

  When a dependency is unloaded, it means Mix only parsed its specification
  and made no attempt to actually load the dependency or validate its
  status. When the dependency is loaded, it means Mix attempted to fetch,
  load and validate it, the status is set in the status field.

  Furthermore, in the `opts` fields, Mix keeps some internal options, which
  can be accessed by SCMs:

  * `:app` - The application name
  * `:dest` - The destination path for the dependency
  * `:lock` - The lock information retrieved from mix.lock

  """
  defstruct scm: nil, app: nil, requirement: nil, status: nil, opts: nil,
            deps: [], extra: nil, manager: nil, from: nil

  @doc """
  Returns all children dependencies for the current project,
  as well as the defined apps in case of umbrella projects.
  The children dependencies returned by this function were
  not loaded yet.

  ## Exceptions

  This function raises an exception if any of the dependencies
  provided in the project are in the wrong format.
  """
  defdelegate children(otps), to: Mix.Dep.Loader

  @doc """
  Returns loaded dependencies recursively as a `Mix.Dep` struct.

  ## Exceptions

  This function raises an exception if any of the dependencies
  provided in the project are in the wrong format.
  """
  def loaded(opts) do
    { deps, _, _ } = Mix.Dep.Converger.all(nil, nil, opts, &{ &1, &2, &3 })
    Mix.Dep.Converger.topsort(deps)
  end

  @doc """
  Receives a list of  dependency names and returns loaded `Mix.Dep`s.
  Logs a message if the dependency could not be found.

  ## Exceptions

  This function raises an exception if any of the dependencies
  provided in the project are in the wrong format.
  """
  def loaded_by_name(given, all_deps \\ nil, opts) do
    all_deps = all_deps || loaded(opts)

    # Ensure all apps are atoms
    apps = to_app_names(given)

    # We need to keep the order of deps, loaded/1 properly orders them
    deps = Enum.filter(all_deps, &(&1.app in apps))

    Enum.each apps, fn(app) ->
      unless Enum.any?(all_deps, &(&1.app == app)) do
        raise Mix.Error, message: "Unknown dependency #{app} for environment #{Mix.env}"
      end
    end

    deps
  end

  @doc """
  Maps and reduces over all unloaded dependencies, one by one.

  This is useful in case you want to retrieve the dependency
  tree for a project but process and change them along the way.
  For example, `mix deps.get` uses it to get all dependencies
  by first fetching the parent and then updating the tree as it goes.

  The callback expects the current dependency and the accumulator
  as arguments. The accumulator is returned as result.

  See `Mix.Dep.Converger.all/3` for options.

  ## Exceptions

  This function raises an exception if any of the dependencies
  provided in the project are in the wrong format.
  """
  def unloaded(acc, lock, opts, callback) do
    { deps, acc, lock } = Mix.Dep.Converger.all(acc, lock, opts, callback)
    { Mix.Dep.Converger.topsort(deps), acc, lock }
  end

  @doc """
  Receives a list of dependency names and maps and reduces over
  them. See `unloaded`.

  ## Exceptions

  This function raises an exception if any of the dependencies
  provided in the project are in the wrong format.
  """
  def unloaded_by_name(given, acc, lock, opts, callback) do
    names = to_app_names(given)

    unloaded(acc, lock, opts, fn dep, acc, lock ->
      if dep.app in names do
        callback.(dep, acc, lock)
      else
        { dep, acc, lock }
      end
    end)
  end

  @doc """
  Runs the given `fun` inside the given dependency project by
  changing the current working directory and loading the given
  project onto the project stack.

  It is expected a loaded dependency as argument.
  """
  def in_dependency(dep, post_config \\ [], fun)

  def in_dependency(%Mix.Dep{app: app, opts: opts}, config, fun) do
    # Set the app_path to be the one stored in the dependency.
    # This is important because the name of application in the
    # mix.exs file can be different than the actual name and we
    # choose to respect the one in the mix.exs
    config = Keyword.merge(Mix.Project.deps_config, config)
    config = Keyword.put(config, :app_path, opts[:build])

    env = opts[:env] || :prod
    old_env = Mix.env

    try do
      Mix.env(env)
      Mix.Project.in_project(app, opts[:dest], config, fun)
    after
      Mix.env(old_env)
    end
  end

  @doc """
  Formats the status of a dependency.
  """
  def format_status(%Mix.Dep{status: { :ok, _vsn }}),
    do: "ok"

  def format_status(%Mix.Dep{status: { :noappfile, path }}),
    do: "could not find an app file at #{Path.relative_to_cwd(path)}, " <>
        "this may happen when you specified the wrong application name in your deps " <>
        "or if the dependency did not compile (which can be amended with `#{mix_env_var}mix deps.compile`)"

  def format_status(%Mix.Dep{status: { :invalidapp, path }}),
    do: "the app file at #{Path.relative_to_cwd(path)} is invalid"

  def format_status(%Mix.Dep{status: { :invalidvsn, vsn }}),
    do: "the app file contains an invalid version: #{inspect vsn}"

  def format_status(%Mix.Dep{status: { :nomatchvsn, vsn }, requirement: req}),
    do: "the dependency does not match the requirement #{inspect req}, got #{inspect vsn}"

  def format_status(%Mix.Dep{status: { :lockmismatch, _ }}),
    do: "lock mismatch: the dependency is out of date"

  def format_status(%Mix.Dep{status: :lockoutdated}),
    do: "lock outdated: the lock is outdated compared to the options in your mixfile"

  def format_status(%Mix.Dep{status: :nolock}),
    do: "the dependency is not locked"

  def format_status(%Mix.Dep{status: :compile}),
    do: "the dependency build is outdated, please run `#{mix_env_var}mix deps.compile`"

  def format_status(%Mix.Dep{app: app, status: { :divergedreq, other }} = dep) do
    "the dependency #{app} defined\n" <>
    "#{dep_status(dep)}" <>
    "\n  does not match the requirement specified\n" <>
    "#{dep_status(other)}" <>
    "\n  Ensure they match or specify one of the above in your #{inspect Mix.Project.get} deps and set `override: true`"
  end

  def format_status(%Mix.Dep{app: app, status: { :diverged, other }} = dep) do
    "different specs were given for the #{app} app:\n" <>
    "#{dep_status(dep)}#{dep_status(other)}" <>
    "\n  Ensure they match or specify one of the above in your #{inspect Mix.Project.get} deps and set `override: true`"
  end

  def format_status(%Mix.Dep{app: app, status: { :overridden, other }} = dep) do
    "the dependency #{app} in #{Path.relative_to_cwd(dep.from)} is overriding a child dependency:\n" <>
    "#{dep_status(dep)}#{dep_status(other)}" <>
    "\n  Ensure they match or specify one of the above in your #{inspect Mix.Project.get} deps and set `override: true`"
  end

  def format_status(%Mix.Dep{status: { :unavailable, _ }, scm: scm}) do
    if scm.fetchable? do
      "the dependency is not available, run `mix deps.get`"
    else
      "the dependency is not available"
    end
  end

  def format_status(%Mix.Dep{status: { :elixirlock, _ }}),
    do: "the dependency is built with an out-of-date elixir version, run `#{mix_env_var}mix deps.compile`"

  defp dep_status(%Mix.Dep{app: app, requirement: req, opts: opts, from: from}) do
    info = { app, req, Dict.drop(opts, [:dest, :lock, :env, :build]) }
    "\n  > In #{Path.relative_to_cwd(from)}:\n    #{inspect info}\n"
  end

  @doc """
  Checks the lock for the given dependency and update its status accordingly.
  """
  def check_lock(%Mix.Dep{scm: scm, app: app, opts: opts} = dep, lock) do
    if rev = lock[app] do
      opts = Keyword.put(opts, :lock, rev)
    end

    if available?(dep) do
      case scm.lock_status(opts) do
        :mismatch ->
          status = if rev, do: { :lockmismatch, rev }, else: :nolock
          %{dep | status: status, opts: opts}
        :outdated ->
          # Don't include the lock in the dependency if it is outdated
          %{dep | status: :lockoutdated}
        :ok ->
          if vsn = old_elixir_lock(dep) do
            %{dep | status: { :elixirlock, vsn }, opts: opts}
          else
            %{dep | opts: opts}
          end
      end
    else
      %{dep | opts: opts}
    end
  end

  @doc """
  Returns true if the dependency is ok.
  """
  def ok?(%Mix.Dep{status: { :ok, _ }}), do: true
  def ok?(%Mix.Dep{}), do: false

  @doc """
  Checks if a dependency is available. Available dependencies
  are the ones that can be loaded.
  """
  def available?(%Mix.Dep{status: { :overridden, _ }}),   do: false
  def available?(%Mix.Dep{status: { :diverged, _ }}),     do: false
  def available?(%Mix.Dep{status: { :divergedreq, _ }}),  do: false
  def available?(%Mix.Dep{status: { :unavailable, _ }}),  do: false
  def available?(%Mix.Dep{}), do: true

  @doc """
  Formats a dependency for printing.
  """
  def format_dep(%Mix.Dep{scm: scm, app: app, status: status, opts: opts}) do
    version =
      case status do
        { :ok, vsn } when vsn != nil -> "#{vsn} "
        _ -> ""
      end

    "#{app} #{version}(#{scm.format(opts)})"
  end

  @doc """
  Returns all load paths for the given dependency. Automatically
  derived from source paths.
  """
  def load_paths(%Mix.Dep{opts: opts} = dep) do
    build_path = Path.dirname(opts[:build])
    Enum.map source_paths(dep), fn path ->
      Path.join [build_path, Path.basename(path), "ebin"]
    end
  end

  @doc """
  Returns all source paths.

  Source paths are the directories that contains ebin files for a given
  dependency. All managers, except rebar, have only one source path.
  """
  def source_paths(%Mix.Dep{manager: :rebar, opts: opts, extra: extra}) do
    # Add root dir and all sub dirs with ebin/ directory
    sub_dirs = Enum.map(extra[:sub_dirs] || [], fn path ->
      Path.join(opts[:dest], path)
    end)

    [opts[:dest] | sub_dirs]
    |> Enum.map(&Path.wildcard(&1))
    |> Enum.concat
    |> Enum.filter(fn p -> p |> Path.join("ebin") |> File.dir? end)
  end

  def source_paths(%Mix.Dep{opts: opts}) do
    [opts[:dest]]
  end

  @doc """
  Return `true` if dependency is a mix project.
  """
  def mix?(%Mix.Dep{manager: manager}) do
    manager == :mix
  end

  @doc """
  Return `true` if dependency is a rebar project.
  """
  def rebar?(%Mix.Dep{manager: manager}) do
    manager == :rebar
  end

  @doc """
  Return `true` if dependency is a make project.
  """
  def make?(%Mix.Dep{manager: manager}) do
    manager == :make
  end

  ## Helpers

  defp mix_env_var do
    if Mix.env == :dev do
      ""
    else
      "MIX_ENV=#{Mix.env} "
    end
  end

  defp to_app_names(given) do
    Enum.map given, fn(app) ->
      if is_binary(app), do: binary_to_atom(app), else: app
    end
  end

  defp old_elixir_lock(%Mix.Dep{opts: opts}) do
    old_vsn = Mix.Dep.Lock.elixir_vsn(opts[:build])
    if old_vsn && old_vsn != System.version do
      old_vsn
    end
  end
end
