defmodule Mix.Dep do
  @moduledoc false

  @doc """
  The Mix.Dep struct keeps information about your project dependencies.

  It contains:

    * `scm` - a module representing the source code management tool (SCM)
      operations

    * `app` - the application name as an atom

    * `requirement` - a binary or regular expression with the dependency's requirement

    * `status` - the current status of the dependency, check
      `Mix.Dep.format_status/1` for more information

    * `opts` - the options given by the developer

    * `deps` - dependencies of this dependency

    * `top_level` - true if dependency was defined in the top-level project

    * `manager` - the project management, possible values:
      `:rebar` | `:rebar3` | `:mix` | `:make` | `nil`

    * `from` - path to the file where the dependency was defined

    * `extra` - a slot for adding extra configuration based on the manager;
      the information on this field is private to the manager and should not be
      relied on

    * `system_env` - an enumerable of key-value tuples of binaries to be set as environment variables
      when loading or compiling the dependency

  A dependency is in two specific states: loaded and unloaded.

  When a dependency is unloaded, it means Mix only parsed its specification
  and made no attempt to actually load the dependency or validate its
  status. When the dependency is loaded, it means Mix attempted to fetch,
  load and validate it, the status is set in the status field.

  Furthermore, in the `opts` fields, Mix keeps some internal options, which
  can be accessed by SCMs:

    * `:app`   - the application name
    * `:dest`  - the destination path for the dependency
    * `:lock`  - the lock information retrieved from mix.lock
    * `:build` - the build path for the dependency

  """
  defstruct scm: nil,
            app: nil,
            requirement: nil,
            status: nil,
            opts: [],
            deps: [],
            top_level: false,
            extra: [],
            manager: nil,
            from: nil,
            system_env: []

  @type t :: %__MODULE__{
          scm: module,
          app: atom,
          requirement: String.t() | Regex.t() | nil,
          status: atom,
          opts: keyword,
          top_level: boolean,
          manager: :rebar | :rebar3 | :mix | :make | nil,
          from: String.t(),
          extra: term,
          system_env: keyword
        }

  @doc """
  Returns loaded dependencies from the cache for the current environment.

  If dependencies have not been cached yet, they are loaded
  and then cached.

  Because the dependencies are cached during deps.loadpaths,
  their status may be outdated (for example, `:compile` did not
  yet become `:ok`). Therefore it is recommended to not rely
  on their status, also given they haven't been checked
  against the lock.
  """
  def cached() do
    if project = Mix.Project.get() do
      read_cached_deps(project, {Mix.env(), Mix.target()}) || load_and_cache()
    else
      load_and_cache()
    end
  end

  @doc """
  Returns loaded dependencies recursively and caches it.

  The result is cached for future `cached/0` calls.

  ## Exceptions

  This function raises an exception if any of the dependencies
  provided in the project are in the wrong format.
  """
  def load_and_cache() do
    env = Mix.env()
    target = Mix.target()

    case Mix.ProjectStack.top_and_bottom() do
      {%{name: top, config: config}, %{name: bottom}} ->
        write_cached_deps(top, {env, target}, load_and_cache(config, top, bottom, env, target))

      _ ->
        converge(env: env, target: target)
    end
  end

  defp load_and_cache(_config, top, top, env, target) do
    converge(env: env, target: target)
  end

  defp load_and_cache(config, _top, bottom, _env, _target) do
    {_, deps} = Mix.ProjectStack.read_cache({:cached_deps, bottom})
    app = Keyword.fetch!(config, :app)
    seen = populate_seen(MapSet.new(), [app])
    children = get_deps(deps, tl(Enum.uniq(get_children(deps, seen, [app]))))

    top_level =
      for dep <- deps,
          dep.app == app,
          child <- dep.deps,
          do: {child.app, Keyword.get(child.opts, :optional, false)},
          into: %{}

    Enum.map(children, fn %{app: app, opts: opts} = dep ->
      # optional only matters at the top level. Any non-top level dependency
      # that is optional and is still available means it has been fulfilled.
      case top_level do
        %{^app => optional} ->
          %{dep | top_level: true, opts: Keyword.put(opts, :optional, optional)}

        %{} ->
          %{dep | top_level: false, opts: Keyword.delete(opts, :optional)}
      end
    end)
  end

  defp read_cached_deps(project, env_target) do
    case Mix.ProjectStack.read_cache({:cached_deps, project}) do
      {^env_target, deps} -> deps
      _ -> nil
    end
  end

  defp write_cached_deps(project, env_target, deps) do
    Mix.ProjectStack.write_cache({:cached_deps, project}, {env_target, deps})
    deps
  end

  @doc """
  Clears loaded dependencies from the cache for the current environment.
  """
  def clear_cached() do
    if project = Mix.Project.get() do
      key = {:cached_deps, project}
      Mix.ProjectStack.delete_cache(key)
    end
  end

  @doc """
  Returns loaded dependencies recursively on the given environment.

  If no environment is passed, dependencies are loaded across all
  environments. The result is not cached.

  ## Exceptions

  This function raises an exception if any of the dependencies
  provided in the project are in the wrong format.
  """
  def load_on_environment(opts) do
    converge(opts)
  end

  defp converge(opts) do
    Mix.Dep.Converger.converge(nil, nil, opts, &{&1, &2, &3}) |> elem(0)
  end

  @doc """
  Filters the given dependencies by name.

  Raises if any of the names are missing.
  """
  def filter_by_name(given, all_deps, opts \\ []) do
    # Ensure all apps are atoms
    apps = to_app_names(given)

    deps =
      if opts[:include_children] do
        seen = populate_seen(MapSet.new(), apps)
        get_deps(all_deps, Enum.uniq(get_children(all_deps, seen, apps)))
      else
        get_deps(all_deps, apps)
      end

    Enum.each(apps, fn app ->
      unless Enum.any?(all_deps, &(&1.app == app)) do
        Mix.raise("Unknown dependency #{app} for environment #{Mix.env()}")
      end
    end)

    deps
  end

  defp get_deps(all_deps, apps) do
    Enum.filter(all_deps, &(&1.app in apps))
  end

  defp get_children(_all_deps, _seen, []), do: []

  defp get_children(all_deps, seen, apps) do
    children_apps =
      for %{deps: children} <- get_deps(all_deps, apps),
          %{app: app} <- children,
          app not in seen,
          do: app

    apps ++ get_children(all_deps, populate_seen(seen, children_apps), children_apps)
  end

  defp populate_seen(seen, apps) do
    Enum.reduce(apps, seen, &MapSet.put(&2, &1))
  end

  @doc """
  Runs the given `fun` inside the given dependency project by
  changing the current working directory and loading the given
  project onto the project stack.

  It expects a loaded dependency as argument.
  """
  def in_dependency(dep, post_config \\ [], fun)

  def in_dependency(%Mix.Dep{app: app, opts: opts, scm: scm}, config, fun) do
    # Set the app_path to be the one stored in the dependency.
    # This is important because the name of application in the
    # mix.exs file can be different than the actual name and we
    # choose to respect the one in the mix.exs
    config =
      Keyword.merge(Mix.Project.deps_config(), config)
      |> Keyword.put(:app_path, opts[:build])
      |> Keyword.put(:build_scm, scm)

    env = opts[:env] || :prod
    old_env = Mix.env()

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
  def format_status(%Mix.Dep{status: {:ok, _vsn}}) do
    "ok"
  end

  def format_status(%Mix.Dep{status: {:noappfile, path}}) do
    "could not find an app file at #{inspect(Path.relative_to_cwd(path))}. " <>
      "This may happen if the dependency was not yet compiled, " <>
      "or you specified the wrong application name in your deps, " <>
      "or the dependency indeed has no app file (then you can pass app: false as option)"
  end

  def format_status(%Mix.Dep{status: {:invalidapp, path}}) do
    "the app file at #{inspect(Path.relative_to_cwd(path))} is invalid"
  end

  def format_status(%Mix.Dep{status: {:invalidvsn, vsn}}) do
    "the app file contains an invalid version: #{inspect(vsn)}"
  end

  def format_status(%Mix.Dep{status: {:nosemver, vsn}, requirement: req}) do
    "the app file specified a non-Semantic Versioning format: #{inspect(vsn)}. Mix can only match the " <>
      "requirement #{inspect(req)} against semantic versions. Please fix the application version " <>
      "or use a regular expression as a requirement to match against any version"
  end

  def format_status(%Mix.Dep{status: {:nomatchvsn, vsn}, requirement: req}) do
    "the dependency does not match the requirement #{inspect(req)}, got #{inspect(vsn)}"
  end

  def format_status(%Mix.Dep{status: {:lockmismatch, _}}) do
    "lock mismatch: the dependency is out of date. To fetch locked version run \"mix deps.get\""
  end

  def format_status(%Mix.Dep{status: :lockoutdated}) do
    "lock outdated: the lock is outdated compared to the options in your mix.exs. To fetch " <>
      "locked version run \"mix deps.get\""
  end

  def format_status(%Mix.Dep{status: :nolock}) do
    "the dependency is not locked. To generate the \"mix.lock\" file run \"mix deps.get\""
  end

  def format_status(%Mix.Dep{status: :compile}) do
    "the dependency build is outdated, please run \"#{mix_env_var()}mix deps.compile\""
  end

  def format_status(%Mix.Dep{app: app, status: {:divergedreq, vsn, other}} = dep) do
    "the dependency #{app} #{vsn}\n" <>
      dep_status(dep) <>
      "\n  does not match the requirement specified\n" <>
      dep_status(other) <>
      "\n  Ensure they match or specify one of the above in your deps and set \"override: true\""
  end

  def format_status(%Mix.Dep{app: app, status: {:divergedonly, other}} = dep) do
    recommendation =
      if Keyword.has_key?(other.opts, :only) do
        "Ensure you specify at least the same environments in :only in your dep"
      else
        "Remove the :only restriction from your dep"
      end

    "the :only option for dependency #{app}\n" <>
      dep_status(dep) <>
      "\n  does not match the :only option calculated for\n" <>
      dep_status(other) <> "\n  #{recommendation}"
  end

  def format_status(%Mix.Dep{app: app, status: {:divergedtargets, other}} = dep) do
    recommendation =
      if Keyword.has_key?(other.opts, :targets) do
        "Ensure you specify at least the same targets in :targets in your dep"
      else
        "Remove the :targets restriction from your dep"
      end

    "the :targets option for dependency #{app}\n" <>
      dep_status(dep) <>
      "\n  does not match the :targets option calculated for\n" <>
      dep_status(other) <> "\n  #{recommendation}"
  end

  def format_status(%Mix.Dep{app: app, status: {:diverged, other}} = dep) do
    "different specs were given for the #{app} app:\n" <>
      "#{dep_status(dep)}#{dep_status(other)}" <>
      "\n  Ensure they match or specify one of the above in your deps and set \"override: true\""
  end

  def format_status(%Mix.Dep{app: app, status: {:overridden, other}} = dep) do
    "the dependency #{app} in #{Path.relative_to_cwd(dep.from)} is overriding a child dependency:\n" <>
      "#{dep_status(dep)}#{dep_status(other)}" <>
      "\n  Ensure they match or specify one of the above in your deps and set \"override: true\""
  end

  def format_status(%Mix.Dep{status: {:unavailable, _}, scm: scm}) do
    if scm.fetchable?() do
      "the dependency is not available, run \"mix deps.get\""
    else
      "the dependency is not available"
    end
  end

  def format_status(%Mix.Dep{status: {:elixirlock, _}}) do
    "the dependency was built with an out-of-date Elixir version, run \"#{mix_env_var()}mix deps.compile\""
  end

  def format_status(%Mix.Dep{status: {:scmlock, _}}) do
    "the dependency was built with another SCM, run \"#{mix_env_var()}mix deps.compile\""
  end

  defp dep_status(%Mix.Dep{} = dep) do
    %{
      app: app,
      requirement: req,
      manager: manager,
      opts: opts,
      from: from,
      system_env: system_env
    } = dep

    opts =
      []
      |> Kernel.++(if manager, do: [manager: manager], else: [])
      |> Kernel.++(if system_env != [], do: [system_env: system_env], else: [])
      |> Kernel.++(opts)
      |> Keyword.drop([:dest, :build, :lock, :manager, :checkout])

    info = if req, do: {app, req, opts}, else: {app, opts}
    "\n  > In #{Path.relative_to_cwd(from)}:\n    #{inspect(info)}\n"
  end

  @doc """
  Checks the lock for the given dependency and update its status accordingly.
  """
  def check_lock(%Mix.Dep{scm: scm, opts: opts} = dep) do
    if available?(dep) do
      case scm.lock_status(opts) do
        :mismatch ->
          status = if rev = opts[:lock], do: {:lockmismatch, rev}, else: :nolock
          %{dep | status: status}

        :outdated ->
          # Don't include the lock in the dependency if it is outdated
          %{dep | status: :lockoutdated}

        :ok ->
          check_manifest(dep, opts[:build])
      end
    else
      dep
    end
  end

  defp check_manifest(%{scm: scm} = dep, build_path) do
    vsn = {System.version(), :erlang.system_info(:otp_release)}

    case Mix.Dep.ElixirSCM.read(Path.join(build_path, ".mix")) do
      {:ok, old_vsn, _} when old_vsn != vsn ->
        %{dep | status: {:elixirlock, old_vsn}}

      {:ok, _, old_scm} when old_scm != scm ->
        %{dep | status: {:scmlock, old_scm}}

      _ ->
        dep
    end
  end

  @doc """
  Returns `true` if the dependency is ok.
  """
  def ok?(%Mix.Dep{status: {:ok, _}}), do: true
  def ok?(%Mix.Dep{}), do: false

  @doc """
  Checks if a dependency is available.

  Available dependencies are the ones that can be loaded.
  """
  def available?(%Mix.Dep{status: {:unavailable, _}}), do: false
  def available?(dep), do: not diverged?(dep)

  @doc """
  Checks if a dependency has diverged.
  """
  def diverged?(%Mix.Dep{status: {:overridden, _}}), do: true
  def diverged?(%Mix.Dep{status: {:diverged, _}}), do: true
  def diverged?(%Mix.Dep{status: {:divergedreq, _}}), do: true
  def diverged?(%Mix.Dep{status: {:divergedonly, _}}), do: true
  def diverged?(%Mix.Dep{status: {:divergedtargets, _}}), do: true
  def diverged?(%Mix.Dep{}), do: false

  @doc """
  Returns `true` if the dependency is compilable.
  """
  def compilable?(%Mix.Dep{status: {:elixirlock, _}}), do: true
  def compilable?(%Mix.Dep{status: {:noappfile, _}}), do: true
  def compilable?(%Mix.Dep{status: {:scmlock, _}}), do: true
  def compilable?(%Mix.Dep{status: :compile}), do: true
  def compilable?(_), do: false

  @doc """
  Formats a dependency for printing.
  """
  def format_dep(%Mix.Dep{scm: scm, app: app, status: status, opts: opts}) do
    version =
      case status do
        {:ok, vsn} when vsn != nil -> "#{vsn} "
        _ -> ""
      end

    "#{app} #{version}(#{scm.format(opts)})"
  end

  @doc """
  Returns all load paths for the given dependency.

  Automatically derived from source paths.
  """
  def load_paths(%Mix.Dep{opts: opts} = dep) do
    build_path = Path.dirname(opts[:build])

    Enum.map(source_paths(dep), fn {_, base} ->
      Path.join([build_path, base, "ebin"])
    end)
  end

  @doc """
  Returns all source paths.

  Source paths are the directories that contain ebin files for a given
  dependency. All managers, except `:rebar`, have only one source path.
  """
  def source_paths(%Mix.Dep{manager: :rebar, app: app, opts: opts, extra: extra}) do
    sub_dirs = extra[:sub_dirs] || []
    dest = opts[:dest]

    # Add root dir and all sub dirs with ebin/ directory
    in_sub_dirs =
      for sub_dir <- sub_dirs,
          path <- Path.wildcard(Path.join(dest, sub_dir)),
          File.dir?(Path.join(path, "ebin")),
          do: {path, Path.basename(path)}

    [{opts[:dest], Atom.to_string(app)}] ++ in_sub_dirs
  end

  def source_paths(%Mix.Dep{app: app, opts: opts}) do
    [{opts[:dest], Atom.to_string(app)}]
  end

  @doc """
  Returns `true` if dependency is a Mix project.
  """
  def mix?(%Mix.Dep{manager: manager}) do
    manager == :mix
  end

  @doc """
  Returns `true` if dependency is a Rebar project.
  """
  def rebar?(%Mix.Dep{manager: manager}) do
    manager in [:rebar, :rebar3]
  end

  @doc """
  Returns `true` if dependency is a Make project.
  """
  def make?(%Mix.Dep{manager: manager}) do
    manager == :make
  end

  ## Helpers

  defp mix_env_var do
    if Mix.env() == :dev do
      ""
    else
      "MIX_ENV=#{Mix.env()} "
    end
  end

  defp to_app_names(given) do
    Enum.map(given, fn app ->
      if is_binary(app), do: String.to_atom(app), else: app
    end)
  end
end
