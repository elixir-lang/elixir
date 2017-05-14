defmodule Mix.Dep do
  @moduledoc false

  @doc """
  The Mix.Dep struct keeps information about your project dependencies.

  It contains:

    * `scm` - a module representing the source code management tool (SCM)
      operations

    * `app` - the application name as an atom

    * `requirement` - a binary or regex with the dependency's requirement

    * `status` - the current status of the dependency, check
      `Mix.Dep.format_status/1` for more info

    * `opts` - the options given by the developer

    * `deps` - dependencies of this dependency

    * `top_level` - true if dependency was defined in the top-level project

    * `manager` - the project management, possible values:
      `:rebar` | `:rebar3` | `:mix` | `:make` | `nil`

    * `from` - path to the file where the dependency was defined

    * `extra` - a slot for adding extra configuration based on the manager;
      the information on this field is private to the manager and should not be
      relied on

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
  defstruct scm: nil, app: nil, requirement: nil, status: nil, opts: [],
            deps: [], top_level: false, extra: [], manager: nil, from: nil

  @type t :: %__MODULE__{
               scm: module,
               app: atom,
               requirement: String.t | Regex.t | nil,
               status: atom,
               opts: Keyword.t,
               top_level: boolean,
               manager: :rebar | :rebar3 | :mix | :make | nil,
               from: String.t,
               extra: term}

  @doc """
  Returns loaded dependencies from the cache for the current environment.

  Because the dependencies are cached during deps.loadpaths,
  their status may be outdated (for example, `:compile` did not
  yet become `:ok`). Therefore it is recommended to not rely
  on their status, also given they haven't been checked
  against the lock.

  If MIX_NO_DEPS is set, we return an empty list of dependencies
  without loading them.
  """
  def cached do
    cond do
      System.get_env("MIX_NO_DEPS") in ~w(1 true) ->
        []
      project = Mix.Project.get ->
        key = {:cached_deps, Mix.env, project}
        Mix.ProjectStack.read_cache(key) ||
          Mix.ProjectStack.write_cache(key, loaded(env: Mix.env))
      true ->
        loaded(env: Mix.env)
    end
  end

  @doc """
  Returns loaded dependencies recursively as a `Mix.Dep` struct.

  ## Exceptions

  This function raises an exception if any of the dependencies
  provided in the project are in the wrong format.
  """
  def loaded(opts) do
    Mix.Dep.Converger.converge(nil, nil, opts, &{&1, &2, &3}) |> elem(0)
  end

  @doc """
  Receives a list of dependency names and returns loaded `Mix.Dep`s.
  Logs a message if the dependency could not be found.

  ## Exceptions

  This function raises an exception if any of the dependencies
  provided in the project are in the wrong format.
  """
  def loaded_by_name(given, all_deps \\ nil, opts) do
    all_deps = all_deps || loaded(opts)

    # Ensure all apps are atoms
    apps = to_app_names(given)
    deps =
      if opts[:include_children] do
        get_deps_with_children(all_deps, apps)
      else
        get_deps(all_deps, apps)
      end

    Enum.each apps, fn(app) ->
      unless Enum.any?(all_deps, &(&1.app == app)) do
        Mix.raise "Unknown dependency #{app} for environment #{Mix.env}"
      end
    end

    deps
  end

  defp get_deps(all_deps, apps) do
    Enum.filter(all_deps, &(&1.app in apps))
  end

  defp get_deps_with_children(all_deps, apps) do
    deps = get_children(all_deps, apps)
    apps = deps |> Enum.map(& &1.app) |> Enum.uniq
    get_deps(all_deps, apps)
  end

  defp get_children(_all_deps, []), do: []
  defp get_children(all_deps, apps) do
    # Current deps
    deps = get_deps(all_deps, apps)

    # Children apps
    apps = for %{deps: children} <- deps,
               %{app: app} <- children,
               do: app

    # Current deps + children deps
    deps ++ get_children(all_deps, apps)
  end

  @doc """
  Runs the given `fun` inside the given dependency project by
  changing the current working directory and loading the given
  project onto the project stack.

  It is expected a loaded dependency as argument.
  """
  def in_dependency(dep, post_config \\ [], fun)

  def in_dependency(%Mix.Dep{app: app, opts: opts, scm: scm}, config, fun) do
    # Set the app_path to be the one stored in the dependency.
    # This is important because the name of application in the
    # mix.exs file can be different than the actual name and we
    # choose to respect the one in the mix.exs
    config =
      Keyword.merge(Mix.Project.deps_config, config)
      |> Keyword.put(:app_path, opts[:build])
      |> Keyword.put(:build_scm, scm)

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
  def format_status(%Mix.Dep{status: {:ok, _vsn}}),
    do: "ok"

  def format_status(%Mix.Dep{status: {:noappfile, path}}),
    do: "could not find an app file at #{Path.relative_to_cwd(path)}. " <>
        "This may happen if the dependency was not yet compiled, " <>
        "or you specified the wrong application name in your deps, " <>
        "or the dependency indeed has no app file (then you can pass app: false as option)"

  def format_status(%Mix.Dep{status: {:invalidapp, path}}),
    do: "the app file at #{Path.relative_to_cwd(path)} is invalid"

  def format_status(%Mix.Dep{status: {:invalidvsn, vsn}}),
    do: "the app file contains an invalid version: #{inspect vsn}"

  def format_status(%Mix.Dep{status: {:nosemver, vsn}, requirement: req}),
    do: "the app file specified a non-Semantic Versioning format: #{inspect vsn}. Mix can only match the " <>
        "requirement #{inspect req} against semantic versions. Please fix the application version " <>
        "or use a regex as a requirement to match against any version"

  def format_status(%Mix.Dep{status: {:nomatchvsn, vsn}, requirement: req}),
    do: "the dependency does not match the requirement #{inspect req}, got #{inspect vsn}"

  def format_status(%Mix.Dep{status: {:lockmismatch, _}}),
    do: "lock mismatch: the dependency is out of date. To fetch locked version run \"mix deps.get\""

  def format_status(%Mix.Dep{status: :lockoutdated}),
    do: "lock outdated: the lock is outdated compared to the options in your mixfile. To fetch locked version run \"mix deps.get\""

  def format_status(%Mix.Dep{status: :nolock}),
    do: "the dependency is not locked. To generate the \"mix.lock\" file run \"mix deps.get\""

  def format_status(%Mix.Dep{status: :compile}),
    do: "the dependency build is outdated, please run \"#{mix_env_var()}mix deps.compile\""

  def format_status(%Mix.Dep{app: app, status: {:divergedreq, vsn, other}} = dep) do
    "the dependency #{app} #{vsn}\n" <>
    "#{dep_status(dep)}" <>
    "\n  does not match the requirement specified\n" <>
    "#{dep_status(other)}" <>
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
    "#{dep_status(dep)}" <>
    "\n  does not match the :only option calculated for\n" <>
    "#{dep_status(other)}" <>
    "\n  #{recommendation}"
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
    if scm.fetchable? do
      "the dependency is not available, run \"mix deps.get\""
    else
      "the dependency is not available"
    end
  end

  def format_status(%Mix.Dep{status: {:elixirlock, _}}),
    do: "the dependency was built with an out-of-date Elixir version, run \"#{mix_env_var()}mix deps.compile\""

  def format_status(%Mix.Dep{status: {:scmlock, _}}),
    do: "the dependency was built with another SCM, run \"#{mix_env_var()}mix deps.compile\""

  defp dep_status(%Mix.Dep{app: app, requirement: req, manager: manager, opts: opts, from: from}) do
    opts = Keyword.drop(opts, [:dest, :build, :lock, :manager, :checkout])
    opts = opts ++ (if manager, do: [manager: manager], else: [])
    info = if req, do: {app, req, opts}, else: {app, opts}
    "\n  > In #{Path.relative_to_cwd(from)}:\n    #{inspect info}\n"
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
    vsn = {System.version, :erlang.system_info(:otp_release)}

    case Mix.Dep.ElixirSCM.read(build_path) do
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
  def diverged?(%Mix.Dep{status: {:overridden, _}}),   do: true
  def diverged?(%Mix.Dep{status: {:diverged, _}}),     do: true
  def diverged?(%Mix.Dep{status: {:divergedreq, _}}),  do: true
  def diverged?(%Mix.Dep{status: {:divergedonly, _}}), do: true
  def diverged?(%Mix.Dep{}), do: false

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
    Enum.map source_paths(dep), fn {_, base} ->
      Path.join [build_path, base, "ebin"]
    end
  end

  @doc """
  Returns all source paths.

  Source paths are the directories that contains ebin files for a given
  dependency. All managers, except `:rebar`, have only one source path.
  """
  def source_paths(%Mix.Dep{manager: :rebar, app: app, opts: opts, extra: extra}) do
    sub_dirs = extra[:sub_dirs] || []
    dest = opts[:dest]

    # Add root dir and all sub dirs with ebin/ directory
    [{opts[:dest], Atom.to_string(app)}] ++
      for(sub_dir <- sub_dirs,
          path <- Path.wildcard(Path.join(dest, sub_dir)),
          File.dir?(Path.join(path, "ebin")),
          do: {path, Path.basename(path)})
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
    if Mix.env == :dev do
      ""
    else
      "MIX_ENV=#{Mix.env} "
    end
  end

  defp to_app_names(given) do
    Enum.map given, fn(app) ->
      if is_binary(app), do: String.to_atom(app), else: app
    end
  end
end
