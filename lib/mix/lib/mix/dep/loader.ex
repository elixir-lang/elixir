# This module is responsible for loading dependencies
# of the current project. This module and its functions
# are private to Mix.
defmodule Mix.Dep.Loader do
  @moduledoc false

  import Mix.Dep, only: [ok?: 1, mix?: 1, rebar?: 1, make?: 1]

  @doc """
  Gets all direct children of the current `Mix.Project`
  as a `Mix.Dep` struct. Umbrella project dependencies
  are included as children.

  By default, it will filter all dependencies that does not match
  current environment, behaviour can be overridden via options.
  """
  def children() do
    mix_children([]) ++ Mix.Dep.Umbrella.unloaded()
  end

  @doc """
  Partitions loaded dependencies by environment.
  """
  def split_by_env_and_target(deps, {nil, nil}), do: {deps, []}

  def split_by_env_and_target(deps, env_target),
    do: Enum.split_with(deps, &(not skip?(&1, env_target)))

  @doc """
  Checks if a dependency must be skipped according to the environment.
  """
  def skip?(%Mix.Dep{status: {:divergedonly, _}}, _), do: false
  def skip?(%Mix.Dep{status: {:divergedtargets, _}}, _), do: false

  def skip?(%Mix.Dep{opts: opts}, {env, target}) do
    skip?(opts[:only], :only, env) or skip?(opts[:targets], :targets, target)
  end

  # The dependency is not filtered
  defp skip?(nil, _key, _value), do: false

  # The command is not filtered
  defp skip?(_maybe_list_of_atoms, _key, nil), do: false

  # The dependency is filtered as well as the command
  defp skip?(maybe_list_of_atoms, key, value) do
    wrapped = List.wrap(maybe_list_of_atoms)

    for entry <- wrapped, not is_atom(entry) do
      Mix.raise(
        "Expected #{inspect(key)} in dependency to be an atom or a list of atoms, " <>
          "got: #{inspect(maybe_list_of_atoms)}"
      )
    end

    value not in wrapped
  end

  def with_system_env(%Mix.Dep{system_env: []}, callback), do: callback.()

  def with_system_env(%Mix.Dep{system_env: new_env}, callback) do
    old_env =
      for {key, _} <- new_env do
        {key, System.get_env(key)}
      end

    try do
      System.put_env(new_env)
      callback.()
    after
      for {key, value} <- old_env do
        if value do
          System.put_env(key, value)
        else
          System.delete_env(key)
        end
      end
    end
  end

  @doc """
  Loads the given dependency information, including its
  latest status and children.
  """
  def load(%Mix.Dep{manager: manager, scm: scm, opts: opts} = dep, children) do
    manager = scm_manager(scm, opts) || manager || infer_manager(opts[:dest])
    dep = %{dep | manager: manager, status: scm_status(scm, opts)}

    {dep, children} =
      cond do
        not ok?(dep) ->
          {dep, []}

        mix?(dep) ->
          mix_dep(dep, children)

        # If not an explicit Rebar or Mix dependency
        # but came from Rebar, assume to be a Rebar dep.
        rebar?(dep) ->
          rebar_dep(dep, children, manager)

        make?(dep) ->
          make_dep(dep)

        true ->
          {dep, []}
      end

    %{validate_app(dep) | deps: attach_only_and_targets(children, opts)}
  end

  @doc """
  Checks if a requirement from a dependency matches
  the given version.
  """
  def vsn_match(nil, _actual, _app), do: {:ok, true}

  def vsn_match(req, actual, app) do
    if Regex.regex?(req) do
      {:ok, actual =~ req}
    else
      case Version.parse(actual) do
        {:ok, version} ->
          case Version.parse_requirement(req) do
            {:ok, req} ->
              {:ok, Version.match?(version, req)}

            :error ->
              Mix.raise("Invalid requirement #{req} for app #{app}")
          end

        :error ->
          {:error, :nosemver}
      end
    end
  end

  ## Helpers

  def to_dep(tuple, from, manager \\ nil) do
    %{opts: opts} = dep = with_scm_and_app(tuple)
    %{dep | from: from, manager: opts[:manager] || manager}
  end

  defp with_scm_and_app({app, opts} = original) when is_atom(app) and is_list(opts) do
    with_scm_and_app(app, nil, opts, original)
  end

  defp with_scm_and_app({app, req} = original) when is_atom(app) do
    if is_binary(req) or Regex.regex?(req) do
      with_scm_and_app(app, req, [], original)
    else
      invalid_dep_format(original)
    end
  end

  defp with_scm_and_app({app, req, opts} = original) when is_atom(app) and is_list(opts) do
    if is_binary(req) or Regex.regex?(req) do
      with_scm_and_app(app, req, opts, original)
    else
      invalid_dep_format(original)
    end
  end

  defp with_scm_and_app(original) do
    invalid_dep_format(original)
  end

  defp with_scm_and_app(app, req, opts, original) do
    unless Keyword.keyword?(opts) do
      invalid_dep_format(original)
    end

    bin_app = Atom.to_string(app)

    dest = Path.join(Mix.Project.deps_path(), bin_app)
    build = Path.join([Mix.Project.build_path(), "lib", bin_app])

    opts =
      opts
      |> Keyword.put(:dest, dest)
      |> Keyword.put(:build, build)

    {system_env, opts} = Keyword.pop(opts, :system_env, [])
    {scm, opts} = get_scm(app, opts)

    {scm, opts} =
      if !scm && Mix.Hex.ensure_installed?(app) do
        _ = Mix.Hex.start()
        get_scm(app, opts)
      else
        {scm, opts}
      end

    unless scm do
      Mix.raise(
        "Could not find an SCM for dependency #{inspect(app)} from #{inspect(Mix.Project.get())}"
      )
    end

    %Mix.Dep{
      scm: scm,
      app: app,
      requirement: req,
      status: scm_status(scm, opts),
      opts: Keyword.put_new(opts, :env, :prod),
      system_env: system_env
    }
  end

  defp get_scm(app, opts) do
    Enum.find_value(Mix.SCM.available(), {nil, opts}, fn scm ->
      (new = scm.accepts_options(app, opts)) && {scm, new}
    end)
  end

  # Notice we ignore Make dependencies because the
  # file based heuristic will always figure it out.
  @scm_managers ~w(mix rebar rebar3)a

  defp scm_manager(scm, opts) do
    managers = scm.managers(opts)
    Enum.find(@scm_managers, &(&1 in managers))
  end

  defp scm_status(scm, opts) do
    if scm.checked_out?(opts) do
      {:ok, nil}
    else
      {:unavailable, opts[:dest]}
    end
  end

  defp infer_manager(dest) do
    cond do
      any_of?(dest, ["mix.exs"]) ->
        :mix

      any_of?(dest, ["rebar", "rebar.config", "rebar.config.script", "rebar.lock"]) ->
        :rebar3

      any_of?(dest, ["Makefile", "Makefile.win"]) ->
        :make

      true ->
        nil
    end
  end

  defp any_of?(dest, files) do
    Enum.any?(files, &File.regular?(Path.join(dest, &1)))
  end

  defp invalid_dep_format(dep) do
    Mix.raise("""
    Dependency specified in the wrong format:

        #{inspect(dep)}

    Expected:

        {app, opts} | {app, requirement} | {app, requirement, opts}

    Where:

        app :: atom
        requirement :: String.t | Regex.t
        opts :: keyword

    If you want to skip the requirement (not recommended), use ">= 0.0.0".
    """)
  end

  ## Fetching

  # We need to override the dependencies so they mirror
  # the :only and :targets requirements from the parent.
  defp attach_only_and_targets(deps, opts) do
    case Keyword.take(opts, [:only, :targets]) do
      [] ->
        deps

      merge_opts ->
        Enum.map(deps, fn %{opts: opts} = dep ->
          %{dep | opts: Keyword.merge(merge_opts, opts)}
        end)
    end
  end

  defp mix_dep(%Mix.Dep{app: app, opts: opts} = dep, nil) do
    Mix.Dep.in_dependency(dep, fn _ ->
      opts =
        if Mix.Project.umbrella?() do
          Keyword.put_new(opts, :app, false)
        else
          opts
        end

      child_opts =
        if opts[:from_umbrella] do
          config = Mix.Project.config()

          if config[:app] != app do
            Mix.raise(
              "Umbrella app loaded from #{Atom.to_string(app)} is named " <>
                "as #{inspect(config[:app])} in mix.exs, but different names aren't allowed. " <>
                "Please rename either app's directory name or app's name in mix.exs to be equal."
            )
          end

          []
        else
          [env: Keyword.fetch!(opts, :env)]
        end

      deps = mix_children(child_opts) ++ Mix.Dep.Umbrella.unloaded()
      {%{dep | opts: opts}, deps}
    end)
  end

  # If we have a Mix dependency that came from a remote converger,
  # we just use the dependencies given by the remote converger,
  # we don't need to load the mix.exs at all. We can only do this
  # because umbrella projects are not supported in remotes.
  defp mix_dep(%Mix.Dep{opts: opts} = dep, children) do
    from = Path.join(opts[:dest], "mix.exs")
    deps = Enum.map(children, &to_dep(&1, from))
    {dep, deps}
  end

  defp rebar_dep(%Mix.Dep{app: app, opts: opts, extra: overrides} = dep, children, manager) do
    config = File.cd!(opts[:dest], fn -> Mix.Rebar.load_config(".") end)

    config = Mix.Rebar.apply_overrides(app, config, overrides)

    deps =
      if children do
        from = Path.join(opts[:dest], "rebar.config")
        # Pass the manager because deps of a Rebar project need
        # to default to Rebar if we cannot chose a manager from
        # files in the dependency
        Enum.map(children, &to_dep(&1, from, manager))
      else
        rebar_children(config, manager, opts[:dest])
      end

    {%{dep | extra: config}, deps}
  end

  defp make_dep(dep) do
    {dep, []}
  end

  defp mix_children(opts) do
    from = Path.absname("mix.exs")

    (Mix.Project.config()[:deps] || [])
    |> Enum.map(&to_dep(&1, from))
    |> split_by_env_and_target({opts[:env], nil})
    |> elem(0)
  end

  defp rebar_children(root_config, manager, dest) do
    from = Path.absname(Path.join(dest, "rebar.config"))

    Mix.Rebar.recur(root_config, fn config ->
      overrides = overrides(manager, config)

      config
      |> Mix.Rebar.deps()
      |> Enum.map(fn dep -> %{to_dep(dep, from, manager) | extra: overrides} end)
    end)
    |> Enum.concat()
  end

  defp overrides(:rebar3, config), do: config[:overrides] || []
  defp overrides(_, _config), do: []

  defp validate_app(%Mix.Dep{opts: opts, requirement: req, app: app} = dep) do
    opts_app = opts[:app]

    cond do
      not ok?(dep) ->
        dep

      recently_fetched?(dep) ->
        %{dep | status: :compile}

      opts_app == false ->
        dep

      true ->
        path = if is_binary(opts_app), do: opts_app, else: "ebin/#{app}.app"
        path = Path.expand(path, opts[:build])
        %{dep | status: app_status(path, app, req)}
    end
  end

  defp recently_fetched?(%Mix.Dep{opts: opts, scm: scm}) do
    scm.fetchable? &&
      Mix.Utils.stale?(
        join_stale(opts, :dest, ".fetch"),
        join_stale(opts, :build, ".mix/compile.fetch")
      )
  end

  defp join_stale(opts, key, file) do
    [Path.join(opts[key], file)]
  end

  defp app_status(app_path, app, req) do
    case :file.consult(app_path) do
      {:ok, [{:application, ^app, config}]} ->
        case List.keyfind(config, :vsn, 0) do
          {:vsn, actual} when is_list(actual) ->
            actual = IO.iodata_to_binary(actual)

            case vsn_match(req, actual, app) do
              {:ok, true} -> {:ok, actual}
              {:ok, false} -> {:nomatchvsn, actual}
              {:error, error} -> {error, actual}
            end

          {:vsn, actual} ->
            {:invalidvsn, actual}

          nil ->
            {:invalidvsn, nil}
        end

      {:ok, _} ->
        {:invalidapp, app_path}

      {:error, _} ->
        case Path.wildcard(Path.join(Path.dirname(app_path), "*.app")) do
          [other_app_path] -> {:noappfile, {app_path, other_app_path}}
          _ -> {:noappfile, {app_path, nil}}
        end
    end
  end
end
