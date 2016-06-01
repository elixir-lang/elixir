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
    mix_children([]) ++ Mix.Dep.Umbrella.unloaded
  end

  @doc """
  Partitions loaded dependencies by environment.
  """
  def partition_by_env(deps, nil), do: {deps, []}
  def partition_by_env(deps, env), do: Enum.partition(deps, &not skip?(&1, env))

  @doc """
  Checks if a dependency must be skipped according to the environment.
  """
  def skip?(_dep, nil), do: false
  def skip?(%Mix.Dep{status: {:divergedonly, _}}, _), do: false
  def skip?(%Mix.Dep{opts: opts}, env) do
    only = opts[:only]
    validate_only!(only)
    only != nil and not env in List.wrap(only)
  end

  @doc """
  Loads the given dependency information, including its
  latest status and children.
  """
  def load(%Mix.Dep{manager: manager, scm: scm, opts: opts} = dep, children) do
    manager = manager ||
              scm_manager(scm, opts) ||
              infer_manager(opts[:dest])

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

    %{validate_app(dep) | deps: attach_only(children, opts)}
  end

  @doc """
  Checks if a requirement from a dependency matches
  the given version.
  """
  def vsn_match(nil, _actual, _app),
    do: {:ok, true}
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
              Mix.raise "Invalid requirement #{req} for app #{app}"
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

  defp with_scm_and_app({app, req, opts} = original) when is_atom(app) and is_list(opts)  do
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

    dest  = Path.join(Mix.Project.deps_path, bin_app)
    build = Path.join([Mix.Project.build_path, "lib", bin_app])
    opts  = opts
            |> Keyword.put(:dest, dest)
            |> Keyword.put(:build, build)

    {scm, opts} = get_scm(app, opts)

    {scm, opts} =
      if !scm && Mix.Hex.ensure_installed?(app) do
        _ = Mix.Hex.start()
        get_scm(app, opts)
      else
        {scm, opts}
      end

    unless scm do
      Mix.raise "Could not find an SCM for dependency #{inspect app} from #{inspect Mix.Project.get}"
    end

    %Mix.Dep{
      scm: scm,
      app: app,
      requirement: req,
      status: scm_status(scm, opts),
      opts: Keyword.put_new(opts, :env, :prod)}
  end

  defp get_scm(app, opts) do
    Enum.find_value Mix.SCM.available, {nil, opts}, fn scm ->
      (new = scm.accepts_options(app, opts)) && {scm, new}
    end
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
      any_of?(dest, ["rebar", "rebar.config", "rebar.config.script"]) ->
        :rebar
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
    Mix.raise """
    Dependency specified in the wrong format:

        #{inspect dep}

    Expected:

        {app, opts} | {app, requirement} | {app, requirement, opts}

    Where:

        app :: atom
        requirement :: String.t | Regex.t
        opts :: Keyword.t

    If you want to skip the requirement (not recommended), use ">= 0.0.0".
    """
  end

  ## Fetching

  # We need to override the dependencies so they mirror
  # the :only requirement in the parent.
  defp attach_only(deps, opts) do
    if only = opts[:only] do
      Enum.map(deps, fn %{opts: opts} = dep ->
        %{dep | opts: Keyword.put_new(opts, :only, only)}
      end)
    else
      deps
    end
  end

  defp mix_dep(%Mix.Dep{opts: opts} = dep, nil) do
    Mix.Dep.in_dependency(dep, fn _ ->
      opts =
        if Mix.Project.umbrella? do
          Keyword.put_new(opts, :app, false)
        else
          opts
        end

      child_opts =
        if opts[:from_umbrella] do
          []
        else
          [env: Keyword.fetch!(opts, :env)]
        end

      deps = mix_children(child_opts) ++ Mix.Dep.Umbrella.unloaded
      {%{dep | opts: opts}, deps}
    end)
  end

  # If we have a Mix dependency that came from a remote converger,
  # we just use the dependencies given by the remote converger,
  # we don't need to load the mixfile at all. We can only do this
  # because umbrella projects are not supported in remotes.
  defp mix_dep(%Mix.Dep{opts: opts} = dep, children) do
    from = Path.join(opts[:dest], "mix.exs")
    deps = Enum.map(children, &to_dep(&1, from))
    {dep, deps}
  end

  defp rebar_dep(%Mix.Dep{app: app} = dep, children, manager) do
    Mix.Dep.in_dependency(dep, fn _ ->
      config = Mix.Rebar.load_config(".")
      extra = Mix.Rebar.merge_config(dep.extra, config)
      deps =
        if children do
          from = Path.absname("rebar.config")
          # Pass the manager because deps of a Rebar project need
          # to default to Rebar if we cannot chose a manager from
          # files in the dependency
          Enum.map(children, &to_dep(&1, from, manager))
        else
          rebar_children(app, config, extra, manager)
        end
      {%{dep | extra: extra}, deps}
    end)
  end

  defp make_dep(dep) do
    {dep, []}
  end

  defp validate_only!(only) do
    for entry <- List.wrap(only), not is_atom(entry) do
      Mix.raise "Expected :only in dependency to be an atom or a list of atoms, got: #{inspect only}"
    end
    only
  end

  defp mix_children(opts) do
    from = Path.absname("mix.exs")
    (Mix.Project.config[:deps] || [])
    |> Enum.map(&to_dep(&1, from))
    |> partition_by_env(opts[:env])
    |> elem(0)
  end

  defp rebar_children(app, root_config, extra, manager) do
    from = Path.absname("rebar.config")
    Mix.Rebar.recur(root_config, fn config ->
      app
      |> Mix.Rebar.deps(config, overrides(manager, extra))
      |> Enum.map(fn dep -> %{to_dep(dep, from, manager) | extra: extra} end)
    end) |> Enum.concat
  end

  defp overrides(:rebar3, extra), do: extra[:overrides] || []
  defp overrides(_, _extra), do: []

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
      Mix.Utils.stale?([Path.join(opts[:dest], ".fetch")],
                       [Path.join(opts[:build], ".compile.fetch")])
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
      {:ok, _} -> {:invalidapp, app_path}
      {:error, _} -> {:noappfile, app_path}
    end
  end
end
