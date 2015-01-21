# This module is responsible for loading dependencies
# of the current project. This module and its functions
# are private to Mix.
defmodule Mix.Dep.Loader do
  @moduledoc false

  @doc """
  Gets all direct children of the current `Mix.Project`
  as a `Mix.Dep` struct. Umbrella project dependencies
  are included as children.

  By default, it will filter all dependencies that does not match
  current environment, behaviour can be overridden via options.

  ## Options

    * `:env` - filter dependencies on given environments
  """
  def children(opts) do
    mix_children(opts) ++ Mix.Dep.Umbrella.unloaded
  end

  @doc """
  Loads the given dependency information, including its
  latest status and children.
  """
  def load(dep, children) do
    %Mix.Dep{manager: manager, scm: scm, opts: opts} = dep
    dep  = %{dep | status: scm_status(scm, opts)}
    dest = opts[:dest]

    {dep, children} =
      cond do
        not ok?(dep.status) ->
          {dep, []}

        manager == :rebar ->
          rebar_dep(dep, children)

        mix?(dest) ->
          mix_dep(dep, children)

        rebar?(dest) ->
          rebar_dep(dep, children)

        make?(dest) ->
          make_dep(dep)

        true ->
          {dep, []}
      end

    %{validate_path(validate_app(dep)) | deps: children}
  end

  @doc """
  Checks if a requirement from a dependency matches
  the given version.
  """
  def vsn_match?(nil, _actual, _app),
    do: true
  def vsn_match?(req, actual, app) do
    if Regex.regex?(req) do
      actual =~ req
    else
      case Version.parse(actual) do
        {:ok, version} ->
          case Version.parse_requirement(req) do
            {:ok, req} ->
              Version.match?(version, req)
            :error ->
              Mix.raise "Invalid requirement #{req} for app #{app}"
          end

        :error ->
          Mix.raise "The application #{app} specified a non Semantic Version #{actual}. " <>
            "Mix can only match the requirement #{req} against Semantic Versions, to match against any " <>
            "version, please use a regex as requirement"
      end
    end
  end

  ## Helpers

  def to_dep(tuple, from, manager \\ nil) do
    %{with_scm_and_app(tuple) | from: from, manager: manager}
  end

  defp with_scm_and_app({app, opts}) when is_list(opts) do
    with_scm_and_app({app, nil, opts})
  end

  defp with_scm_and_app({app, req}) do
    with_scm_and_app({app, req, []})
  end

  defp with_scm_and_app({app, req, opts} = other) when is_atom(app) and is_list(opts) do
    unless is_binary(req) or Regex.regex?(req) or is_nil(req) do
      invalid_dep_format(other)
    end

    unless Keyword.keyword?(opts) do
      invalid_dep_format(other)
    end

    bin_app = Atom.to_string(app)

    dest  = Path.join(Mix.Project.deps_path, bin_app)
    build = Path.join([Mix.Project.build_path, "lib", bin_app])
    opts  = opts
            |> Keyword.put(:dest, dest)
            |> Keyword.put(:build, build)

    {scm, opts} = get_scm(app, opts)

    if !scm && Mix.Tasks.Local.Hex.ensure_installed?(app) do
      _ = Mix.Tasks.Local.Hex.start()
      {scm, opts} = get_scm(app, opts)
    end

    unless scm do
      Mix.raise "Could not find a SCM for dependency #{inspect app} from #{inspect Mix.Project.get}"
    end

    %Mix.Dep{
      scm: scm,
      app: app,
      requirement: req,
      status: scm_status(scm, opts),
      opts: opts}
  end

  defp with_scm_and_app(other) do
    invalid_dep_format(other)
  end

  defp get_scm(app, opts) do
    Enum.find_value Mix.SCM.available, {nil, opts}, fn(scm) ->
      (new = scm.accepts_options(app, opts)) && {scm, new}
    end
  end

  defp scm_status(scm, opts) do
    if scm.checked_out?(opts) do
      {:ok, nil}
    else
      {:unavailable, opts[:dest]}
    end
  end

  defp ok?({:ok, _}), do: true
  defp ok?(_), do: false

  defp mix?(dest) do
    any_of?(dest, ["mix.exs"])
  end

  defp rebar?(dest) do
    any_of?(dest, ["rebar", "rebar.config", "rebar.config.script"])
  end

  defp make?(dest) do
    any_of?(dest, ["Makefile", "Makefile.win"])
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

    """
  end

  ## Fetching

  defp mix_dep(%Mix.Dep{opts: opts} = dep, nil) do
    Mix.Dep.in_dependency(dep, fn _ ->
      if Mix.Project.umbrella? do
        opts = Keyword.put_new(opts, :app, false)
      end

      deps = mix_children(env: opts[:env] || :prod) ++ Mix.Dep.Umbrella.unloaded
      {%{dep | manager: :mix, opts: opts}, deps}
    end)
  end

  # If we have a Mix dependency that came from a remote converger,
  # we just use the dependencies given by the remote converger,
  # we don't need to load the mixfile at all. We can only do this
  # because umbrella projects are not supported in remotes.
  defp mix_dep(%Mix.Dep{opts: opts} = dep, children) do
    from = Path.join(opts[:dest], "mix.exs")
    deps = Enum.map(children, &to_dep(&1, from))
    {%{dep | manager: :mix}, deps}
  end

  defp rebar_dep(%Mix.Dep{} = dep, children) do
    Mix.Dep.in_dependency(dep, fn _ ->
      rebar = Mix.Rebar.load_config(".")
      extra = Dict.take(rebar, [:sub_dirs])
      deps  = if children do
        from = Path.absname("rebar.config")
        Enum.map(children, &to_dep(&1, from, :rebar))
      else
        rebar_children(rebar)
      end
      {%{dep | manager: :rebar, extra: extra}, deps}
    end)
  end

  defp make_dep(dep) do
    {%{dep | manager: :make}, []}
  end

  defp mix_children(opts) do
    from = Path.absname("mix.exs")
    deps = Enum.map(Mix.Project.config[:deps] || [], &to_dep(&1, from))

    # Filter deps not matching mix environment
    if env = opts[:env] do
      Enum.filter(deps, fn %Mix.Dep{opts: opts} ->
        only = opts[:only]
        if only, do: env in List.wrap(only), else: true
      end)
    else
      deps
    end
  end

  defp rebar_children(root_config) do
    from = Path.absname("rebar.config")
    Mix.Rebar.recur(root_config, fn config ->
      Mix.Rebar.deps(config) |> Enum.map(&to_dep(&1, from, :rebar))
    end) |> Enum.concat
  end

  defp validate_path(%Mix.Dep{scm: scm, manager: manager} = dep) do
    if scm == Mix.SCM.Path and not manager in [:mix, nil] do
      Mix.raise ":path option can only be used with mix projects, " <>
                                "invalid path dependency for #{inspect dep.app}"
    else
      dep
    end
  end

  defp validate_app(%Mix.Dep{opts: opts, requirement: req, app: app, status: status} = dep) do
    opts_app = opts[:app]

    cond do
      not ok?(status) ->
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
            if vsn_match?(req, actual, app) do
              {:ok, actual}
            else
              {:nomatchvsn, actual}
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
