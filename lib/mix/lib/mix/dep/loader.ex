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
  current environment, behaviour can be overriden via options.

  ## Options

  * `:env` - Filter dependencies on given environments
  """
  def children(opts) do
    from = Path.absname("mix.exs")
    deps = Enum.map(Mix.Project.config[:deps] || [], &to_dep(&1, from))

    # Filter deps not matching mix environment
    if env = opts[:env] do
      deps =
        Enum.filter(deps, fn %Mix.Dep{opts: opts} ->
          only = opts[:only]
          if only, do: env in List.wrap(only), else: true
        end)
    end

    deps ++ Mix.Dep.Umbrella.unloaded
  end

  @doc """
  Loads the given dependency information, including its
  latest status and children.
  """
  def load(dep) do
    %Mix.Dep{manager: manager, scm: scm, opts: opts} = dep
    dep  = %{dep | status: scm_status(scm, opts)}
    dest = opts[:dest]

    {dep, children} =
      cond do
        not ok?(dep.status) ->
          {dep, []}

        manager == :rebar ->
          rebar_dep(dep)

        mix?(dest) ->
          mix_dep(%{dep | manager: :mix})

        rebar?(dest) ->
          rebar_dep(%{dep | manager: :rebar})

        make?(dest) ->
          {%{dep | manager: :make}, []}

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
              raise Mix.Error, message: "Invalid requirement #{req} for app #{app}"
          end

        :error ->
          raise Mix.Error, message: "The application #{app} specified a non Semantic Version #{actual}. " <>
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
    unless is_binary(req) or Regex.regex?(req) or nil?(req) do
      invalid_dep_format(other)
    end

    bin_app = Atom.to_string(app)

    dest  = Path.join(Mix.Project.deps_path, bin_app)
    build = Path.join([Mix.Project.build_path, "lib", bin_app])
    opts  = opts
            |> Keyword.put(:dest, dest)
            |> Keyword.put(:build, build)

    {scm, opts} = get_scm(app, opts)

    unless scm do
      Mix.Tasks.Local.Hex.maybe_install(app)
      Mix.Tasks.Local.Hex.maybe_start()
      {scm, opts} = get_scm(app, opts)
    end

    unless scm do
      raise Mix.Error,
        message: "could not find a SCM for dependency #{inspect app} from #{inspect Mix.Project.get}"
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
    File.regular?(Path.join(dest, "mix.exs"))
  end

  defp rebar?(dest) do
    Enum.any?(["rebar.config", "rebar.config.script"], fn file ->
      File.regular?(Path.join(dest, file))
    end) or File.regular?(Path.join(dest, "rebar"))
  end

  defp make?(dest) do
    File.regular? Path.join(dest, "Makefile")
  end

  defp invalid_dep_format(dep) do
    raise Mix.Error, message: """
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

  defp mix_dep(%Mix.Dep{opts: opts} = dep) do
    Mix.Dep.in_dependency(dep, fn _ ->
      config    = Mix.Project.config
      umbrella? = Mix.Project.umbrella?

      if umbrella? do
        opts = Keyword.put_new(opts, :app, false)
      end

      if req = old_elixir_req(config) do
        Mix.shell.error "warning: the dependency #{dep.app} requires Elixir #{inspect req} " <>
                        "but you are running on v#{System.version}"
      end

      dep = %{dep | manager: :mix, opts: opts, extra: [umbrella: umbrella?]}
      {dep, children(env: opts[:env] || :prod)}
    end)
  end

  defp rebar_dep(%Mix.Dep{} = dep) do
    Mix.Dep.in_dependency(dep, fn _ ->
      rebar = Mix.Rebar.load_config(".")
      extra = Dict.take(rebar, [:sub_dirs])
      dep   = %{dep | manager: :rebar, extra: extra}
      {dep, rebar_children(rebar)}
    end)
  end

  defp rebar_children(root_config) do
    from = Path.absname("rebar.config")
    Mix.Rebar.recur(root_config, fn config ->
      Mix.Rebar.deps(config) |> Enum.map(&to_dep(&1, from, :rebar))
    end) |> Enum.concat
  end

  defp validate_path(%Mix.Dep{scm: scm, manager: manager} = dep) do
    if scm == Mix.SCM.Path and not manager in [:mix, nil] do
      raise Mix.Error, message: ":path option can only be used with mix projects, " <>
                                "invalid path dependency for #{inspect dep.app}"
    else
      dep
    end
  end

  defp validate_app(%Mix.Dep{opts: opts, requirement: req, app: app, status: status} = dep) do
    opts_app = opts[:app]
    build    = opts[:build]

    cond do
      not ok?(status) ->
        dep
      File.exists?(Path.join(opts[:build], ".compile")) ->
        %{dep | status: :compile}
      opts_app == false ->
        dep
      true ->
        path  = if is_binary(opts_app), do: opts_app, else: "ebin/#{app}.app"
        path  = Path.expand(path, build)
        %{dep | status: app_status(path, app, req)}
    end
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

  defp old_elixir_req(config) do
    req = config[:elixir]
    if req && not Version.match?(System.version, req) do
      req
    end
  end
end
