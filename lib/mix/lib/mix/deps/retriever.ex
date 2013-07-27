# This module is responsible for retrieving
# dependencies of a given project. This
# module and its functions are private to Mix.
defmodule Mix.Deps.Retriever do
  @moduledoc false

  @doc """
  Gets all direct children of the current Mix.Project
  as a `Mix.Dep` record.
  """
  def children() do
    scms = Mix.SCM.available
    from = current_source(:mix)
    Enum.map(Mix.project[:deps], update(&1, scms, from))
  end

  @doc """
  Gets all children of a given dependency using
  the base project configuration.
  """
  def children(dep, config) do
    cond do
      Mix.Deps.available?(dep) and mixfile?(dep) ->
        Mix.Deps.in_dependency(dep, config, fn _ ->
          mix_children(config)
        end)

      Mix.Deps.available?(dep) and rebarconfig?(dep) ->
        Mix.Deps.in_dependency(dep, config, fn _ ->
          rebar_children(".")
        end)

      true ->
        []
    end
  end

  @doc """
  Updates the status of a dependency.
  """
  def update(Mix.Dep[scm: scm, app: app, requirement: req, opts: opts,
                     manager: manager, from: from]) do
    update({ app, req, opts }, [scm], from, manager)
  end

  ## Helpers

  defp mix_children(config) do
    scms = Mix.SCM.available
    Mix.Project.recur(config, fn _ ->
      from = current_source(:mix)

      # The manager must be nil because mix supports mix,
      # rebar and make dependencies/managers.
      (Mix.project[:deps] || []) |> Enum.map(update(&1, scms, from))
    end) |> List.concat
  end

  defp rebar_children(dir) do
    scms = Mix.SCM.available
    Mix.Rebar.recur(dir, fn config ->
      from = current_source(:rebar)

      # Rebar dependencies are always managed by rebar.
      Mix.Rebar.deps(config) |> Enum.map(update(&1, scms, from, :rebar))
    end) |> List.concat
  end

  defp update(tuple, scms, from, manager // nil) do
    dep = with_scm_and_app(tuple, scms).from(from)

    if match?({ _, req, _ } when is_regex(req), tuple) and
        not String.ends_with?(from, "rebar.config") do
      invalid_dep_format(tuple)
    end

    if Mix.Deps.available?(dep) do
      validate_app(cond do
        # If the manager was already set to rebar, let's use it
        manager == :rebar ->
          rebar_dep(dep)

        mixfile?(dep) ->
          Mix.Deps.in_dependency(dep, fn project ->
            mix_dep(dep, project)
          end)

        rebarconfig?(dep) or rebarexec?(dep) ->
          rebar_dep(dep)

        makefile?(dep) ->
          make_dep(dep)

        true ->
          dep
      end)
    else
      dep
    end
  end

  defp current_source(manager) do
    case manager do
      :mix   -> "mix.exs"
      :rebar -> "rebar.config"
    end |> Path.absname
  end

  defp mix_dep(Mix.Dep[manager: nil, opts: opts, app: app] = dep, project) do
    opts =
      if Mix.Project.umbrella? do
        Keyword.put_new(opts, :app, false)
      else
        Keyword.put_new(opts, :app, Path.join(Mix.project[:compile_path], "#{app}.app"))
      end

    dep.manager(:mix).source(project).opts(opts)
  end

  defp mix_dep(dep, _project), do: dep

  defp rebar_dep(Mix.Dep[manager: nil, opts: opts] = dep) do
    config = Mix.Rebar.load_config(opts[:dest])
    dep.manager(:rebar).source(config)
  end

  defp rebar_dep(dep), do: dep

  defp make_dep(Mix.Dep[manager: nil] = dep) do
    dep.manager(:make)
  end

  defp make_dep(dep), do: dep

  defp with_scm_and_app({ app, opts }, scms) when is_atom(app) and is_list(opts) do
    with_scm_and_app({ app, nil, opts }, scms)
  end

  defp with_scm_and_app({ app, req, opts }, scms) when is_atom(app) and
      (is_binary(req) or is_regex(req) or req == nil) and is_list(opts) do

    path = Path.join(Mix.project[:deps_path], app)
    opts = Keyword.put(opts, :dest, path)

    { scm, opts } = Enum.find_value scms, fn(scm) ->
      (new = scm.accepts_options(app, opts)) && { scm, new }
    end

    if scm do
      Mix.Dep[
        scm: scm,
        app: app,
        requirement: req,
        status: scm_status(scm, opts),
        opts: opts
      ]
    else
      supported = Enum.join scms, ", "
      raise Mix.Error, message: "#{inspect Mix.Project.get} did not specify a supported scm, expected one of: " <> supported
    end
  end

  defp with_scm_and_app(other, _scms) do
    invalid_dep_format(other)
  end

  defp scm_status(scm, opts) do
    if scm.checked_out? opts do
      { :ok, nil }
    else
      { :unavailable, opts[:dest] }
    end
  end

  defp validate_app(Mix.Dep[opts: opts, requirement: req, app: app] = dep) do
    opts_app = opts[:app]

    if opts_app == false do
      dep
    else
      path = if is_binary(opts_app), do: opts_app, else: "ebin/#{app}.app"
      path = Path.expand(path, opts[:dest])
      dep.status app_status(path, app, req)
    end
  end

  defp app_status(app_path, app, req) do
    case :file.consult(app_path) do
      { :ok, [{ :application, ^app, config }] } ->
        case List.keyfind(config, :vsn, 0) do
          { :vsn, actual } when is_list(actual) ->
            actual = list_to_binary(actual)
            if vsn_match?(req, actual) do
              { :ok, actual }
            else
              { :nomatchvsn, actual }
            end
          { :vsn, actual } ->
            { :invalidvsn, actual }
          nil ->
            { :invalidvsn, nil }
        end
      { :ok, _ } -> { :invalidapp, app_path }
      { :error, _ } -> { :noappfile, app_path }
    end
  end

  defp vsn_match?(nil, _actual), do: true
  defp vsn_match?(req, actual) when is_regex(req),  do: actual =~ req
  defp vsn_match?(req, actual) when is_binary(req) do
    Mix.Version.match?(actual, req)
  end

  defp mixfile?(dep) do
    File.regular?(Path.join(dep.opts[:dest], "mix.exs"))
  end

  defp rebarexec?(dep) do
    File.regular?(Path.join(dep.opts[:dest], "rebar"))
  end

  defp rebarconfig?(dep) do
    Enum.any?(["rebar.config", "rebar.config.script"], fn file ->
      File.regular?(Path.join(dep.opts[:dest], file))
    end)
  end

  defp makefile?(dep) do
    File.regular? Path.join(dep.opts[:dest], "Makefile")
  end

  defp invalid_dep_format(dep) do
    raise Mix.Error, message: %b(Dependency specified in the wrong format: #{inspect dep}, ) <>
      %b(expected { app :: atom, opts :: Keyword.t } | { app :: atom, requirement :: String.t, opts :: Keyword.t })
  end
end
