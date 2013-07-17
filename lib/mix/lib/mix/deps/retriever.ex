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
    # Don't run recursively for the top-level project
    scms = Mix.SCM.available
    from = current_source(:mix)
    (Mix.project[:deps] || []) |> Enum.map(update(&1, scms, from))
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
    dep = with_scm_and_status(tuple, scms).from(from)

    if match?({ _, req, _ } when is_regex(req), tuple) and
        not String.ends_with?(from, "rebar.config") do
      Mix.shell.info("[WARNING] Regex version requirements for dependencies are " <>
        "deprecated, please use Mix.Version instead")
    end

    cond do
      # If it is not available, there is nothing we can do
      not Mix.Deps.available?(dep) ->
        dep

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
    end
  end

  defp current_source(manager) do
    case manager do
      :mix   -> "mix.exs"
      :rebar -> "rebar.config"
    end |> Path.absname
  end

  defp mix_dep(Mix.Dep[manager: nil] = dep, project) do
    if match?({ :noappfile, _ }, dep.status) and Mix.Project.umbrella? do
      dep = dep.update_opts(Keyword.put(&1, :app, false))
               .status({ :ok, nil })
    end
    dep.manager(:mix).source(project)
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

  defp with_scm_and_status({ app, opts }, scms) when is_atom(app) and is_list(opts) do
    with_scm_and_status({ app, nil, opts }, scms)
  end

  defp with_scm_and_status({ app, req, opts }, scms) when is_atom(app) and
      (is_binary(req) or is_regex(req) or req == nil) and is_list(opts) do

    path = Path.join(Mix.project[:deps_path], app)
    opts = Keyword.put(opts, :dest, path)

    { scm, opts } = Enum.find_value scms, fn(scm) ->
      (new = scm.accepts_options(opts)) && { scm, new }
    end

    if scm do
      Mix.Dep[
        scm: scm,
        app: app,
        requirement: req,
        status: status(scm, app, req, opts),
        opts: opts
      ]
    else
      supported = Enum.join scms, ", "
      raise Mix.Error, message: "did not specify a supported scm, expected one of: " <> supported
    end
  end

  defp with_scm_and_status(other, _scms) do
    raise Mix.Error, message: %b(dependency specified in the wrong format: #{inspect other}, ) <>
      %b(expected { :app, scm: "location" } | { :app, "requirement", scm: "location" })
  end

  defp status(scm, app, req, opts) do
    if scm.checked_out? opts do
      opts_app = opts[:app]

      if opts_app == false do
        { :ok, nil }
      else
        path = if is_binary(opts_app), do: opts_app, else: "ebin/#{app}.app"
        path = Path.join(opts[:dest], path)
        validate_app_file(path, app, req)
      end
    else
      { :unavailable, opts[:dest] }
    end
  end

  defp validate_app_file(app_path, app, req) do
    case :file.consult(app_path) do
      { :ok, [{ :application, ^app, config }] } ->
        case List.keyfind(config, :vsn, 0) do
          { :vsn, actual } ->
            actual = list_to_binary(actual)
            if vsn_match?(req, actual) do
              { :ok, actual }
            else
              { :invalidvsn, actual }
            end
          nil -> { :invalidvsn, nil }
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
end
