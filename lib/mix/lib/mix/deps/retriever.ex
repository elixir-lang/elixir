# This module is responsible for retrieving
# dependencies of a given project. This
# module and its functions are private to Mix.
defmodule Mix.Deps.Retriever do
  @moduledoc false

  @doc """
  Gets all direct children for the current Mix.Project
  as a `Mix.Dep` record.
  """
  def children() do
    # Don't run recursively for the top-level project
    scms = Mix.SCM.available
    (Mix.project[:deps] || []) |> Enum.map(update(&1, scms, nil))
  end

  @doc """
  Gets all children for a given dependency.
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
  Receives a dependency and update its status.
  """
  def update(Mix.Dep[scm: scm, app: app, requirement: req, opts: opts]) do
    update({ app, req, opts }, [scm], nil)
  end

  ## Helpers

  defp mix_children(config) do
    scms = Mix.SCM.available
    Mix.Project.recur(config, fn _ ->
      (Mix.project[:deps] || []) |> Enum.map(update(&1, scms, nil))
    end) |> List.concat
  end

  defp rebar_children(dir) do
    scms = Mix.SCM.available
    Mix.Rebar.recur(dir, fn config ->
      Mix.Rebar.deps(config) |> Enum.map(update(&1, scms, :rebar))
    end) |> List.concat
  end

  defp update(tuple, scms, manager) do
    dep = with_scm_and_status(tuple, scms)

    if Mix.Deps.available?(dep) do
      cond do
        mixfile?(dep) ->
          Mix.Deps.in_dependency(dep, fn project ->
            mix_dep(dep, project)
          end)

        rebarconfig?(dep) ->
          rebar_dep(dep)

        makefile?(dep) ->
          make_dep(dep)

        true ->
          dep.manager(manager)
      end
    else
      dep
    end
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
  defp vsn_match?(expected, actual) when is_binary(expected), do: actual == expected
  defp vsn_match?(expected, actual) when is_regex(expected),  do: actual =~ expected

  defp mixfile?(dep) do
    File.regular?(Path.join(dep.opts[:dest], "mix.exs"))
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
