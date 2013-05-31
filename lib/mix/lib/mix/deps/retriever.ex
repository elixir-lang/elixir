# This module is responsible for retrieving
# dependencies of a given project. This
# module and its functions are private to Mix.
defmodule Mix.Deps.Retriever do
  @moduledoc false

  @doc """
  Returns all dependencies for the current Mix.Project
  as a `Mix.Dep` record.

  ## Exceptions

  This function raises an exception in case the developer
  provides a dependency in the wrong format.
  """
  def all(post_config // []) do
    { deps, _ } = all(nil, children(post_config), post_config, fn(dep, acc) -> { dep, acc } end)
    deps
  end

  @doc """
  Like `all/0` but takes a callback that is invoked for
  each dependency and must return an updated depedency
  in case some processing is done.
  """
  def all(rest, post_config // [], callback) do
    all(rest, children(post_config), post_config, callback)
  end

  defp all(rest, childs, post_config, callback) do
    Enum.map_reduce childs, rest, fn (dep, rest) ->
      { dep, rest } = callback.(dep, rest)

      cond do
        Mix.Deps.available?(dep) and mixfile?(dep) ->
          Mix.Deps.in_dependency(dep, post_config, fn project ->
            { deps, rest } = all(rest, children(post_config), post_config, callback)

            # We need to call with_mix_project once again
            # here in case the dependency was not available
            # the first time and the callback hook just
            # happened to fetch it.
            { with_mix_project(dep, project).deps(deps), rest }
          end)

        Mix.Deps.available?(dep) and rebarconfig?(dep) ->
          dep = rebar_dep(dep)

          Mix.Deps.in_dependency(dep, post_config, fn _ ->
            { deps, rest } = all(rest, rebar_children("."), post_config, callback)
            { dep.deps(deps), rest }
          end)

        true ->
          { dep, rest }
      end
    end
  end

  @doc """
  Gets all direct children for the current Mix.Project
  as a `Mix.Dep` record. Unlike with `all` the `deps`
  field is not populated.
  """
  def children(post_config // []) do
    Mix.Project.recur(post_config, fn _ ->
      (Mix.project[:deps] || []) |> setup_deps
    end) |> List.concat
  end

  @doc """
  Receives a dependency and update its status.
  """
  def update(Mix.Dep[scm: scm, app: app, requirement: req, opts: opts]) do
    with_scm_and_status({ app, req, opts }, [scm])
  end

  ## Helpers

  defp rebar_children(dir) do
    Mix.Rebar.recur(dir, fn config ->
      Mix.Rebar.deps(config) |> setup_deps
    end) |> List.concat
  end

  defp setup_deps(deps) do
    scms = Mix.SCM.available

    Enum.map deps, fn dep ->
      dep = with_scm_and_status(dep, scms)

      cond do
        Mix.Deps.available?(dep) and mixfile?(dep) ->
          Mix.Deps.in_dependency(dep, fn project ->
            with_mix_project(dep, project)
          end)

        Mix.Deps.available?(dep) and rebarconfig?(dep) ->
          rebar_dep(dep)

        true ->
          dep
      end
    end
  end

  defp with_mix_project(Mix.Dep[manager: nil] = dep, project) do
    if match?({ :noappfile, _ }, dep.status) and Mix.Project.umbrella? do
      dep = dep.update_opts(Keyword.put(&1, :app, false))
               .status({ :ok, nil })
    end
    dep.manager(:mix).source(project)
  end

  defp with_mix_project(dep, _project), do: dep

  defp rebar_dep(Mix.Dep[manager: nil, opts: opts] = dep) do
    config = Mix.Rebar.load_config(opts[:dest])
    dep.manager(:rebar).source(config)
  end

  defp rebar_dep(dep), do: dep

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
end
