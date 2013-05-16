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
    { deps, _ } = all(nil, post_config, fn(dep, acc) -> { dep, acc } end)
    deps
  end

  @doc """
  Like `all/0` but takes a callback that is invoked for
  each dependency and must return an updated depedency
  in case some processing is done.
  """
  def all(rest, post_config // [], callback) do
    Enum.map_reduce children, rest, fn (dep, rest) ->
      { dep, rest } = callback.(dep, rest)

      if Mix.Deps.available?(dep) and Mix.Deps.mix?(dep) do
        { dep, rest } = Mix.Deps.in_dependency dep, post_config, fn _ ->
          { deps, rest } = all(rest, callback)
          { dep.deps(deps), rest }
        end
      end

      { dep, rest }
    end
  end

  @doc """
  Gets all direct children for the current Mix.Project
  as a `Mix.Dep` record. Unlike with `all` the `deps`
  field is not populated.
  """
  def children() do
    deps = Mix.project[:deps] || []
    scms = Mix.SCM.available

    Enum.map deps, fn dep ->
      dep = with_scm_and_status(dep, scms)

      # Set properties if dependency is a mix project
      if Mix.Deps.available?(dep) and mixfile?(dep) do
        dep = Mix.Deps.in_dependency dep, fn project ->
          if match?({ :noappfile, _ }, dep.status) and Mix.Project.umbrella? do
            dep = dep.update_opts(Keyword.put(&1, :app, false))
                     .status({ :ok, nil })
          end
          dep.project(project)
        end
      end

      dep
    end
  end

  @doc """
  Receives a dependency and update its status.
  """
  def update(Mix.Dep[scm: scm, app: app, requirement: req, opts: opts]) do
    with_scm_and_status({ app, req, opts }, [scm])
  end

  ## Helpers

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
    File.regular?(Path.join dep.opts[:dest], "mix.exs")
  end
end
