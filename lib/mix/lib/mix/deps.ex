defrecord Mix.Dep, [scm: nil, app: nil, requirement: nil, status: nil, opts: nil], moduledoc: """
This is a record that keeps information about your project
dependencies. It keeps:

* scm - a module representing the source code management tool (SCM) operations;
* app - the app name as an atom;
* requirements - a binary or regexp with the deps requirement;
* status - the current status of dependency, check `Mix.Deps.format_status/1` for more info;
* opts - the options given by the developer
"""

defmodule Mix.Deps do
  @moduledoc """
  A module with common functions to work with dependencies.
  """

  @doc """
  Returns all dependencies in as `Mix.Dep` record.

  ## Exceptions

  This function raises an exception in case the developer
  provides a dependency in the wrong format.

  ## Statuses

  The `status` element in the tuple returns the current
  situation of the repository. Check `format_status/1`
  for more information.
  """
  def all do
    deps = Mix.project[:deps] || []
    Enum.map deps, with_scm_and_status(&1)
  end

  @doc """
  Get all dependencies that match the specific `status`.
  """
  def all(status) do
    Enum.filter all, match?(Mix.Dep[status: { ^status, _ }], &1)
  end

  @doc """
  Receives a list of deps names and returns deps records.
  Raises an error if the dependency does not exist.
  """
  def by_name(given) do
    candidates = all

    Enum.map given, fn(app) ->
      case List.keyfind(candidates, app, 2) do
        nil -> raise Mix.Error, message: "unknown dependency #{app}"
        dep -> dep
      end
    end
  end

  @doc """
  Formats the status of a dependency. It can be either:

  * `{ :ok, vsn }` - Everything is :ok, got version `vsn`;
  * `{ :unavailable, path }` - The dependency is not available;
  * `{ :noappfile, path }` - The .app file at path could not be found;
  * `{ :invalidapp, path }` - The .app file at path is not properly formatted;
  * `{ :invalidvsn, actual }` - The dependency does not match the specified requirement, got `actual`;

  """
  def format_status({ :ok, _vsn }),         do: "ok"
  def format_status({ :unavailable, _ }),   do: "the dependency is not available, run `mix deps.get`"
  def format_status({ :noappfile, path }),  do: "could not find app file at #{path}"
  def format_status({ :invalidapp, path }), do: "the app file at #{path} is invalid"
  def format_status({ :invalidvsn, vsn }),  do: "the dependency does not match the specified version, got #{vsn}"

  @doc """
  Receives a dependency and update its status
  """
  def update_status(Mix.Dep[app: app, requirement: req, opts: opts]) do
    with_scm_and_status({ app, req, opts })
  end

  @doc """
  Format the dependency for printing.
  """
  def format_dep(Mix.Dep[scm: scm, app: app, status: status, opts: opts]) do
    version =
      case status do
        { :ok, vsn } -> "(#{vsn}) "
        _ -> ""
      end

    "#{app} #{version}[#{scm.key}: #{inspect opts[scm.key]}]"
  end

  @doc """
  Returns the dependency path for the given application.
  """
  def deps_path(app) do
    File.join deps_path, app
  end

  @doc """
  Return the dependency path.
  """
  def deps_path do
    Mix.project[:deps_path] || "deps"
  end

  ## Helpers

  defp with_scm_and_status({ app, opts }) when is_atom(app) and is_list(opts) do
    with_scm_and_status({ app, nil, opts })
  end

  defp with_scm_and_status({ app, req, opts }) when is_atom(app) and
      (is_binary(req) or is_regex(req) or req == nil) and is_list(opts) do
    scm = Enum.find Mix.SCM.available, opts[&1]

    if scm do
      scm_module = Mix.SCM.to_module(scm)

      Mix.Dep[
        scm: scm_module,
        app: app,
        requirement: req,
        status: status(scm_module, app, req, opts),
        opts: opts
      ]
    else
      supported = Enum.join Mix.SVM.available, ", "
      raise Mix.Error, message: "did not specify a supported scm, expected one of: " <> supported
    end
  end

  defp with_scm_and_status(other) do
    raise Mix.Error, message: %b(dependency specified in the wrong format: #{inspect other}, ) <>
      %b(expected { "app", "requirement", git: "location" })
  end

  defp status(scm, app, req, _) do
    deps_path = deps_path(app)
    if scm.available? deps_path do
      app_path = File.join deps_path, "ebin/#{app}.app"
      validate_app_file(app_path, app, req)
    else
      { :unavailable, deps_path }
    end
  end

  defp validate_app_file(app_path, app, req) do
    case :file.consult(app_path) do
      { :ok, [{ :application, ^app, config }] } ->
        case List.keyfind(config, :vsn, 1) do
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
end
