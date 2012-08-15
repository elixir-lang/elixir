defrecord Mix.Dep, [scm: nil, app: nil, requirement: nil, status: nil, opts: nil] do
  @moduledoc """
  This is a record that keeps information about your project
  dependencies. It keeps:

  * scm - a module representing the source code management tool (SCM) operations;
  * app - the app name as an atom;
  * requirements - a binary or regexp with the deps requirement;
  * status - the current status of dependency, check `Mix.Deps.format_status/1` for more info;
  * opts - the options given by the developer
  """
end

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
    scms = Mix.SCM.available
    Enum.map deps, with_scm_and_status(&1, scms)
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
      if is_binary(app), do: app = binary_to_atom(app)
      case List.keyfind(candidates, app, 2) do
        nil -> raise Mix.Error, message: "unknown dependency #{app}"
        dep -> dep
      end
    end
  end

  @doc """
  Formats the status of a dependency.
  """
  def format_status({ :ok, _vsn }),         do: "ok"
  def format_status({ :noappfile, path }),  do: "could not find app file at #{path}"
  def format_status({ :invalidapp, path }), do: "the app file at #{path} is invalid"
  def format_status({ :invalidvsn, vsn }),  do: "the dependency does not match the specified version, got #{vsn}"
  def format_status({ :lockmismatch, _ }),  do: "lock mismatch: the dependency is out of date"
  def format_status(:nolock),               do: "the dependency is not locked"
  def format_status({ :unavailable, _ }),   do: "the dependency is not available, run `mix deps.get`"

  @doc """
  Receives a dependency and update its status
  """
  def update_status(Mix.Dep[scm: scm, app: app, requirement: req, opts: opts]) do
    with_scm_and_status({ app, req, opts }, [scm])
  end

  @doc """
  Checks the lock for the given dependency and update its status accordingly.
  """
  def check_lock(Mix.Dep[status: { :unavailable, _}] = dep, _lock) do
    dep
  end

  def check_lock(Mix.Dep[scm: scm, app: app, opts: opts] = dep, lock) do
    rev  = lock[app]
    opts = Keyword.put(opts, :lock, rev)

    if scm.check?(deps_path(dep), opts) do
      dep
    else
      status = if rev, do: { :lockmismatch, rev }, else: :nolock
      dep.status(status)
    end
  end

  @doc """
  Check if a dependency is out of date or not, considering its
  lock status. Therefore, be sure to call `check_lock` before
  invoking this function.
  """
  def out_of_date?(Mix.Dep[status: { :unavailable, _ }]),  do: true
  def out_of_date?(Mix.Dep[status: { :lockmismatch, _ }]), do: true
  def out_of_date?(Mix.Dep[status: :nolock]),              do: true
  def out_of_date?(_),                                     do: false

  @doc """
  Format the dependency for printing.
  """
  def format_dep(Mix.Dep[scm: scm, app: app, status: status, opts: opts]) do
    version =
      case status do
        { :ok, vsn } when vsn != nil -> "(#{vsn}) "
        _ -> ""
      end

    "#{app} #{version}[#{scm.key}: #{inspect opts[scm.key]}]"
  end

  @doc """
  Returns the path for the given dependency.
  """
  def deps_path(Mix.Dep[app: app, opts: opts]) do
    deps_path(app, opts)
  end

  @doc """
  The default path for dependencies.
  """
  def deps_path do
    Mix.project[:deps_path] || "deps"
  end

  ## Helpers

  defp with_scm_and_status({ app, opts }, scms) when is_atom(app) and is_list(opts) do
    with_scm_and_status({ app, nil, opts }, scms)
  end

  defp with_scm_and_status({ app, req, opts }, scms) when is_atom(app) and
      (is_binary(req) or is_regex(req) or req == nil) and is_list(opts) do

    { scm, opts } = Enum.find_value scms, fn(scm) ->
      (new = scm.consumes?(opts)) && { scm, new }
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
      %b(expected { "app", "requirement", scm: "location" })
  end

  defp status(scm, app, req, opts) do
    deps_path = deps_path(app, opts)
    if scm.available? deps_path, opts do
      if req do
        app_path = File.join deps_path, "ebin/#{app}.app"
        validate_app_file(app_path, app, req)
      else
        { :ok, nil }
      end
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

  defp vsn_match?(expected, actual) when is_binary(expected), do: actual == expected
  defp vsn_match?(expected, actual) when is_regex(expected),  do: actual =~ expected

  defp deps_path(app, opts) do
    opts[:path] || File.join(deps_path, app)
  end
end