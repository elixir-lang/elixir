defmodule Mix.Tasks.Deps do
  use Mix.Task

  @shortdoc "List dependencies and their status"

  @moduledoc """
  List all dependencies and their status.
  """

  @doc """
  Returns all dependencies in the following format:

      { SCM, :app", "requirement", status, [opts] }

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
    Enum.filter all, match?({ _, _, _, { ^status, _ }, _ }, &1)
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
  Format the dependency for printing.
  """
  def format_dep({ _scm, app, _req, status, opts }) do
    opts = Enum.map_join(opts, ", ", fn({ key, value }) ->
      "#{key}: #{inspect value}"
    end)

    version =
      case status do
        { :ok, vsn } -> "(#{vsn}) "
        _ -> ""
      end

    "#{app} #{version}[#{opts}]"
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

  def run(_) do
    shell = Mix.shell

    Enum.each all, fn(dep) ->
      shell.info "* #{format_dep(dep)}"
      shell.info "  #{format_status elem(dep, 4)}"
    end
  end

  ## Helpers

  defp with_scm_and_status({ app, req, opts }) when is_atom(app) and
      (is_binary(req) or is_regex(req)) and is_list(opts) do
    scm = Enum.find available_scm, opts[&1]

    if scm do
      scm_module = available_scm(scm)
      { scm_module, app, req, status(scm_module, app, req, opts), opts }
    else
      supported = Enum.join available_scm, ", "
      raise Mix.Error, message: "did not specify a supported scm, expected one of: " <> supported
    end
  end

  defp with_scm_and_status(other) do
    raise Mix.Error, message: %b(dependency specified in the wrong format: #{inspect other}, ) <>
      %b(expected { "app", "requirement", git: "location" })
  end

  defp available_scm do
    [:git]
  end

  defp available_scm(:git), do: Mix.SCM.Git

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

  defp vsn_match?(expected, actual) when is_binary(expected) do
    expected == actual
  end

  defp vsn_match?(expected, actual) when is_regex(expected) do
    actual =~ expected
  end
end
