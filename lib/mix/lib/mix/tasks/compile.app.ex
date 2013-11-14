defmodule Mix.Tasks.Compile.App do
  use Mix.Task

  @hidden true
  @shortdoc "Write .app file"
  @recursive true

  @moduledoc """
  Writes an .app file.

  By default, this task will detect all modules in your `compile_path`
  (defaults to `ebin`) and generate a best guess for your application
  specification. This best guess also includes `kernel`, `stdlib`
  and `elixir` as application dependencies.

  You can optionally define an `application/0` function inside your
  `Mix.Project` that returns a keyword list to further configure
  your application according to the OTP design principles:

  http://www.erlang.org/doc/design_principles/applications.html

  ## Command line options

  * `--force` - forces compilation regardless of modification times

  ## Configuration

  * `:app` - The application name as a binary (required)
  * `:version` - The application version as a binary (required)

  """
  def run(args) do
    { opts, _, _ } = OptionParser.parse(args, switches: [force: :boolean])

    project = Mix.Project.get!
    config  = Mix.project

    app     = Keyword.fetch!(config, :app)
    version = Keyword.fetch!(config, :version)

    validate_app(app)
    validate_version(version)

    path = Mix.Project.compile_path
    mods = modules_from(Path.wildcard('#{path}/*.beam')) |> Enum.sort

    target  = Path.join(path, "#{app}.app")
    sources = Mix.Project.config_files

    if opts[:force] || Mix.Utils.stale?(sources, [target]) || modules_changed?(mods, target) do
      best_guess = [
        vsn: to_char_list(version),
        modules: mods,
        applications: [:kernel, :stdlib, :elixir]
      ]

      properties = if function_exported?(project, :application, 0) do
        Keyword.merge(best_guess, project.application)
      else
        best_guess
      end

      properties = ensure_correct_properties(app, properties)
      contents   = { :application, app, properties }

      Mix.Project.build_structure(config)
      File.open!(target, [:write], &:io.fwrite(&1, "~p.", [contents]))

      Mix.shell.info "Generated #{app}.app"
      :ok
    else
      :noop
    end
  end

  defp modules_changed?(mods, target) do
    case :file.consult(target) do
      { :ok, { :application, _app, properties } } ->
        properties[:registered] == mods
      _ ->
        false
    end
  end

  defp validate_app(app) when is_atom(app), do: :ok
  defp validate_app(_), do: raise(Mix.Error, message: "Expected :app to be an atom")

  defp validate_version(version) when is_binary(version), do: :ok
  defp validate_version(_), do: raise(Mix.Error, message: "Expected :version to be a binary")

  defp modules_from(beams) do
    Enum.map beams, &(&1 |> Path.basename |> Path.rootname('.beam') |> list_to_atom)
  end

  defp ensure_correct_properties(app, properties) do
    properties
    |> Keyword.put_new(:description, to_char_list(app))
    |> Keyword.put_new(:registered, [])
    |> validate_properties
  end

  defp validate_properties(properties) do
    Enum.each properties, fn
      { :description, value } ->
        unless is_list(value), do:
          invalid "Application description (:description) is not a character list (got #{inspect value})"
      { :id, value } ->
        unless is_list(value), do:
          invalid "Application id (:id) is not a character list (got #{inspect value} instead)"
      { :vsn, value } ->
        unless is_list(value), do:
          invalid "Application vsn (:vsn) is not a character list (got #{inspect value} instead)"
      { :maxT, value } ->
        unless value == :infinity or is_integer(value), do:
          invalid "Application maximum time (:maxT) is not an integer or :infinity (got #{inspect value} instead)"
      { :modules, value } ->
        unless is_list(value) and Enum.all?(value, &is_atom(&1)), do:
          invalid "Application modules (:modules) should be a list of atoms (got #{inspect value} instead)"
      { :registered, value } ->
        unless is_list(value) and Enum.all?(value, &is_atom(&1)), do:
          invalid "Application registered processes (:registered) should be a list of atoms (got #{inspect value} instead)"
      { :included_applications, value } ->
        unless is_list(value) and Enum.all?(value, &is_atom(&1)), do:
          invalid "Application included applications (:included_applications) should be a list of atoms (got #{inspect value} instead)"
      { :applications, value } ->
        unless is_list(value) and Enum.all?(value, &is_atom(&1)), do:
          invalid "Application dependencies (:applications) should be a list of atoms (got #{inspect value} instead)"
      { :env, value } ->
        unless Keyword.keyword?(value), do:
          invalid "Application dependencies (:env) should be a keyword list (got #{inspect value} instead)"
      { :start_phases, value } ->
        unless Keyword.keyword?(value), do:
          invalid "Application start phases (:start_phases) should be a keyword list (got #{inspect value} instead)"
      { :mod, [] } ->
        :ok
      { :mod, { module, _args } } when is_atom(module) ->
        :ok
      { :mod, value } ->
        invalid "Application callback module (:mod) should be either [] or {module, start_args} (got #{inspect value} instead)"
      _ ->
        :ok
    end

    properties
  end

  defp invalid(message) do
    raise Mix.Error, message: message
  end
end
