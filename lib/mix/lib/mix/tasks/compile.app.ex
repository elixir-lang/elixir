defmodule Mix.Tasks.Compile.App do
  use Mix.Task

  @shortdoc "Write .app file"

  @moduledoc """
  Writes an .app file.

  By default, this task will detect all modules in your compile_path
  (default to "ebin") and generate a best guess for your application
  specification. This best guess also includes "kernel", "stdlib"
  and "elixir" as application dependencies.

  You can optionally define an `application/0` function inside your
  `Mix.Project` that returns a keywords list to further configure
  your application according to OTP design principles:

  http://www.erlang.org/doc/design_principles/applications.html

  ## Configuration
  
  * `:app` - The application name as a binary (required)
  * `:version` - The application version as a binary (required)

  ## Command line options

  * `--force` forces compilation regardless of mod times

  """
  def run(args) do
    destructure [force], args

    project = Mix.Project.current
    config  = Mix.project

    app     = Keyword.get!(config, :app)
    version = Keyword.get!(config, :version)

    validate_app(app)
    validate_version(version)

    path    = config[:compile_path] || "ebin"
    beams   = File.wildcard('#{path}/*.beam')

    target  = File.join(path, "#{app}.app")
    sources = [Mix.Utils.source(project) | beams]

    if force == "--force" or Mix.Utils.stale?(sources, [target]) do
      best_guess = [
        vsn: to_char_list(version),
        modules: modules_from(beams),
        applications: ['kernel', 'stdlib', 'elixir']
      ]

      contents = if function_exported?(project, :application, 0) do
        Mix.Utils.config_merge(best_guess, project.application)
      else
        best_guess
      end

      contents = Keyword.put contents, :applications, Enum.map(contents[:applications], to_char_list(&1))
      contents = { :application, app, contents }

      File.mkdir_p!(File.dirname(target))
      file = File.open!(target, [:write])
      :io.fwrite(file, "~p.", [contents])
      File.close(file)

      Mix.shell.info "Generated #{app}.app"
      :ok
    else
      :noop
    end
  end

  defp validate_app(app) when is_atom(app), do: :ok
  defp validate_app(_), do: raise(Mix.Error, message: "Expected :app to be an atom")

  defp validate_version(version) when is_binary(version), do: :ok
  defp validate_version(_), do: raise(Mix.Error, message: "Expected :version to be a binary")

  defp modules_from(beams) do
    Enum.map beams, &1 /> File.basename /> File.rootname('.beam') /> list_to_atom
  end
end