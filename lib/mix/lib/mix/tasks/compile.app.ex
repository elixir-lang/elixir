defmodule Mix.Tasks.Compile.App do
  use Mix.Task

  @shortdoc "Writes an .app file"

  @moduledoc """
  Writes an .app file.

  It expects your `Mix.Project` to define an application function
  that will return the application configuration according to
  OTP's design principles for applications:

  http://www.erlang.org/doc/design_principles/applications.html

  ## Configuration
  
  * `:app` - The application name as a binary (required)
  * `:version` - The application version as a binary (required)

  ## Command line options

  * `--force` forces compilation regardless of mod times;

  """
  def run(args) do
    destructure [force], args

    project = Mix.Project.current
    config  = Mix.project

    app     = Keyword.get!(config, :app)
    version = Keyword.get!(config, :version)

    path    = config[:compile_path] || "ebin"
    beams   = File.wildcard('#{path}/*.beam')

    target  = File.join(path, "#{app}.app")
    sources = [project.location | beams]

    if force == "--force" or Mix.Utils.stale?(sources, [target]) do
      best_guess = [vsn: to_char_list(version), modules: modules_from(beams)]

      contents = if function_exported?(project, :application, 0) do
        Keyword.merge(best_guess, project.application)
      else
        best_guess
      end

      contents = { :application, binary_to_atom(app), contents }

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

  defp modules_from(beams) do
    Enum.map beams, &1 /> File.basename /> File.rootname('.beam') /> list_to_atom
  end
end