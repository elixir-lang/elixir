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
  
  * `:app` - The application name (required)
  * `:version` - The application version (required)

  """
  def run(_args) do
    project = Mix.Project.current
    config  = Mix.Project.config

    app     = Keyword.get!(config, :app)
    version = Keyword.get!(config, :version)
    path    = config[:compile_path] || "ebin"

    best_guess = [vsn: to_char_list(version), modules: modules_from(path)]

    contents = if function_exported?(project, :application, 0) do
      Keyword.merge(best_guess, project.application)
    else
      best_guess
    end

    contents = { :application, app, contents }

    file = File.open!(File.join(path, "#{app}.app"), [:write])
    :io.fwrite(file, "~p.", [contents])
    File.close(file)

    Mix.shell.info "Generated #{app}.app"
  end

  defp modules_from(path) do
    beams = File.wildcard('#{path}/**/*.beam')
    Enum.map beams, &1 /> File.rootname('.beam') /> list_to_atom
  end
end