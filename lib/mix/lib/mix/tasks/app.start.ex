defmodule Mix.Tasks.App.Start do
  use Mix.Task

  @hidden true
  @shortdoc "Start registered apps"
  @recursive true

  @moduledoc """
  Starts all registered apps. If no apps key exists,
  it starts the current application.

  ## Command line options

  * `--no-compile` - do not compile even if files require compilation
  * `--no-start` - do not start applications after compilation
  * `--no-deps` - do not load dependencies
  * `--no-deps-check` - do not check dependencies
  * `--no-elixir-version-check` - do not check elixir version

  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args)

    project = Mix.project
    Mix.Task.run "loadpaths", args

    unless opts[:no_compile] do
      Mix.Task.run "compile", args
    end

    unless opts[:no_start] do
      start_app(project)
    end
  end

  defp start_app(project) do
    if app = project[:app] do
      ebin   = project[:compile_path]
      parent = ebin |> Path.expand |> Path.dirname |> Path.basename

      if parent != atom_to_binary(app) do
        Mix.shell.error "Mix requires the compile path #{inspect ebin} to be inside " <>
                        "a directory with the same name as the application name " <>
                        "#{inspect app}, got #{inspect Path.expand(ebin)}"
      end

      case Application.Behaviour.start(app) do
        :ok -> :ok
        { :error, reason } ->
          raise Mix.Error, message: "Could not start application #{app}: #{inspect reason}"
      end
    end
  end
end
