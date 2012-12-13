defmodule Mix.Tasks.App.Start do
  use Mix.Task

  import Mix.Deps, only: [all: 0, ok?: 1]

  @hidden true
  @shortdoc "Start registered apps"

  @moduledoc """
  Starts all registered apps. If no apps key exists,
  it starts the current application.

  ## Command line options

  * `--no-compile` - do not compile even if files require compilation;
  * `--no-start` - do not start applications after compilation;

  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args)

    project = Mix.project

    unless opts[:no_compile] do
      Mix.Task.run "compile", args
    end

    unless opts[:no_start] do
      start_apps(project)
    end
  end

  defp start_apps(project) do
    apps =
      cond do
        # Eventually we can support custom start apps
        # apps = project[:start_apps] -> apps
        app = project[:app]   -> [app]
        true                  -> []
      end

    lc app inlist apps do
      case Application.Behaviour.start(app) do
        :ok -> :ok
        { :error, reason } ->
          Mix.shell.error "Could not start application #{app}: #{inspect reason}"
      end
    end
  end
end
