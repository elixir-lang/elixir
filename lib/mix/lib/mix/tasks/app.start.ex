defmodule Mix.Tasks.App.Start do
  use Mix.Task

  @recursive true

  @moduledoc """
  Starts all registered apps. If no apps key exists,
  it starts the current application.

  ## Command line options

  * `--force` - force compilation regardless of compilation times
  * `--no-compile` - do not compile even if files require compilation
  * `--no-deps-check` - do not check dependencies
  * `--no-elixir-version-check` - do not check elixir version
  * `--no-start` - do not start applications after compilation

  """
  def run(args) do
    {opts, _, _} = OptionParser.parse(args)

    Mix.Task.run "deps.loadpaths", args
    Mix.Task.run "loadpaths", args

    unless opts[:no_compile] do
      Mix.Task.run "compile", args
    end

    unless opts[:no_start] do
      start(Mix.Project.config[:app])
    end
  end

  @doc false
  def start(app) do
    if app do
      case Application.ensure_all_started(app) do
        {:ok, _} -> :ok
        {:error, {app, reason}} ->
          Mix.raise "Could not start application #{app}: " <>
            Application.format_error(reason)
      end
    else
      :error
    end
  end
end
