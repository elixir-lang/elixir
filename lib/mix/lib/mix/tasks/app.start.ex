defmodule Mix.Tasks.App.Start do
  use Mix.Task

  @recursive true

  @moduledoc """
  Starts all registered apps. If no apps key exists,
  it starts the current application.

  ## Command line options

    * `--force`         - force compilation regardless of compilation times
    * `--no-compile`    - do not compile even if files require compilation
    * `--no-deps-check` - do not check dependencies
    * `--no-elixir-version-check`
                        - do not check elixir version
    * `--no-start`      - do not start applications after compilation

  """
  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    Mix.Project.get!
    Mix.Task.run "loadpaths", ["--no-readd"|args]

    unless "--no-compile" in args do
      Mix.Task.run "compile", ["--no-readd"|args]
    end

    unless "--no-start" in args do
      # Stop the Logger when starting the application as it is
      # up to the application to decide if it should be restarted
      # or not.
      #
      # Mix should not depend directly on Logger so check that it's loaded.
      if Code.ensure_loaded?(Logger), do: Logger.App.stop()
      start(Mix.Project.config[:app])
    end

    Code.readd_paths()
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
