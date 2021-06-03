defmodule Mix.Tasks.Loadpaths do
  use Mix.Task

  @moduledoc """
  Loads the application and its dependencies paths.

  This task is never directly invoked from the command line,
  but it is rather used as building block by other tasks.

  Dependencies are checked, compiled, and loaded. Each step
  can be explicitly disabled with flags.

  ## Configuration

    * `:elixir` - matches the current Elixir version against the
      given requirement

  ## Command line options

    * `--no-archives-check` - does not check archives
    * `--no-compile` - does not compile dependencies, only check and load them
    * `--no-deps-check` - does not check dependencies, only load available ones
    * `--no-elixir-version-check` - does not check Elixir version
    * `--no-load-deps` - does not add deps loadpaths to the code path

  """
  @impl true
  def run(args) do
    config = Mix.Project.config()

    unless "--no-archives-check" in args do
      Mix.Task.run("archive.check", args)
    end

    # --from-mix-deps-compile is used only internally to avoid
    # recursively checking and loading dependencies that have
    # already been loaded. It has no purpose from Mix.CLI
    # because running a task may load deps.
    unless "--from-mix-deps-compile" in args do
      Mix.Task.run("deps.loadpaths", args)
    end

    if config[:app] do
      load_project(config, args)
    end

    :ok
  end

  defp load_project(config, _args) do
    vsn = {System.version(), :erlang.system_info(:otp_release)}
    scm = config[:build_scm]

    # Erase the app build if we have lock mismatch.
    # We do this to force full recompilation when
    # any of SCM or Elixir version changes. Applies
    # to dependencies and the main project alike.
    case Mix.Dep.ElixirSCM.read() do
      {:ok, old_vsn, _} when old_vsn != vsn -> rm_rf_app(config)
      {:ok, _, old_scm} when old_scm != scm -> rm_rf_app(config)
      _ -> :ok
    end

    Code.prepend_path(Mix.Project.compile_path(config))
  end

  defp rm_rf_app(config) do
    File.rm_rf(Mix.Project.app_path(config))
    File.rm_rf(Mix.Project.consolidation_path(config))
  end
end
