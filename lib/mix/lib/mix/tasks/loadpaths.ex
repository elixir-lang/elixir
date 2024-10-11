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
    * `--no-optional-deps` - does not compile or load optional deps

  """
  @impl true
  def run(args) do
    config = Mix.Project.config()

    # --from-mix-deps-compile is used only internally to avoid
    # recursively checking and loading dependencies that have
    # already been loaded. It has no purpose from Mix.CLI
    # because running a task may load deps.
    if "--from-mix-deps-compile" not in args do
      Mix.Task.run("deps.loadpaths", args)
    end

    if config[:app] do
      load_project(config)
    end

    :ok
  end

  defp load_project(config) do
    vsn = {System.version(), :erlang.system_info(:otp_release)}
    scm = config[:build_scm]

    # Erase the app build if we have lock mismatch.
    # We do this to force full recompilation when
    # any of SCM or Elixir version changes. Applies
    # to dependencies and the main project alike.
    case Mix.Dep.ElixirSCM.read() do
      {:ok, old_vsn, old_scm} when old_vsn != vsn or old_scm != scm ->
        File.rm_rf(Mix.Project.app_path(config))
        File.rm_rf(Mix.Project.consolidation_path(config))

      _ ->
        :ok
    end

    # We don't cache the current application as we may still write to it
    Code.prepend_path(Mix.Project.compile_path(config))
  end
end
