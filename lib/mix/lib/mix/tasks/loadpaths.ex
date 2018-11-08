defmodule Mix.Tasks.Loadpaths do
  use Mix.Task

  @moduledoc """
  Loads the application and its dependencies paths.

  ## Configuration

    * `:elixir` - matches the current Elixir version against the
      given requirement

  ## Command line options

    * `--no-archives-check` - does not check archive
    * `--no-deps-check` - does not check dependencies
    * `--no-elixir-version-check` - does not check Elixir version

  """

  @impl true
  def run(args) do
    config = Mix.Project.config()

    unless "--no-elixir-version-check" in args do
      check_elixir_version(config, args)
    end

    unless "--no-archives-check" in args do
      Mix.Task.run("archive.check", args)
    end

    # --no-deps is used only internally. It has no purpose
    # from Mix.CLI because running a task may load deps.
    unless "--no-deps" in args do
      Mix.Task.run("deps.loadpaths", args)
    end

    if config[:app] do
      load_project(config, args)
    end

    :ok
  end

  defp check_elixir_version(config, _) do
    if req = config[:elixir] do
      case Version.parse_requirement(req) do
        {:ok, req} ->
          unless Version.match?(System.version(), req) do
            raise Mix.ElixirVersionError,
              target: config[:app] || Mix.Project.get(),
              expected: req,
              actual: System.version()
          end

        :error ->
          Mix.raise("Invalid Elixir version requirement #{req} in mix.exs file")
      end
    end
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

    Enum.each(Mix.Project.load_paths(config), &Code.prepend_path(&1))
  end

  defp rm_rf_app(config) do
    File.rm_rf(Mix.Project.app_path(config))
    File.rm_rf(Mix.Project.consolidation_path(config))
  end
end
