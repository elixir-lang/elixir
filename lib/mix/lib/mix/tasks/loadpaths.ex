defmodule Mix.Tasks.Loadpaths do
  use Mix.Task

  @moduledoc """
  Loads the application and its dependencies paths.

  ## Command line options

    * `--no-deps-check` - do not check dependencies
    * `--no-elixir-version-check` - do not check elixir version

  """

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    config = Mix.Project.config

    unless "--no-elixir-version-check" in args do
      check_elixir_version(config, args)
    end

    # --no-deps is used only internally. It has not purpose
    # from Mix.CLI because the CLI itself already loads deps.
    unless "--no-deps" in args do
      load_deps(config, args)
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
          unless Version.match?(System.version, req) do
            Mix.raise Mix.ElixirVersionError, target: config[:app] || Mix.Project.get,
                                              expected: req,
                                              actual: System.version
          end
        :error ->
          Mix.raise "Invalid Elixir version requirement #{req} in mix.exs file"
      end
    end
  end

  defp load_deps(_config, args) do
    unless "--no-deps-check" in args do
      Mix.Task.run "deps.check", args
    end

    Mix.Task.run "deps.loadpaths"
  end

  defp load_project(config, _args) do
    # Force recompile if we have an app and a version mismatch
    old_vsn = Mix.Dep.Lock.elixir_vsn
    if old_vsn && old_vsn != System.version, do: Mix.Dep.Lock.touch

    Enum.each Mix.Project.load_paths(config), &Code.prepend_path(&1)
  end
end
