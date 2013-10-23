defmodule Mix.Tasks.Loadpaths do
  use Mix.Task

  @hidden true
  @shortdoc "Load the application and its dependencies paths"

  @moduledoc """
  Load the application and its dependencies paths.

  ## Configuration

  * `:load_paths` extra load paths to be added.
    They are added with lower priority than the app ones.

  ## Command line options

  * `--no-deps` - do not load dependencies
  * `--no-deps-check` - do not check dependencies
  * `--no-elixir-version-check` - do not check elixir version

  """
  def run(args) do
    { opts, _, _ } = OptionParser.parse(args)

    unless opts[:no_deps] do
      Mix.Task.run "deps.loadpaths", args
    end

    unless opts[:no_elixir_version_check] do
      config = Mix.project

      if requirement = config[:elixir] do
        unless Version.match?(System.version, requirement) do
          raise Mix.ElixirVersionError, target: config[:app] || Mix.Project.get,
                                        expected: requirement,
                                        actual: System.version
        end
      end
    end

    # Force recompile if we have a version mismatch
    old_vsn = Mix.Deps.Lock.elixir_vsn
    if old_vsn && old_vsn != System.version, do: Mix.Deps.Lock.touch

    Enum.each Mix.Project.load_paths, &Code.prepend_path(&1)
  end
end
