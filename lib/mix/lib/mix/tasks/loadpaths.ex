defmodule Mix.Tasks.Loadpaths do
  use Mix.Task

  @moduledoc """
  Load the application and its dependencies paths.

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
      target = config[:app] || Mix.Project.get

      if requirement = config[:elixir] do
        case Version.parse_requirement(requirement) do
          { :ok, req } ->
            unless Version.match?(System.version, req) do
              raise Mix.ElixirVersionError, target: target,
                                            expected: requirement,
                                            actual: System.version
            end
          :error ->
            raise Mix.Error, message: "Invalid Elixir version requirement " <>
              "#{requirement} declared in mix.exs file for #{target}"
        end
      end
    end

    # Force recompile if we have a version mismatch.
    # Skip it for umbrella apps since they have no build.
    old_vsn = Mix.Deps.Lock.elixir_vsn
    if old_vsn && old_vsn != System.version, do: Mix.Deps.Lock.touch

    Enum.each Mix.Project.load_paths, &Code.prepend_path(&1)
  end
end
