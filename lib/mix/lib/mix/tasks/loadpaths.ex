defmodule Mix.Tasks.Loadpaths do
  use Mix.Task

  @moduledoc """
  Load the application and its dependencies paths.

  ## Command line options

  * `--no-elixir-version-check` - do not check elixir version

  """
  def run(args) do
    { opts, _, _ } = OptionParser.parse(args)

    unless opts[:no_elixir_version_check] do
      config = Mix.project

      if req = config[:elixir] do
        case Version.parse_requirement(req) do
          { :ok, req } ->
            unless Version.match?(System.version, req) do
              raise Mix.ElixirVersionError, target: config[:app] || Mix.Project.get,
                                            expected: req,
                                            actual: System.version
            end
          :error ->
            raise Mix.Error, message: "Invalid Elixir version requirement #{req} in mix.exs file"
        end
      end
    end

    # Force recompile if we have a version mismatch.
    # Skip it for umbrella apps since they have no build.
    old_vsn = Mix.Dep.Lock.elixir_vsn
    if old_vsn && old_vsn != System.version, do: Mix.Dep.Lock.touch

    Enum.each Mix.Project.load_paths, &Code.prepend_path(&1)
  end
end
