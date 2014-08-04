defmodule Mix.Tasks.Clean do
  use Mix.Task

  @shortdoc "Delete generated application files"
  @recursive true

  @moduledoc """
  Delete generated application files.

  This command deletes all build artifacts for the current application
  across all environments. Dependencies are only cleaned if the
  `--all` option is given.

  ## Command line options

    * `--all` - clean everything, including builds and dependencies

  """

  def run(args) do
    Mix.Project.get!
    loadpaths!

    {opts, _, _} = OptionParser.parse(args)

    _ = for compiler <- Mix.Tasks.Compile.compilers(),
            module = Mix.Task.get("compile.#{compiler}"),
            function_exported?(module, :clean, 0),
            do: module.clean

    if opts[:all] do
      Mix.Task.run("deps.clean", args)
      Mix.Project.build_path
      |> Path.dirname
      |> File.rm_rf
    else
      config = Mix.Project.config
      Mix.Project.build_path(config)
      |> Path.dirname
      |> Path.join("*/lib/#{config[:app]}")
      |> Path.wildcard
      |> Enum.each(&File.rm_rf/1)
    end
  end

  # Loadpaths without checks because compilers may be defined in deps.
  defp loadpaths! do
    Mix.Task.run "loadpaths", ["--no-elixir-version-check", "--no-deps-check"]
    Mix.Task.reenable "loadpaths"
  end
end
