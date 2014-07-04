defmodule Mix.Tasks.Clean do
  use Mix.Task

  @shortdoc "Clean generated application files"
  @recursive true

  @moduledoc """
  Clean generated application files.

  This command delete all build artifacts for the current application
  accross all environments. Dependencies are only cleaned up if the
  `--all` option is given.

  ## Command line options

    * `--all` - clean everything, including builds and dependencies

  """

  def run(args) do
    {opts, _, _} = OptionParser.parse(args)

    _ = for compiler <- Mix.Tasks.Compile.compilers() do
      module = Mix.Task.get!("compile.#{compiler}")
      if function_exported?(module, :clean, 0) do
        module.clean
      end
    end

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
end
