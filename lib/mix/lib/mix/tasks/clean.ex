defmodule Mix.Tasks.Clean do
  use Mix.Task

  @shortdoc "Delete generated application files"
  @recursive true

  @moduledoc """
  Delete generated application files.

  This command deletes all build artifacts for the current project.
  Dependencies' build files are cleaned if the `--deps` option is given.

  By default this task works across all environments, unless `--only`
  is given.
  """

  @switches [deps: :boolean, only: :string]

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    Mix.Project.get!
    loadpaths!

    {opts, _, _} = OptionParser.parse(args, switches: @switches)

    _ = for compiler <- [:protocols] ++ Mix.Tasks.Compile.compilers(),
            module = Mix.Task.get("compile.#{compiler}"),
            function_exported?(module, :clean, 0),
            do: module.clean

    build = Mix.Project.build_path
            |> Path.dirname
            |> Path.join("#{opts[:only] || :*}")

    if opts[:deps] do
      build
      |> Path.wildcard
      |> Enum.each(&File.rm_rf/1)
    else
      build
      |> Path.join("lib/#{Mix.Project.config[:app]}")
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
