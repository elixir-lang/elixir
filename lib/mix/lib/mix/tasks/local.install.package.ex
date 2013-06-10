defmodule Mix.Tasks.Local.Install.Package do
  use Mix.Task

  @shortdoc "Install task archive locally"

  @moduledoc """
  Install a package archive for this project in MIX_HOME/tasks.
  Similar to local.install but installs all the current project's beam files, though NOT their dependencies.
  After installation, any tasks part of this project can be executed using:

      mix some_task

  ## Command line options
  * `--no-compile` - do not compile even if files require compilation;
  """

  def run(args) do
    { opts, _ } = OptionParser.parse(args, switches: [force: :boolean, no_compile: :boolean])

    unless opts[:no_compile] do
      Mix.Task.run :compile, args
    end

    Mix.Package.create_package(Path.join(Mix.Utils.mix_home,"tasks"))
  end
end
