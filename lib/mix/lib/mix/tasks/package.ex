defmodule Mix.Tasks.Package do
  use Mix.Task

  @shortdoc "Package this project in an archive file."

  @moduledoc """
  Packages the current project (though not its dependencies) into a zip file according
  to the specification of the [Erlang Archive Format](http://www.erlang.org/doc/man/code.html#id102400).

  The file will be created in the current directory (which is expected to be the project root), unless
  an argument is supplied in which case the argument is expected to be the PATH where the file will be created.

  ## Command line options
  * `--no-compile` - do not compile even if files require compilation;
  """

  def run(args) do
    { opts, argv } = OptionParser.parse(args, switches: [force: :boolean, no_compile: :boolean])

    unless opts[:no_compile] do
      Mix.Task.run :compile, args
    end

    case argv do
      [] ->
        Mix.Package.create_package(".")
      [path|_] ->
        Mix.Package.create_package(path)
    end
  end
end
