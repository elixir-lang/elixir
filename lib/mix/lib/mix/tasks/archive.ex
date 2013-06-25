defmodule Mix.Tasks.Archive do
  use Mix.Task

  @shortdoc "Archive this project into a .ez file"

  @moduledoc """
  Packages the current project (though not its dependencies) into a
  zip file according to the specification of the
  [Erlang Archive Format](http://www.erlang.org/doc/man/code.html).

  Archives are meant to bundle small projects, usually installed
  locally.

  The file will be created in the current directory (which is expected
  to be the project root), unless an argument -o is provided with the file name.

  ## Command line options

  * `-o` specify output file name
  * `--no-compile` - skip compilation

  """

  def run(args) do
    { opts, _ } = OptionParser.parse(args, switches: [force: :boolean, no_compile: :boolean])

    unless opts[:no_compile] do
      Mix.Task.run :compile, args
    end

    if opts[:o] do
      archive_file = opts[:o]
    else
      app_name = Mix.project[:app] |> atom_to_binary
      archive_file = app_name <> "-" <> (Mix.project[:version] || "") <> ".ez"
    end

    Mix.Archive.create(archive_file)
  end
end
