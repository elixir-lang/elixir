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

  * `-o` - specify output file name.
           If there is a mix.exs, defaults to app-vsn.ez

  * `-i` - specify the input directory to archive.
           If there is a mix.exs, defaults to the current application build

  * `--no-compile` - skip compilation.
                     Only applies to projects.

  """

  def run(args) do
    { opts, _, _ } = OptionParser.parse(args, aliases: [o: :output, i: :input],
                                        switches: [force: :boolean, no_compile: :boolean])

    project = Mix.Project.get

    if project && !opts[:no_compile] do
      Mix.Task.run :compile, args
    end

    source = cond do
      input = opts[:input] ->
        input
      project ->
        Mix.Project.app_path
      true ->
        raise Mix.Error, message: "Cannot create archive without input directory, " <>
          "please pass -i as an option"
    end

    target = cond do
      output = opts[:output] ->
        output
      app = Mix.project[:app] ->
        Mix.Archive.name(app, Mix.project[:version])
      true ->
        raise Mix.Error, message: "Cannot create archive without a name, " <>
          "please pass -o as an option"
    end

    unless File.dir?(source) do
      raise Mix.Error, message: "Expected archive source #{inspect source} to be a directory"
    end

    Mix.Archive.create(source, target)
  end
end
