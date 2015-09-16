defmodule Mix.Tasks.Archive.Build do
  use Mix.Task

  @shortdoc "Archives this project into a .ez file"

  @moduledoc """
  Builds an archive according to the specification of the
  [Erlang Archive Format](http://www.erlang.org/doc/man/code.html).

  The archive will be created in the current directory (which is
  expected to be the project root), unless an argument `-o` is
  provided with the file name.

  Archives are meant to bundle small projects, usually installed
  locally.  By default, this command archives the current project
  but the `-i` and `-o` options can be used to archive any directory.
  For example, `mix archive.build` with no options translates to:

      mix archive.build -i _build/ENV/lib/APP -o APP-VERSION.ez

  ## Command line options

    * `-o` - specify output file name.
      If there is a `mix.exs`, defaults to "APP-VERSION.ez".

    * `-i` - specify the input directory to archive.
      If there is a `mix.exs`, defaults to the current application build.

    * `--no-compile` - skip compilation.
      Only applies when `mix.exs` is available.

  """
  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, aliases: [o: :output, i: :input],
                                      switches: [force: :boolean, compile: :boolean])

    project = Mix.Project.get

    if project && Keyword.get(opts, :compile, true) do
      Mix.Task.run :compile, args
    end

    source = cond do
      input = opts[:input] ->
        input
      project ->
        path = Mix.Project.app_path
        if elixir = Mix.Project.config[:elixir] do
          File.write Path.join(path, ".elixir"), elixir
        else
          File.rm Path.join(path, ".elixir")
        end
        path
      true ->
        Mix.raise "Cannot create archive without input directory, " <>
          "please pass -i as an option"
    end

    target = cond do
      output = opts[:output] ->
        output
      app = Mix.Project.config[:app] ->
        Mix.Archive.name(app, Mix.Project.config[:version])
      true ->
        Mix.raise "Cannot create archive without output file, " <>
          "please pass -o as an option"
    end

    unless File.dir?(source) do
      Mix.raise "Expected archive source #{inspect source} to be a directory"
    end

    Mix.Archive.create(source, target)

    Mix.shell.info "Generated archive #{inspect target} with MIX_ENV=#{Mix.env}"
    :ok
  end
end
