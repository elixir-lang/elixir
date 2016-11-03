defmodule Mix.Tasks.Archive.Build do
  use Mix.Task

  @shortdoc "Archives this project into a .ez file"

  @moduledoc """
  Builds an archive according to the specification of the
  [Erlang Archive Format](http://www.erlang.org/doc/man/code.html).

  The archive will be created in the current directory (which is
  expected to be the project root), unless an argument `-o` is
  provided with the file name.

  Archives are meant to contain small projects, usually installed
  locally.  By default, this command archives the current project
  but the `-i` and `-o` options can be used to archive any directory.
  For example, `mix archive.build` with no options translates to:

      mix archive.build -i _build/ENV/lib/APP -o APP-VERSION.ez

  ## Command line options

    * `-o` - specifies output file name.
      If there is a `mix.exs`, defaults to "APP-VERSION.ez".

    * `-i` - specifies the input directory to archive.
      If there is a `mix.exs`, defaults to the current application build.

    * `--no-compile` - skips compilation.
      Only applies when `mix.exs` is available.

  """
  @switches [force: :boolean, compile: :boolean, output: :string, input: :string,
             deps_check: :boolean, archives_check: :boolean, elixir_version_check: :boolean]

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    {opts, _} = OptionParser.parse!(args, aliases: [o: :output, i: :input], strict: @switches)

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

    project_config = Mix.Project.config
    target = cond do
      output = opts[:output] ->
        output
      project_config[:app] ->
        Mix.Local.name_for(:archive, project_config)
      true ->
        Mix.raise "Cannot create archive without output file, " <>
          "please pass -o as an option"
    end

    unless File.dir?(source) do
      Mix.raise "Expected archive source #{inspect source} to be a directory"
    end

    create(source, target)

    Mix.shell.info "Generated archive #{inspect target} with MIX_ENV=#{Mix.env}"
    :ok
  end

  defp create(source, target) do
    source_path = Path.expand(source)
    target_path = Path.expand(target)
    dir = Mix.Local.archive_name(target_path) |> String.to_charlist
    {:ok, _} = :zip.create(String.to_charlist(target_path),
                  files_to_add(source_path, dir))
    :ok
  end

  defp files_to_add(path, dir) do
    File.cd! path, fn ->
      evsn = Path.wildcard(".elixir")
      ebin = Path.wildcard("ebin/*.{beam,app}")
      priv = Path.wildcard("priv/**/*")

      Enum.reduce evsn ++ ebin ++ priv, [], fn(f, acc) ->
        case File.read(f) do
          {:ok, bin} ->
            [{Path.join(dir, f) |> String.to_charlist, bin} | acc]
          {:error, _} ->
            acc
        end
      end
    end
  end
end
