defmodule Mix.Archive do
  @moduledoc """
  Module responsible for managing [archives](http://www.erlang.org/doc/man/code.html).

  An archive is a zip file containing the app and beam files.
  A valid archive must be named with the name of the application and
  it should contain the relative paths beginning with the application
  name, e.g. root of zip file should be `my_app/ebin/Elixir.My.App.beam`.
  """

  @doc """
  Creates an application archive from the current project.
  The path will be the root of the project and must contain an ebin folder.
  The archive file will be created in archive_path.
  """
  def create(project_path // ".", archive_file) do
    project_path = Path.expand(project_path)
    archive_file = Path.expand(archive_file)
    opts = [ cwd: Path.dirname(project_path),
             uncompress: ['.beam', '.app'] ]
    {:ok, _ } = :zip.create(archive_file, files_to_add(project_path), opts)
  end


  defp files_to_add(path) do
    ebin = Path.join([path, "ebin", "*.{beam,app}"]) |> Path.wildcard
    priv = Path.join([path, "priv", "*"]) |> Path.wildcard
    Enum.map(ebin ++ priv, fn(f) ->
      Path.relative_to(f, Path.dirname(path)) |> :unicode.characters_to_list
    end)
  end
end
