defmodule Mix.Package do
  @moduledoc """
  Module responsible for managing [archive](http://www.erlang.org/doc/man/code.html#id102400) packages.

  An archive package is a zip file containing the app and beam files.
  A valid package must be named with the name of the application and it should contain the relative paths beginning with the application name, e.g. root of zip file should be my_application/ebin/Elixir.My.Application.beam.
  """

  @doc """
  Creates an application archive package from the current project.
  The path will be the root of the project and must contain an ebin folder.
  The archive file will be created in archive_path.
  """
  def create_package(project_path // ".", archive_path // ".") do
    project_path = Path.expand(project_path)
    archive_path = Path.expand(archive_path)
    archive_file = Path.join(archive_path, app_name(project_path) <> ".ez")
    {:ok, _ } = :zip.create(archive_file, files_to_add(project_path),
                [{:cwd, parent(project_path)},
                 {:uncompress, ['.beam', '.app']}])
  end

  defp app_name(path), do: Path.basename(path)

  defp parent(path) do
    Path.split(path) |> Enum.reverse
    |> Enum.drop(1) |> Enum.reverse |> Path.join
  end

  defp files_to_add(path) do
    ebin = Path.join([path, "ebin", "*.{beam,app}"]) |> Path.wildcard
    priv = Path.join([path, "priv", "*"]) |> Path.wildcard
    Enum.map(ebin ++ priv, fn(f) -> 
             Path.relative_to(f, parent(path)) |> binary_to_list  end)
  end
end
