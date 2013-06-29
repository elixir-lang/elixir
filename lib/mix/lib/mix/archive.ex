defmodule Mix.Archive do
  @moduledoc """
  Module responsible for managing [archives](http://www.erlang.org/doc/man/code.html).

  An archive is a zip file containing the app and beam files.
  A valid archive must be named with the name of the application and
  it should contain the relative paths beginning with the application
  name, e.g. root of zip file should be `my_app/ebin/Elixir.My.App.beam`.
  """

  @doc """
  Returns the archive name based on the app and version.
  """
  def name(app, nil), do: "#{app}.ez"
  def name(app, vsn), do: "#{app}-#{vsn}.ez"

  @doc """
  Returns the archive internal directory from its full path.
  """
  def dir(path) do
    path |> Path.basename |> Path.rootname
  end

  @doc """
  Creates an application archive from the current project.
  The path will be the root of the project and must contain an ebin folder.
  The archive file will be created in archive_path.
  """
  def create(archive_file, project_path // ".") do
    project_path = Path.expand(project_path)
    archive_file = Path.expand(archive_file)
    dir = dir(archive_file) |> :unicode.characters_to_list
    {:ok, _ } = :zip.create(archive_file,
                  files_to_add(project_path, dir),
                  uncompress: ['.beam', '.app'])
  end

  defp files_to_add(path, dir) do
    File.cd! path, fn ->
      ebin = Path.wildcard('ebin/*.{beam,app}')
      priv = Path.wildcard('priv/**/*')

      Enum.reduce ebin ++ priv, [], fn(f, acc) ->
        case File.read(f) do
          { :ok, bin } ->
            [{ Path.join(dir, f), bin }|acc]
          { :error, _ } ->
            acc
        end
      end
    end
  end
end
