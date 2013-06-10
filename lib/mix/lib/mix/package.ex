defmodule Mix.Package do
  @moduledoc """
  Module responsible for managing [archive](http://www.erlang.org/doc/man/code.html#id102400) packages.

  An archive package is a zip file containing the app and beam files.
  A valid package must be named with the name of the application and it should contain the relative paths beginning with the application name, e.g. root of zip file should be my_application/ebin/Elixir.My.Application.beam.
  """

  @doc """
  Creates an application archive package from the current project.
  The archive file will be created in path and named <application>.ez
  """
  def create_package(path) do
    if app_name_matches_cwd do
      package_name = Path.join(path, app_name <> ".ez")
      {:ok, _ } = :zip.create(package_name, files_to_add,
                  [{:cwd, Path.expand('..')},
                   {:uncompress, ['.beam', '.app']}])
    else
      IO.puts "This task must be run from the root directory of your project, and that directory name must match the app name specified in mix.exs."
    end
  end

  defp app_name_matches_cwd do
    Path.expand(".") |> Path.split |> Enum.reverse |> Enum.first == app_name
  end

  defp files_to_add do
    Path.join([compile_path, "*.{beam,app}"])
    |> Path.wildcard
    |> Enum.map(fn(f) -> Path.join(app_name,f) |> binary_to_list end)
  end

  defp compile_path, do: Mix.project[:compile_path] || "ebin"
  defp app_name, do: Mix.project[:app] |> atom_to_binary

  @doc """
  Returns a list of the beams contained in the archive file.
  """
  def package_beams(archive_file) when is_binary(archive_file) do
    binary_to_list(archive_file) |> package_beams |> Enum.map(list_to_binary &1)
  end
  def package_beams(archive_file) when is_list(archive_file) do
    zip_file_names(archive_file)
    |> Enum.reduce([],fn(file, files) ->
       case Regex.captures(%r/(?<path>.*)\.beam/g, file)[:path] do
         nil -> files
         file -> [binary_to_list(file) | files]
       end
    end)
  end

  defp zip_file_names(zip_file) do
    {:ok, zip_list} = :zip.list_dir(zip_file)
    Enum.reduce(zip_list, [], fn(entry, files) ->
      case entry do
        {:zip_file, file, _,_,_,_} -> [list_to_binary(file) | files]
        _ -> files
      end
    end)
  end
end
