defmodule Mix.Local do
  @moduledoc """
  Module responsible to manage local mix archives and paths.
  """

  @doc """
  The path for local archives.
  """
  def archives_path do
    Path.join Mix.Utils.mix_home, "archives"
  end

  @doc """
  Append archives paths into Erlang code path.
  """
  def append_archives do
    Enum.each(archives_ebin, Code.append_path(&1))
  end

  @doc """
  Append mix paths into Erlang code path.
  """
  def append_paths do
    Enum.each(Mix.Utils.mix_paths, Code.append_path(&1))
  end

  @doc """
  Returns all tasks in local archives.
  """
  def all_tasks, do: Mix.Task.load_tasks(archives_ebin)

  defp archives_ebin do
    Path.join(archives_path, "*.ez") |> Path.wildcard |> Enum.map(archive_ebin(&1))
  end

  defp archive_ebin(archive_file) do
    Path.join(archive_file,zip_ebin(archive_file))
  end

  #the inner app folder optionally includes the version according to Erlang spec
  #so just find the actual ebin folder in the zip file
  defp zip_ebin(zip_file) do
    {:ok, zip_list} = :zip.list_dir(binary_to_list(zip_file))
    Enum.find(zip_list, fn(entry) ->
      case entry do
        {:zip_file, file, _,_,_,_} -> Regex.match?(%r/\/ebin/, file)
        _ -> false
      end
    end) |> elem(1) |> Path.dirname
  end
end
