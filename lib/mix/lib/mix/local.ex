defmodule Mix.Local do
  @moduledoc """
  Module responsible to manage local .mix installation.
  """

  @doc """
  The path for local tasks.
  """
  def tasks_path do
    Path.join Mix.Utils.mix_home, "tasks"
  end

  @doc """
  Append local tasks path into Erlang code path.
  """
  def append_tasks do
    Enum.each(all_local_paths,Code.append_path(&1))
  end

  @doc """
  Append mix paths into Erlang code path.
  """
  def append_paths do
    Enum.each Mix.Utils.mix_path, Code.append_path(&1)
  end

  @doc """
  Returns all tasks modules in .mix/tasks.
  """
  def all_tasks, do: Mix.Task.load_paths(all_local_paths)

  defp all_local_paths do
    [tasks_path | Path.join([Mix.Utils.mix_home, "tasks", "*.ez"])
                |> Path.wildcard
                |> Enum.map(package_code_path(&1))]
    |> Enum.map(binary_to_list(&1))
  end

  defp package_code_path(archive_file) do
    app_name = archive_file |> Path.rootname |> Path.split |> Enum.reverse |> Enum.first
    Path.join([archive_file, app_name, "ebin"])
  end
end
