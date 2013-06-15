defmodule Mix.Local do
  @moduledoc """
  Module responsible to manage local .mix installation.
  """

  @doc """
  The path for local tasks and archives.
  """
  def tasks_path do
    Path.join Mix.Utils.mix_home, "tasks"
  end

  @doc """
  Append local tasks and archives paths into Erlang code path.
  """
  def append_tasks do
    Enum.each(all_tasks_paths, Code.append_path(&1))
  end

  @doc """
  Append mix paths into Erlang code path.
  """
  def append_paths do
    Enum.each(Mix.Utils.mix_path, Code.append_path(&1))
  end

  @doc """
  Returns all tasks modules in ~/.mix/tasks and archives.
  """
  def all_tasks, do: Mix.Task.load_paths(all_tasks_paths)

  defp all_tasks_paths do
    [tasks_path | archives_paths]
  end

  defp archives_paths do
    Path.join(tasks_path, "*.ez") |> Path.wildcard |> Enum.map(archive_ebin(&1))
  end

  defp archive_ebin(archive_file) do
    app_name = archive_file |> Path.rootname |> Path.split |> List.last
    Path.join([archive_file, app_name, "ebin"])
  end
end
