defmodule Mix.Local do
  @moduledoc false

  @doc """
  The path for local archives.

  It checks the `MIX_ARCHIVES` variable or it uses the
  "archives" directory inside `Mix.Utils.mix_home/0`.

  Since archives are specific to Elixir versions, it is
  expected from build tools to swap the `MIX_ARCHIVES`
  variable to different locations based on a particular
  Elixir installation.
  """
  def archives_path do
    System.get_env("MIX_ARCHIVES") ||
      Path.join(Mix.Utils.mix_home, "archives")
  end

  @doc """
  Append archives paths into Erlang code path.
  """
  def append_archives do
    Enum.each(archives_ebin, &Code.append_path(&1))
  end

  @doc """
  Append mix paths into Erlang code path.
  """
  def append_paths do
    Enum.each(Mix.Utils.mix_paths, &Code.append_path(&1))
  end

  @doc """
  Returns all tasks in local archives.
  """
  def all_tasks, do: Mix.Task.load_tasks(archives_ebin)

  @doc """
  Returns paths of all archive files matching given
  application name.
  """
  def archive_files(name) do
    archives(name, ".ez") ++ archives(name, "-*.ez")
  end

  defp archives(name, suffix) do
    Mix.Local.archives_path
    |> Path.join(name <> suffix)
    |> Path.wildcard
  end

  defp archives_ebin do
    Path.join(archives_path, "*.ez") |> Path.wildcard |> Enum.map(&Mix.Archive.ebin/1)
  end
end
