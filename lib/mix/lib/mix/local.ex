defmodule Mix.Local do
  @moduledoc false

  @doc """
  The path for local archives.

  Check `mix archive` for info.
  """
  def archives_path do
    System.get_env("MIX_ARCHIVES") ||
      Path.join(Mix.Utils.mix_home, "archives")
  end

  @doc """
  Append archives paths into Erlang code path.
  """
  def append_archives do
    archives = archives_ebin()
    Enum.each(archives, &check_elixir_version_in_ebin/1)
    Enum.each(archives, &Code.append_path/1)
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
  def all_tasks do
    Mix.Task.load_tasks(archives_ebin())
  end

  @doc """
  Returns paths of all archive files matching given
  application name.
  """
  def archive_files(name) do
    archives(name, ".ez") ++ archives(name, "-*.ez")
  end

  @doc """
  Check Elixir version requirement stored in the archive and print a warning if it is not satisfied.
  """
  def check_archive_elixir_version(path) do
    path |> Mix.Archive.ebin |> check_elixir_version_in_ebin()
  end

  defp archives(name, suffix) do
    archives_path()
    |> Path.join(name <> suffix)
    |> Path.wildcard
  end

  defp archives_ebin do
    Path.join(archives_path(), "*.ez") |> Path.wildcard |> Enum.map(&Mix.Archive.ebin/1)
  end

  defp check_elixir_version_in_ebin(ebin) do
    elixir = ebin |> Path.dirname |> Path.join(".elixir") |> String.to_char_list
    case :erl_prim_loader.get_file(elixir) do
      {:ok, req, _} ->
        unless Version.match?(System.version, req) do
          archive = ebin |> Path.dirname |> Path.basename
          Mix.shell.error "warning: the archive #{archive} requires Elixir #{inspect req} " <>
                          "but you are running on v#{System.version}"
        end
        :ok
      :error ->
        :ok
    end
  end
end
