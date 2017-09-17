# This module keeps a lock file and the manifest for the lock file.
# The lockfile keeps the latest dependency information while the
# manifest is used whenever a dependency is affected via any of the
# deps.* tasks. We also keep the Elixir version in the manifest file.
defmodule Mix.Dep.Lock do
  @moduledoc false

  @manifest ".compile.lock"

  @doc """
  Returns the manifest file for dependencies.

  The manifest is used to check if the lockfile
  itself is up to date.
  """
  def manifest(path \\ Mix.Project.manifest_path) do
    Path.join(path, @manifest)
  end

  @doc """
  Touches the manifest file to force recompilation.
  """
  def touch_manifest do
    path = Mix.Project.manifest_path
    File.mkdir_p!(path)
    File.touch!(manifest(path))
  end

  @doc """
  Reads the lockfile, returns a map containing
  each app name and its current lock information.
  """
  @spec read() :: map
  def read() do
    case File.read(lockfile()) do
      {:ok, info} ->
        assert_no_merge_conflicts_in_lockfile(lockfile(), info)
        case Code.eval_string(info, [], file: lockfile()) do
          {lock, _binding} when is_map(lock)  -> lock
          {_, _binding} -> %{}
        end
      {:error, _} ->
        %{}
    end
  end

  @doc """
  Receives a map and writes it as the latest lock.
  """
  @spec write(map) :: :ok
  def write(map) do
    unless map == read() do
      lines =
        for {app, rev} <- Enum.sort(map), rev != nil do
          ~s(  "#{app}": #{inspect rev, limit: :infinity},\n)
        end
      File.write! lockfile(), ["%{\n", lines, "}\n"]
      touch_manifest()
    end
    :ok
  end

  defp lockfile do
    Mix.Project.config[:lockfile]
  end

  defp assert_no_merge_conflicts_in_lockfile(lockfile, info) do
    if String.contains?(info, ~w(<<<<<<< ======= >>>>>>>)) do
      Mix.raise "Your #{lockfile} contains merge conflicts. Please resolve the conflicts " <>
                "and run the command again"
    end
  end
end
