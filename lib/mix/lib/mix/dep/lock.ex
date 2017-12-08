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
  def manifest(path \\ Mix.Project.manifest_path()) do
    Path.join(path, @manifest)
  end

  @doc """
  Touches the manifest file to force recompilation.
  """
  def touch_manifest do
    path = Mix.Project.manifest_path()
    File.mkdir_p!(path)
    File.touch!(manifest(path))
  end

  @doc """
  Reads the lockfile, returns a map containing
  each app name and its current lock information.
  """
  @spec read() :: map
  def read() do
    lockfile = lockfile()

    case File.read(lockfile) do
      {:ok, content} ->
        read_lock(content, lockfile)

      {:error, _} ->
        %{}
    end
  end

  @start "<<<<<<<"
  @separator "======="
  @finish ">>>>>>>"
  @start_regex ~r/#{@start} .+\n/
  @finish_regex ~r/#{@finish} .+\n/
  @ancestor_regex ~r/\|{7} .+\n[\w\W\s]+\n#{@separator}/
  @conflict_regex ~r/#{@start} .+\n([\w\W\s]+)\n#{@separator}\n([\w\W\s]+)\n#{@finish} .+\n/

  defp read_lock(content, lockfile) do
    if Regex.match?(@conflict_regex, content) do
      resolve_merge_conflicts(content, lockfile)
    else
      eval_content(content, lockfile)
    end
  end

  defp resolve_merge_conflicts(content, lockfile) do
    new_map =
      content
      |> String.replace(@ancestor_regex, @separator)
      |> String.replace(@start_regex, "")
      |> String.replace(@separator, "")
      |> String.replace(@finish_regex, "")
      |> eval_content(lockfile)

    write_lock(new_map, %{}, lockfile)

    new_map
  rescue
    SyntaxError ->
      Mix.raise("""
      Your #{lockfile} contains merge conflicts we cannot automatically solve.
      Please resolve the conflicts manually and run the command again
      """)
  end

  defp eval_content(content, lockfile) do
    case Code.eval_string(content, [], file: lockfile) do
      {lock, _binding} when is_map(lock) -> lock
      {_, _binding} -> %{}
    end
  end

  @doc """
  Receives a map and writes it as the latest lock.
  """
  @spec write(map) :: :ok
  def write(map) do
    write_lock(map, read(), lockfile())
  end

  defp write_lock(map, map, _lockfile) do
    :ok
  end

  defp write_lock(map, _old, lockfile) do
    lines =
      for {app, rev} <- Enum.sort(map), rev != nil do
        ~s(  "#{app}": #{inspect(rev, limit: :infinity)},\n)
      end

    File.write!(lockfile, ["%{\n", lines, "}\n"])
    touch_manifest()
  end

  defp lockfile do
    Mix.Project.config()[:lockfile]
  end
end
