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

  @conflict_delimiters ["<<<<<<<", "=======", ">>>>>>>"]
  @conflict_regex ~r/<{7} .+\n|\|{7} .+\n[\w\W\s]+={7}\n|={7}\n|>{7} .+\n/

  defp read_lock(content, lockfile) do
    if String.contains?(content, @conflict_delimiters) do
      case content |> String.replace(@conflict_regex, "") |> eval_content(lockfile) do
        {:ok, new_map} ->
          write_lock(new_map, %{}, lockfile)
          new_map

        :error ->
          Mix.raise("""
          Your #{lockfile} contains merge conflicts we cannot automatically solve.
          Please resolve the conflicts manually and run the command again
          """)
      end
    else
      case eval_content(content, lockfile) do
        {:ok, map} -> map
        :error -> Mix.raise("Could not read #{lockfile} due to a syntax error")
      end
    end
  end

  defp eval_content(content, lockfile) do
    {lock, _binding} = Code.eval_string(content, [], file: lockfile)
    if is_map(lock), do: {:ok, lock}, else: {:ok, %{}}
  rescue
    SyntaxError -> :error
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
