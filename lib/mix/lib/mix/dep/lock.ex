# This module keeps a lock file and the manifest for the lock file.
# The lockfile keeps the latest dependency information while the
# manifest is used whenever a dependency is affected via any of the
# deps.* tasks. We also keep the Elixir version in the manifest file.
defmodule Mix.Dep.Lock do
  @moduledoc false

  @doc """
  Reads the lockfile, returns a map containing
  each app name and its current lock information.
  """
  @spec read() :: map
  def read() do
    lockfile = lockfile()
    opts = [file: lockfile, warn_on_unnecessary_quotes: false]

    with {:ok, contents} <- File.read(lockfile),
         assert_no_merge_conflicts_in_lockfile(lockfile, contents),
         {:ok, quoted} <- Code.string_to_quoted(contents, opts),
         {%{} = lock, _binding} <- Code.eval_quoted(quoted, opts) do
      lock
    else
      _ -> %{}
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
          ~s(  "#{app}": #{inspect(rev, limit: :infinity)},\n)
        end

      File.write!(lockfile(), ["%{\n", lines, "}\n"])
      Mix.Task.run("will_recompile")
    end

    :ok
  end

  defp lockfile do
    Mix.Project.config()[:lockfile]
  end

  defp assert_no_merge_conflicts_in_lockfile(lockfile, info) do
    if String.contains?(info, ~w(<<<<<<< ======= >>>>>>>)) do
      Mix.raise(
        "Your #{lockfile} contains merge conflicts. Please resolve the conflicts " <>
          "and run the command again"
      )
    end
  end
end
