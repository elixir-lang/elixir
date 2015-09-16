# This module keeps a lock file and the manifest for the lock file.
# The lockfile keeps the latest dependency information while the
# manifest is used whenever a dependency is affected via any of the
# deps.* tasks. We also keep the Elixir version in the manifest file.
defmodule Mix.Dep.Lock do
  @moduledoc false
  @manifest ".compile.lock"

  @doc """
  Returns the manifest file for dependencies.
  """
  @spec manifest(Path.t) :: Path.t
  def manifest(manifest_path \\ Mix.Project.manifest_path) do
    Path.join(manifest_path, @manifest)
  end

  @doc """
  Touches the manifest storing the current project info unless it is an umbrella application.
  """
  @spec touch :: :ok
  def touch do
    config = Mix.Project.config
    unless Mix.Project.umbrella?(config) do
      manifest_path = Mix.Project.manifest_path(config)
      data = {:v1, System.version, config[:build_scm]}
      File.mkdir_p!(manifest_path)
      File.write!(Path.join(manifest_path, @manifest), :io_lib.format('~p.~n', [data]))
    end
    :ok
  end

  @doc """
  Returns the manifest status with Elixir version and scm.
  """
  @spec status(Path.t) :: {:ok, vsn :: String.t, scm :: atom} | :error
  def status(manifest_path \\ Mix.Project.manifest_path) do
    case :file.consult(manifest(manifest_path)) do
      {:ok, [{:v1, vsn, scm}]} -> {:ok, vsn, scm}
      {:error, {_, :erl_parse, _}} -> {:ok, "1.0.0", nil} # Force old version if file exists but old format
      _ -> :error
    end
  end

  @doc """
  Reads the lockfile, returns a map containing
  each app name and its current lock information.
  """
  @spec read() :: map
  def read() do
    case File.read(lockfile) do
      {:ok, info} ->
        case Code.eval_string(info, [], file: lockfile) do
          # lock could be a keyword list
          {lock, _binding} when is_list(lock) -> Enum.into(lock, %{})
          {lock, _binding} when is_map(lock)  -> lock
          {nil, _binding}                     -> %{}
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
    unless map == read do
      lines =
        for {app, rev} <- Enum.sort(map), rev != nil do
          ~s("#{app}": #{inspect rev, limit: :infinity})
        end
      File.write! lockfile, "%{" <> Enum.join(lines, ",\n  ") <> "}\n"
      touch
    end
    :ok
  end

  defp lockfile do
    Mix.Project.config[:lockfile]
  end
end
