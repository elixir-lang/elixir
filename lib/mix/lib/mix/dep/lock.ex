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
  Touches the manifest timestamp unless it is an umbrella application.
  """
  @spec touch() :: :ok
  def touch() do
    _ = unless Mix.Project.umbrella?, do: touch(Mix.Project.manifest_path)
    :ok
  end

  @doc """
  Touches the manifest timestamp and updates the elixir version.
  """
  @spec touch(Path.t) :: :ok
  def touch(manifest_path) do
    File.mkdir_p!(manifest_path)
    File.write!(Path.join(manifest_path, @manifest), System.version)
  end

  @doc """
  Returns the elixir version in the lock manifest.
  """
  @spec elixir_vsn() :: binary | nil
  def elixir_vsn() do
    elixir_vsn(Mix.Project.manifest_path)
  end

  @doc """
  Returns the elixir version in the lock manifest in the given path.
  """
  @spec elixir_vsn(Path.t) :: binary | nil
  def elixir_vsn(manifest_path) do
    case File.read(manifest(manifest_path)) do
      {:ok, contents} ->
        contents
      {:error, _} ->
        nil
    end
  end

  @doc """
  Read the lockfile, returns a map containing
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
