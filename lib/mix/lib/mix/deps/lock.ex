# This module keeps a lock file and the manifest for the lock file.
# The lockfile keeps the latest dependency information while the
# manifest is used whenever a dependency is affected via any of the
# deps.* tasks. We also keep the Elixir version in the manifest file.
defmodule Mix.Deps.Lock do
  @moduledoc false
  @manifest ".compile.lock"

  @doc """
  Returns the manifest file for dependencies.
  """
  def manifest(manifest_path // Mix.Project.manifest_path) do
    Path.join(manifest_path, @manifest)
  end

  @doc """
  Touches the manifest timestamp and updates the elixir version.
  """
  def touch(manifest_path // Mix.Project.manifest_path) do
    File.mkdir_p!(manifest_path)
    File.write!(Path.join(manifest_path, @manifest), "#{System.version}\n#{Mix.env}")
  end

  @doc """
  Returns the elixir version in the lock manifest.
  """
  def elixir_vsn(manifest_path // Mix.Project.manifest_path) do
    read_manifest(manifest_path) |> elem(0)
  end

  @doc """
  Returns the mix env version in the lock manifest.
  """
  def mix_env(manifest_path // Mix.Project.manifest_path) do
    read_manifest(manifest_path) |> elem(1)
  end

  defp read_manifest(manifest_path) do
    case File.read(manifest(manifest_path)) do
      { :ok, contents } ->
        destructure [vsn, env], String.split(contents, "\n")
        { vsn, env }
      { :error, _ } ->
        { nil, nil }
    end
  end

  @doc """
  Read the lockfile, returns a keyword list containing
  each app name and its current lock information.
  """
  def read() do
    case File.read(lockfile) do
      { :ok, info } ->
        { value, _binding } = Code.eval_string(info)
        value || []
      { :error, _ } ->
        []
    end
  end

  @doc """
  Receives a keyword list and writes it as the latest lock.
  """
  def write(dict) do
    sorted = lc { app, rev } inlist Enum.sort(dict), rev != nil, do: { app, rev }

    unless sorted == read do
      lines  = Enum.map_join sorted, ",\n  ", fn { app, rev } ->
        %s("#{app}": #{inspect rev, raw: true, limit: :infinity})
      end

      File.write! lockfile, "[ " <> lines <> " ]\n"
      touch
    end
  end

  defp lockfile do
    Mix.project[:lockfile]
  end
end