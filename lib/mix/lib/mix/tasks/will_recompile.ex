defmodule Mix.Tasks.WillRecompile do
  use Mix.Task

  @moduledoc false
  @recursive true
  @manifest "compile.lock"

  @doc """
  Returns the will_recompile manifest for the project.
  """
  def manifest(path \\ Mix.Project.manifest_path()) do
    Path.join(path, @manifest)
  end

  @doc """
  Annotates the current project will recompile.
  """
  def run(_) do
    path = Mix.Project.manifest_path()
    File.mkdir_p!(path)
    File.touch!(manifest(path))
  end
end
