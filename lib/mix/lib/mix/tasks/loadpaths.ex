defmodule Mix.Tasks.Loadpaths do
  use Mix.Task

  @moduledoc """
  Loads the application paths.

  This task is does not appear when `mix help` is invoked
  since it is hidden (it does not contain a shortdoc),
  however it can be freely invoked from other tasks.
  """

  def run(_) do
    path = Mix.Project.config[:compile_path] || "ebin"
    Code.prepend_path(path)
  end
end