defmodule Mix.Tasks.Format.Gen do
  use Mix.Task

  @shortdoc "Generates a .formatter.exs file in the current project"

  @path ".formatter.exs"

  @impl true
  def run(_) do
    cond do
      File.exists?(@path) -> Mix.raise(@path <> " already exists")
      File.exists?("apps/") -> File.write!(@path, Mix.Tasks.New.formatter_umbrella_template())
      true -> File.write!(@path, Mix.Tasks.New.formatter_template())
    end
  end
end
