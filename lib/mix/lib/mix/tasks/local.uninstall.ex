defmodule Mix.Tasks.Local.Uninstall do
  use Mix.Task

  def run(args) do
    IO.puts :stderr, "mix local.uninstall is deprecated, please use mix archive.uninstall instead"
    Mix.Task.run "archive.uninstall", args
  end
end
