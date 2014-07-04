defmodule Mix.Tasks.Local.Install do
  use Mix.Task

  def run(args) do
    IO.puts :stderr, "mix local.install is deprecated, please use mix archive.install instead"
    Mix.Task.run "archive.install", args
  end
end
