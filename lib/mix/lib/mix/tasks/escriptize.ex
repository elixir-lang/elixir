defmodule Mix.Tasks.Escriptize do
  use Mix.Task

  def run(args) do
    IO.puts :stderr, "mix escriptize is deprecated, please use mix escript.build instead"
    Mix.Task.run "escript.build", args
  end
end