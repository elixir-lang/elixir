defmodule Mix.Tasks.Iex do
  use Mix.Task

  @shortdoc "Start IEx with your project's settings"

  @moduledoc """
  Starts an iex repl with your project settings.

  Before starting IEx, it invokes the prepare task
  which defaults to compile and load your project.
  """
  def run(_) do
    Mix.Task.run Mix.project[:prepare_task]
    IEx.run
    :timer.sleep(:infinity)
  end
end
