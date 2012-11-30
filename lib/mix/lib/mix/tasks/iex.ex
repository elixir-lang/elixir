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
    case IEx.release do
      :ok ->
        :timer.sleep(:infinity)
      :error ->
        raise Mix.Error, message: "could not start IEx. Due to booting constraints, " <>
          "IEx needs to be started on its own, like `mix iex` and it cannot be mixed " <>
          "with other tasks as in `mix do compile, iex`"
    end
  end
end
