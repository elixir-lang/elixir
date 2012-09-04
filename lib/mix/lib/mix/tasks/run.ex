defmodule Mix.Tasks.Run do
  use Mix.Task

  @shortdoc "Run the given expression"

  @moduledoc """
  Run the given expression in the application context.

  Before running the code, it invokes the prepare task
  which defaults to compile and load your project.

  ## Examples

      mix run Hello.world
      mix run "Some.function with_args"

  """
  def run(args) do
    Mix.Task.run Mix.project[:prepare_task]
    Code.eval Enum.join(args, " ")
  end
end
