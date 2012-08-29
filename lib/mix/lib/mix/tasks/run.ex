defmodule Mix.Tasks.Run do
  use Mix.Task

  @shortdoc "Run the given expression"

  @moduledoc """
  Run the given expression in the application context.

  ## Examples

      mix run Hello.world
      mix run "Some.function with_args"

  """
  def run(args) do
    Mix.Task.run "compile"
    Code.eval Enum.join(args, " ")
  end
end
