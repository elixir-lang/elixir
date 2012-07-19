defmodule Mix.Tasks.Run do
  use Mix.Task

  @shortdoc "Run the given expression"

  @moduledoc """
  Run the given expression in the application context.

  ## Examples

      mix run Hello.world

  """
  def run(args) do
    code = Enum.filter(args, fn(x) -> not match?("-" <> _, x) end)
    Mix.Task.run "compile"
    Code.eval Enum.join(code, " ")
  end
end
