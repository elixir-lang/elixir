defmodule Mix.Tasks.Run do
  use Mix.Task

  @shortdoc "Run the given expression"

  @moduledoc """
  Run the given expression in the application context.

  ## Examples

      mix run Hello.world
      mix run "Some.function with_args"

  ## Command line options

  * `-f`, `--file` - runs the given file / pattern instead
    of evaling code.

  """
  def run([tag, pattern|_]) when tag in ["-f", "--file"] do
    Mix.Task.run "compile"
    Enum.each File.wildcard(pattern), Code.require_file(&1)
  end

  def run(args) do
    Mix.Task.run "compile"
    Code.eval Enum.join(args, " ")
  end
end
