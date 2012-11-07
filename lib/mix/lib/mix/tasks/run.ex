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
    { opts, head } = OptionParser.parse_head(args,
      aliases: [r: :require, pr: :parallel_require])

    Enum.each opts, fn({ key, value }) ->
      case key do
        :parallel_require ->
          value /> filter_patterns /> Kernel.ParallelRequire.files
        :require ->
          value /> filter_patterns /> Enum.each Code.require_file(&1)
        _ ->
          :ok
      end
    end

    Mix.Task.run Mix.project[:prepare_task], args
    Code.eval Enum.join(head, " ")
  end

  defp filter_patterns(pattern) do
    Enum.filter(List.uniq(File.wildcard(pattern)), File.regular?(&1))
  end
end
