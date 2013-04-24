defmodule Mix.Tasks.Run do
  use Mix.Task

  @shortdoc "Run the given expression"

  @moduledoc """
  Run the given expression in the application context.

  Before running the code, it invokes the prepare task
  which defaults to compile and load your project.

  ## Command line options

  * `--require`, `-r` - Requires a file before running the command
  * `--parallel-require`, `-pr` - Requires a file in parallel
  * `--no-halt` - Does not halt the system after running the command
  * `--no-compile` - Do not compile even if files require compilation;
  * `--no-start` - Do not start applications after compilation;

  ## Examples

      mix run Hello.world
      mix run "Some.function with_args"

  """
  def run(args) do
    { opts, head } = OptionParser.parse_head(args,
      aliases: [r: :require, pr: :parallel_require],
      switches: [parallel_require: :keep, require: :keep])

    Enum.each opts, fn({ key, value }) ->
      case key do
        :parallel_require ->
          value |> filter_patterns |> Kernel.ParallelRequire.files
        :require ->
          value |> filter_patterns |> Enum.each Code.require_file(&1)
        _ ->
          :ok
      end
    end

    Mix.Task.run "app.start", args
    if head != [], do: Code.eval Enum.join(head, " ")
    if opts[:no_halt], do: :timer.sleep(:infinity)
  end

  defp filter_patterns(pattern) do
    Enum.filter(Enum.uniq(Path.wildcard(pattern)), File.regular?(&1))
  end
end
