defmodule Mix.Tasks.Run do
  use Mix.Task

  @shortdoc "Run the given file or expression"

  @moduledoc """
  Runs the given file or expession in the context of the application.

  Before running the code, it invokes the app.start task
  which defaults to compile and load your project.

  ## Command line options

  * `--eval`, `-e` - Evaluates the given code
  * `--require`, `-r` - Requires pattern before running the command
  * `--parallel-require`, `-pr` - Requires pattern in parallel
  * `--no-halt` - Does not halt the system after running the command
  * `--no-compile` - Does not compile even if files require compilation
  * `--no-start` - Does not start applications after compilation

  ## Examples

      mix run -e Hello.world
      mix run -e "Some.function with_args"
      mix run -r some_file.exs

  Command line options given to the `elixir` executable can be passed as:

      elixir --sname hello -S mix run -e "My.code"

  """
  def run(args) do
    { opts, head } = OptionParser.parse_head(args,
      aliases: [r: :require, pr: :parallel_require, e: :eval],
      switches: [parallel_require: :keep, require: :keep])

    Mix.Task.run "app.start", args

    Enum.each opts, fn({ key, value }) ->
      case key do
        :parallel_require ->
          value |> filter_patterns |> Kernel.ParallelRequire.files
        :require ->
          value |> filter_patterns |> Enum.each Code.require_file(&1)
        :eval ->
          Code.eval_string(value)
        _ ->
          :ok
      end
    end

    if head != [] do
      Mix.shell.error "[WARNING] mix run EXPR is deprecated, please use mix run -e EXPR instead"
      Code.eval_string Enum.join(head, " ")
    end
    if opts[:no_halt], do: :timer.sleep(:infinity)
  end

  defp filter_patterns(pattern) do
    Enum.filter(Enum.uniq(Path.wildcard(pattern)), File.regular?(&1))
  end
end
