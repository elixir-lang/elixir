defmodule Mix.Tasks.Run do
  use Mix.Task

  @shortdoc "Run the given file or expression"

  @moduledoc """
  Runs the given file or expression in the context of the application.

  Before running the code, it invokes the `app.start` task which compiles
  and loads your project.

  It is the goal of this task to provide a subset of the functionality
  existent in the `elixir` executable, including setting up the `System.argv`:

      mix run -e Hello.world
      mix run my_script.exs arg1 arg2 arg3

  Many command line options need to be passed to the `elixir` executable
  directly, which can be done as follows:

      elixir --sname hello -S mix run -e "My.code"

  ## Command line options

  * `--eval`, `-e` - Evaluates the given code
  * `--require`, `-r` - Requires pattern before running the command
  * `--parallel-require`, `-pr` - Requires pattern in parallel
  * `--no-halt` - Does not halt the system after running the command
  * `--no-compile` - Does not compile even if files require compilation
  * `--no-start` - Does not start applications after compilation

  """
  def run(args) do
    { opts, head } = OptionParser.parse_head(args,
      aliases: [r: :require, pr: :parallel_require, e: :eval],
      switches: [parallel_require: :keep, require: :keep])

    # Require the project to be available
    Mix.Project.get!

    Mix.Task.run "app.start", args

    file =
      case head do
        ["--"|t] -> System.argv(t); nil
        [h|t]    -> System.argv(t); h
        []       -> System.argv([]); nil
      end

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

    if file, do: Code.require_file(h)
    if opts[:no_halt], do: :timer.sleep(:infinity)
  end

  defp filter_patterns(pattern) do
    Enum.filter(Enum.uniq(Path.wildcard(pattern)), File.regular?(&1))
  end
end
