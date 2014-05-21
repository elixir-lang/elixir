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

  * `--eval`, `-e` - evaluate the given code
  * `--require`, `-r` - require pattern before running the command
  * `--parallel-require`, `-pr` - requires pattern in parallel
  * `--no-compile` - do not compile even if files require compilation
  * `--no-deps-check` - do not check dependencies
  * `--no-halt` - do not halt the system after running the command
  * `--no-start` - do not start applications after compilation

  """
  def run(args) do
    {opts, head, _} = OptionParser.parse_head(args,
      aliases: [r: :require, pr: :parallel_require, e: :eval],
      switches: [parallel_require: :keep, require: :keep, eval: :keep])

    # Require the project to be available
    Mix.Project.get!

    # Check if there is actually a file to run
    run_file? = not match?({_, [], _}, OptionParser.parse_head(head))

    {file, argv} =
      case {Keyword.has_key?(opts, :eval), run_file?, head} do
        {true, _, _}      -> {nil, head}
        {_, true, [h|t]}  -> {h, t}
        {_, _, rest}      -> {nil, rest}
      end

    System.argv(argv)

    # Start app after rewriting System.argv,
    # but before requiring and evaling
    Mix.Task.run "app.start", args

    Enum.each opts, fn({key, value}) ->
      case key do
        :parallel_require ->
          case filter_patterns(value) do
            [] ->
              report_error("parallel-require: No files matched pattern #{value}")

            filtered ->
              Kernel.ParallelRequire.files(filtered)
          end

        :require ->
          case filter_patterns(value) do
            [] ->
              report_error("require: No files matched pattern #{value}")

            filtered ->
              Enum.each(filtered, &Code.require_file(&1))
          end

        :eval ->
          Code.eval_string(value)
        _ ->
          :ok
      end
    end

    if file do
      if File.regular?(file) do
        Code.require_file(file)
      else
        report_error("No such file: #{file}")
      end
    end
    if opts[:no_halt], do: :timer.sleep(:infinity)
  end

  defp filter_patterns(pattern) do
    Enum.filter(Enum.uniq(Path.wildcard(pattern)), &File.regular?(&1))
  end

  defp report_error(msg) do
    raise Mix.Error, message: msg
  end
end
