defmodule Mix.Tasks.Test do
  use Mix.Task

  @shortdoc "Run a project's tests"

  @moduledoc """
  Run the tests for a project.

  This task will preload the `test/test_helper.exs` which
  should do all testing setup and then require all files
  that matches the given `test_pattern` in parallel.

  Before running tests, it invokes the prepare task
  which defaults to compile and load your project.

  A list of files can be given after the task name in
  order to select the files to compile.

  ## Configuration

  * `:test_pattern` - a pattern to load test files.
    Defaults to `test/**/*_test.exs`.

  * `:test_helper` - a file that sets up whatever is necessary
  for testing. Defaults to `test/test_helper.exs`.

  """
  def run(args) do
    { _, files } = OptionParser.parse(args)

    Mix.Task.run Mix.project[:prepare_task]
    project = Mix.project

    test_helper = Keyword.get(project, :test_helper, "test/test_helper.exs")
    test_helper && Code.require_file(test_helper)

    files =
      if files == [] do
        test_pattern = project[:test_pattern] || "test/**/*_test.exs"
        File.wildcard test_pattern
      else
        files
      end

    Kernel.ParallelRequire.files files
  end
end
