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

  ## Command line options

  * `--force` - forces compilation regardless of module times;
  * `--quick`, `-q` - only compile files that changed;
  * `--no-compile` - do not compile even if files require compilation;
  * `--no-start` - do not start applications after compilation;

  ## Configuration

  * `:test_paths` - path containing tests.
    Defaults to `["test"]`.

  * `:test_pattern` - a pattern to load test files.
    Defaults to `*_test.exs`.

  * `:test_helper` - a file that sets up whatever is necessary
    for testing. Defaults to `test/test_helper.exs`.

  """
  def run(args) do
    { _, files } = OptionParser.parse(args, aliases: [q: :quick],
                     switches: [quick: :boolean, force: :boolean])

    unless System.get_env("MIX_ENV") do
      Mix.env(:test)
      Mix.Project.refresh
    end

    Mix.Task.run Mix.project[:prepare_task], args
    project = Mix.project

    test_helper = project[:test_helper] || "test/test_helper.exs"
    test_helper && Code.require_file(test_helper)

    test_paths   = if files == [], do: project[:test_paths] || ["test"], else: files
    test_pattern = project[:test_pattern] || "*_test.exs"

    files = Mix.Utils.extract_files(test_paths, test_pattern)
    Kernel.ParallelRequire.files files
  end
end
