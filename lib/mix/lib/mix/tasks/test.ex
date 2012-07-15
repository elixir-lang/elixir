defmodule Mix.Tasks.Test do
  use Mix.Task

  @shortdoc "Run a project's tests"

  @moduledoc """
  Run the tests for a project.

  This task will preload the `test/test_helper.exs` which
  should do all testing setup and then require all files
  that matches the given `test_pattern` in parallel.

  It ensures the project is compiled before executing.

  ## Configuration

  * `:test_pattern` - a pattern to load test files.
    Defaults to `test/**/*_test.exs`.

  * `:test_helper` - a file that sets up whatever is necessary
  for testing. Defaults to `test/test_helper.exs`.
  
  """
  def run(_) do
    Mix.Task.run "compile"
    project = Mix.Project.config

    test_pattern = project[:test_pattern] || "test/**/*_test.exs"
    test_helper  = project[:test_helper]  || "test/test_helper.exs"

    Code.require_file test_helper
    Elixir.ParallelRequire.files File.wildcard(test_pattern)
  end
end
