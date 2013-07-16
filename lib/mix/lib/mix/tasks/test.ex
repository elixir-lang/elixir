defmodule Mix.Tasks.Test do
  use Mix.Task

  @shortdoc "Run a project's tests"
  @recursive true

  @moduledoc """
  Run the tests for a project.

  This task will preload the `test/test_helper.exs` which
  should do all testing setup and then require all files
  that match the given `test_pattern` in parallel.

  Before running tests, it invokes the `App.Start` task
  which defaults to compile and load your project.

  A list of files can be given after the task name in
  order to select the files to compile.

  ## Command line options

  * `--trace` - run tests with detailed reporting. Automatically sets `max-cases` to 1;
  * `--max-cases` - set the maximum number of cases running async;
  * `--cover` - the directory to include coverage results;
  * `--force` - forces compilation regardless of module times;
  * `--quick`, `-q` - only compile files that changed;
  * `--no-compile` - do not compile even if files require compilation;
  * `--no-start` - do not start applications after compilation;

  ## Configuration

  * `:test_paths` - list of paths containing test files.
    Defaults to `["test"]`.

  * `:test_pattern` - a pattern to load test files.
    Defaults to `*_test.exs`.

  * `:test_coverage` - the directory to include test coverage results.
    Defaults to `nil`.

  """

  @switches [quick: :boolean, force: :boolean,
             trace: :boolean, max_cases: :integer]

  def run(args) do
    { opts, files } = OptionParser.parse(args, aliases: [q: :quick], switches: @switches)

    unless System.get_env("MIX_ENV") do
      Mix.env(:test)
      Mix.Project.refresh
    end

    Mix.Task.run "app.start", args
    project = Mix.project

    cover = Keyword.get(project, :test_coverage, opts[:cover])
    if cover, do: enable_cover(project, cover)

    test_paths   = if files == [], do: project[:test_paths] || ["test"], else: files
    test_pattern = project[:test_pattern] || "*_test.exs"

    :application.load(:ex_unit)
    ExUnit.configure(Dict.take(opts, [:trace, :max_cases]))

    files = Mix.Utils.extract_files(test_paths, test_pattern)
    Kernel.ParallelRequire.files files
  end

  defp enable_cover(project, cover) do
    IO.write "Cover compiling modules ... "
    :cover.start
    :cover.compile_beam_directory(project[:compile_path] |> to_char_list)
    IO.puts "ok"

    System.at_exit fn(_) ->
      IO.write "Generating cover results ... "
      File.mkdir_p!(cover)
      Enum.each :cover.modules, fn(mod) ->
        :cover.analyse_to_file(mod, '#{cover}/#{mod}.html', [:html])
      end
      IO.puts "ok"
    end
  end
end
