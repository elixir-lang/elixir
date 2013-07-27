defmodule Mix.Tasks.Test do
  defmodule Cover do
    @moduledoc false

    def start(compile_path, opts) do
      IO.write "Cover compiling modules ... "
      :cover.start
      :cover.compile_beam_directory(compile_path |> to_char_list)
      IO.puts "ok"

      output = opts[:output]

      System.at_exit fn(_) ->
        IO.write "\nGenerating cover results ... "
        File.mkdir_p!(output)
        Enum.each :cover.modules, fn(mod) ->
          :cover.analyse_to_file(mod, '#{output}/#{mod}.html', [:html])
        end
        IO.puts "ok"
      end
    end
  end

  use Mix.Task

  @shortdoc "Run a project's tests"
  @recursive true

  @moduledoc """
  Run the tests for a project.

  This task starts the current application, loads up
  `test/test_helper.exs` and then requires all files matching the
  `test/**/_test.exs` pattern in parallel.

  A list of files can be given after the task name in order to select
  the files to compile:

      mix test test/some/particular/file_test.exs

  ## Command line options

  * `--trace` - run tests with detailed reporting. Automatically sets `--max-cases` to 1
  * `--max-cases` - set the maximum number of cases running async
  * `--cover` - the directory to include coverage results
  * `--force` - forces compilation regardless of modification times
  * `--no-compile` - do not compile, even if files require compilation
  * `--no-start` - do not start applications after compilation
  * `--no-color` - disable color in the output

  ## Configuration

  * `:test_paths` - list of paths containing test files, defaults to `["test"]`.
                    it is expected all test paths to contain a `test_helper.exs` file

  * `:test_pattern` - a pattern to load test files, defaults to `*_test.exs`

  * `:test_coverage` - a set of options to be passed down to the coverage mechanism

  ## Coverage

  The `:test_coverage` configuration accepts the following options:

  * `:output` - the output for cover results, defaults to `"cover"`
  * `:tool`   - the coverage tool

  By default, a very simple wrapper around OTP's `cover` is used as a tool,
  but it can be overriden as follows:

      test_coverage: [tool: CoverModule]

  `CoverModule` can be any module that exports `start/2`, receiving the
  compilation path and the `test_coverage` options as arguments.
  """

  @switches [force: :boolean, color: :boolean,
             trace: :boolean, max_cases: :integer]

  @cover [output: "cover", tool: Cover]

  def run(args) do
    { opts, files } = OptionParser.parse(args, switches: @switches)

    unless System.get_env("MIX_ENV") || Mix.env == :test do
      raise Mix.Error, message: "mix test is running on environment #{Mix.env}. If you are " <>
                                "running tests along another task, please set MIX_ENV explicitly"
    end

    Mix.Task.run "app.start", args

    project = Mix.project
    cover   = project[:test_coverage] || []

    if is_binary(cover) do
      IO.puts "[WARNING] test_coverage: \"PATH\" is deprecated, " <>
              "please use test_coverage: [output: \"PATH\"] instead"
      cover = [output: cover]
    end

    cover = Keyword.merge(@cover, cover)

    case opts[:cover] do
      _ in ["true", true] ->
        cover[:tool].start(project[:compile_path], cover)
      nil ->
        :ok
      _ ->
        IO.puts "[WARNING] --cover PATH is deprecated, " <>
                "please set the output directory as test_coverage: [output: \"PATH\"] instead"
    end

    :application.load(:ex_unit)
    ExUnit.configure(Dict.take(opts, [:trace, :max_cases, :color]))

    test_paths = project[:test_paths] || ["test"]
    Enum.each(test_paths, require_test_helper(&1))

    test_paths   = if files == [], do: test_paths, else: files
    test_pattern = project[:test_pattern] || "*_test.exs"

    files = Mix.Utils.extract_files(test_paths, test_pattern)
    Kernel.ParallelRequire.files files
  end

  defp require_test_helper(dir) do
    file = Path.join(dir, "test_helper.exs")

    if File.exists?(file) do
      Code.require_file file
    else
      raise Mix.Error, message: "Cannot run tests because test helper file #{inspect file} does not exist"
    end
  end
end
