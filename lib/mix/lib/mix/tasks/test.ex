defmodule Mix.Tasks.Test do
  defmodule Cover do
    @moduledoc false

    def start(compile_path, opts) do
      Mix.shell.info "Cover compiling modules ... "
      :cover.start
      :cover.compile_beam_directory(compile_path |> to_char_list)

      if :application.get_env(:cover, :started) != { :ok, true } do
        output = opts[:output]

        System.at_exit fn(_) ->
          Mix.shell.info "\nGenerating cover results ... "
          File.mkdir_p!(output)
          Enum.each :cover.modules, fn(mod) ->
            :cover.analyse_to_file(mod, '#{output}/#{mod}.html', [:html])
          end
        end

        :application.set_env(:cover, :started, true)
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
  * `--include` - include tests that match the filter
  * `--exclude` - exclude tests that match the filter
  * `--only` - run only tests that match the filter

  ## Filters

  ExUnit provides tags and filtering functionality that allows developers
  to select which tests to run. The most common functionality is to exclude
  some particular tests from running by default in your test helper file:

      # Exclude all external tests from running
      ExUnit.configure exclude: [external: true]

  Then, whenever desired, those tests could be included in the run via the
  `--include` flag:

      mix test --include external:true

  The example above will run all tests that have the external flag set to
  true. It is also possible to include all examples that have a given tag,
  regardless of its value:

      mix test --include external

  Note that all tests are included by default, so unless they are excluded
  first (either in the test helper or via the `--exclude` option), the
  `--include` flag has no effect.

  For this reason, mix also provides an `--only` option that excludes all
  tests and includes only the given ones:

      mix test --only external

  Which is equivalent to:

      mix test --include external --exclude test

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
  but it can be overridden as follows:

      test_coverage: [tool: CoverModule]

  `CoverModule` can be any module that exports `start/2`, receiving the
  compilation path and the `test_coverage` options as arguments.
  """

  @switches [force: :boolean, color: :boolean, cover: :boolean,
             trace: :boolean, max_cases: :integer, include: :keep,
             exclude: :keep, seed: :integer]

  @cover [output: "cover", tool: Cover]

  def run(args) do
    { opts, files, _ } = OptionParser.parse(args, switches: @switches)

    unless System.get_env("MIX_ENV") || Mix.env == :test do
      raise Mix.Error, message: "mix test is running on environment #{Mix.env}. If you are " <>
                                "running tests along another task, please set MIX_ENV explicitly"
    end

    Mix.Task.run "app.start", args

    project = Mix.project
    cover   = Keyword.merge(@cover, project[:test_coverage] || [])

    if opts[:cover] do
      cover[:tool].start(Mix.Project.compile_path(project), cover)
    end

    :application.load(:ex_unit)

    # Configure exunit with command line options before requiring
    # test_helper so that the configuration is available in test_helper
    # Then configure exunit again it so command line options override
    # test_helper
    opts = ex_unit_opts(opts)
    ExUnit.configure(opts)

    test_paths = project[:test_paths] || ["test"]
    Enum.each(test_paths, &require_test_helper(&1))

    ExUnit.configure(opts)

    test_paths   = if files == [], do: test_paths, else: files
    test_pattern = project[:test_pattern] || "*_test.exs"

    files = Mix.Utils.extract_files(test_paths, test_pattern)
    Kernel.ParallelRequire.files files
  end

  @doc false
  def ex_unit_opts(opts) do
    opts = opts
           |> filter_opts(:include)
           |> filter_opts(:exclude)
           |> filter_only_opts()

    Dict.take(opts, [:trace, :max_cases, :color, :include, :exclude, :seed])
  end

  defp parse_filters(opts, key) do
    if Keyword.has_key?(opts, key) do
      ExUnit.Filters.parse(Keyword.get_values(opts, key))
    end
  end

  defp filter_opts(opts, key) do
    if filters = parse_filters(opts, key) do
      Keyword.put(opts, key, filters)
    else
      opts
    end
  end

  defp filter_only_opts(opts) do
    if filters = parse_filters(opts, :only) do
      opts
      |> Keyword.put_new(:include, [])
      |> Keyword.put_new(:exclude, [])
      |> Keyword.update!(:include, &(filters ++ &1))
      |> Keyword.update!(:exclude, &[:test|&1])
    else
      opts
    end
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
