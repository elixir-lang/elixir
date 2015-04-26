defmodule Mix.Tasks.Test do
  defmodule Cover do
    @moduledoc false

    def start(compile_path, opts) do
      Mix.shell.info "Cover compiling modules ... "
      _ = :cover.start

      case :cover.compile_beam_directory(compile_path |> to_char_list) do
        results when is_list(results) ->
          :ok
        {:error, _} ->
          Mix.raise "Failed to cover compile directory: " <> compile_path
      end

      output = opts[:output]

      fn() ->
        Mix.shell.info "\nGenerating cover results ... "
        File.mkdir_p!(output)
        Enum.each :cover.modules, fn(mod) ->
          {:ok, _} = :cover.analyse_to_file(mod, '#{output}/#{mod}.html', [:html])
        end
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

    * `--trace`      - run tests with detailed reporting; automatically sets `--max-cases` to 1
    * `--max-cases`  - set the maximum number of cases running async
    * `--cover`      - the directory to include coverage results
    * `--force`      - forces compilation regardless of modification times
    * `--no-compile` - do not compile, even if files require compilation
    * `--no-start`   - do not start applications after compilation
    * `--no-color`   - disable color in the output
    * `--color`      - enable color in the output
    * `--include`    - include tests that match the filter
    * `--exclude`    - exclude tests that match the filter
    * `--only`       - run only tests that match the filter
    * `--seed`       - seeds the random number generator used to randomize tests order
    * `--timeout`    - set the timeout for the tests

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
  `true`. It is also possible to include all examples that have a given tag,
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

  In case a single file is being tested, it is possible pass a specific
  line number:

      mix test test/some/particular/file_test.exs:12

  Which is equivalent to:

      mix test --only line:12 test/some/particular/file_test.exs

  ## Configuration

    * `:test_paths` - list of paths containing test files, defaults to
      `["test"]`. It is expected all test paths to contain a `test_helper.exs`
      file.

    * `:test_pattern` - a pattern to load test files, defaults to `*_test.exs`.

    * `:test_coverage` - a set of options to be passed down to the coverage
      mechanism.

  ## Coverage

  The `:test_coverage` configuration accepts the following options:

    * `:output` - the output for cover results, defaults to `"cover"`
    * `:tool`   - the coverage tool

  By default, a very simple wrapper around OTP's `cover` is used as a tool,
  but it can be overridden as follows:

      test_coverage: [tool: CoverModule]

  `CoverModule` can be any module that exports `start/2`, receiving the
  compilation path and the `test_coverage` options as arguments. It must
  return an anonymous function of zero arity that will be run after the
  test suite is done or `nil`.
  """

  @switches [force: :boolean, color: :boolean, cover: :boolean,
             trace: :boolean, max_cases: :integer, include: :keep,
             exclude: :keep, seed: :integer, only: :keep, compile: :boolean,
             start: :boolean, timeout: :integer]

  @cover [output: "cover", tool: Cover]

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    {opts, files, _} = OptionParser.parse(args, switches: @switches)

    unless System.get_env("MIX_ENV") || Mix.env == :test do
      Mix.raise "mix test is running on environment #{Mix.env}. If you are " <>
                                "running tests along another task, please set MIX_ENV explicitly"
    end

    Mix.Task.run "loadpaths", args

    if Keyword.get(opts, :compile, true) do
      Mix.Task.run "compile", args
    end

    project = Mix.Project.config
    cover   = Keyword.merge(@cover, project[:test_coverage] || [])

    # Start cover after we load deps but before we start the app.
    cover =
      if opts[:cover] do
        cover[:tool].start(Mix.Project.compile_path(project), cover)
      end

    # Start the app and configure exunit with command line options
    # before requiring test_helper.exs so that the configuration is
    # available in test_helper.exs. Then configure exunit again so
    # that command line options override test_helper.exs
    Mix.shell.print_app
    Mix.Task.run "app.start", args

    # Ensure ex_unit is loaded.
    case Application.load(:ex_unit) do
      :ok -> :ok
      {:error, {:already_loaded, :ex_unit}} -> :ok
    end

    opts = ex_unit_opts(opts)
    ExUnit.configure(opts)

    test_paths = project[:test_paths] || ["test"]
    Enum.each(test_paths, &require_test_helper(&1))
    ExUnit.configure(opts)

    # Finally parse, require and load the files
    test_files   = parse_files(files, test_paths)
    test_pattern = project[:test_pattern] || "*_test.exs"

    test_files = Mix.Utils.extract_files(test_files, test_pattern)
    _ = Kernel.ParallelRequire.files(test_files)

    # Run the test suite, coverage tools and register an exit hook
    %{failures: failures} = ExUnit.run
    if cover, do: cover.()

    System.at_exit fn _ ->
      if failures > 0, do: exit({:shutdown, 1})
    end
  end

  @doc false
  def ex_unit_opts(opts) do
    opts = opts
           |> filter_opts(:include)
           |> filter_opts(:exclude)
           |> filter_only_opts()

    default_opts(opts) ++
      Dict.take(opts, [:trace, :max_cases, :include, :exclude, :seed, :timeout])
  end

  defp default_opts(opts) do
    # Set autorun to false because Mix
    # automatically runs the test suite for us.
    case Dict.get(opts, :color) do
      nil -> [autorun: false]
      enabled? -> [autorun: false, colors: [enabled: enabled?]]
    end
  end

  defp parse_files([], test_paths) do
    test_paths
  end

  defp parse_files([single_file], _test_paths) do
    # Check if the single file path matches test/path/to_test.exs:123, if it does
    # apply `--only line:123` and trim the trailing :123 part.
    {single_file, opts} = ExUnit.Filters.parse_path(single_file)
    ExUnit.configure(opts)
    [single_file]
  end

  defp parse_files(files, _test_paths) do
    files
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
      Mix.raise "Cannot run tests because test helper file #{inspect file} does not exist"
    end
  end
end
