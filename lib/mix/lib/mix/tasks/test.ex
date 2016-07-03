defmodule Mix.Tasks.Test do
  defmodule Cover do
    @moduledoc false

    def start(compile_path, opts) do
      Mix.shell.info "Cover compiling modules ..."
      _ = :cover.start

      case :cover.compile_beam_directory(compile_path |> to_charlist) do
        results when is_list(results) ->
          :ok
        {:error, _} ->
          Mix.raise "Failed to cover compile directory: " <> compile_path
      end

      output = opts[:output]

      fn() ->
        Mix.shell.info "\nGenerating cover results ..."
        File.mkdir_p!(output)
        Enum.each :cover.modules, fn(mod) ->
          {:ok, _} = :cover.analyse_to_file(mod, '#{output}/#{mod}.html', [:html])
        end
      end
    end
  end

  use Mix.Task

  alias Mix.Compilers.Test, as: CT

  @shortdoc "Runs a project's tests"
  @recursive true
  @preferred_cli_env :test

  @moduledoc """
  Runs the tests for a project.

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
    * `--raise`      - raise if the test suit failed
    * `--force`      - forces compilation regardless of modification times
    * `--no-compile` - do not compile, even if files require compilation
    * `--no-start`   - do not start applications after compilation
    * `--no-color`   - disable color in the output
    * `--color`      - enable color in the output
    * `--include`    - include tests that match the filter
    * `--exclude`    - exclude tests that match the filter
    * `--only`       - run only tests that match the filter
    * `--seed`       - seeds the random number generator used to randomize tests order;
      `--seed 0` disables randomization
    * `--timeout`    - set the timeout for the tests
    * `--no-deps-check` - do not check dependencies
    * `--no-archives-check` - do not check archives
    * `--no-elixir-version-check` - do not check the Elixir version from mix.exs
    * `--stale` - run only tests which reference modules that changed since the
      last `test --stale`. You can read more about this option in the "Stale" section below.
    * `--listen-on-stdin` - run tests, and then listen on stdin. Receiving a newline will
      result in the tests being run again. Very useful when combined with `--stale` and
      external commands which produce output on stdout upon file system modification.

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

  For this reason, Mix also provides an `--only` option that excludes all
  tests and includes only the given ones:

      mix test --only external

  Which is equivalent to:

      mix test --include external --exclude test

  In case a single file is being tested, it is possible pass a specific
  line number:

      mix test test/some/particular/file_test.exs:12

  Which is equivalent to:

      mix test --only line:12 test/some/particular/file_test.exs

  Note that line filter takes the closest test on or before the given line number.
  In the case a single file contains more than one test module (test case),
  line filter applies to every test case before the given line number, thus more
  than one test might be taken for the run.

  ## Configuration

    * `:test_paths` - list of paths containing test files, defaults to
      `["test"]`. It is expected all test paths to contain a `test_helper.exs`
      file.

    * `:test_pattern` - a pattern to load test files, defaults to `*_test.exs`.

    * `:warn_test_pattern` - a pattern to match potentially missed test files
      and display a warning, defaults to `*_test.ex`.

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

  ## "Stale"

  The `--stale` command line option attempts to run only those test files which
  reference modules that have changed since the last time you ran this task with
  `--stale`.

  The first time this task is run with `--stale`, all tests are run and a manifest
  is generated. On subsequent runs, a test file is marked "stale" if any modules it
  references (and any modules those modules reference, recursively) were modified
  since the last run with `--stale`. A test file is also marked "stale" if it has
  been changed since the last run with `--stale`.
  """

  @switches [force: :boolean, color: :boolean, cover: :boolean,
             trace: :boolean, max_cases: :integer, include: :keep,
             exclude: :keep, seed: :integer, only: :keep, compile: :boolean,
             start: :boolean, timeout: :integer, raise: :boolean,
             deps_check: :boolean, archives_check: :boolean, elixir_version_check: :boolean,
             stale: :boolean, listen_on_stdin: :boolean]

  @cover [output: "cover", tool: Cover]

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    {opts, files} = OptionParser.parse!(args, strict: @switches)

    if opts[:listen_on_stdin] do
      System.at_exit fn _ ->
        IO.gets(:stdio, "")
        Mix.shell.info "Restarting..."
        :init.restart()
        :timer.sleep(:infinity)
      end
    end

    unless System.get_env("MIX_ENV") || Mix.env == :test do
      Mix.raise "\"mix test\" is running on environment \"#{Mix.env}\". If you are " <>
                                "running tests along another task, please set MIX_ENV explicitly"
    end

    Mix.Task.run "loadpaths", args

    if Keyword.get(opts, :compile, true) do
      Mix.Project.compile(args)
    end

    project = Mix.Project.config

    # Start cover after we load deps but before we start the app.
    cover =
      if opts[:cover] do
        compile_path = Mix.Project.compile_path(project)
        cover = Keyword.merge(@cover, project[:test_coverage] || [])
        cover[:tool].start(compile_path, cover)
      end

    # Start the app and configure exunit with command line options
    # before requiring test_helper.exs so that the configuration is
    # available in test_helper.exs. Then configure exunit again so
    # that command line options override test_helper.exs
    Mix.shell.print_app
    Mix.Task.run "app.start", args

    # Ensure ExUnit is loaded.
    case Application.load(:ex_unit) do
      :ok -> :ok
      {:error, {:already_loaded, :ex_unit}} -> :ok
    end

    # Configure ExUnit with command line options before requiring
    # test helpers so that the configuration is available in helpers.
    # Then configure ExUnit again so command line options override
    ex_unit_opts = ex_unit_opts(opts)
    ExUnit.configure(ex_unit_opts)

    test_paths = project[:test_paths] || ["test"]
    Enum.each(test_paths, &require_test_helper(&1))
    ExUnit.configure(merge_helper_opts(ex_unit_opts))

    # Finally parse, require and load the files
    test_files = parse_files(files, test_paths)
    test_pattern = project[:test_pattern] || "*_test.exs"
    warn_test_pattern = project[:warn_test_pattern] || "*_test.ex"

    matched_test_files = Mix.Utils.extract_files(test_files, test_pattern)
    matched_warn_test_files =
      Mix.Utils.extract_files(test_files, warn_test_pattern) -- matched_test_files

    display_warn_test_pattern(matched_warn_test_files, test_pattern)

    case CT.require_and_run(files, matched_test_files, test_paths, opts) do
      {:ok, %{failures: failures}} ->
        cover && cover.()

        cond do
          failures > 0 and opts[:raise] ->
            Mix.raise "mix test failed"
          failures > 0 ->
            System.at_exit fn _ -> exit({:shutdown, 1}) end
          true ->
            :ok
        end

      :noop ->
        :ok
    end
  end

  defp display_warn_test_pattern(files, pattern) do
    for file <- files do
      Mix.shell.info "warning: #{file} does not match #{inspect pattern} and won't be loaded"
    end
  end

  @doc false
  def ex_unit_opts(opts) do
    opts =
      opts
      |> filter_opts(:include)
      |> filter_opts(:exclude)
      |> filter_only_opts()

    default_opts(opts) ++
      Keyword.take(opts, [:trace, :max_cases, :include, :exclude, :seed, :timeout])
  end

  defp merge_helper_opts(opts) do
    opts
    |> merge_opts(:exclude)
  end

  defp default_opts(opts) do
    # Set autorun to false because Mix
    # automatically runs the test suite for us.
    case Keyword.fetch(opts, :color) do
      {:ok, enabled?} -> [autorun: false, colors: [enabled: enabled?]]
      :error -> [autorun: false]
    end
  end

  defp parse_files([], test_paths) do
    test_paths
  end

  defp parse_files([single_file], _test_paths) do
    # Check if the single file path matches test/path/to_test.exs:123, if it does
    # apply "--only line:123" and trim the trailing :123 part.
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

  defp merge_opts(opts, key) do
    value = List.wrap Application.get_env(:ex_unit, key, [])
    Keyword.update(opts, key, value, &Enum.uniq(&1 ++ value))
  end

  defp filter_only_opts(opts) do
    if filters = parse_filters(opts, :only) do
      opts
      |> Keyword.put_new(:include, [])
      |> Keyword.put_new(:exclude, [])
      |> Keyword.update!(:include, &(filters ++ &1))
      |> Keyword.update!(:exclude, &[:test | &1])
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
