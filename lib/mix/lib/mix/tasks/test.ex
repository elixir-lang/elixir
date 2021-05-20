defmodule Mix.Tasks.Test do
  use Mix.Task

  alias Mix.Compilers.Test, as: CT

  @compile {:no_warn_undefined, [ExUnit, ExUnit.Filters]}
  @shortdoc "Runs a project's tests"
  @recursive true
  @preferred_cli_env :test

  @moduledoc ~S"""
  Runs the tests for a project.

  This task starts the current application, loads up
  `test/test_helper.exs` and then, requires all files matching the
  `test/**/*_test.exs` pattern in parallel.

  A list of files and/or directories can be given after the task
  name in order to select the files to run:

      mix test test/some/particular/file_test.exs
      mix test test/some/particular/dir

  Tests in umbrella projects can be run from the root by specifying
  the full suite path, including `apps/my_app/test`, in which case
  recursive tests for other child apps will be skipped completely:

      # To run all tests for my_app from the umbrella root
      mix test apps/my_app/test

      # To run a given test file on my_app from the umbrella root
      mix test apps/my_app/test/some/particular/file_test.exs

  ## Understanding test results

  When you run your test suite, it prints results as they run with
  a summary at the end, as seen below:

      $ mix test
      ...

        1) test greets the world (FooTest)
           test/foo_test.exs:5
           Assertion with == failed
           code:  assert Foo.hello() == :world!
           left:  :world
           right: :world!
           stacktrace:
             test/foo_test.exs:6: (test)

      ........

      Finished in 0.05 seconds (0.00s async, 0.05s sync)
      1 doctest, 11 tests, 1 failure

      Randomized with seed 646219

  For each test, the test suite will print a dot. Failed tests
  are printed immediately in the format described in the next
  section.

  After all tests run, we print the suite summary. The first
  line contains the total time spent on the suite, followed
  by how much time was spent on async tests (defined with
  `use ExUnit.Case, async: true`) vs sync ones:

      Finished in 0.05 seconds (0.00s async, 0.05s sync)

  Developers want to minimize the time spent on sync tests
  whenever possible, as sync tests run serially and async
  tests run concurrently.

  Finally, how many tests we have run, how many of them
  failed, how many were invalid, etc.

  ### Understanding test failures

  First, it contains the failure counter, followed by the test
  name and the module the test was defined:

      1) test greets the world (FooTest)

  The next line contains the exact location of the test in the
  `FILE:LINE` format:

      test/foo_test.exs:5

  If you want to re-run only this test, all you need to do is to
  copy the line above and past it in front of `mix test`:

      mix test test/foo_test.exs:5

  Then we show the error message, code snippet, and general information
  about the failed test:

      Assertion with == failed
      code:  assert Foo.hello() == :world!
      left:  :world
      right: :world!

  If your terminal supports coloring (see the  "Coloring" section below),
  a diff is typically shown between `left` and `right` sides. Finally,
  we print the stacktrace of the failure:

      stacktrace:
        test/foo_test.exs:6: (test)

  ## Command line options

    * `--color` - enables color in the output

    * `--cover` - runs coverage tool. See "Coverage" section below

    * `--exclude` - excludes tests that match the filter

    * `--export-coverage` - the name of the file to export coverage results to.
      Only has an effect when used with `--cover`

    * `--failed` - runs only tests that failed the last time they ran.
      If there are no pending --failed tests, `mix test` will run all available tests

    * `--force` - forces compilation regardless of modification times

    * `--formatter` - sets the formatter module that will print the results.
      Defaults to ExUnit's built-in CLI formatter

    * `--include` - includes tests that match the filter

    * `--listen-on-stdin` - runs tests, and then listens on stdin. It will
      re-run tests once a newline is received. See the "File system watchers"
      section below

    * `--max-cases` - sets the maximum number of tests running asynchronously. Only tests from
      different modules run in parallel. Defaults to twice the number of cores

    * `--max-failures` - the suite stops evaluating tests when this number of test
      failures is reached. It runs all tests if omitted

    * `--no-archives-check` - does not check archives

    * `--no-color` - disables color in the output

    * `--no-compile` - does not compile, even if files require compilation

    * `--no-deps-check` - does not check dependencies

    * `--no-elixir-version-check` - does not check the Elixir version from `mix.exs`

    * `--no-start` - does not start applications after compilation

    * `--only` - runs only tests that match the filter

    * `--partitions` - sets the amount of partitions to split tests in. This option
      requires the `MIX_TEST_PARTITION` environment variable to be set. See the
      "Operating system process partitioning" section for more information

    * `--preload-modules` - preloads all modules defined in applications

    * `--profile-require` - profiles the time spent to require test files

    * `--raise` - raises if the test suite failed

    * `--seed` - seeds the random number generator used to randomize the order of tests;
      `--seed 0` disables randomization so the tests in a single file will always be ran
      in the same order they were defined in

    * `--slowest` - prints timing information for the N slowest tests.
      Automatically sets `--trace` and `--preload-modules`

    * `--stale` - runs only tests which reference modules that changed since the
      last time tests were ran with `--stale`. You can read more about this option
      in the "The --stale option" section below

    * `--timeout` - sets the timeout for the tests

    * `--trace` - runs tests with detailed reporting. Automatically sets `--max-cases` to `1`.
      Note that in trace mode test timeouts will be ignored as timeout is set to `:infinity`

    * `--warnings-as-errors` - (since v1.12.0) treats warnings as errors and returns a non-zero
      exit code. This option only applies to test files. To treat warnings as errors during
      compilation and during tests, run:
          MIX_ENV=test mix do compile --warnings-as-errors, test --warnings-as-errors

  ## Configuration

  These configurations can be set in the `def project` section of your `mix.exs`:

    * `:test_paths` - list of paths containing test files. Defaults to
      `["test"]` if the `test` directory exists; otherwise, it defaults to `[]`.
      It is expected that all test paths contain a `test_helper.exs` file

    * `:test_pattern` - a pattern to load test files. Defaults to `*_test.exs`

    * `:warn_test_pattern` - a pattern to match potentially misnamed test files
      and display a warning. Defaults to `*_test.ex`

    * `:test_coverage` - a set of options to be passed down to the coverage
      mechanism. See the "Coverage" section for more information

  ## Coloring

  Coloring is enabled by default on most Unix terminals. They are also
  available on Windows consoles from Windows 10, although it must be
  explicitly enabled for the current user in the registry by running
  the following command:

      reg add HKCU\Console /v VirtualTerminalLevel /t REG_DWORD /d 1

  After running the command above, you must restart your current console.

  ## Filters

  ExUnit provides tags and filtering functionality that allow developers
  to select which tests to run. The most common functionality is to exclude
  some particular tests from running by default in your test helper file:

      # Exclude all external tests from running
      ExUnit.configure(exclude: [external: true])

  Then, whenever desired, those tests could be included in the run via the
  `--include` option:

      mix test --include external:true

  The example above will run all tests that have the external option set to
  `true`. It is also possible to include all examples that have a given tag,
  regardless of its value:

      mix test --include external

  Note that all tests are included by default, so unless they are excluded
  first (either in the test helper or via the `--exclude` option) the
  `--include` option has no effect.

  For this reason, Mix also provides an `--only` option that excludes all
  tests and includes only the given ones:

      mix test --only external

  Which is similar to:

      mix test --include external --exclude test

  It differs in that the test suite will fail if no tests are executed when the `--only` option is used.

  In case a single file is being tested, it is possible to pass one or more specific
  line numbers to run only those given tests:

      mix test test/some/particular/file_test.exs:12

  Which is equivalent to:

      mix test --exclude test --include line:12 test/some/particular/file_test.exs

  Or:

      mix test test/some/particular/file_test.exs:12:24

  Which is equivalent to:

      mix test --exclude test --include line:12 --include line:24 test/some/particular/file_test.exs

  If a given line starts a `describe` block, that line filter runs all tests in it.
  Otherwise, it runs the closest test on or before the given line number.

  ## Coverage

  The `:test_coverage` configuration accepts the following options:

    * `:output` - the output directory for cover results. Defaults to `"cover"`

    * `:tool` - the coverage tool

    * `:summary` - summary output configuration; can be either a boolean
      or a keyword list. When a keyword list is passed, it can specify a
      `:threshold`, which is a boolean or numeric value that enables coloring
      of code coverage results in red or green depending on whether the
      percentage is below or above the specified threshold, respectively.
      Defaults to `[threshold: 90]`

    * `:export` - a file name to export results to instead of generating
      the result on the fly. The `.coverdata` extension is automatically
      added to the given file. This option is automatically set via the
      `--export-coverage` option or when using process partitioning.
      See `mix test.coverage` to compile a report from multiple exports

    * `:ignore_modules` - modules to ignore from generating reports and
      in summaries. It is a list of module names as atoms and regular
      expressions that are matched against the module names

    * `:local_only` - by default coverage only tracks local calls, set this
      option to false if you plan to run coverage across nodes

  By default, a very simple wrapper around OTP's `cover` is used as a tool,
  but it can be overridden as follows:

      def project() do
        [
          ...
          test_coverage: [tool: CoverModule]
          ...
        ]
      end

  `CoverModule` can be any module that exports `start/2`, receiving the
  compilation path and the `test_coverage` options as arguments.
  It must return either `nil` or an anonymous function of zero arity that will
  be run after the test suite is done.

  ## Operating system process partitioning

  While ExUnit supports the ability to run tests concurrently within the same
  Elixir instance, it is not always possible to run all tests concurrently. For
  example, some tests may rely on global resources.

  For this reason, `mix test` supports partitioning the test files across
  different Elixir instances. This is done by setting the `--partitions` option
  to an integer, with the number of partitions, and setting the `MIX_TEST_PARTITION`
  environment variable to control which test partition that particular instance
  is running. This can also be useful if you want to distribute testing across
  multiple machines.

  For example, to split a test suite into 4 partitions and run them, you would
  use the following commands:

      MIX_TEST_PARTITION=1 mix test --partitions 4
      MIX_TEST_PARTITION=2 mix test --partitions 4
      MIX_TEST_PARTITION=3 mix test --partitions 4
      MIX_TEST_PARTITION=4 mix test --partitions 4

  The test files are sorted upfront in a round-robin fashion. Note the partition
  itself is given as an environment variable so it can be accessed in config files
  and test scripts. For example, it can be used to setup a different database instance
  per partition in `config/test.exs`.

  If partitioning is enabled and `--cover` is used, no cover reports are generated,
  as they only contain a subset of the coverage data. Instead, the coverage data
  is exported to files such as `cover/MIX_TEST_PARTITION.coverdata`. Once you have
  the results of all partitions inside `cover/`, you can run `mix test.coverage` to
  get the unified report.

  ## The --stale option

  The `--stale` command line option attempts to run only the test files which
  reference modules that have changed since the last time you ran this task with
  `--stale`.

  The first time this task is run with `--stale`, all tests are run and a manifest
  is generated. On subsequent runs, a test file is marked "stale" if any modules it
  references (and any modules those modules reference, recursively) were modified
  since the last run with `--stale`. A test file is also marked "stale" if it has
  been changed since the last run with `--stale`.

  The `--stale` option is extremely useful for software iteration, allowing you to
  run only the relevant tests as you perform changes to the codebase.

  ## File-system watchers

  You can integrate `mix test` with filesystem watchers through the command line
  via the `--listen-on-stdin` option. For example, you can use [fswatch](https://github.com/emcrisostomo/fswatch)
  or similar to emit newlines whenever there is a change, which will cause your test
  suite to re-run:

      fswatch lib test | mix test --listen-on-stdin

  This can be combined with the `--stale` option to re-run only the test files that
  have changed as well as the tests that have gone stale due to changes in `lib`.

  ## Aborting the suite

  It is possible to abort the test suite with `Ctrl+\ `, which sends a SIGQUIT
  signal to the Erlang VM. ExUnit will intercept this signal to show all tests
  that have been aborted and print the results collected so far.

  This can be useful in case the suite gets stuck and you don't want to wait
  until the timeout times passes (which defaults to 30 seconds).
  """

  @switches [
    force: :boolean,
    color: :boolean,
    cover: :boolean,
    export_coverage: :string,
    trace: :boolean,
    max_cases: :integer,
    max_failures: :integer,
    include: :keep,
    exclude: :keep,
    seed: :integer,
    only: :keep,
    compile: :boolean,
    start: :boolean,
    timeout: :integer,
    raise: :boolean,
    deps_check: :boolean,
    archives_check: :boolean,
    elixir_version_check: :boolean,
    failed: :boolean,
    stale: :boolean,
    listen_on_stdin: :boolean,
    formatter: :keep,
    slowest: :integer,
    partitions: :integer,
    preload_modules: :boolean,
    warnings_as_errors: :boolean,
    profile_require: :string
  ]

  @cover [output: "cover", tool: Mix.Tasks.Test.Coverage]

  @impl true
  def run(args) do
    {opts, files} = OptionParser.parse!(args, strict: @switches)

    if not Mix.Task.recursing?() do
      do_run(opts, args, files)
    else
      {files_in_apps_path, files_not_in_apps_path} =
        Enum.split_with(files, &String.starts_with?(&1, "apps/"))

      app = Mix.Project.config()[:app]
      current_app_path = "apps/#{app}/"

      files_in_current_app_path =
        for file <- files_in_apps_path,
            String.starts_with?(file, current_app_path) or not relative_app_file_exists?(file),
            do: String.trim_leading(file, current_app_path)

      files = files_in_current_app_path ++ files_not_in_apps_path

      if files == [] and files_in_apps_path != [] do
        :ok
      else
        do_run([test_location_relative_path: "apps/#{app}"] ++ opts, args, files)
      end
    end
  end

  defp relative_app_file_exists?(file) do
    {file, _} = ExUnit.Filters.parse_path(file)
    File.exists?(Path.join("../..", file))
  end

  defp do_run(opts, args, files) do
    if opts[:listen_on_stdin] do
      System.at_exit(fn _ ->
        IO.gets(:stdio, "")
        Mix.shell().info("Restarting...")
        :init.restart()
        Process.sleep(:infinity)
      end)
    end

    unless System.get_env("MIX_ENV") || Mix.env() == :test do
      Mix.raise("""
      "mix test" is running in the \"#{Mix.env()}\" environment. If you are \
      running tests from within another command, you can either:

        1. set MIX_ENV explicitly:

            MIX_ENV=test mix test.another

        2. set the :preferred_cli_env for a command inside "def project" in your mix.exs:

            preferred_cli_env: ["test.another": :test]
      """)
    end

    # Load ExUnit before we compile anything
    Application.ensure_loaded(:ex_unit)

    old_warnings_as_errors = Code.get_compiler_option(:warnings_as_errors)
    args = args -- ["--warnings-as-errors"]

    Mix.Task.run("compile", args)

    if opts[:warnings_as_errors] do
      Code.put_compiler_option(:warnings_as_errors, true)
    end

    project = Mix.Project.config()

    # Start cover after we load deps but before we start the app.
    cover =
      if opts[:cover] do
        compile_path = Mix.Project.compile_path(project)
        partition = opts[:partitions] && System.get_env("MIX_TEST_PARTITION")

        cover =
          @cover
          |> Keyword.put(:export, opts[:export_coverage] || partition)
          |> Keyword.merge(project[:test_coverage] || [])

        cover[:tool].start(compile_path, cover)
      end

    # Start the app and configure ExUnit with command line options
    # before requiring test_helper.exs so that the configuration is
    # available in test_helper.exs
    Mix.shell().print_app
    app_start_args = if opts[:slowest], do: ["--preload-modules" | args], else: args
    Mix.Task.run("app.start", app_start_args)

    # The test helper may change the Mix.shell(), so revert it whenever we raise and after suite
    shell = Mix.shell()

    # Configure ExUnit now and then again so the task options override test_helper.exs
    {ex_unit_opts, allowed_files} = process_ex_unit_opts(opts)
    ExUnit.configure(ex_unit_opts)

    test_paths = project[:test_paths] || default_test_paths()
    Enum.each(test_paths, &require_test_helper(shell, &1))
    ExUnit.configure(merge_helper_opts(ex_unit_opts))

    # Finally parse, require and load the files
    test_files = parse_files(files, shell, test_paths)
    test_pattern = project[:test_pattern] || "*_test.exs"
    warn_test_pattern = project[:warn_test_pattern] || "*_test.ex"

    matched_test_files =
      test_files
      |> Mix.Utils.extract_files(test_pattern)
      |> filter_to_allowed_files(allowed_files)
      |> filter_by_partition(shell, opts)

    display_warn_test_pattern(test_files, test_pattern, matched_test_files, warn_test_pattern)

    result =
      case CT.require_and_run(matched_test_files, test_paths, opts) do
        {:ok, %{excluded: excluded, failures: failures, total: total}} ->
          Mix.shell(shell)
          cover && cover.()

          cond do
            failures > 0 and opts[:raise] ->
              raise_with_shell(shell, "\"mix test\" failed")

            failures > 0 ->
              System.at_exit(fn _ -> exit({:shutdown, 1}) end)

            excluded == total and Keyword.has_key?(opts, :only) ->
              message = "The --only option was given to \"mix test\" but no test was executed"
              raise_or_error_at_exit(shell, message, opts)

            true ->
              :ok
          end

        :noop ->
          cond do
            opts[:stale] ->
              Mix.shell().info("No stale tests")

            files == [] ->
              Mix.shell().info("There are no tests to run")

            true ->
              message = "Paths given to \"mix test\" did not match any directory/file: "
              raise_or_error_at_exit(shell, message <> Enum.join(files, ", "), opts)
          end

          :ok
      end

    Code.put_compiler_option(:warnings_as_errors, old_warnings_as_errors)
    result
  end

  defp raise_with_shell(shell, message) do
    Mix.shell(shell)
    Mix.raise(message)
  end

  defp raise_or_error_at_exit(shell, message, opts) do
    cond do
      opts[:raise] ->
        raise_with_shell(shell, message)

      Mix.Task.recursing?() ->
        Mix.shell().info(message)

      true ->
        Mix.shell().error(message)
        System.at_exit(fn _ -> exit({:shutdown, 1}) end)
    end
  end

  defp display_warn_test_pattern(test_files, test_pattern, matched_test_files, warn_test_pattern) do
    files = Mix.Utils.extract_files(test_files, warn_test_pattern) -- matched_test_files

    for file <- files do
      Mix.shell().info(
        "warning: #{file} does not match #{inspect(test_pattern)} and won't be loaded"
      )
    end
  end

  @option_keys [
    :trace,
    :max_cases,
    :max_failures,
    :include,
    :exclude,
    :seed,
    :timeout,
    :formatters,
    :colors,
    :slowest,
    :failures_manifest_file,
    :only_test_ids,
    :test_location_relative_path
  ]

  @doc false
  def process_ex_unit_opts(opts) do
    {opts, allowed_files} = manifest_opts(opts)

    opts =
      opts
      |> filter_opts(:include)
      |> filter_opts(:exclude)
      |> filter_opts(:only)
      |> formatter_opts()
      |> color_opts()
      |> Keyword.take(@option_keys)
      |> default_opts()

    {opts, allowed_files}
  end

  defp merge_helper_opts(opts) do
    # The only options that are additive from app env are the excludes
    merge_opts(opts, :exclude)
  end

  defp merge_opts(opts, key) do
    value = List.wrap(Application.get_env(:ex_unit, key, []))
    Keyword.update(opts, key, value, &Enum.uniq(&1 ++ value))
  end

  defp default_opts(opts) do
    # Set autorun to false because Mix
    # automatically runs the test suite for us.
    [autorun: false] ++ opts
  end

  defp parse_files([], _shell, test_paths) do
    test_paths
  end

  defp parse_files([single_file], _shell, _test_paths) do
    # Check if the single file path matches test/path/to_test.exs:123. If it does,
    # apply "--only line:123" and trim the trailing :123 part.
    {single_file, opts} = ExUnit.Filters.parse_path(single_file)
    ExUnit.configure(opts)
    [single_file]
  end

  defp parse_files(files, shell, _test_paths) do
    if Enum.any?(files, &match?({_, [_ | _]}, ExUnit.Filters.parse_path(&1))) do
      raise_with_shell(shell, "Line numbers can only be used when running a single test file")
    else
      files
    end
  end

  defp parse_filters(opts, key) do
    if Keyword.has_key?(opts, key) do
      ExUnit.Filters.parse(Keyword.get_values(opts, key))
    end
  end

  defp filter_opts(opts, :only) do
    if filters = parse_filters(opts, :only) do
      opts
      |> Keyword.update(:include, filters, &(filters ++ &1))
      |> Keyword.update(:exclude, [:test], &[:test | &1])
    else
      opts
    end
  end

  defp filter_opts(opts, key) do
    if filters = parse_filters(opts, key) do
      Keyword.put(opts, key, filters)
    else
      opts
    end
  end

  defp formatter_opts(opts) do
    if Keyword.has_key?(opts, :formatter) do
      formatters =
        opts
        |> Keyword.get_values(:formatter)
        |> Enum.map(&Module.concat([&1]))

      Keyword.put(opts, :formatters, formatters)
    else
      opts
    end
  end

  @manifest_file_name ".mix_test_failures"

  defp manifest_opts(opts) do
    manifest_file = Path.join(Mix.Project.manifest_path(), @manifest_file_name)
    opts = Keyword.put(opts, :failures_manifest_file, manifest_file)

    if opts[:failed] do
      if opts[:stale] do
        Mix.raise("Combining --failed and --stale is not supported.")
      end

      {allowed_files, failed_ids} = ExUnit.Filters.failure_info(manifest_file)

      if MapSet.size(failed_ids) == 0 do
        Mix.shell().info("No pending --failed tests, re-running all available tests...")
        {opts, nil}
      else
        {Keyword.put(opts, :only_test_ids, failed_ids), allowed_files}
      end
    else
      {opts, nil}
    end
  end

  defp filter_to_allowed_files(matched_test_files, nil), do: matched_test_files

  defp filter_to_allowed_files(matched_test_files, %MapSet{} = allowed_files) do
    Enum.filter(matched_test_files, &MapSet.member?(allowed_files, Path.expand(&1)))
  end

  defp filter_by_partition(files, shell, opts) do
    if total = opts[:partitions] do
      partition = System.get_env("MIX_TEST_PARTITION")

      case partition && Integer.parse(partition) do
        {partition, ""} when partition in 1..total ->
          partition = partition - 1

          # We sort the files because Path.wildcard does not guarantee
          # ordering, so different OSes could return a different order,
          # meaning run across OSes on different partitions could run
          # duplicate files.
          for {file, index} <- Enum.with_index(Enum.sort(files)),
              rem(index, total) == partition,
              do: file

        _ ->
          raise_with_shell(
            shell,
            "The MIX_TEST_PARTITION environment variable must be set to an integer between " <>
              "1..#{total} when the --partitions option is set, got: #{inspect(partition)}"
          )
      end
    else
      files
    end
  end

  defp color_opts(opts) do
    case Keyword.fetch(opts, :color) do
      {:ok, enabled?} ->
        Keyword.put(opts, :colors, enabled: enabled?)

      :error ->
        opts
    end
  end

  defp require_test_helper(shell, dir) do
    file = Path.join(dir, "test_helper.exs")

    if File.exists?(file) do
      Code.require_file(file)
    else
      raise_with_shell(
        shell,
        "Cannot run tests because test helper file #{inspect(file)} does not exist"
      )
    end
  end

  defp default_test_paths do
    if File.dir?("test") do
      ["test"]
    else
      []
    end
  end
end
