# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Mix.Tasks.Test do
  use Mix.Task

  alias Mix.Compilers.Test, as: CT

  @compile {:no_warn_undefined, [IEx, ExUnit, ExUnit.Filters]}
  @shortdoc "Runs a project's tests"
  @recursive true

  @moduledoc ~S"""
  Runs the tests for a project.

  This task starts the current application, loads up
  `test/test_helper.exs` and then, requires all files matching the
  `test/**/*_test.exs` pattern in parallel.

  A list of files and/or directories can be given after the task
  name in order to select the files to run:

      $ mix test test/some/particular/file_test.exs
      $ mix test test/some/particular/dir

  Tests in umbrella projects can be run from the root by specifying
  the full suite path, including `apps/my_app/test`, in which case
  recursive tests for other child apps will be skipped completely:

      # To run all tests for my_app from the umbrella root
      $ mix test apps/my_app/test

      # To run a given test file on my_app from the umbrella root
      $ mix test apps/my_app/test/some/particular/file_test.exs

  ## Understanding test results

  When you run your test suite, it prints results as they run with
  a summary at the end, as seen below:

      $ mix test
      Running ExUnit with seed: 646219, max_cases: 16
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
  failed, how many were invalid, and so on.

  ### Understanding test failures

  First, it contains the failure counter, followed by the test
  name and the module the test was defined:

      1) test greets the world (FooTest)

  The next line contains the exact location of the test in the
  `FILE:LINE` format:

      test/foo_test.exs:5

  If you want to re-run only this test, all you need to do is to
  copy the line above and paste it in front of `mix test`:

      $ mix test test/foo_test.exs:5

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

    * `--all-warnings` (`--no-all-warnings`) - prints all warnings, including previous compilations
      (default is true except on errors)

    * `-b`, `--breakpoints` *(since v1.17.0)* - sets a breakpoint at the beginning
      of every test. The debugger goes line-by-line and can access all variables
      and imports (but not local functions). You can press `n` for the next line
      and `c` for the next test. This automatically sets `--trace`

    * `--color` - enables color in ExUnit formatting results

    * `--cover` - runs coverage tool. See "Coverage" section below

    * `--exclude` - excludes tests that match the filter. This option may be given
      several times to apply different filters, such as `--exclude ci --exclude slow`

    * `--exit-status` - use an alternate exit status to use when the test suite
      fails (default is 2).

    * `--export-coverage` - the name of the file to export coverage results to.
      Only has an effect when used with `--cover`

    * `--failed` - runs only tests that failed the last time they ran

    * `--force` - forces compilation regardless of modification times

    * `--formatter` - sets the formatter module that will print the results.
      Defaults to ExUnit's built-in CLI formatter

    * `--include` - includes tests that match the filter. This option may be given
      several times to apply different filters, such as `--include ci --include slow`

    * `--listen-on-stdin` - runs tests, and then listens on stdin. It will
      re-run tests once a newline is received. See the "File system watchers"
      section below

    * `--max-cases` - sets the maximum number of tests running asynchronously. Only tests from
      different modules run in parallel. Defaults to twice the number of cores

    * `--max-failures` - the suite stops evaluating tests when this number of test
      failures is reached. It runs all tests if omitted

    * `--max-requires` - sets the maximum number of test files to compile in parallel.
      Setting this to 1 will compile test files sequentially.

    * `-n`, `--name-pattern` *(since v1.19.0)* - only run tests with names that match
      the given regular expression

    * `--no-archives-check` - does not check archives

    * `--no-color` - disables color in the output

    * `--no-compile` - does not compile, even if files require compilation

    * `--no-deps-check` - does not check dependencies

    * `--no-elixir-version-check` - does not check the Elixir version from `mix.exs`

    * `--no-start` - does not start applications after compilation

    * `--only` - runs only tests that match the filter

    * `--partitions` - sets the amount of partitions to split tests in. It must be
      a number greater than zero. If set to one, it acts a no-op. If more than one,
      then you must also set the `MIX_TEST_PARTITION` environment variable with the
      partition to use in the current test run. See the "Operating system process
      partitioning" section for more information

    * `--preload-modules` - preloads all modules defined in applications

    * `--profile-require time` - profiles the time spent to require test files.
      Used only for debugging. The test suite does not run

    * `--raise` - immediately raises if the test suite fails, instead of continuing
      the execution of other Mix tasks

    * `--repeat-until-failure` *(since v1.17.0)* - sets the number of repetitions for running
      the suite until it fails. This is useful for debugging flaky tests within the same instance
      of the Erlang VM. For example, `--repeat-until-failure 10000` repeats the test suite
      up to 10000 times until the first failure. This can be combined with `--max-failures 1`
      to immediately stop if one test fails. However, if there is any leftover global state
      after running the tests, re-running the suite may trigger unrelated failures.

    * `--seed` - seeds the random number generator used to randomize the order of tests;
      `--seed 0` disables randomization so the tests in a single file will always be ran
      in the same order they were defined in

    * `--slowest` - prints timing information for the N slowest tests. Includes time spent in
     `ExUnit.Callbacks.setup/1`. Automatically sets `--trace` and `--preload-modules`

    * `--slowest-modules` *(since v1.17.0)* - prints timing information for the N slowest
      modules. Includes time spent in `ExUnit.Callbacks.setup/1`. Automatically sets
      `--trace` and `--preload-modules`

    * `--stale` - runs only tests which reference modules that changed since the
      last time tests were ran with `--stale`. You can read more about this option
      in the "The --stale option" section below

    * `--timeout` - sets the timeout for the tests

    * `--trace` - runs tests with detailed reporting. Automatically sets `--max-cases` to `1`.
      Note that in trace mode test timeouts will be ignored as timeout is set to `:infinity`

    * `--warnings-as-errors` *(since v1.12.0)* - treats compilation warnings (from loading the
      test suite) as errors and returns an exit status of 1 if the test suite would otherwise
      pass. If the test suite fails and also include warnings as errors, the exit
      status returned will be the value of the `--exit-status` option, which
      defaults to `2`, plus one. Therefore in the default case, this will be exit status `3`.

      Note that failures reported by `--warnings-as-errors` cannot be retried with the
      `--failed` flag.

      This option only applies to test files. To treat warnings as errors during compilation and
      during tests, run:
          MIX_ENV=test mix do compile --warnings-as-errors + test --warnings-as-errors

  ## Configuration

  These configurations can be set in the `def project` section of your `mix.exs`:

    * `:test_coverage` - a set of options to be passed down to the coverage
      mechanism. See the "Coverage" section for more information

    * `:test_elixirc_options` - the compiler options to used when
      loading/compiling test files. By default it disables the debug chunk,
      docs chunk, and module type inference

    * `:test_paths` - list of paths containing test files. Defaults to
      `["test"]` if the `test` directory exists, otherwise, it defaults to `[]`.
      It is expected that all test paths contain a `test_helper.exs` file

    * `:test_pattern` - a pattern to find potential test files.
      Defaults to `"*.{ex,exs}"`.

      In Elixir versions earlier than 1.19.0, this option defaulted to `*_test.exs`,
      but to allow better warnings for misnamed test files, it since matches any
      Elixir file and expects those to be filtered by `:test_load_filters` and
      `:test_ignore_filters`.

    * `:test_load_filters` - a list of files, regular expressions or one-arity
      functions to restrict which files matched by the `:test_pattern` are loaded.
      Defaults to `[&String.ends_with?(&1, "_test.exs")]`. Paths are relative to
      the project root and separated by `/`, even on Windows.

    * `:test_ignore_filters` - a list of files, regular expressions or one-arity
      functions to restrict which files matched by the `:test_pattern`, but not loaded
      by `:test_load_filters`, trigger a warning for a potentially misnamed test file.

      Mix ignores files ending in `_helper.exs` by default, as well as any file
      included in the project's `:elixirc_paths`. This ensures that any helper
      or test support files are not triggering a warning.

      Any extra filters configured in the project are appended to the defaults.
      Warnings can be disabled by setting this option to `[fn _ -> true end]`.
      Paths are relative to the project root and separated by `/`, even on Windows.

  ## Coloring

  Coloring is enabled by default on most Unix terminals. They are also
  available on Windows consoles from Windows 10, although it must be
  explicitly enabled for the current user in the registry by running
  the following command:

      $ reg add HKCU\Console /v VirtualTerminalLevel /t REG_DWORD /d 1

  After running the command above, you must restart your current console.

  ## Filters

  ExUnit provides tags and filtering functionality that allow developers
  to select which tests to run. The most common functionality is to exclude
  some particular tests from running by default in your test helper file:

      # Exclude all external tests from running
      ExUnit.configure(exclude: [external: true])

  Then, whenever desired, those tests could be included in the run via the
  `--include` option:

      $ mix test --include external:true

  The example above will run all tests that have the external option set to
  `true`. It is also possible to include all examples that have a given tag,
  regardless of its value:

      $ mix test --include external

  Note that all tests are included by default, so unless they are excluded
  first (either in the test helper or via the `--exclude` option) the
  `--include` option has no effect.

  For this reason, Mix also provides an `--only` option that excludes all
  tests and includes only the given ones:

      $ mix test --only external

  Which is similar to:

      $ mix test --include external --exclude test

  It differs in that the test suite will fail if no tests are executed when the `--only` option is used.

  In case a single file is being tested, it is possible to pass one or more specific
  line numbers to run only those given tests:

      $ mix test test/some/particular/file_test.exs:12

  Which is equivalent to:

      $ mix test --exclude test --include line:12 test/some/particular/file_test.exs

  Or:

      $ mix test test/some/particular/file_test.exs:12:24

  Which is equivalent to:

      $ mix test --exclude test --include line:12 --include line:24 test/some/particular/file_test.exs

  If a given line starts a `describe` block, that line filter runs all tests in it.
  Otherwise, it runs the closest test on or before the given line number.

  ## Coverage

  Elixir provides built-in line-based test coverage via the `--cover` flag.
  The test coverages shows which lines of code and in which files were executed
  during the test run.

  ### Limitations

  Coverage in Elixir has the following limitations:

    * Literals, such as atoms, strings, and numbers, are not traced by coverage.
      For example, if a function simply returns `:ok`, the atom `:ok` itself is
      never taken into account by coverage;

    * Macros, such as the ones defined by `defmacro/2` and `defguard/2`, and code
      invoked only by macros are never considered as covered, unless they are also
      invoked during the tests themselves. That's because macros are invoked at
      compilation time, before the test coverage instrumentation begins;

  ### Configuration

  The `:test_coverage` configures the coverage tool and accepts the following options:

    * `:output` - the output directory for cover results. Defaults to `"cover"`.

    * `:tool` - a module specifying the coverage tool to use.

    * `:summary` - at the end of each coverage run, a summary of each
      module is printed, with results in red or green depending on whether
      the percentage is below or above a given threshold. The task will
      exit with status of 1 if the total coverage is below the threshold.
      The `:summary` option allows you to customize the summary generation
      and defaults to `[threshold: 90]`, but it may be set to `false` to
      disable such reports.

    * `:export` - a filename to export results to instead of generating
      the coverage result on the fly. The `.coverdata` extension is
      automatically added to the given file. This option is automatically
      set via the `--export-coverage` option or when using process partitioning.
      See `mix test.coverage` to compile a report from multiple exports.

    * `:ignore_modules` - modules to ignore from generating reports and
      in summaries. It is a list of module names as atoms and regular
      expressions that are matched against the module names.

    * `:local_only` - by default coverage only tracks local calls,
      set this option to false if you plan to run coverage across nodes.

  By default, a wrapper around OTP's `cover` is used as the default coverage
  tool. You can learn more about how it works in the docs for
  `mix test.coverage`. Your tool of choice can be given as follows:

      def project() do
        [
          ...
          test_coverage: [tool: CoverModule]
          ...
        ]
      end

  `CoverModule` can be any module that exports `start/2`, receiving the
  compilation path and the `test_coverage` options as arguments.
  It must return either `nil` or an anonymous function of zero arity that
  will run after the test suite is done.

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

      $ MIX_TEST_PARTITION=1 mix test --partitions 4
      $ MIX_TEST_PARTITION=2 mix test --partitions 4
      $ MIX_TEST_PARTITION=3 mix test --partitions 4
      $ MIX_TEST_PARTITION=4 mix test --partitions 4

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

  You can integrate `mix test` with file system watchers through the command line
  via the `--listen-on-stdin` option. For example, you can use [fswatch](https://github.com/emcrisostomo/fswatch)
  or similar to emit newlines whenever there is a change, which will cause your test
  suite to re-run:

      $ fswatch lib test | mix test --listen-on-stdin

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
    all_warnings: :boolean,
    breakpoints: :boolean,
    force: :boolean,
    color: :boolean,
    cover: :boolean,
    export_coverage: :string,
    trace: :boolean,
    max_cases: :integer,
    max_failures: :integer,
    max_requires: :integer,
    include: :keep,
    exclude: :keep,
    seed: :integer,
    name_pattern: :regex,
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
    slowest_modules: :integer,
    partitions: :integer,
    preload_modules: :boolean,
    warnings_as_errors: :boolean,
    profile_require: :string,
    exit_status: :integer,
    repeat_until_failure: :integer
  ]

  @aliases [b: :breakpoints, n: :name_pattern]

  @cover [output: "cover", tool: Mix.Tasks.Test.Coverage]

  @impl true
  def run(args) do
    {opts, files} = OptionParser.parse!(args, strict: @switches, aliases: @aliases)

    if not Mix.Task.recursing?() do
      do_run(opts, args, files)
    else
      parent_umbrella = Path.dirname(Mix.Project.parent_umbrella_project_file())

      {files_in_apps_path, files_not_in_apps_path} =
        files
        |> Enum.map(&Path.expand(&1, parent_umbrella))
        |> Enum.map(&Path.relative_to(&1, parent_umbrella))
        |> Enum.split_with(&String.starts_with?(&1, "apps/"))

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
    {[file], _} = ExUnit.Filters.parse_paths([file])
    File.exists?(Path.join("../..", file))
  end

  defp do_run(opts, args, files) do
    _ = Mix.Project.get!()

    if System.get_env("MIX_ENV") == nil && Mix.env() != :test do
      Mix.raise("""
      "mix test" is running in the \"#{Mix.env()}\" environment. If you are \
      running tests from within another command, you can either:

        1. set MIX_ENV explicitly:

            MIX_ENV=test mix test.another

        2. set the :preferred_envs for "def cli" in your mix.exs:

            def cli do
              [preferred_envs: ["test.another": :test]]
            end
      """)
    end

    if opts[:listen_on_stdin] do
      System.at_exit(fn _ ->
        IO.gets(:stdio, "")
        Mix.shell().info("Restarting...")
        System.restart()
        Process.sleep(:infinity)
      end)
    end

    # Load ExUnit before we compile anything in case we are compiling
    # helper modules that depend on ExUnit.
    Application.ensure_loaded(:ex_unit)

    # --warnings-as-errors in test does not pass down to compile,
    # if you need this, call compile explicitly before.
    Mix.Task.run("compile", args -- ["--warnings-as-errors"])

    project = Mix.Project.config()

    {partitions, opts} = Keyword.pop(opts, :partitions)
    partitioned? = is_integer(partitions) and partitions > 1

    # Start cover after we load deps but before we start the app.
    cover =
      if opts[:cover] do
        compile_path = Mix.Project.compile_path(project)
        partition = partitioned? && System.get_env("MIX_TEST_PARTITION")

        cover =
          @cover
          |> Keyword.put(:export, opts[:export_coverage] || partition)
          |> Keyword.merge(project[:test_coverage] || [])

        cover[:tool].start(compile_path, cover)
      end

    # Start the app and configure ExUnit with command line options
    # before requiring test_helper.exs so that the configuration is
    # available in test_helper.exs
    Mix.shell().print_app()
    app_start_args = if opts[:slowest], do: ["--preload-modules" | args], else: args
    Mix.Task.run("app.start", app_start_args)

    # The test helper may change the Mix.shell(), so revert it whenever we raise and after suite
    shell = Mix.shell()
    test_elixirc_options = project[:test_elixirc_options] || []

    {test_elixirc_options, opts} =
      cond do
        not Keyword.get(opts, :breakpoints, false) ->
          {test_elixirc_options, opts}

        IEx.started?() ->
          {Keyword.put(test_elixirc_options, :debug_info, true), opts}

        true ->
          Mix.shell().error("you must run \"iex -S mix test\" when using -b/--breakpoints")
          {test_elixirc_options, Keyword.delete(opts, :breakpoints)}
      end

    # Configure ExUnit now and then again so the task options override test_helper.exs
    {ex_unit_opts, allowed_files} = process_ex_unit_opts(opts)
    ExUnit.configure(ex_unit_opts)

    warnings_as_errors? = Keyword.get(opts, :warnings_as_errors, false)
    exit_status = Keyword.fetch!(ex_unit_opts, :exit_status)

    # Prepare and extract all files to require and run
    test_paths = project[:test_paths] || default_test_paths()
    test_pattern = project[:test_pattern] || "*.{ex,exs}"

    # Warn about deprecated warn configuration
    if project[:warn_test_pattern] do
      Mix.shell().info("""
      warning: the `:warn_test_pattern` configuration is deprecated and will be ignored. \
      Use `:test_load_filters` and `:test_ignore_filters` instead.
      """)
    end

    {test_files, test_opts} =
      if files != [], do: ExUnit.Filters.parse_paths(files), else: {test_paths, []}

    # get a list of all files in the test folders, which we filter by the test_load_filters
    {potential_test_files, directly_included_test_files} = extract_files(test_files, test_pattern)

    {load_files, _ignored_files, warn_files} =
      classify_test_files(shell, potential_test_files, project)

    # ensure that files given as direct argument to mix test are loaded,
    # even if the test_load_filters don't match
    load_files =
      if files != [],
        do: Enum.uniq(load_files ++ directly_included_test_files),
        else: load_files

    matched_test_files =
      load_files
      |> filter_to_allowed_files(allowed_files)
      |> filter_by_partition(shell, partitions)

    warn_files != [] && warn_misnamed_test_files(warn_files)

    try do
      helper_warned? = Enum.any?(test_paths, &require_test_helper(shell, &1))
      # test_opts always wins because those are given via args
      ExUnit.configure(ex_unit_opts |> merge_helper_opts() |> Keyword.merge(test_opts))

      {CT.require_and_run(matched_test_files, test_paths, test_elixirc_options, opts),
       helper_warned?}
    catch
      kind, reason ->
        # Also mark the whole suite as failed
        file = get_manifest_path(opts)
        ExUnit.Filters.fail_all!(file)
        :erlang.raise(kind, reason, __STACKTRACE__)
    else
      {{:ok, %{excluded: excluded, failures: failures, warnings?: warnings?, total: total}},
       helper_warned?} ->
        Mix.shell(shell)
        cover && cover.()

        cond do
          warnings_as_errors? and (warnings? or helper_warned?) and failures == 0 ->
            message =
              "\nERROR! Test suite aborted after successful execution due to warnings while using the --warnings-as-errors option"

            IO.puts(:stderr, IO.ANSI.format([:red, message]))

            System.at_exit(fn _ ->
              exit({:shutdown, 1})
            end)

          failures > 0 and opts[:raise] ->
            raise_with_shell(shell, "\"mix test\" failed")

          warnings_as_errors? and warnings? and failures > 0 ->
            System.at_exit(fn _ ->
              exit({:shutdown, exit_status + 1})
            end)

          failures > 0 ->
            System.at_exit(fn _ ->
              exit({:shutdown, exit_status})
            end)

          excluded == total and Keyword.has_key?(opts, :only) ->
            nothing_executed(shell, "--only", opts)

          excluded == total and Keyword.has_key?(opts, :name_pattern) ->
            nothing_executed(shell, "--name-pattern", opts)

          true ->
            :ok
        end

      {:noop, _} ->
        cond do
          opts[:stale] ->
            Mix.shell().info("No stale tests")

          opts[:failed] || files == [] ->
            Mix.shell().info("There are no tests to run")

          true ->
            message = "Paths given to \"mix test\" did not match any directory/file: "
            raise_or_error_at_exit(shell, message <> Enum.join(files, ", "), opts)
        end

        :ok
    end
  end

  # similar to Mix.Utils.extract_files/2, but returns a list of directly included test files,
  # that should be not filtered by the test_load_filters, e.g.
  # mix test test/some_file.exs
  defp extract_files(paths, pattern) do
    {files, directly_included} =
      for path <- paths, reduce: {[], []} do
        {acc, directly_included} ->
          case :elixir_utils.read_file_type(path) do
            {:ok, :directory} ->
              {[Path.wildcard("#{path}/**/#{pattern}") | acc], directly_included}

            {:ok, :regular} ->
              {[path | acc], [path | directly_included]}

            _ ->
              {acc, directly_included}
          end
      end

    files =
      files
      |> List.flatten()
      |> Enum.uniq()

    {files, directly_included}
  end

  defp raise_with_shell(shell, message) do
    Mix.shell(shell)
    Mix.raise(message)
  end

  defp nothing_executed(shell, option, opts) do
    message = "The #{option} option was given to \"mix test\" but no test was executed"
    raise_or_error_at_exit(shell, message, opts)
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

  defp classify_test_files(shell, potential_test_files, project) do
    test_load_filters = project[:test_load_filters] || [&String.ends_with?(&1, "_test.exs")]

    test_ignore_filters = get_test_ignore_filters(shell, project)

    {to_load, to_ignore, to_warn} =
      for file <- potential_test_files, reduce: {[], [], []} do
        {to_load, to_ignore, to_warn} ->
          cond do
            any_file_matches?(file, test_load_filters) ->
              {[file | to_load], to_ignore, to_warn}

            any_file_matches?(file, test_ignore_filters) ->
              {to_load, [file | to_ignore], to_warn}

            true ->
              {to_load, to_ignore, [file | to_warn]}
          end
      end

    # get the files back in the original order
    {Enum.reverse(to_load), Enum.reverse(to_ignore), Enum.reverse(to_warn)}
  end

  defp get_test_ignore_filters(shell, project) do
    elixirc_paths = project[:elixirc_paths] || []

    case Keyword.get(project, :test_ignore_filters, []) do
      list when is_list(list) ->
        # ignore any _helper.exs files and files that are compiled (test support files)
        [
          &String.ends_with?(&1, "_helper.exs"),
          fn file -> Enum.any?(elixirc_paths, &String.starts_with?(file, &1)) end
        ] ++ list

      other ->
        raise_with_shell(
          shell,
          "Invalid configuration for :test_ignore_filters, expected a list, got: #{inspect(other)}"
        )
    end
  end

  defp any_file_matches?(file, filters) do
    Enum.any?(filters, fn filter ->
      case filter do
        regex when is_struct(regex, Regex) ->
          Regex.match?(regex, file)

        binary when is_binary(binary) ->
          file == binary

        fun when is_function(fun, 1) ->
          fun.(file)
      end
    end)
  end

  defp warn_misnamed_test_files(ignored) do
    Mix.shell().info("""
    warning: the following files do not match any of the configured `:test_load_filters` / `:test_ignore_filters`:

    #{Enum.join(ignored, "\n")}

    This might indicate a typo in a test file name (for example, using "foo_tests.exs" instead of "foo_test.exs").

    You can adjust which files trigger this warning by configuring the `:test_ignore_filters` option in your
    Mix project's configuration. To disable the warning entirely, set that option to [fn _ -> true end].

    For more information, run `mix help test`.
    """)
  end

  @option_keys [
    :breakpoints,
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
    :slowest_modules,
    :failures_manifest_path,
    :only_test_ids,
    :test_location_relative_path,
    :exit_status,
    :repeat_until_failure
  ]

  @doc false
  def process_ex_unit_opts(opts) do
    {opts, allowed_files} = manifest_opts(opts)

    opts =
      opts
      |> filter_opts(:include)
      |> filter_opts(:exclude)
      |> filter_only_and_name_pattern()
      |> formatter_opts()
      |> color_opts()
      |> exit_status_opts()
      |> Keyword.take(@option_keys)
      |> default_opts()

    {opts, allowed_files}
  end

  defp merge_helper_opts(opts) do
    # The only options that are additive from app env are the excludes
    value = List.wrap(Application.get_env(:ex_unit, :exclude, []))

    opts
    |> Keyword.update(:exclude, value, &Enum.uniq(&1 ++ value))
    |> Keyword.put_new_lazy(:failures_manifest_path, fn -> get_manifest_path([]) end)
  end

  defp default_opts(opts) do
    # Set autorun to false because Mix automatically runs the test suite for us.
    [autorun: false] ++ opts
  end

  defp parse_filters(opts, key) do
    if Keyword.has_key?(opts, key) do
      ExUnit.Filters.parse(Keyword.get_values(opts, key))
    else
      []
    end
  end

  defp filter_only_and_name_pattern(opts) do
    only = parse_filters(opts, :only)
    name_patterns = opts |> Keyword.get_values(:name_pattern) |> Enum.map(&{:test, &1})

    case only ++ name_patterns do
      [] ->
        opts

      filters ->
        opts
        |> Keyword.update(:include, filters, &(filters ++ &1))
        |> Keyword.update(:exclude, [:test], &[:test | &1])
    end
  end

  defp filter_opts(opts, key) do
    case parse_filters(opts, key) do
      [] -> opts
      filters -> Keyword.put(opts, key, filters)
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

  defp get_manifest_path(opts) do
    opts[:failures_manifest_path] ||
      Application.get_env(:ex_unit, :failures_manifest_path) ||
      Path.join(Mix.Project.manifest_path(), @manifest_file_name)
  end

  defp manifest_opts(opts) do
    manifest_file = get_manifest_path(opts)

    if opts[:failed] do
      if opts[:stale] do
        Mix.raise("Combining --failed and --stale is not supported.")
      end

      case ExUnit.Filters.failure_info(manifest_file) do
        {allowed_files, failed_ids} ->
          {Keyword.put(opts, :only_test_ids, failed_ids), allowed_files}

        :all ->
          {opts, nil}
      end
    else
      {opts, nil}
    end
  end

  defp filter_to_allowed_files(matched_test_files, nil), do: matched_test_files

  defp filter_to_allowed_files(matched_test_files, %MapSet{} = allowed_files) do
    Enum.filter(matched_test_files, &MapSet.member?(allowed_files, Path.expand(&1)))
  end

  defp filter_by_partition(files, _shell, total) when total in [nil, 1],
    do: files

  defp filter_by_partition(files, shell, total) when total > 1 do
    partition = System.get_env("MIX_TEST_PARTITION")

    case partition && Integer.parse(partition) do
      {partition, ""} when partition in 1..total//1 ->
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
  end

  defp filter_by_partition(_files, shell, total) do
    raise_with_shell(
      shell,
      "--partitions : expected to be positive integer, got #{total}"
    )
  end

  defp color_opts(opts) do
    case Keyword.fetch(opts, :color) do
      {:ok, enabled?} ->
        Keyword.put(opts, :colors, enabled: enabled?)

      :error ->
        opts
    end
  end

  defp exit_status_opts(opts) do
    Keyword.put_new(opts, :exit_status, 2)
  end

  defp require_test_helper(shell, dir) do
    file = Path.join(dir, "test_helper.exs")

    if File.exists?(file) do
      {_result, warnings} = Code.with_diagnostics([log: true], fn -> Code.require_file(file) end)
      warnings != []
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
