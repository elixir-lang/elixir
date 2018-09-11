defmodule Mix.Tasks.Test do
  defmodule Cover do
    @default_threshold 90

    @moduledoc false

    def start(compile_path, opts) do
      Mix.shell().info("Cover compiling modules ...")
      _ = :cover.stop()
      _ = :cover.start()

      case :cover.compile_beam_directory(compile_path |> to_charlist) do
        results when is_list(results) ->
          :ok

        {:error, _} ->
          Mix.raise("Failed to cover compile directory: " <> compile_path)
      end

      fn ->
        Mix.shell().info("\nGenerating cover results ...\n")
        {:result, ok, _fail} = :cover.analyse(:coverage, :line)

        {module_results, totals} = gather_coverage(ok, :cover.modules())
        module_results = Enum.sort_by(module_results, &percentage(elem(&1, 1)), &>=/2)

        if summary_opts = Keyword.get(opts, :summary, true) do
          console(module_results, totals, summary_opts)
        end

        html(module_results, opts)
      end
    end

    defp gather_coverage(results, keep) do
      # When gathering coverage results, we need to skip any
      # entry with line equal to 0 as those are generated code.
      #
      # We may also have multiple entries on the same line.
      # Each line is only considered once.
      #
      # We use ETS for performance, to avoid working with nested maps.
      table = :ets.new(__MODULE__, [:set, :private])

      try do
        Enum.each(results, fn
          {{module, 0}, _} -> :ets.insert(table, {{module, 0}, :dummy})
          {{module, line}, {1, 0}} -> :ets.insert(table, {{module, line}, true})
          {{module, line}, {0, 1}} -> :ets.insert_new(table, {{module, line}, false})
        end)

        module_results =
          for module <- keep,
              results = read_module_cover_results(table, module),
              do: {module, results}

        total_covered = :ets.select_count(table, [{{:_, true}, [], [true]}])
        total_not_covered = :ets.select_count(table, [{{:_, false}, [], [true]}])

        {module_results, {total_covered, total_not_covered}}
      after
        :ets.delete(table)
      end
    end

    defp read_module_cover_results(table, module) do
      covered = :ets.select_count(table, [{{{module, :_}, true}, [], [true]}])
      not_covered = :ets.select_count(table, [{{{module, :_}, false}, [], [true]}])
      {covered, not_covered}
    end

    defp console(results, totals, true), do: console(results, totals, [])

    defp console(results, totals, opts) when is_list(opts) do
      Mix.shell().info("Percentage | Module")
      Mix.shell().info("-----------|--------------------------")
      Enum.each(results, &display(&1, opts))
      Mix.shell().info("-----------|--------------------------")
      display({"Total", totals}, opts)
      Mix.shell().info("")
    end

    defp html(results, opts) do
      output = opts[:output]
      File.mkdir_p!(output)

      for {mod, _} <- results do
        {:ok, _} = :cover.analyse_to_file(mod, '#{output}/#{mod}.html', [:html])
      end

      Mix.shell().info([
        "Generated HTML coverage results in '",
        output,
        "' directory\n"
      ])
    end

    defp color(percentage, true), do: color(percentage, @default_threshold)
    defp color(_, false), do: ""
    defp color(percentage, threshold) when percentage > threshold, do: :green
    defp color(_, _), do: :red

    defp display({name, coverage}, opts) do
      threshold = Keyword.get(opts, :threshold, @default_threshold)
      percentage = percentage(coverage)

      Mix.shell().info([
        color(percentage, threshold),
        format(percentage, 9),
        "%",
        :reset,
        " | ",
        format_name(name)
      ])
    end

    defp percentage({0, 0}), do: 100.0
    defp percentage({covered, not_covered}), do: covered / (covered + not_covered) * 100

    defp format(number, length), do: :io_lib.format("~#{length}.2f", [number])

    defp format_name(name) when is_binary(name), do: name
    defp format_name(mod) when is_atom(mod), do: inspect(mod)
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

    * `--color` - enables color in the output
    * `--cover` - runs coverage tool. See "Coverage" section below
    * `--exclude` - excludes tests that match the filter
    * `--failed` - runs only tests that failed the last time they ran
    * `--force` - forces compilation regardless of modification times
    * `--formatter` - formatter module
    * `--include` - includes tests that match the filter
    * `--listen-on-stdin` - runs tests, and then listens on stdin. Receiving a newline will
      result in the tests being run again. Very useful when combined with `--stale` and
      external commands which produce output on stdout upon file system modification
    * `--max-cases` - sets the maximum number of tests running async. Only tests from
      different modules run in parallel. Defaults to twice the number of cores
    * `--max-failures` - the suite stops evaluating tests when this number of test failures
      is reached
    * `--no-archives-check` - does not check archives
    * `--no-color` - disables color in the output
    * `--no-compile` - does not compile, even if files require compilation
    * `--no-deps-check` - does not check dependencies
    * `--no-elixir-version-check` - does not check the Elixir version from `mix.exs`
    * `--no-start` - does not start applications after compilation
    * `--only` - runs only tests that match the filter
    * `--preload-modules` - preloads all modules defined in applications
    * `--raise` - raises if the test suite failed
    * `--seed` - seeds the random number generator used to randomize the order of tests;
      `--seed 0` disables randomization
    * `--slowest` - prints timing information for the N slowest tests.
      Automatically sets `--trace` and `--preload-modules`
    * `--stale` - runs only tests which reference modules that changed since the
      last time tests were ran with `--stale`. You can read more about this option
      in the "Stale" section below
    * `--timeout` - sets the timeout for the tests
    * `--trace` - runs tests with detailed reporting. Automatically sets `--max-cases` to 1.
      Note that in trace mode test timeouts will be ignored

  See `ExUnit.configure/1` for more information on configuration options.

  ## Filters

  ExUnit provides tags and filtering functionality that allow developers
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
  first (either in the test helper or via the `--exclude` option) the
  `--include` flag has no effect.

  For this reason, Mix also provides an `--only` option that excludes all
  tests and includes only the given ones:

      mix test --only external

  Which is equivalent to:

      mix test --include external --exclude test

  In case a single file is being tested, it is possible to pass a specific
  line number:

      mix test test/some/particular/file_test.exs:12

  Which is equivalent to:

      mix test --only line:12 test/some/particular/file_test.exs

  If the given line starts a `describe` block, the line filter runs all tests in it.
  Otherwise, it runs the closest test on or before the given line number.

  Note that in the case where a single file contains more than one test module (test case),
  the line filter applies to every test case before the given line number. Thus, more
  than one test might be executed for the run.

  ## Configuration

    * `:test_paths` - list of paths containing test files. Defaults to
      `["test"]` if the `test` directory exists; otherwise, it defaults to `[]`.
      It is expected that all test paths contain a `test_helper.exs` file

    * `:test_pattern` - a pattern to load test files. Defaults to `*_test.exs`

    * `:warn_test_pattern` - a pattern to match potentially misnamed test files
      and display a warning. Defaults to `*_test.ex`

    * `:test_coverage` - a set of options to be passed down to the coverage
      mechanism

  ## Coverage

  The `:test_coverage` configuration accepts the following options:

    * `:output` - the output directory for cover results. Defaults to `"cover"`
    * `:tool` - the coverage tool
    * `:summary` - summary output configuration; can be either a boolean
      or a keyword list. When a keyword list is passed, it can specify a `:threshold`,
      which is a boolean or numeric value that enables coloring of code coverage
      results in red or green depending on whether the percentage is below or
      above the specified threshold, respectively. Defaults to `[threshold: 90]`

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

  @switches [
    force: :boolean,
    color: :boolean,
    cover: :boolean,
    trace: :boolean,
    max_cases: :integer,
    max_failures: :string,
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
    preload_modules: :boolean
  ]

  @cover [output: "cover", tool: Cover]

  def run(args) do
    {opts, files} = OptionParser.parse!(args, strict: @switches)

    if opts[:listen_on_stdin] do
      System.at_exit(fn _ ->
        IO.gets(:stdio, "")
        Mix.shell().info("Restarting...")
        :init.restart()
        Process.sleep(:infinity)
      end)
    end

    unless System.get_env("MIX_ENV") || Mix.env() == :test do
      Mix.raise(
        "\"mix test\" is running in the \"#{Mix.env()}\" environment. If you are " <>
          "running tests alongside another task, please set MIX_ENV explicitly"
      )
    end

    Mix.Task.run("loadpaths", args)

    if Keyword.get(opts, :compile, true) do
      Mix.Project.compile(args)
    end

    opts = validate_max_failures_opts(opts)

    project = Mix.Project.config()

    # Start cover after we load deps but before we start the app.
    cover =
      if opts[:cover] do
        compile_path = Mix.Project.compile_path(project)
        cover = Keyword.merge(@cover, project[:test_coverage] || [])
        cover[:tool].start(compile_path, cover)
      end

    # Start the app and configure ExUnit with command line options
    # before requiring test_helper.exs so that the configuration is
    # available in test_helper.exs
    Mix.shell().print_app
    app_start_args = if opts[:slowest], do: ["--preload-modules" | args], else: args
    Mix.Task.run("app.start", app_start_args)

    # Ensure ExUnit is loaded.
    case Application.load(:ex_unit) do
      :ok -> :ok
      {:error, {:already_loaded, :ex_unit}} -> :ok
    end

    # Then configure ExUnit again so that command line options
    # override test_helper.exs
    {ex_unit_opts, allowed_files} = process_ex_unit_opts(opts)
    ExUnit.configure(ex_unit_opts)

    test_paths = project[:test_paths] || default_test_paths()
    Enum.each(test_paths, &require_test_helper(&1))
    ExUnit.configure(merge_helper_opts(ex_unit_opts))

    # Finally parse, require and load the files
    test_files = parse_files(files, test_paths)
    test_pattern = project[:test_pattern] || "*_test.exs"
    warn_test_pattern = project[:warn_test_pattern] || "*_test.ex"

    matched_test_files =
      test_files
      |> Mix.Utils.extract_files(test_pattern)
      |> filter_to_allowed_files(allowed_files)

    display_warn_test_pattern(test_files, test_pattern, matched_test_files, warn_test_pattern)

    case CT.require_and_run(matched_test_files, test_paths, opts) do
      {:ok, %{excluded: excluded, failures: failures, total: total}} ->
        cover && cover.()

        option_only_present? = Keyword.has_key?(opts, :only)

        cond do
          failures > 0 and opts[:raise] ->
            Mix.raise("\"mix test\" failed")

          failures > 0 ->
            System.at_exit(fn _ -> exit({:shutdown, 1}) end)

          excluded == total and option_only_present? ->
            message = "The --only option was given to \"mix test\" but no test was executed"
            raise_or_error_at_exit(message, opts)

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
            raise_or_error_at_exit(message <> Enum.join(files, ", "), opts)
        end

        :ok
    end
  end

  defp raise_or_error_at_exit(message, opts) do
    if opts[:raise] do
      Mix.raise(message)
    else
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
    :only_test_ids
  ]

  @doc false
  def process_ex_unit_opts(opts) do
    {opts, allowed_files} =
      opts
      |> manifest_opts()
      |> failed_opts()

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
    merge_opts(opts, :exclude)
  end

  defp default_opts(opts) do
    # Set autorun to false because Mix
    # automatically runs the test suite for us.
    [autorun: false] ++ opts
  end

  defp parse_files([], test_paths) do
    test_paths
  end

  defp parse_files([single_file], _test_paths) do
    # Check if the single file path matches test/path/to_test.exs:123. If it does,
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

  def formatter_opts(opts) do
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
    Keyword.put(opts, :failures_manifest_file, manifest_file)
  end

  defp failed_opts(opts) do
    if opts[:failed] do
      if opts[:stale] do
        Mix.raise("Combining `--failed` and `--stale` is not supported.")
      end

      {allowed_files, failed_ids} = ExUnit.Filters.failure_info(opts[:failures_manifest_file])
      {Keyword.put(opts, :only_test_ids, failed_ids), allowed_files}
    else
      {opts, nil}
    end
  end

  defp filter_to_allowed_files(matched_test_files, nil), do: matched_test_files

  defp filter_to_allowed_files(matched_test_files, %MapSet{} = allowed_files) do
    Enum.filter(matched_test_files, &MapSet.member?(allowed_files, Path.expand(&1)))
  end

  defp color_opts(opts) do
    case Keyword.fetch(opts, :color) do
      {:ok, enabled?} ->
        Keyword.put(opts, :colors, enabled: enabled?)

      :error ->
        opts
    end
  end

  defp validate_max_failures_opts(opts) do
    case Keyword.fetch(opts, :max_failures) do
      {:ok, value} ->
        validate_max_failures_opt!(opts, value)

      :error ->
        Keyword.put(opts, :max_failures, :infinity)
    end
  end

  defp validate_max_failures_opt!(opts, value) when value in ["infinity", ":infinity"],
    do: Keyword.put(opts, :max_failures, :infinity)

  defp validate_max_failures_opt!(opts, value) do
    try do
      String.to_integer(value)
    rescue
      ArgumentError ->
        opts
    else
      integer when integer >= 1 ->
        Keyword.put(opts, :max_failures, integer)

      _ ->
        opts
    end
  end

  defp merge_opts(opts, key) do
    value = List.wrap(Application.get_env(:ex_unit, key, []))
    Keyword.update(opts, key, value, &Enum.uniq(&1 ++ value))
  end

  defp require_test_helper(dir) do
    file = Path.join(dir, "test_helper.exs")

    if File.exists?(file) do
      Code.require_file(file)
    else
      Mix.raise("Cannot run tests because test helper file #{inspect(file)} does not exist")
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
