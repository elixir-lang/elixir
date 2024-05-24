defmodule Mix.Tasks.Test.Coverage do
  use Mix.Task

  @moduledoc """
  Build reports from exported test coverage.

  In this moduledoc, we will describe how the default test
  coverage works in Elixir and also explore how it is capable
  of exporting coverage results to group reports from multiple
  test runs.

  ## Line coverage

  Elixir uses Erlang's [`:cover`](https://www.erlang.org/doc/man/cover.html)
  for its default test coverage. Erlang coverage is done by tracking
  *executable lines of code*. This implies blank lines, code comments,
  function signatures, and patterns are not necessarily executable and
  therefore won't be tracked in coverage reports. Code in macros are
  also often executed at compilation time, and therefore may not be covered.
  Similarly, Elixir AST literals, such as atoms, are not executable either.

  Let's see an example:

      if some_condition? do
        do_this()
      else
        do_that()
      end

  In the example above, if your tests exercise both `some_condition? == true`
  and `some_condition? == false`, all branches will be covered, as they all
  have executable code. However, the following code

      if some_condition? do
        do_this()
      else
        :default
      end

  won't ever mark the `:default` branch as covered, as there is no executable
  code in the `else` branch. Note, however, this issue does not happen on `case`
  or `cond`, as Elixir is able to mark the clause operator `->` as executable in
  such corner cases:

      case some_condition? do
        true ->
          do_this()

        false ->
          :default
      end

  If the code above is tested with both conditions, you should see entries
  in both branches marked as covered.

  Finally, it is worth discussing that line coverage by itself has its own
  limitations. For example, take the following code:

      do_this() || do_that()

  Line coverage is not capable of expressing that both `do_this()` and
  `do_that()` have been executed, since as soon as `do_this()` is executed,
  the whole line is covered. Other techniques, such as branch coverage,
  can help spot those cases, but they are not currently supported by the
  default coverage tool.

  Overall, code coverage can be a great tool for finding flaws in our
  code (such as functions that haven't been covered) but it can also lead
  teams into a false sense of security since 100% coverage never means all
  different executions flows have been asserted, even with the most advanced
  coverage techniques. It is up to you and your team to specify how much
  emphasis you want to place on it.

  ## Exporting coverage

  This task can be used when you need to group the coverage
  across multiple test runs. Let's see some examples.

  ### Example: aggregating partitioned runs

  If you partition your tests across multiple runs,
  you can unify the report as shown below:

      $ MIX_TEST_PARTITION=1 mix test --partitions 2 --cover
      $ MIX_TEST_PARTITION=2 mix test --partitions 2 --cover
      $ mix test.coverage

  This works because the `--partitions` option
  automatically exports the coverage results.

  ### Example: aggregating coverage reports from all umbrella children

  If you run `mix test.coverage` inside an umbrella,
  it will automatically gather exported cover results
  from all umbrella children - as long as the coverage
  results have been exported, like this:

      # from the umbrella root
      $ mix test --cover --export-coverage default
      $ mix test.coverage

  Of course, if you want to actually partition the tests,
  you can also do:

      # from the umbrella root
      $ MIX_TEST_PARTITION=1 mix test --partitions 2 --cover
      $ MIX_TEST_PARTITION=2 mix test --partitions 2 --cover
      $ mix test.coverage

  On the other hand, if you want partitioned tests but
  per-app reports, you can do:

      # from the umbrella root
      $ MIX_TEST_PARTITION=1 mix test --partitions 2 --cover
      $ MIX_TEST_PARTITION=2 mix test --partitions 2 --cover
      $ mix cmd mix test.coverage

  When running `test.coverage` from the umbrella root, it
  will use the `:test_coverage` configuration from the umbrella
  root.

  Finally, note the coverage itself is not measured across
  the projects themselves. For example, if project B depends
  on A, and if there is code in A that is only executed from
  project B, those lines will not be marked as covered, which
  is important, as those projects should be developed and tested
  in isolation.

  ### Other scenarios

  There may be other scenarios where you want to export coverage.
  For example, you may have broken your test suite into two, one
  for unit tests and another for integration tests. In such scenarios,
  you can explicitly use the `--export-coverage` command line option,
  or the `:export` option under `:test_coverage` in your `mix.exs` file.
  """

  @shortdoc "Build report from exported test coverage"
  @default_threshold 90

  @doc false
  def run(_args) do
    Mix.Task.run("compile")
    Mix.ensure_application!(:tools)
    config = Mix.Project.config()
    test_coverage = config[:test_coverage] || []
    {cover_paths, compile_paths} = apps_paths(config, test_coverage)
    pid = cover_compile(compile_paths)

    case Enum.flat_map(cover_paths, &Path.wildcard(Path.join(&1, "*.coverdata"))) do
      [] ->
        Mix.shell().error(
          "Could not find .coverdata file in any of the paths: " <>
            Enum.join(cover_paths, ", ")
        )

      entries ->
        for entry <- entries do
          Mix.shell().info("Importing cover results: #{entry}")
          :ok = :cover.import(String.to_charlist(entry))
        end

        # Silence analyse import messages emitted by cover
        {:ok, string_io} = StringIO.open("")
        Process.group_leader(pid, string_io)
        Mix.shell().info("")
        generate_cover_results(test_coverage)
    end
  end

  defp apps_paths(config, test_coverage) do
    output = Keyword.get(test_coverage, :output, "cover")

    if apps_paths = Mix.Project.apps_paths(config) do
      build_path = Mix.Project.build_path(config)
      apps_paths = Enum.sort(apps_paths)

      compile_paths =
        Enum.map(apps_paths, fn {app, _} ->
          Path.join([build_path, "lib", Atom.to_string(app), "ebin"])
        end)

      {Enum.map(apps_paths, fn {_, path} -> Path.join(path, output) end), compile_paths}
    else
      {[output], [Mix.Project.compile_path(config)]}
    end
  end

  @doc false
  def start(compile_path, opts) do
    Mix.shell().info("Cover compiling modules ...")
    Mix.ensure_application!(:tools)

    if Keyword.get(opts, :local_only, true) do
      :cover.local_only()
    end

    cover_compile([compile_path])

    if name = opts[:export] do
      fn ->
        Mix.shell().info("\nExporting cover results ...\n")
        export_cover_results(name, opts)
      end
    else
      fn ->
        Mix.shell().info("\nGenerating cover results ...\n")
        generate_cover_results(opts)
      end
    end
  end

  defp cover_compile(compile_paths) do
    _ = :cover.stop()
    {:ok, pid} = :cover.start()

    for compile_path <- compile_paths do
      case :cover.compile_beam(beams(compile_path)) do
        results when is_list(results) ->
          :ok

        {:error, reason} ->
          Mix.raise(
            "Failed to cover compile directory #{inspect(Path.relative_to_cwd(compile_path))} " <>
              "with reason: #{inspect(reason)}"
          )
      end
    end

    pid
  end

  # Pick beams from the compile_path but if by any chance it is a protocol,
  # gets its path from the code server (which will most likely point to
  # the consolidation directory as long as it is enabled).
  defp beams(dir) do
    consolidation_dir = Mix.Project.consolidation_path()

    consolidated =
      case File.ls(consolidation_dir) do
        {:ok, files} -> files
        _ -> []
      end

    for file <- File.ls!(dir), Path.extname(file) == ".beam" do
      with true <- file in consolidated,
           [_ | _] = path <- :code.which(file |> Path.rootname() |> String.to_atom()) do
        path
      else
        _ -> String.to_charlist(Path.join(dir, file))
      end
    end
  end

  defp export_cover_results(name, opts) do
    output = Keyword.get(opts, :output, "cover")
    File.mkdir_p!(output)

    case :cover.export(~c"#{output}/#{name}.coverdata") do
      :ok ->
        Mix.shell().info("Run \"mix test.coverage\" once all exports complete")

      {:error, reason} ->
        Mix.shell().error("Export failed with reason: #{inspect(reason)}")
    end
  end

  defp generate_cover_results(opts) do
    {:result, ok, _fail} = :cover.analyse(:coverage, :line)
    ignore = opts[:ignore_modules] || []
    modules = Enum.reject(:cover.modules(), &ignored?(&1, ignore))

    if summary_opts = Keyword.get(opts, :summary, true) do
      summary(ok, modules, summary_opts)
    end

    html(modules, opts)
  end

  defp ignored?(mod, ignores) do
    Enum.any?(ignores, &ignored_any?(mod, &1))
  end

  defp ignored_any?(mod, %Regex{} = re), do: Regex.match?(re, inspect(mod))
  defp ignored_any?(mod, other), do: mod == other

  defp html(modules, opts) do
    output = Keyword.get(opts, :output, "cover")
    File.mkdir_p!(output)

    modules
    |> Enum.map(fn mod ->
      pid = :cover.async_analyse_to_file(mod, ~c"#{output}/#{mod}.html", [:html])
      Process.monitor(pid)
    end)
    |> Enum.each(fn ref ->
      receive do
        {:DOWN, ^ref, :process, _pid, _reason} ->
          :ok
      end
    end)

    Mix.shell().info("Generated HTML coverage results in #{inspect(output)} directory")
  end

  defp summary(results, keep, summary_opts) do
    {module_results, totals} = gather_coverage(results, keep)
    module_results = Enum.sort(module_results, :desc)
    print_summary(module_results, totals, summary_opts)

    if totals < get_threshold(summary_opts) do
      print_failed_threshold(totals, get_threshold(summary_opts))
      System.at_exit(fn _ -> exit({:shutdown, 3}) end)
    end

    :ok
  end

  defp gather_coverage(results, keep) do
    keep_set = MapSet.new(keep)

    # When gathering coverage results, we need to skip any
    # entry with line equal to 0 as those are generated code.
    #
    # We may also have multiple entries on the same line.
    # Each line is only considered once.
    #
    # We use ETS for performance, to avoid working with nested maps.
    table = :ets.new(__MODULE__, [:set, :private])

    try do
      for {{module, line}, cov} <- results, module in keep_set, line != 0 do
        case cov do
          {1, 0} -> :ets.insert(table, {{module, line}, true})
          {0, 1} -> :ets.insert_new(table, {{module, line}, false})
        end
      end

      module_results = for module <- keep, do: {read_cover_results(table, module), module}
      {module_results, read_cover_results(table, :_)}
    after
      :ets.delete(table)
    end
  end

  defp read_cover_results(table, module) do
    covered = :ets.select_count(table, [{{{module, :_}, true}, [], [true]}])
    not_covered = :ets.select_count(table, [{{{module, :_}, false}, [], [true]}])
    percentage(covered, not_covered)
  end

  defp percentage(0, 0), do: 100.0
  defp percentage(covered, not_covered), do: covered / (covered + not_covered) * 100

  defp print_summary(results, totals, true), do: print_summary(results, totals, [])

  defp print_summary(results, totals, opts) when is_list(opts) do
    threshold = get_threshold(opts)
    Mix.shell().info("Percentage | Module")
    Mix.shell().info("-----------|--------------------------")
    results |> Enum.sort() |> Enum.each(&display(&1, threshold))
    Mix.shell().info("-----------|--------------------------")
    display({totals, "Total"}, threshold)
    Mix.shell().info("")
  end

  defp print_failed_threshold(totals, threshold) do
    Mix.shell().info("Coverage test failed, threshold not met:\n")
    Mix.shell().info("    Coverage:  #{format_number(totals, 6)}%")
    Mix.shell().info("    Threshold: #{format_number(threshold, 6)}%")
    Mix.shell().info("")
  end

  defp display({percentage, name}, threshold) do
    Mix.shell().info([
      color(percentage, threshold),
      format_number(percentage, 9),
      "%",
      :reset,
      " | ",
      format_name(name)
    ])
  end

  defp color(percentage, true), do: color(percentage, @default_threshold)
  defp color(_, false), do: ""
  defp color(percentage, threshold) when percentage >= threshold, do: :green
  defp color(_, _), do: :red

  defp format_number(number, length) when is_integer(number),
    do: format_number(number / 1, length)

  defp format_number(number, length), do: :io_lib.format("~#{length}.2f", [number])

  defp format_name(name) when is_binary(name), do: name
  defp format_name(mod) when is_atom(mod), do: inspect(mod)

  defp get_threshold(true), do: @default_threshold
  defp get_threshold(opts), do: Keyword.get(opts, :threshold, @default_threshold)
end
