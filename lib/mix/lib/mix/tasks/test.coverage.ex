defmodule Mix.Tasks.Test.Coverage do
  use Mix.Task

  @moduledoc """
  Build coverage report from exported coverage:

      MIX_TEST_PARTITION=1 mix test --partitions 2
      MIX_TEST_PARTITION=2 mix test --partitions 2
      mix test.coverage

  When using `--cover` with the default coverage tool,
  it supports an `:export` option that exports the coverage
  results into a directory. This is useful when there are
  multiple test suites or when a single test suite is
  partitioned across multiple runs when using the
  `mix test --partitions N` option.

  Once multiple test runs are exported, this task can be
  used to generate an aggregated report.
  """

  @shortdoc "Build coverage report from exported coverage"
  @recursive true
  @preferred_cli_env :test
  @default_threshold 90

  @doc false
  def run(_args) do
    config = Mix.Project.config()
    compile_path = Mix.Project.compile_path(config)
    test_coverage = config[:test_coverage] || []
    output = Keyword.get(test_coverage, :output, "cover")
    pid = cover_compile(compile_path)

    case Path.wildcard("#{output}/*.coverdata") do
      [] ->
        Mix.shell().error("Could not find any .coverdata file inside #{output}/")

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

  @doc false
  def start(compile_path, opts) do
    Mix.shell().info("Cover compiling modules ...")
    cover_compile(compile_path)

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

  defp cover_compile(compile_path) do
    _ = :cover.stop()
    {:ok, pid} = :cover.start()

    case :cover.compile_beam_directory(compile_path |> to_charlist) do
      results when is_list(results) ->
        :ok

      {:error, reason} ->
        Mix.raise(
          "Failed to cover compile directory #{inspect(Path.relative_to_cwd(compile_path))} " <>
            "with reason: #{inspect(reason)}"
        )
    end

    pid
  end

  defp export_cover_results(name, opts) do
    output = Keyword.get(opts, :output, "cover")
    File.mkdir_p!(output)

    case :cover.export('#{output}/#{name}.coverdata') do
      :ok ->
        Mix.shell().info("Run \"mix test.coverage\" once all exports complete")

      {:error, reason} ->
        Mix.shell().error("Export failed with reason: #{inspect(reason)}")
    end
  end

  defp generate_cover_results(opts) do
    {:result, ok, _fail} = :cover.analyse(:coverage, :line)
    modules = :cover.modules()

    if summary_opts = Keyword.get(opts, :summary, true) do
      summary(ok, modules, summary_opts)
    end

    html(modules, opts)
  end

  defp html(modules, opts) do
    output = Keyword.get(opts, :output, "cover")
    File.mkdir_p!(output)

    for mod <- modules do
      {:ok, _} = :cover.analyse_to_file(mod, '#{output}/#{mod}.html', [:html])
    end

    Mix.shell().info("Generated HTML coverage results in #{inspect(output)} directory")
  end

  defp summary(results, keep, summary_opts) do
    {module_results, totals} = gather_coverage(results, keep)
    module_results = Enum.sort(module_results, :desc)
    print_summary(module_results, totals, summary_opts)
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
    Mix.shell().info("Percentage | Module")
    Mix.shell().info("-----------|--------------------------")
    results |> Enum.sort() |> Enum.each(&display(&1, opts))
    Mix.shell().info("-----------|--------------------------")
    display({totals, "Total"}, opts)
    Mix.shell().info("")
  end

  defp display({percentage, name}, opts) do
    threshold = Keyword.get(opts, :threshold, @default_threshold)

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
  defp color(percentage, threshold) when percentage > threshold, do: :green
  defp color(_, _), do: :red

  defp format_number(number, length), do: :io_lib.format("~#{length}.2f", [number])

  defp format_name(name) when is_binary(name), do: name
  defp format_name(mod) when is_atom(mod), do: inspect(mod)
end
