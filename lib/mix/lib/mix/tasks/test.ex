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
          {mods, funcs} = coverage_data()
          write_module_overview(mods, output)
          write_function_overview(funcs, output)
        end

        :application.set_env(:cover, :started, true)
      end
    end

    def write_module_overview(modules, output) do
      {:ok, _} = File.open(Path.join(output, "modules.html"), [:write], fn(file) ->
        write_header(file, "Modules")
        Enum.each modules, fn({mod, {cov, uncov}}) ->
          write_line(file, "#{mod}</td>", cov, uncov)
        end
        writer_footer(file)
      end)
    end

    def write_function_overview(funcs, output) do
      {:ok, _} = File.open(Path.join(output, "functions.html"), [:write], fn(file) ->
        write_header(file, "Functions")
        Enum.each funcs, fn({{m, f, a}, {cov, uncov}}) ->
          write_line(file, "#{m}.#{f}/#{a}", cov, uncov)
        end
        writer_footer(file)
      end)
    end
    
    defp write_line(file, entity, cov, uncov) do
      ratio = (1.0 * cov/(cov + uncov))*100
      IO.puts(file, "<tr><td>#{entity}</td>" <> 
                    "<td align=\"right\">#{cov}</td><td align=\"right\">#{uncov}</td>" <>
                    "<td align=\"right\">#{Float.round(ratio, 2)}%</td></tr>")
    end
    
    defp write_header(file, title) do
      IO.puts(file, "<html><head><title>#{title} Coverage Report</title></head><body><table>\n" <>
        "<h1>#{title} Coverage Report</h1>" <>
        "<tr><th>#{title}</th><th>Covered Lines</th><th>Uncovered Lines</th><th>Coverage Ratio</th></tr>" )      
    end
  
    defp writer_footer(file), do: IO.puts(file, "</table></body></html>")

    @doc """
    Returns detailed coverage data `{mod, mf}` for all modules from the `:cover` application. 

    ## The `mod` data
    The `mod` data is a list of pairs: `{modulename, {no of covered lines, no of uncovered lines}}`
  
    ## The `mf` data
    The `mf` data is list of pairs: `{{m, f, a}, {no of covered lines, no of uncovered lines}}`

    """
    def coverage_data() do
      modules = Enum.map(:cover.modules, fn(mod) ->
        {:ok, {m, {cov, noncov}}} = :cover.analyse(mod, :coverage, :module) 
        {m, {cov, noncov}}
      end) |> Enum.sort
      mfunc = Enum.flat_map(:cover.modules, fn(mod) ->
        {:ok, funcs} = :cover.analyse(mod, :coverage, :function)
        funcs
      end) |> Enum.sort
      {modules, mfunc}
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
  * `--include` - skip tests that do not match the filter
  * `--exclude` - skip tests that match the filter

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
             exclude: :keep]

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

    test_paths = project[:test_paths] || ["test"]
    Enum.each(test_paths, &require_test_helper(&1))

    if opts[:include] do
      inclusions = ExUnit.Filters.parse(Keyword.get_values(opts, :include))
      opts = Keyword.put(opts, :include, inclusions)
    end

    if opts[:exclude] do
      exclusions = ExUnit.Filters.parse(Keyword.get_values(opts, :exclude))
      opts = Keyword.put(opts, :exclude, exclusions)
    end

    opts = Dict.take(opts, [:trace, :max_cases, :color, :include, :exclude])
    ExUnit.configure(opts)

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
