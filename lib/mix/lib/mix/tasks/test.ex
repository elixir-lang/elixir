defmodule Mix.Tasks.Test do
  use Mix.Task

  @shortdoc "Run a project's tests"

  @moduledoc """
  Run the tests for a project.

  This task will preload the `test/test_helper.exs` which
  should do all testing setup and then require all files
  that matches the given `test_pattern` in parallel.

  Before running tests, it invokes the prepare task
  which defaults to compile and load your project.

  A list of files can be given after the task name in
  order to select the files to compile.

  ## Command line options

  * `--cover` - the directory to include coverage results;
  * `--lines` - reports each uncovered line (if generating coverage);
  * `--merge` - merge the generated HTML files to one per source file;
  * `--force` - forces compilation regardless of module times;
  * `--quick`, `-q` - only compile files that changed;
  * `--no-compile` - do not compile even if files require compilation;
  * `--no-start` - do not start applications after compilation;

  ## Configuration

  * `:test_paths` - path containing tests.
    Defaults to `["test"]`.

  * `:test_pattern` - a pattern to load test files.
    Defaults to `*_test.exs`.

  * `:test_helper` - a file that sets up whatever is necessary
    for testing. Defaults to `test/test_helper.exs`.

  * `:test_coverage` - the directory to include test coverage results.
    Defaults to nil.

  """
  def run(args) do
    { opts, files } = OptionParser.parse(args, aliases: [q: :quick],
                        switches: [quick: :boolean, force: :boolean, lines: :boolean, merge: :boolean])

    unless System.get_env("MIX_ENV") do
      Mix.env(:test)
      Mix.Project.refresh
    end

    Mix.Task.run Mix.project[:prepare_task], args
    project = Mix.project

    cover = Keyword.get(project, :test_coverage, opts[:cover])
    if cover, do: enable_cover(project, cover, opts[:lines], opts[:merge])

    test_helper = Keyword.get(project, :test_helper, "test/test_helper.exs")
    test_helper?(test_helper) && Code.require_file(test_helper)

    test_paths   = if files == [], do: project[:test_paths] || ["test"], else: files
    test_pattern = project[:test_pattern] || "*_test.exs"

    files = Mix.Utils.extract_files(test_paths, test_pattern)
    Kernel.ParallelRequire.files files
  end

  defp test_helper?(file) do
    if nil?(file) or File.exists?(file) do
      true
    else
      raise Mix.Error, message: "Cannot run tests because test helper file #{inspect file} does not exist"
    end
  end

  defp enable_cover(project, cover, do_lines, to_merge) do
    IO.puts "Cover compiling modules ..."
    { ms, _ } = :timer.tc fn() ->
      :cover.start
      :cover.compile_beam_directory(project[:compile_path] |> to_char_list)
    end
    IO.puts "Finished in #{format_ms(ms)} seconds"
    IO.write "Collect test coverage "

    System.at_exit fn(_) ->
      generate_cover(cover, do_lines, to_merge)
    end
  end

  defp generate_cover(cover, do_lines, to_merge) do
    IO.write "Generating cover results "
    File.rm_rf(cover)
    File.mkdir_p!(cover)
    { ms, { covered_lines, unexpected_lines, skipped_lines, uncovered_lines } } = :timer.tc fn() ->
      Enum.each :cover.modules, fn(mod) ->
        IO.write "."
        :cover.analyse_to_file(mod, '#{cover}/#{mod}.html', [:html])
      end
      if to_merge do
        { :ok, html_names } = :file.list_dir(cover)
        merged_files = Enum.reduce html_names, HashDict.new, merge_cover_html(cover, &1, &2)
        File.rm_rf(cover)
        File.mkdir_p!(cover)
        merged_files |> Dict.to_list |> Enum.each write_merged_html(cover, &1)
      end
      { :ok, html_names } = :file.list_dir(cover)
      coverage = Enum.reduce html_names, HashDict.new, analyze_cover_html(cover, do_lines, &1, &2)
      IO.puts ""
      write_index(cover, coverage, do_lines, to_merge)
    end
    IO.puts "Finished in #{format_ms(ms)} seconds"
    print_lines("covered", covered_lines, "red", "green")
    if do_lines do
      print_lines("unexpected", unexpected_lines, "green", "yellow")
      print_lines("skipped", skipped_lines, "green", "yellow")
    end
    print_lines("uncovered", uncovered_lines, "green", "red")
    lines = covered_lines + unexpected_lines + skipped_lines + uncovered_lines
    if lines > 0 do
      color = if do_lines do
                cond do
                  uncovered_lines > 0 -> "red"
                  unexpected_lines + skipped_lines > 0 -> "yellow"
                  true -> "green"
                end
              else
                case perdec(covered_lines, covered_lines + uncovered_lines) do
                  10 -> "green"
                  8 -> "yellow"
                  7 -> "yellow"
                  _low -> "red"
                end
              end
      IO.puts(IO.ANSI.escape("%{#{color}}#{percent(covered_lines + unexpected_lines, lines)}% coverage"))
    end
  end

  defp print_lines(name, number, zero_color, non_zero_color) do
    if number == 0 do
      IO.write(IO.ANSI.escape("%{#{zero_color}}#{number} #{name} lines, "))
    else
      IO.write(IO.ANSI.escape("%{#{non_zero_color}}#{number} #{name} lines, "))
    end
  end

  defp merge_cover_html(cover, html_name, merged_files) do
    IO.write "."
    { :ok, file } = File.open("#{cover}/#{html_name}")
    { file_name, lines } = File.iterator(file) |> Enum.reduce({ html_name, [] }, collect_cover_line(&1, &2))
    lines = Enum.reverse(lines)
    merged_files |> Dict.update(file_name, lines, merge_cover_lines(lines, &1))
  end

  defp collect_cover_line(line, { file_name, lines }) do
    case Regex.captures %r/File generated from (?<file_name>\S+)/g, line do
      [ { :file_name, captured_file_name } ] ->
        { :ok, cwd } = File.cwd
        file_name = captured_file_name |> String.replace("#{cwd}/", "")
                                       |> String.replace("/", "-")
      _mismatch ->
        :ok
    end
    { file_name, [ line | lines ] }
  end

  # TRICKY: This assumes everything matches, except for lines with coverage
  # information in one file and without it in the other.

  defp merge_cover_lines([], []) do
    []
  end

  defp merge_cover_lines([ left_line | left_remaining_lines ], [ right_line | right_remaining_lines ]) do
    line = cond do
             left_line == right_line ->
               left_line
             Regex.match? %r/^\s+\|/, left_line ->
               right_line
             true ->
               left_line
           end
    [ line | merge_cover_lines(left_remaining_lines, right_remaining_lines) ]
  end

  defp write_merged_html(cover, { file_name, lines }) do
    { :ok, file } = File.open("#{cover}/#{file_name}", [ :write ])
    lines |> Enum.each IO.write(file, &1)
    File.close(file)
  end

  defp analyze_cover_html(cover, do_lines, html_name, coverage) do
    IO.write "."
    { :ok, file } = File.open("#{cover}/#{html_name}")
    { file_name, covered_lines, unexpected_lines, skipped_lines, uncovered_lines, _line_number, nesting_level, open_line } =
      File.iterator(file) |> Enum.reduce { html_name, 0, 0, 0, 0, 0, 0, 0 }, analyze_cover_line(do_lines, &1, &2)
    if nesting_level > 0 do
      IO.puts(IO.ANSI.escape("%{red}#{file_name}:#{open_line}: unmatched '# [' directive"))
    end
    coverage = Dict.update coverage, file_name, { [ html_name ], covered_lines, unexpected_lines, skipped_lines, uncovered_lines },
                           fn({ old_html_names, old_covered_lines, old_unexpected_lines, old_skipped_lines, old_uncovered_lines }) ->
                             { [ html_name | old_html_names ],
                               covered_lines + old_covered_lines,
                               unexpected_lines + old_unexpected_lines,
                               skipped_lines + old_skipped_lines,
                               uncovered_lines + old_uncovered_lines }
                           end
    :ok = File.close(file)
    coverage
  end

  defp analyze_cover_line(do_lines, line, { file_name,
                                            covered_lines, unexpected_lines, skipped_lines, uncovered_lines,
                                            line_number, nesting_level, open_line }) do
    if Regex.match? %r/\|/, line do
      line_number = line_number + 1
    end
    case Regex.captures %r/File generated from (?<file_name>\S+)/g, line do
      [ { :file_name, captured_file_name } ] ->
        { :ok, cwd } = File.cwd
        file_name = String.replace(captured_file_name, "#{cwd}/", "")
      _mismatch ->
        :ok
    end
    if do_lines do
      if Regex.match? %r/^\s+\|\s+#\s*\[/, line do
        if nesting_level == 0 do
          open_line = line_number
        end
        nesting_level = nesting_level + 1
      end
      if Regex.match? %r/^\s+\|\s+#\s*\]/, line do
        nesting_level = nesting_level - 1
        if nesting_level < 0 do
          IO.puts(IO.ANSI.escape("%{red}#{file_name}:#{line_number}: unmatched '# ]' directive"))
        end
      end
    end
    cond do
      Regex.match? %r/^\s+[0-9]+\.\.\|\s+\S/, line ->
        if nesting_level > 0 do
          unexpected_lines = unexpected_lines + 1
          IO.puts(IO.ANSI.escape("%{red}#{file_name}:#{line_number}: was reached"))
        else
          covered_lines = covered_lines + 1
        end
      Regex.match? %r/^<font color=red>\s+0\.\.\|/, line ->
        if nesting_level > 0
        || (do_lines && Regex.match? %r/^<font color=red>\s+0\.\.\|\s+def/, line) do
          skipped_lines = skipped_lines + 1
        else
          uncovered_lines = uncovered_lines + 1
          if do_lines do
            IO.puts(IO.ANSI.escape("%{red}#{file_name}:#{line_number}: not reached"))
          end
        end
      true ->
        :ok
    end
    { file_name,
      covered_lines, unexpected_lines, skipped_lines, uncovered_lines,
      line_number, nesting_level, open_line }
  end

  defp write_index(cover, coverage, do_lines, to_merge) do
    { :ok, file } = File.open("#{cover}/index.html", [ :write ])
    IO.write file, """
    <html>
    <head>
    <style type="text/css">
    * {
      font-family: Sans-Serif;
    }
    table {
      border-spacing: 0;
      border-collapse: collapse;
    }
    td {
      border: black solid 1px;
      padding: 0.5em;
      text-align: right;
    }
    td.src, td.modules {
      text-align: left;
    }
    tfoot td, thead td {
      font-weight: bold;
    }
    td.d10, td.d9, td.d8, td.d7, td.d6, td.d5, td.d4 {
      background-color: salmon;
    }
    td.d3, td.d2, td.d1 {
      background-color: gold;
    }
    td.d0 {
      background-color: limegreen;
    }
    td.covered.d0, td.covered.d1, td.covered.d2, td.covered.d3, td.covered.d4, td.covered.d5, td.covered.d6 {
      background-color: salmon;
    }
    td.covered.d7, td.covered.d8, td.covered.d9 {
      background-color: gold;
    }
    td.covered.d10 {
      background-color: limegreen;
    }
    </style>
    </head>
    <body>
    <table>
    <thead>
    <tr>
    <td class='src'>Source file</td>
    <td class='lines'>Lines</td>
    <td class='covered'>Covered</td>
    """
    if do_lines do
      IO.write file, """
      <td class='unexpected'>Unexpected</td>
      <td class='skipped'>Skipped</td>
      """
    end
    IO.puts file, "<td class='uncovered'>Uncovered</td>"
    if !to_merge do
      IO.puts file, "<td class='modules'>Modules</td>"
    end
    IO.write file, """
    </tr>
    </thead>
    <tbody>
    """
    list = Dict.to_list(coverage)
    list |> Enum.sort
         |> Enum.each fn({ source_name, { html_files, covered_lines, unexpected_lines, skipped_lines, uncovered_lines } }) ->
                        lines = covered_lines + unexpected_lines + skipped_lines + uncovered_lines
                        IO.puts file, "<tr>"
                        if to_merge do
                          IO.puts file, "<td class='src'><a href='#{String.replace(source_name, "/", "-")}'>#{source_name}</a></td>"
                        else
                          IO.puts file, "<td class='src'>#{source_name}</td>"
                        end
                        IO.write file, """
                        <td class='lines'>#{lines}</td>
                        <td class='covered #{perdec(covered_lines, lines)}'>#{covered_lines}</td>
                        """
                        if do_lines do
                          IO.write file, """
                          <td class='unexpected #{perdec(unexpected_lines, lines)}'>#{unexpected_lines}</td>
                          <td class='skipped #{perdec(skipped_lines, lines)}'>#{skipped_lines}</td>
                          """
                        end
                        IO.puts file, "<td class='uncovered #{perdec(uncovered_lines, lines)}'>#{uncovered_lines}</td>"
                        if !to_merge do
                          IO.puts file, "<td class='modules'>"
                          html_files |> Enum.sort |> Enum.each fn(html_file) ->
                            IO.puts file, "<a href='#{html_file}'>#{clean_html_name(html_file)}</a>"
                          end
                          IO.puts file, "</td>"
                        end
                        IO.puts file, "</tr>"
                      end
    total = { total_covered_lines, total_unexpected_lines, total_skipped_lines, total_uncovered_lines } =
      list |> Enum.reduce { 0, 0, 0, 0 },
                          fn ({ _source_name, { _html_files, covered_lines, unexpected_lines, skipped_lines, uncovered_lines } },
                              { total_covered_lines, total_unexpected_lines, total_skipped_lines, total_uncovered_lines }) ->
        { total_covered_lines + covered_lines,
          total_unexpected_lines + unexpected_lines,
          total_skipped_lines + skipped_lines,
          total_uncovered_lines + uncovered_lines }
      end
    total_lines = total_covered_lines + total_unexpected_lines + total_skipped_lines + total_uncovered_lines
    IO.write file, """
    </tbody>
    <tfoot>
    <tr>
    <td class='src'>Total</td>
    <td class='lines'>#{total_lines}</td>
    <td class='covered #{perdec(total_covered_lines, total_lines)}'>#{total_covered_lines}</td>
    """
    if do_lines do
      IO.write file, """
      <td class='unexpected #{perdec(total_unexpected_lines, total_lines)}'>#{total_unexpected_lines}</td>
      <td class='skipped #{perdec(total_skipped_lines, total_lines)}'>#{total_skipped_lines}</td>
      """
    end
    IO.puts file, "<td class='uncovered #{perdec(total_uncovered_lines, total_lines)}'>#{total_uncovered_lines}</td>"
    if !to_merge do
      IO.puts file, "<td class='modules'></td>"
    end
    IO.write file, """
    </tr>
    </tfoot>
    </table>
    </body>
    """
    total
  end

  defp percent(part, total) do
    if total == 0 do
      ""
    else
      "#{:erlang.round(part * 100.0 / total)}"
    end
  end

  defp perdec(part, total) do
    if total == 0 do
      "na"
    else
      "d#{:erlang.round(part * 10.0 / total)}"
    end
  end

  defp clean_html_name(html_file) do
    "#{html_file}" |> String.replace("Elixir-", "")
                   |> String.replace(".html", "")
                   |> String.replace("-", ".")
  end

  defp format_ms(ms) do
    if ms < 100000 do
      "0.0#{div(ms, 10000)}"
    else
      ms = div ms, 100000
      "#{div(ms, 10)}.#{rem(ms, 10)}"
    end
  end

end
