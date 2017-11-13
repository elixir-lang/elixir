defmodule Mix.Tasks.Format do
  use Mix.Task

  @shortdoc "Formats the given files/patterns"

  @moduledoc """
  Formats the given files and patterns.

      mix format mix.exs "lib/**/*.{ex,exs}" "test/**/*.{ex,exs}"

  If any of the files is `-`, then the output is read from stdin
  and written to stdout.

  Formatting is done with the `Code.format_string!/2` function.
  A `.formatter.exs` file can also be defined for customizing input
  files and the formatter itself.

  ## Options

    * `--check-formatted` - check that the file is already formatted.
      This is useful in pre-commit hooks and CI scripts if you want to
      reject contributions with unformatted code. However, keep in mind,
      that the formatting output may differ between Elixir versions as
      improvements and fixes are applied to the formatter.

    * `--check-equivalent` - check if the file after formatting has the
      same AST. If the ASTs are not equivalent, it is a bug in the code
      formatter. This option is recommended if you are automatically
      formatting files.

    * `--dry-run` - do not save files after formatting.

    * `--dot-formatter` - the file with formatter configuration.
      Defaults to `.formatter.exs` if one is available, see next section.

  If any of the `--check-*` flags are given and a check fails, the formatted
  contents won't be written to disk nor printed to stdout.

  ## .formatter.exs

  The formatter will read a `.formatter.exs` in the current directory for
  formatter configuration. It should return a keyword list with any of the
  options supported by `Code.format_string!/2`.

  The `.formatter.exs` also supports an `:inputs` field which specifies the
  default inputs to be used by this task:

      [
        inputs: ["mix.exs", "{config,lib,test}/**/*.{ex,exs}"]
      ]

  ## When to format code

  We recommend developers to format code directly in their editors. Either
  automatically on save or via an explicit command/key binding. If such option
  is not yet available in your editor of choice, adding the required integration
  is relatively simple as it is a matter of invoking

      cd $project && mix format $file

  where `$file` refers to the current file and `$project` is the root of your
  project.

  It is also possible to format code across the whole project by passing a list
  of patterns and files to `mix format`, as showed at the top of this task
  documentation. This list can also be set in the `.formatter.exs` under the
  `:inputs` key.
  """

  @switches [
    check_equivalent: :boolean,
    check_formatted: :boolean,
    dot_formatter: :string,
    dry_run: :boolean
  ]

  def run(args) do
    {opts, args} = OptionParser.parse!(args, strict: @switches)
    formatter_opts = eval_dot_formatter(opts)

    args
    |> expand_args(formatter_opts)
    |> Task.async_stream(&format_file(&1, opts, formatter_opts), ordered: false, timeout: 30000)
    |> Enum.reduce({[], [], []}, &collect_status/2)
    |> check!()
  end

  defp eval_dot_formatter(opts) do
    case dot_formatter(opts) do
      {:ok, dot_formatter} ->
        {formatter_opts, _} = Code.eval_file(dot_formatter)

        unless Keyword.keyword?(formatter_opts) do
          Mix.raise(
            "Expected #{inspect(dot_formatter)} to return a keyword list, " <>
              "got: #{inspect(formatter_opts)}"
          )
        end

        formatter_opts

      :error ->
        []
    end
  end

  defp dot_formatter(opts) do
    cond do
      dot_formatter = opts[:dot_formatter] -> {:ok, dot_formatter}
      File.regular?(".formatter.exs") -> {:ok, ".formatter.exs"}
      true -> :error
    end
  end

  defp expand_args([], formatter_opts) do
    if inputs = formatter_opts[:inputs] do
      expand_files_and_patterns(List.wrap(inputs), ".formatter.exs")
    else
      Mix.raise(
        "Expected one or more files/patterns to be given to mix format " <>
          "or for a .formatter.exs to exist with an :inputs key"
      )
    end
  end

  defp expand_args(files_and_patterns, _formatter_opts) do
    expand_files_and_patterns(files_and_patterns, "command line")
  end

  defp expand_files_and_patterns(files_and_patterns, context) do
    files =
      for file_or_pattern <- files_and_patterns,
          file <- stdin_or_wildcard(file_or_pattern),
          uniq: true,
          do: file

    if files == [] do
      Mix.raise(
        "Could not find a file to format. The files/patterns from #{context} " <>
          "did not point to any existing file. Got: #{inspect(files_and_patterns)}"
      )
    end

    files
  end

  defp stdin_or_wildcard("-"), do: [:stdin]
  defp stdin_or_wildcard(path), do: Path.wildcard(path)

  defp read_file(:stdin) do
    {IO.stream(:stdio, :line) |> Enum.to_list() |> IO.iodata_to_binary(), file: "stdin"}
  end

  defp read_file(file) do
    {File.read!(file), file: file}
  end

  defp write_file(:stdin, contents), do: IO.write(contents)
  defp write_file(file, contents), do: File.write!(file, contents)

  defp format_file(file, task_opts, formatter_opts) do
    {input, extra_opts} = read_file(file)
    output = [Code.format_string!(input, extra_opts ++ formatter_opts), ?\n]

    check_equivalent? = Keyword.get(task_opts, :check_equivalent, false)
    check_formatted? = Keyword.get(task_opts, :check_formatted, false)

    if check_equivalent? or check_formatted? do
      output_string = IO.iodata_to_binary(output)

      cond do
        check_equivalent? and not equivalent?(input, output_string) ->
          {:not_equivalent, file}

        check_formatted? and input != output_string ->
          {:not_formatted, file}

        true ->
          write_or_print(file, output, check_formatted?, task_opts)
      end
    else
      write_or_print(file, output, check_formatted?, task_opts)
    end
  rescue
    exception ->
      stacktrace = System.stacktrace()
      {:exit, file, exception, stacktrace}
  end

  defp write_or_print(file, output, check_formatted?, task_opts) do
    dry_run? = Keyword.get(task_opts, :dry_run, false)

    if dry_run? or check_formatted? do
      :ok
    else
      write_file(file, output)
      :ok
    end
  end

  defp collect_status({:ok, :ok}, acc), do: acc

  defp collect_status({:ok, {:exit, _, _, _} = exit}, {exits, not_equivalent, not_formatted}) do
    {[exit | exits], not_equivalent, not_formatted}
  end

  defp collect_status({:ok, {:not_equivalent, file}}, {exits, not_equivalent, not_formatted}) do
    {exits, [file | not_equivalent], not_formatted}
  end

  defp collect_status({:ok, {:not_formatted, file}}, {exits, not_equivalent, not_formatted}) do
    {exits, not_equivalent, [file | not_formatted]}
  end

  defp check!({[], [], []}) do
    :ok
  end

  defp check!({[{:exit, file, exception, stacktrace} | _], _not_equivalent, _not_formatted}) do
    Mix.shell().error("mix format failed for file: #{file}")
    reraise exception, stacktrace
  end

  defp check!({_exits, [_ | _] = not_equivalent, _not_formatted}) do
    Mix.raise("""
    mix format failed due to --check-equivalent.
    The following files were not equivalent:

    #{to_bullet_list(not_equivalent)}

    Please report this bug with the input files at github.com/elixir-lang/elixir/issues
    """)
  end

  defp check!({_exits, _not_equivalent, [_ | _] = not_formatted}) do
    Mix.raise("""
    mix format failed due to --check-formatted.
    The following files were not formatted:

    #{to_bullet_list(not_formatted)}
    """)
  end

  defp to_bullet_list(files) do
    Enum.map_join(files, "\n", &"  * #{&1}")
  end

  defp equivalent?(input, output) do
    Code.Formatter.equivalent(input, output) == :ok
  end
end
