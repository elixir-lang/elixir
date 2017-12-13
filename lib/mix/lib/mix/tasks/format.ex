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

  ## `.formatter.exs`

  The formatter will read a `.formatter.exs` in the current directory for
  formatter configuration. It should return a keyword list with any of the
  options supported by `Code.format_string!/2`.

  The `.formatter.exs` also supports other options:

    * `:input` (a list of paths and patterns) - specifies the default inputs
      to be used by this task. For example, `["mix.exs", "{config,lib,test}/**/*.{ex,exs}"]`.

    * `:import_deps` (a list of dependencies as atoms) - specifies a list
       of dependencies whose formatter configuration will be imported.
       See the "Importing dependencies configuration" section below for more
       information.

    * `:export_locals_without_parens` (a list of function names and arities) -
      specifies a list of function names and arities (like `:locals_without_parens`
      in `Code.format_string!/2`) that projects that depend on this project
      can read and use.

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

  ## Importing dependencies configuration

  This task supports importing formatter configuration from dependencies.

  A dependency that wants to export formatter configuration needs to have a
  `.formatter.exs` file at the root of the project. In this file, the
  dependency can export a `:export_locals_without_parens` option whose value
  has the same shape as the value of the `:locals_without_parens` in
  `Code.format_string!/2`.

  The functions listed under `:export_locals_without_parens` of a dependency
  can be imported in a project by listing that dependency in the `:import_deps`
  option of the formatter configuration file of the project.

  For example, consider I have a project `my_app` that depends on `my_dep`.
  `my_dep` wants to export some configuration, so `my_dep/.formatter.exs`
  would look like this:

      # my_dep/.formatter.exs
      [
        # Regular formatter configuration for my_dep
        # ...

        export_locals_without_parens: [some_dsl_call: :*]
      ]

  In order to import configuration, `my_app`'s `.formatter.exs` would look like
  this:

      # my_app/.formatter.exs
      [
        import_deps: [:my_dep]
      ]

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
    formatter_opts = fetch_deps_opts(formatter_opts)

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

  # This function reads exported configuration from the imported dependencies and deals with
  # caching the result of reading such configuration in a manifest file.
  defp fetch_deps_opts(formatter_opts) do
    case formatter_opts[:import_deps] do
      deps when deps in [nil, []] ->
        formatter_opts

      deps when is_list(deps) and deps != [] ->
        dep_parenless_calls =
          if deps_dot_formatters_stale?() do
            dep_parenless_calls = eval_deps_opts(deps)

            if dep_parenless_calls != [] do
              write_deps_manifest(dep_parenless_calls)
            end

            dep_parenless_calls
          else
            read_deps_manifest()
          end

        Keyword.update(
          formatter_opts,
          :locals_without_parens,
          dep_parenless_calls,
          &(&1 ++ dep_parenless_calls)
        )

      other ->
        Mix.raise(
          "Expected :import_deps to return a list of dependencies, got: #{inspect(other)}"
        )
    end
  end

  defp deps_dot_formatters_stale?() do
    Mix.Utils.stale?([".formatter.exs" | Mix.Project.config_files()], [deps_manifest()])
  end

  defp deps_manifest() do
    Path.join(Mix.Project.build_path(), ".cached_deps_formatter.exs")
  end

  defp read_deps_manifest() do
    deps_manifest() |> File.read!() |> :erlang.binary_to_term()
  end

  defp write_deps_manifest(parenless_calls) do
    File.write!(deps_manifest(), :erlang.term_to_binary(parenless_calls))
  end

  defp eval_deps_opts(deps) do
    deps_paths = Mix.Project.deps_paths()

    for dep <- deps,
        dep_dot_formatter = Path.join(Map.fetch!(deps_paths, dep), ".formatter.exs"),
        File.regular?(dep_dot_formatter),
        {dep_opts, _} = Code.eval_file(dep_dot_formatter),
        parenless_call <- Keyword.get(dep_opts, :export_locals_without_parens, []),
        uniq: true,
        do: parenless_call
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

  defp format_file(file, task_opts, formatter_opts) do
    {input, extra_opts} = read_file(file)
    output = IO.iodata_to_binary([Code.format_string!(input, extra_opts ++ formatter_opts), ?\n])

    check_equivalent? = Keyword.get(task_opts, :check_equivalent, false)
    check_formatted? = Keyword.get(task_opts, :check_formatted, false)
    dry_run? = Keyword.get(task_opts, :dry_run, false)

    cond do
      check_equivalent? and not equivalent?(input, output) ->
        {:not_equivalent, file}

      check_formatted? ->
        if input == output, do: :ok, else: {:not_formatted, file}

      dry_run? ->
        :ok

      true ->
        write_or_print(file, input, output)
    end
  rescue
    exception ->
      stacktrace = System.stacktrace()
      {:exit, file, exception, stacktrace}
  end

  defp write_or_print(file, input, output) do
    cond do
      file == :stdin -> IO.write(output)
      input == output -> :ok
      true -> File.write!(file, output)
    end

    :ok
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
