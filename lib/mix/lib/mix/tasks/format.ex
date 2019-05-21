defmodule Mix.Tasks.Format do
  use Mix.Task

  @shortdoc "Formats the given files/patterns"

  @moduledoc """
  Formats the given files and patterns.

      mix format mix.exs "lib/**/*.{ex,exs}" "test/**/*.{ex,exs}"

  If any of the files is `-`, then the output is read from stdin
  and written to stdout.

  ## Formatting options

  The formatter will read a `.formatter.exs` in the current directory for
  formatter configuration. Evaluating this file should return a keyword list.

  Here is an example `.formatter.exs` that works as a starting point:

      [
        inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"]
      ]

  Besides the options listed in `Code.format_string!/2`, the `.formatter.exs`
  supports the following options:

    * `:inputs` (a list of paths and patterns) - specifies the default inputs
      to be used by this task. For example, `["mix.exs", "{config,lib,test}/**/*.{ex,exs}"]`.
      Patterns are expanded with `Path.wildcard/2`.

    * `:subdirectories` (a list of paths and patterns) - specifies subdirectories
      that have their own formatting rules. Each subdirectory should have a
      `.formatter.exs` that configures how entries in that subdirectory should be
      formatted as. Configuration between `.formatter.exs` are not shared nor
      inherited. If a `.formatter.exs` lists "lib/app" as a subdirectory, the rules
      in `.formatter.exs` won't be available in `lib/app/.formatter.exs`.
      Note that the parent `.formatter.exs` must not specify files inside the "lib/app"
      subdirectory in its `:inputs` configuration. If this happens, the behaviour of
      which formatter configuration will be picked is unspecified.

    * `:import_deps` (a list of dependencies as atoms) - specifies a list
       of dependencies whose formatter configuration will be imported.
       When specified, the formatter should run in the same directory as
       the `mix.exs` file that defines those dependencies. See the "Importing
       dependencies configuration" section below for more information.

    * `:export` (a keyword list) - specifies formatter configuration to be exported.
      See the "Importing dependencies configuration" section below.

  ## Task-specific options

    * `--check-formatted` - checks that the file is already formatted.
      This is useful in pre-commit hooks and CI scripts if you want to
      reject contributions with unformatted code. However keep in mind
      that the formatted output may differ between Elixir versions as
      improvements and fixes are applied to the formatter.

    * `--check-equivalent` - checks if the files after formatting have the
      same AST as before formatting. If the ASTs are not equivalent, it is
      a bug in the code formatter. This option is useful if you suspect you
      have ran into a formatter bug and you would like confirmation.

    * `--dry-run` - does not save files after formatting.

    * `--dot-formatter` - path to the file with formatter configuration.
      Defaults to `.formatter.exs` if one is available. See the "`.formatter.exs`"
      section for more information.

  If any of the `--check-*` options are given and a check fails, the formatted
  contents won't be written to disk nor printed to standard output.

  ## When to format code

  We recommend developers to format code directly in their editors, either
  automatically when saving a file or via an explicit command or key binding. If
  such option is not yet available in your editor of choice, adding the required
  integration is usually a matter of invoking:

      cd $project && mix format $file

  where `$file` refers to the current file and `$project` is the root of your
  project.

  It is also possible to format code across the whole project by passing a list
  of patterns and files to `mix format`, as shown at the top of this task
  documentation. This list can also be set in the `.formatter.exs` under the
  `:inputs` key.

  ## Importing dependencies configuration

  This task supports importing formatter configuration from dependencies.

  A dependency that wants to export formatter configuration needs to have a
  `.formatter.exs` file at the root of the project. In this file, the dependency
  can export a `:export` option with configuration to export. For now, only one
  option is supported under `:export`: `:locals_without_parens` (whose value has
  the same shape as the value of the `:locals_without_parens` in `Code.format_string!/2`).

  The functions listed under `:locals_without_parens` in the `:export` option of
  a dependency can be imported in a project by listing that dependency in the
  `:import_deps` option of the formatter configuration file of the project.

  For example, consider I have a project `my_app` that depends on `my_dep`.
  `my_dep` wants to export some configuration, so `my_dep/.formatter.exs`
  would look like this:

      # my_dep/.formatter.exs
      [
        # Regular formatter configuration for my_dep
        # ...

        export: [
          locals_without_parens: [some_dsl_call: 2, some_dsl_call: 3]
        ]
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

  @manifest "cached_dot_formatter"
  @manifest_vsn 1

  @impl true
  def run(args) do
    {opts, args} = OptionParser.parse!(args, strict: @switches)
    {dot_formatter, formatter_opts} = eval_dot_formatter(opts)

    {formatter_opts_and_subs, _sources} =
      eval_deps_and_subdirectories(dot_formatter, [], formatter_opts, [dot_formatter])

    args
    |> expand_args(dot_formatter, formatter_opts_and_subs)
    |> Task.async_stream(&format_file(&1, opts), ordered: false, timeout: 30000)
    |> Enum.reduce({[], [], []}, &collect_status/2)
    |> check!()
  end

  @doc """
  Returns formatter options to be used for the given file.
  """
  def formatter_opts_for_file(file, opts \\ []) do
    {dot_formatter, formatter_opts} = eval_dot_formatter(opts)

    {formatter_opts_and_subs, _sources} =
      eval_deps_and_subdirectories(dot_formatter, [], formatter_opts, [dot_formatter])

    split = file |> Path.relative_to_cwd() |> Path.split()
    find_formatter_opts_for_file(split, formatter_opts_and_subs)
  end

  defp eval_dot_formatter(opts) do
    cond do
      dot_formatter = opts[:dot_formatter] ->
        {dot_formatter, eval_file_with_keyword_list(dot_formatter)}

      File.regular?(".formatter.exs") ->
        {".formatter.exs", eval_file_with_keyword_list(".formatter.exs")}

      true ->
        {".formatter.exs", []}
    end
  end

  # This function reads exported configuration from the imported
  # dependencies and subdirectories and deals with caching the result
  # of reading such configuration in a manifest file.
  defp eval_deps_and_subdirectories(dot_formatter, prefix, formatter_opts, sources) do
    deps = Keyword.get(formatter_opts, :import_deps, [])
    subs = Keyword.get(formatter_opts, :subdirectories, [])

    if not is_list(deps) do
      Mix.raise("Expected :import_deps to return a list of dependencies, got: #{inspect(deps)}")
    end

    if not is_list(subs) do
      Mix.raise("Expected :subdirectories to return a list of directories, got: #{inspect(subs)}")
    end

    if deps == [] and subs == [] do
      {{formatter_opts, []}, sources}
    else
      manifest = Path.join(Mix.Project.manifest_path(), @manifest)

      maybe_cache_in_manifest(dot_formatter, manifest, fn ->
        {subdirectories, sources} = eval_subs_opts(subs, prefix, sources)
        {{eval_deps_opts(formatter_opts, deps), subdirectories}, sources}
      end)
    end
  end

  defp maybe_cache_in_manifest(dot_formatter, manifest, fun) do
    cond do
      is_nil(Mix.Project.get()) or dot_formatter != ".formatter.exs" -> fun.()
      entry = read_manifest(manifest) -> entry
      true -> write_manifest!(manifest, fun.())
    end
  end

  defp read_manifest(manifest) do
    with {:ok, binary} <- File.read(manifest),
         {:ok, {@manifest_vsn, entry, sources}} <- safe_binary_to_term(binary),
         expanded_sources = Enum.flat_map(sources, &Path.wildcard(&1, match_dot: true)),
         false <- Mix.Utils.stale?([Mix.Project.config_mtime() | expanded_sources], [manifest]) do
      {entry, sources}
    else
      _ -> nil
    end
  end

  defp safe_binary_to_term(binary) do
    {:ok, :erlang.binary_to_term(binary)}
  rescue
    _ -> :error
  end

  defp write_manifest!(manifest, {entry, sources}) do
    File.mkdir_p!(Path.dirname(manifest))
    File.write!(manifest, :erlang.term_to_binary({@manifest_vsn, entry, sources}))
    {entry, sources}
  end

  defp eval_deps_opts(formatter_opts, []) do
    formatter_opts
  end

  defp eval_deps_opts(formatter_opts, deps) do
    deps_paths = Mix.Project.deps_paths()

    parenless_calls =
      for dep <- deps,
          dep_path = assert_valid_dep_and_fetch_path(dep, deps_paths),
          dep_dot_formatter = Path.join(dep_path, ".formatter.exs"),
          File.regular?(dep_dot_formatter),
          dep_opts = eval_file_with_keyword_list(dep_dot_formatter),
          parenless_call <- dep_opts[:export][:locals_without_parens] || [],
          uniq: true,
          do: parenless_call

    Keyword.update(
      formatter_opts,
      :locals_without_parens,
      parenless_calls,
      &(&1 ++ parenless_calls)
    )
  end

  defp eval_subs_opts(subs, prefix, sources) do
    {subs, sources} =
      Enum.flat_map_reduce(subs, sources, fn sub, sources ->
        prefix = Path.join(prefix ++ [sub])
        {Path.wildcard(prefix), [Path.join(prefix, ".formatter.exs") | sources]}
      end)

    Enum.flat_map_reduce(subs, sources, fn sub, sources ->
      sub_formatter = Path.join(sub, ".formatter.exs")

      if File.exists?(sub_formatter) do
        formatter_opts = eval_file_with_keyword_list(sub_formatter)

        {formatter_opts_and_subs, sources} =
          eval_deps_and_subdirectories(:in_memory, [sub], formatter_opts, sources)

        {[{sub, formatter_opts_and_subs}], sources}
      else
        {[], sources}
      end
    end)
  end

  defp assert_valid_dep_and_fetch_path(dep, deps_paths) when is_atom(dep) do
    case Map.fetch(deps_paths, dep) do
      {:ok, path} ->
        if File.dir?(path) do
          path
        else
          Mix.raise(
            "Unavailable dependency #{inspect(dep)} given to :import_deps in the formatter configuration. " <>
              "The dependency cannot be found in the file system, please run \"mix deps.get\" and try again"
          )
        end

      :error ->
        Mix.raise(
          "Unknown dependency #{inspect(dep)} given to :import_deps in the formatter configuration. " <>
            "The dependency is not listed in your mix.exs for environment #{inspect(Mix.env())}"
        )
    end
  end

  defp assert_valid_dep_and_fetch_path(dep, _deps_paths) do
    Mix.raise("Dependencies in :import_deps should be atoms, got: #{inspect(dep)}")
  end

  defp eval_file_with_keyword_list(path) do
    {opts, _} = Code.eval_file(path)

    unless Keyword.keyword?(opts) do
      Mix.raise("Expected #{inspect(path)} to return a keyword list, got: #{inspect(opts)}")
    end

    opts
  end

  defp expand_args([], dot_formatter, formatter_opts_and_subs) do
    if no_entries_in_formatter_opts?(formatter_opts_and_subs) do
      Mix.raise(
        "Expected one or more files/patterns to be given to mix format " <>
          "or for a .formatter.exs to exist with an :inputs or :subdirectories key"
      )
    end

    dot_formatter
    |> expand_dot_inputs([], formatter_opts_and_subs, %{})
    |> Enum.map(fn {file, {_dot_formatter, formatter_opts}} -> {file, formatter_opts} end)
  end

  defp expand_args(files_and_patterns, _dot_formatter, {formatter_opts, subs}) do
    files =
      for file_or_pattern <- files_and_patterns,
          file <- stdin_or_wildcard(file_or_pattern),
          uniq: true,
          do: file

    if files == [] do
      Mix.raise(
        "Could not find a file to format. The files/patterns given to command line " <>
          "did not point to any existing file. Got: #{inspect(files_and_patterns)}"
      )
    end

    for file <- files do
      if file == :stdin do
        {file, formatter_opts}
      else
        split = file |> Path.relative_to_cwd() |> Path.split()
        {file, find_formatter_opts_for_file(split, {formatter_opts, subs})}
      end
    end
  end

  defp expand_dot_inputs(dot_formatter, prefix, {formatter_opts, subs}, acc) do
    if no_entries_in_formatter_opts?({formatter_opts, subs}) do
      Mix.raise("Expected :inputs or :subdirectories key in #{dot_formatter}")
    end

    map =
      for input <- List.wrap(formatter_opts[:inputs]),
          file <- Path.wildcard(Path.join(prefix ++ [input]), match_dot: true),
          do: {expand_relative_to_cwd(file), {dot_formatter, formatter_opts}},
          into: %{}

    acc =
      Map.merge(acc, map, fn file, {dot_formatter1, _}, {dot_formatter2, formatter_opts} ->
        Mix.shell().error(
          "Both #{dot_formatter1} and #{dot_formatter2} specify the file " <>
            "#{Path.relative_to_cwd(file)} in their :inputs option. To resolve the " <>
            "conflict, the configuration in #{dot_formatter1} will be ignored. " <>
            "Please change the list of :inputs in one of the formatter files so only " <>
            "one of them matches #{Path.relative_to_cwd(file)}"
        )

        {dot_formatter2, formatter_opts}
      end)

    Enum.reduce(subs, acc, fn {sub, formatter_opts_and_subs}, acc ->
      sub_formatter = Path.join(sub, ".formatter.exs")
      expand_dot_inputs(sub_formatter, [sub], formatter_opts_and_subs, acc)
    end)
  end

  defp expand_relative_to_cwd(path) do
    case File.cwd() do
      {:ok, cwd} -> Path.expand(path, cwd)
      _ -> path
    end
  end

  defp find_formatter_opts_for_file(split, {formatter_opts, subs}) do
    Enum.find_value(subs, formatter_opts, fn {sub, formatter_opts_and_subs} ->
      if List.starts_with?(split, Path.split(sub)) do
        find_formatter_opts_for_file(split, formatter_opts_and_subs)
      end
    end)
  end

  defp no_entries_in_formatter_opts?({formatter_opts, subs}) do
    is_nil(formatter_opts[:inputs]) and subs == []
  end

  defp stdin_or_wildcard("-"), do: [:stdin]
  defp stdin_or_wildcard(path), do: path |> Path.expand() |> Path.wildcard(match_dot: true)

  defp read_file(:stdin) do
    {IO.stream(:stdio, :line) |> Enum.to_list() |> IO.iodata_to_binary(), file: "stdin"}
  end

  defp read_file(file) do
    {File.read!(file), file: file}
  end

  defp format_file({file, formatter_opts}, task_opts) do
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
      {:exit, file, exception, __STACKTRACE__}
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

  defp check!({[{:exit, :stdin, exception, stacktrace} | _], _not_equivalent, _not_formatted}) do
    Mix.shell().error("mix format failed for stdin")
    reraise exception, stacktrace
  end

  defp check!({[{:exit, file, exception, stacktrace} | _], _not_equivalent, _not_formatted}) do
    Mix.shell().error("mix format failed for file: #{Path.relative_to_cwd(file)}")
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
