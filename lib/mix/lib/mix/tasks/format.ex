defmodule Mix.Tasks.Format do
  use Mix.Task

  @shortdoc "Formats the given files/patterns"

  @moduledoc """
  Formats the given files and patterns.

      $ mix format mix.exs "lib/**/*.{ex,exs}" "test/**/*.{ex,exs}"

  If any of the files is `-`, then the input is read from stdin and the output
  is written to stdout.

  ## Formatting options

  The formatter will read a `.formatter.exs` file in the current directory for
  formatter configuration. Evaluating this file should return a keyword list.

  Here is an example of a `.formatter.exs` file that works as a starting point:

      [
        inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"]
      ]

  Besides the options listed in `Code.format_string!/2`, the `.formatter.exs`
  file supports the following options:

    * `:inputs` (a list of paths and patterns) - specifies the default inputs
      to be used by this task. For example, `["mix.exs", "{config,lib,test}/**/*.{ex,exs}"]`.
      Patterns are expanded with `Path.wildcard/2`.

    * `:plugins` (a list of modules) (since v1.13.0) - specifies a list of
      modules to customize how the formatter works. See the "Plugins" section
      below for more information.

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
       See the "Importing dependencies configuration" section below for more
       information.

    * `:export` (a keyword list) - specifies formatter configuration to be exported.
      See the "Importing dependencies configuration" section below.

  ## Task-specific options

    * `--force` - force formatting to happen on all files, instead of
      relying on cache.

    * `--check-formatted` - checks that the file is already formatted.
      This is useful in pre-commit hooks and CI scripts if you want to
      reject contributions with unformatted code. If the check fails,
      the formatted contents are not written to disk. Keep in mind
      that the formatted output may differ between Elixir versions as
      improvements and fixes are applied to the formatter.

    * `--no-exit` - only valid when used with `--check-formatted`.
      Pass this if you don't want this Mix task to fail (and return a non-zero exit code),
      but still want to check for format errors and print them to the console.

    * `--dry-run` - does not save files after formatting.

    * `--dot-formatter` - path to the file with formatter configuration.
      Defaults to `.formatter.exs` if one is available. See the
      "Formatting options" section above for more information.

    * `--stdin-filename` - path to the file being formatted on stdin.
      This is useful if you are using plugins to support custom filetypes such
      as `.heex`. Without passing this flag, it is assumed that the code being
      passed via stdin is valid Elixir code. Defaults to "stdin.exs".

    * `--migrate` - enables the `:migrate` option, which should be able to
      automatically fix some deprecation warnings but changes the AST.
      This should be safe in typical projects, but there is a non-zero risk
      of breaking code for meta-programming heavy projects that relied on a
      specific AST. We recommend running this task in its separate commit and
      reviewing its output before committing. See the "Migration formatting"
      section in `Code.format_string!/2` for more information.

  ## When to format code

  We recommend developers to format code directly in their editors, either
  automatically when saving a file or via an explicit command or key binding. If
  such option is not available in your editor of choice, adding the required
  integration is usually a matter of invoking:

      $ cd $project && mix format $file

  where `$file` refers to the current file and `$project` is the root of your
  project.

  It is also possible to format code across the whole project by passing a list
  of patterns and files to `mix format`, as shown at the top of this task
  documentation. This list can also be set in the `.formatter.exs` file under the
  `:inputs` key.

  ## Plugins

  It is possible to customize how the formatter behaves. Plugins must implement
  the `Mix.Tasks.Format` behaviour. For example, imagine that your project uses
  Markdown in two distinct ways: via a custom `~M` sigil and via files with the
  `.md` and `.markdown` extensions. A custom plugin would look like this:

      defmodule MixMarkdownFormatter do
        @behaviour Mix.Tasks.Format

        def features(_opts) do
          [sigils: [:M], extensions: [".md", ".markdown"]]
        end

        def format(contents, opts) do
          # logic that formats markdown
        end
      end

  The `opts` passed to `format/2` contains all the formatting options and either:

    * `:sigil` (atom) - the sigil being formatted, e.g. `:M`.

    * `:modifiers` (charlist) - list of sigil modifiers.

    * `:extension` (string) - the extension of the file being formatted, e.g. `".md"`.

  Now any application can use your formatter as follows:

      # .formatter.exs
      [
        # Define the desired plugins
        plugins: [MixMarkdownFormatter, AnotherMarkdownFormatter],
        # Remember to update the inputs list to include the new extensions
        inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}", "posts/*.{md,markdown}"]
      ]

  Notice that, when running the formatter with plugins, your code will be
  compiled first.

  In addition, the order by which you input your plugins is the format order.
  So, in the above `.formatter.exs`, the `MixMarkdownFormatter` will format
  the markdown files and sigils before `AnotherMarkdownFormatter`.

  ## Importing dependencies configuration

  This task supports importing formatter configuration from dependencies.

  A dependency that wants to export formatter configuration needs to have a
  `.formatter.exs` file at the root of the project. In this file, the dependency
  can list an `:export` option with configuration to export. For now, only one
  option is supported under `:export`: `:locals_without_parens` (whose value has
  the same shape as the value of the `:locals_without_parens` in `Code.format_string!/2`).

  The functions listed under `:locals_without_parens` in the `:export` option of
  a dependency can be imported in a project by listing that dependency in the
  `:import_deps` option of the formatter configuration file of the project.

  For example, consider you have a project called `my_app` that depends on another one called `my_dep`.
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
    no_exit: :boolean,
    dot_formatter: :string,
    dry_run: :boolean,
    stdin_filename: :string,
    force: :boolean,
    migrate: :boolean
  ]

  @manifest_timestamp "format_timestamp"
  @manifest_dot_formatter "cached_dot_formatter"
  @manifest_vsn 2

  @newline "\n"
  @blank " "

  @separator "|"
  @cr "↵"
  @line_num_pad @blank

  @gutter [
    del: " -",
    eq: "  ",
    ins: " +",
    skip: "  "
  ]

  @colors [
    del: [text: :red, space: :red_background],
    ins: [text: :green, space: :green_background]
  ]

  @doc """
  Returns which features this plugin should plug into.
  """
  @callback features(Keyword.t()) :: [sigils: [atom()], extensions: [binary()]]

  @doc """
  Receives a string to be formatted with options and returns said string.
  """
  @callback format(String.t(), Keyword.t()) :: String.t()

  @impl true
  def run(all_args) do
    cwd = File.cwd!()
    {opts, args} = OptionParser.parse!(all_args, strict: @switches)
    {dot_formatter, formatter_opts} = eval_dot_formatter(cwd, opts)

    if opts[:check_equivalent] do
      IO.warn("--check-equivalent has been deprecated and has no effect")
    end

    if opts[:no_exit] && !opts[:check_formatted] do
      Mix.raise("--no-exit can only be used together with --check-formatted")
    end

    {formatter_opts_and_subs, _sources} =
      eval_deps_and_subdirectories(cwd, dot_formatter, formatter_opts, [dot_formatter])

    formatter_opts_and_subs = load_plugins(formatter_opts_and_subs)
    files = expand_args(args, cwd, dot_formatter, formatter_opts_and_subs, opts)

    maybe_cache_timestamps(all_args, files, fn files ->
      files
      |> Task.async_stream(&format_file(&1, opts), ordered: false, timeout: :infinity)
      |> Enum.reduce({[], []}, &collect_status/2)
      |> check!(opts)
    end)
  end

  defp maybe_cache_timestamps([], files, fun) do
    if Mix.Project.get() do
      # We fetch the time from before we read files so any future
      # change to files are still picked up by the formatter
      timestamp = System.os_time(:second)
      dir = Mix.Project.manifest_path()
      manifest_timestamp = Path.join(dir, @manifest_timestamp)
      manifest_dot_formatter = Path.join(dir, @manifest_dot_formatter)
      last_modified = Mix.Utils.last_modified(manifest_timestamp)
      sources = [Mix.Project.config_mtime(), manifest_dot_formatter, ".formatter.exs"]

      files =
        if Mix.Utils.stale?(sources, [last_modified]) do
          files
        else
          Enum.filter(files, fn {file, _opts} ->
            Mix.Utils.last_modified(file) > last_modified
          end)
        end

      try do
        fun.(files)
      after
        File.mkdir_p!(dir)
        File.touch!(manifest_timestamp, timestamp)
      end
    else
      fun.(files)
    end
  end

  defp maybe_cache_timestamps([_ | _], files, fun), do: fun.(files)

  defp load_plugins({formatter_opts, subs}) do
    plugins = Keyword.get(formatter_opts, :plugins, [])

    if not is_list(plugins) do
      Mix.raise("Expected :plugins to return a list of modules, got: #{inspect(plugins)}")
    end

    if plugins != [] do
      Mix.Task.run("loadpaths", [])
    end

    if not Enum.all?(plugins, &Code.ensure_loaded?/1) do
      Mix.Task.run("compile", [])
    end

    for plugin <- plugins do
      cond do
        not Code.ensure_loaded?(plugin) ->
          Mix.raise("Formatter plugin #{inspect(plugin)} cannot be found")

        not function_exported?(plugin, :features, 1) ->
          Mix.raise("Formatter plugin #{inspect(plugin)} does not define features/1")

        true ->
          :ok
      end
    end

    sigils =
      for plugin <- plugins,
          sigil <- find_sigils_from_plugins(plugin, formatter_opts),
          do: {sigil, plugin}

    sigils =
      sigils
      |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
      |> Enum.map(fn {sigil, plugins} ->
        {sigil,
         fn input, opts ->
           Enum.reduce(plugins, input, fn plugin, input ->
             plugin.format(input, opts ++ formatter_opts)
           end)
         end}
      end)

    {Keyword.put(formatter_opts, :sigils, sigils),
     Enum.map(subs, fn {path, opts} -> {path, load_plugins(opts)} end)}
  end

  @doc """
  Returns a formatter function and the formatter options to
  be used for the given file.

  The function must be called with the contents of the file
  to be formatted. The options are returned for reflection
  purposes.
  """
  @doc since: "1.13.0"
  def formatter_for_file(file, opts \\ []) do
    cwd = Keyword.get_lazy(opts, :root, &File.cwd!/0)
    {dot_formatter, formatter_opts} = eval_dot_formatter(cwd, opts)

    {formatter_opts_and_subs, _sources} =
      eval_deps_and_subdirectories(cwd, dot_formatter, formatter_opts, [dot_formatter])

    formatter_opts_and_subs = load_plugins(formatter_opts_and_subs)

    find_formatter_and_opts_for_file(Path.expand(file, cwd), formatter_opts_and_subs)
  end

  @doc """
  Returns formatter options to be used for the given file.
  """
  # TODO: Remove me Elixir v1.17
  @deprecated "Use formatter_for_file/2 instead"
  def formatter_opts_for_file(file, opts \\ []) do
    {_, formatter_opts} = formatter_for_file(file, opts)
    formatter_opts
  end

  defp eval_dot_formatter(cwd, opts) do
    {dot_formatter, format_opts} =
      cond do
        dot_formatter = opts[:dot_formatter] ->
          {dot_formatter, eval_file_with_keyword_list(dot_formatter)}

        File.regular?(Path.join(cwd, ".formatter.exs")) ->
          dot_formatter = Path.join(cwd, ".formatter.exs")
          {".formatter.exs", eval_file_with_keyword_list(dot_formatter)}

        true ->
          {".formatter.exs", []}
      end

    # the --migrate flag overrides settings from the dot formatter
    {dot_formatter, Keyword.take(opts, [:migrate]) ++ format_opts}
  end

  # This function reads exported configuration from the imported
  # dependencies and subdirectories and deals with caching the result
  # of reading such configuration in a manifest file.
  defp eval_deps_and_subdirectories(cwd, dot_formatter, formatter_opts, sources) do
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
      manifest = Path.join(Mix.Project.manifest_path(), @manifest_dot_formatter)

      {{locals_without_parens, subdirectories}, sources} =
        maybe_cache_in_manifest(dot_formatter, manifest, fn ->
          {subdirectories, sources} = eval_subs_opts(subs, cwd, sources)
          {{eval_deps_opts(deps), subdirectories}, sources}
        end)

      formatter_opts =
        Keyword.update(
          formatter_opts,
          :locals_without_parens,
          locals_without_parens,
          &(locals_without_parens ++ &1)
        )

      {{formatter_opts, subdirectories}, sources}
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

  defp eval_deps_opts([]) do
    []
  end

  defp eval_deps_opts(deps) do
    deps_paths = Mix.Project.deps_paths()

    for dep <- deps,
        dep_path = assert_valid_dep_and_fetch_path(dep, deps_paths),
        dep_dot_formatter = Path.join(dep_path, ".formatter.exs"),
        File.regular?(dep_dot_formatter),
        dep_opts = eval_file_with_keyword_list(dep_dot_formatter),
        parenless_call <- dep_opts[:export][:locals_without_parens] || [],
        uniq: true,
        do: parenless_call
  end

  defp eval_subs_opts(subs, cwd, sources) do
    {subs, sources} =
      Enum.flat_map_reduce(subs, sources, fn sub, sources ->
        cwd = Path.expand(sub, cwd)
        {Path.wildcard(cwd), [Path.join(cwd, ".formatter.exs") | sources]}
      end)

    Enum.flat_map_reduce(subs, sources, fn sub, sources ->
      sub_formatter = Path.join(sub, ".formatter.exs")

      if File.exists?(sub_formatter) do
        formatter_opts = eval_file_with_keyword_list(sub_formatter)

        {formatter_opts_and_subs, sources} =
          eval_deps_and_subdirectories(sub, :in_memory, formatter_opts, sources)

        {[{sub, formatter_opts_and_subs}], sources}
      else
        {[], sources}
      end
    end)
  end

  defp assert_valid_dep_and_fetch_path(dep, deps_paths) when is_atom(dep) do
    with %{^dep => path} <- deps_paths,
         true <- File.dir?(path) do
      path
    else
      _ ->
        Mix.raise(
          "Unknown dependency #{inspect(dep)} given to :import_deps in the formatter configuration. " <>
            "Make sure the dependency is listed in your mix.exs for environment #{inspect(Mix.env())} " <>
            "and you have run \"mix deps.get\""
        )
    end
  end

  defp assert_valid_dep_and_fetch_path(dep, _deps_paths) do
    Mix.raise("Dependencies in :import_deps should be atoms, got: #{inspect(dep)}")
  end

  defp eval_file_with_keyword_list(path) do
    {opts, _} = Code.eval_file(path)

    if not Keyword.keyword?(opts) do
      Mix.raise("Expected #{inspect(path)} to return a keyword list, got: #{inspect(opts)}")
    end

    opts
  end

  defp expand_args([], cwd, dot_formatter, formatter_opts_and_subs, _opts) do
    if no_entries_in_formatter_opts?(formatter_opts_and_subs) do
      Mix.raise(
        "Expected one or more files/patterns to be given to mix format " <>
          "or for a .formatter.exs file to exist with an :inputs or :subdirectories key"
      )
    end

    dot_formatter
    |> expand_dot_inputs(cwd, formatter_opts_and_subs, %{})
    |> Enum.map(fn {file, {_dot_formatter, formatter_opts}} ->
      {file, find_formatter_for_file(file, formatter_opts)}
    end)
  end

  defp expand_args(files_and_patterns, cwd, _dot_formatter, {formatter_opts, subs}, opts) do
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
        stdin_filename = Path.expand(Keyword.get(opts, :stdin_filename, "stdin.exs"), cwd)

        {formatter, _opts} =
          find_formatter_and_opts_for_file(stdin_filename, {formatter_opts, subs})

        {file, formatter}
      else
        {formatter, _opts} = find_formatter_and_opts_for_file(file, {formatter_opts, subs})
        {file, formatter}
      end
    end
  end

  defp expand_dot_inputs(dot_formatter, cwd, {formatter_opts, subs}, acc) do
    if no_entries_in_formatter_opts?({formatter_opts, subs}) do
      Mix.raise("Expected :inputs or :subdirectories key in #{dot_formatter}")
    end

    map =
      for input <- List.wrap(formatter_opts[:inputs]),
          file <- Path.wildcard(Path.expand(input, cwd), match_dot: true),
          do: {file, {dot_formatter, formatter_opts}},
          into: %{}

    acc =
      Map.merge(acc, map, fn file, {dot_formatter1, _}, {dot_formatter2, formatter_opts} ->
        Mix.shell().error(
          "Both #{dot_formatter1} and #{dot_formatter2} specify the file #{file} in their " <>
            ":inputs option. To resolve the conflict, the configuration in #{dot_formatter1} " <>
            "will be ignored. Please change the list of :inputs in one of the formatter files " <>
            "so only one of them matches #{file}"
        )

        {dot_formatter2, formatter_opts}
      end)

    Enum.reduce(subs, acc, fn {sub, formatter_opts_and_subs}, acc ->
      sub_formatter = Path.join(sub, ".formatter.exs")
      expand_dot_inputs(sub_formatter, sub, formatter_opts_and_subs, acc)
    end)
  end

  defp find_formatter_for_file(file, formatter_opts) do
    ext = Path.extname(file)

    cond do
      plugins = find_plugins_for_extension(formatter_opts, ext) ->
        fn input ->
          Enum.reduce(plugins, input, fn plugin, input ->
            plugin.format(input, [extension: ext, file: file] ++ formatter_opts)
          end)
        end

      ext in ~w(.ex .exs) ->
        &elixir_format(&1, [file: file] ++ formatter_opts)

      true ->
        & &1
    end
  end

  defp find_plugins_for_extension(formatter_opts, ext) do
    plugins = Keyword.get(formatter_opts, :plugins, [])

    plugins =
      Enum.filter(plugins, fn plugin ->
        Code.ensure_loaded?(plugin) and function_exported?(plugin, :features, 1) and
          ext in List.wrap(plugin.features(formatter_opts)[:extensions])
      end)

    if plugins != [], do: plugins, else: nil
  end

  defp find_formatter_and_opts_for_file(file, formatter_opts_and_subs) do
    formatter_opts = recur_formatter_opts_for_file(file, formatter_opts_and_subs)
    {find_formatter_for_file(file, formatter_opts), formatter_opts}
  end

  defp recur_formatter_opts_for_file(file, {formatter_opts, subs}) do
    Enum.find_value(subs, formatter_opts, fn {sub, formatter_opts_and_subs} ->
      size = byte_size(sub)

      case file do
        <<prefix::binary-size(size), dir_separator, _::binary>>
        when prefix == sub and dir_separator in [?\\, ?/] ->
          recur_formatter_opts_for_file(file, formatter_opts_and_subs)

        _ ->
          nil
      end
    end)
  end

  defp no_entries_in_formatter_opts?({formatter_opts, subs}) do
    is_nil(formatter_opts[:inputs]) and subs == []
  end

  defp stdin_or_wildcard("-"), do: [:stdin]

  defp stdin_or_wildcard(path),
    do: path |> Path.expand() |> Path.wildcard(match_dot: true) |> Enum.filter(&File.regular?/1)

  defp elixir_format(content, formatter_opts) do
    case Code.format_string!(content, formatter_opts) do
      [] -> ""
      formatted_content -> IO.iodata_to_binary([formatted_content, ?\n])
    end
  end

  defp find_sigils_from_plugins(plugin, formatter_opts) do
    if Code.ensure_loaded?(plugin) and function_exported?(plugin, :features, 1) do
      List.wrap(plugin.features(formatter_opts)[:sigils])
    else
      []
    end
  end

  defp read_file(:stdin), do: IO.stream() |> Enum.to_list() |> IO.iodata_to_binary()
  defp read_file(file), do: File.read!(file)

  defp format_file({file, formatter}, task_opts) do
    input = read_file(file)
    output = formatter.(input)
    check_formatted? = Keyword.get(task_opts, :check_formatted, false)
    dry_run? = Keyword.get(task_opts, :dry_run, false)

    cond do
      check_formatted? ->
        if input == output, do: :ok, else: {:not_formatted, {file, input, output}}

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

  defp collect_status({:ok, {:exit, _, _, _} = exit}, {exits, not_formatted}) do
    {[exit | exits], not_formatted}
  end

  defp collect_status({:ok, {:not_formatted, file}}, {exits, not_formatted}) do
    {exits, [file | not_formatted]}
  end

  defp check!({[], []}, _task_opts) do
    :ok
  end

  defp check!({[{:exit, :stdin, exception, stacktrace} | _], _not_formatted}, _task_opts) do
    Mix.shell().error("mix format failed for stdin")
    reraise exception, stacktrace
  end

  defp check!({[{:exit, file, exception, stacktrace} | _], _not_formatted}, _task_opts) do
    Mix.shell().error("mix format failed for file: #{Path.relative_to_cwd(file)}")
    reraise exception, stacktrace
  end

  defp check!({_exits, [_ | _] = not_formatted}, task_opts) do
    no_exit? = Keyword.get(task_opts, :no_exit, false)

    message = """
    The following files are not formatted:

    #{to_diffs(not_formatted)}
    """

    if no_exit? do
      Mix.shell().info(message)
    else
      Mix.raise("""
      mix format failed due to --check-formatted.
      #{message}
      """)
    end
  end

  defp to_diffs(files) do
    Enum.map_intersperse(files, "\n", fn
      {:stdin, unformatted, formatted} ->
        [IO.ANSI.reset(), text_diff_format(unformatted, formatted)]

      {file, unformatted, formatted} ->
        [
          IO.ANSI.bright(),
          IO.ANSI.red(),
          file,
          "\n",
          IO.ANSI.reset(),
          "\n",
          text_diff_format(unformatted, formatted)
        ]
    end)
  end

  @doc false
  @spec text_diff_format(String.t(), String.t()) :: iolist()
  def text_diff_format(old, new, opts \\ [])

  def text_diff_format(code, code, _opts), do: []

  def text_diff_format(old, new, opts) do
    opts = Keyword.validate!(opts, after: 2, before: 2, color: IO.ANSI.enabled?(), line: 1)
    crs? = String.contains?(old, "\r") || String.contains?(new, "\r")

    old = String.split(old, "\n")
    new = String.split(new, "\n")

    max = max(length(new), length(old))
    line_num_digits = max |> Integer.digits() |> length()
    opts = Keyword.put(opts, :line_num_digits, line_num_digits)

    {line, opts} = Keyword.pop!(opts, :line)

    old
    |> List.myers_difference(new)
    |> insert_cr_symbols(crs?)
    |> diff_to_iodata({line, line}, opts)
  end

  defp diff_to_iodata(diff, line_nums, opts, iodata \\ [])

  defp diff_to_iodata([], _line_nums, _opts, iodata), do: Enum.reverse(iodata)

  defp diff_to_iodata([{:eq, [""]}], _line_nums, _opts, iodata), do: Enum.reverse(iodata)

  defp diff_to_iodata([{:eq, lines}], line_nums, opts, iodata) do
    lines_after = Enum.take(lines, opts[:after])
    iodata = lines(iodata, {:eq, lines_after}, line_nums, opts)

    iodata =
      case length(lines) > opts[:after] do
        false -> iodata
        true -> lines(iodata, :skip, opts)
      end

    Enum.reverse(iodata)
  end

  defp diff_to_iodata([{:eq, lines} | diff], {line, line}, opts, [] = iodata) do
    {start, lines_before} = Enum.split(lines, opts[:before] * -1)

    iodata =
      case length(lines) > opts[:before] do
        false -> iodata
        true -> lines(iodata, :skip, opts)
      end

    line = line + length(start)
    iodata = lines(iodata, {:eq, lines_before}, {line, line}, opts)

    line = line + length(lines_before)
    diff_to_iodata(diff, {line, line}, opts, iodata)
  end

  defp diff_to_iodata([{:eq, lines} | diff], line_nums, opts, iodata) do
    case length(lines) > opts[:after] + opts[:before] do
      true ->
        {lines1, lines2, lines3} = split(lines, opts[:after], opts[:before] * -1)

        iodata =
          iodata
          |> lines({:eq, lines1}, line_nums, opts)
          |> lines(:skip, opts)
          |> lines({:eq, lines3}, add_line_nums(line_nums, length(lines1) + length(lines2)), opts)

        line_nums = add_line_nums(line_nums, length(lines))

        diff_to_iodata(diff, line_nums, opts, iodata)

      false ->
        iodata = lines(iodata, {:eq, lines}, line_nums, opts)
        line_nums = add_line_nums(line_nums, length(lines))

        diff_to_iodata(diff, line_nums, opts, iodata)
    end
  end

  defp diff_to_iodata([{:del, [del]}, {:ins, [ins]} | diff], line_nums, opts, iodata) do
    iodata = lines(iodata, {:chg, del, ins}, line_nums, opts)
    diff_to_iodata(diff, add_line_nums(line_nums, 1), opts, iodata)
  end

  defp diff_to_iodata([{kind, lines} | diff], line_nums, opts, iodata) do
    iodata = lines(iodata, {kind, lines}, line_nums, opts)
    line_nums = add_line_nums(line_nums, length(lines), kind)

    diff_to_iodata(diff, line_nums, opts, iodata)
  end

  defp split(list, count1, count2) do
    {split1, split2} = Enum.split(list, count1)
    {split2, split3} = Enum.split(split2, count2)
    {split1, split2, split3}
  end

  defp lines(iodata, :skip, opts) do
    line_num = String.duplicate(@blank, opts[:line_num_digits] * 2 + 1)
    [[line_num, @gutter[:skip], @separator, @newline] | iodata]
  end

  defp lines(iodata, {:chg, del, ins}, line_nums, opts) do
    {del, ins} = line_diff(del, ins, opts)

    [
      [gutter(line_nums, :ins, opts), ins, @newline],
      [gutter(line_nums, :del, opts), del, @newline]
      | iodata
    ]
  end

  defp lines(iodata, {kind, lines}, line_nums, opts) do
    lines
    |> Enum.with_index()
    |> Enum.reduce(iodata, fn {line, offset}, iodata ->
      line_nums = add_line_nums(line_nums, offset, kind)
      [[gutter(line_nums, kind, opts), colorize(line, kind, false, opts), @newline] | iodata]
    end)
  end

  defp gutter(line_nums, kind, opts) do
    [line_num(line_nums, kind, opts), colorize(@gutter[kind], kind, false, opts), @separator]
  end

  defp line_num({line_num_old, line_num_new}, :eq, opts) do
    old =
      line_num_old
      |> to_string()
      |> String.pad_leading(opts[:line_num_digits], @line_num_pad)

    new =
      line_num_new
      |> to_string()
      |> String.pad_leading(opts[:line_num_digits], @line_num_pad)

    [old, @blank, new]
  end

  defp line_num({line_num_old, _line_num_new}, :del, opts) do
    old =
      line_num_old
      |> to_string()
      |> String.pad_leading(opts[:line_num_digits], @line_num_pad)

    new = String.duplicate(@blank, opts[:line_num_digits])
    [old, @blank, new]
  end

  defp line_num({_line_num_old, line_num_new}, :ins, opts) do
    old = String.duplicate(@blank, opts[:line_num_digits])

    new =
      line_num_new
      |> to_string()
      |> String.pad_leading(opts[:line_num_digits], @line_num_pad)

    [old, @blank, new]
  end

  defp line_diff(del, ins, opts) do
    diff = String.myers_difference(del, ins)

    Enum.reduce(diff, {[], []}, fn
      {:eq, str}, {del, ins} -> {[del | str], [ins | str]}
      {:del, str}, {del, ins} -> {[del | colorize(str, :del, true, opts)], ins}
      {:ins, str}, {del, ins} -> {del, [ins | colorize(str, :ins, true, opts)]}
    end)
  end

  defp colorize(str, kind, space?, opts) do
    if Keyword.fetch!(opts, :color) && Keyword.has_key?(@colors, kind) do
      color = Keyword.fetch!(@colors, kind)

      if space? do
        str
        |> String.split(~r/[\t\s]+/, include_captures: true)
        |> Enum.map(fn
          <<start::binary-size(1), _::binary>> = str when start in ["\t", "\s"] ->
            IO.ANSI.format([color[:space], str])

          str ->
            IO.ANSI.format([color[:text], str])
        end)
      else
        IO.ANSI.format([color[:text], str])
      end
    else
      str
    end
  end

  defp add_line_nums({line_num_old, line_num_new}, lines, kind \\ :eq) do
    case kind do
      :eq -> {line_num_old + lines, line_num_new + lines}
      :ins -> {line_num_old, line_num_new + lines}
      :del -> {line_num_old + lines, line_num_new}
    end
  end

  defp insert_cr_symbols(diffs, false), do: diffs
  defp insert_cr_symbols(diffs, true), do: do_insert_cr_symbols(diffs, [])

  defp do_insert_cr_symbols([], acc), do: Enum.reverse(acc)

  defp do_insert_cr_symbols([{:del, del}, {:ins, ins} | rest], acc) do
    {del, ins} = do_insert_cr_symbols(del, ins, {[], []})
    do_insert_cr_symbols(rest, [{:ins, ins}, {:del, del} | acc])
  end

  defp do_insert_cr_symbols([diff | rest], acc) do
    do_insert_cr_symbols(rest, [diff | acc])
  end

  defp do_insert_cr_symbols([left | left_rest], [right | right_rest], {left_acc, right_acc}) do
    {left, right} = insert_cr_symbol(left, right)
    do_insert_cr_symbols(left_rest, right_rest, {[left | left_acc], [right | right_acc]})
  end

  defp do_insert_cr_symbols([], right, {left_acc, right_acc}) do
    left = Enum.reverse(left_acc)
    right = right_acc |> Enum.reverse() |> Enum.concat(right)
    {left, right}
  end

  defp do_insert_cr_symbols(left, [], {left_acc, right_acc}) do
    left = left_acc |> Enum.reverse() |> Enum.concat(left)
    right = Enum.reverse(right_acc)
    {left, right}
  end

  defp insert_cr_symbol(left, right) do
    case {String.ends_with?(left, "\r"), String.ends_with?(right, "\r")} do
      {bool, bool} -> {left, right}
      {true, false} -> {String.replace(left, "\r", @cr), right}
      {false, true} -> {left, String.replace(right, "\r", @cr)}
    end
  end
end
