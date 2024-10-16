defmodule Mix.Compilers.Erlang do
  @moduledoc false

  @manifest_vsn 1

  @doc """
  Compiles the given `mappings`.

  `mappings` is a list of `{src, dest}` pairs, where the source
  extensions are compiled into the destination extension,
  automatically invoking the callback for each stale pair (or for
  all if `force` is `true`) and removing files that no longer have
  a source, while keeping the `manifest` up to date.

  ## Options

    * `:force` - forces compilation regardless of modification times

    * `:parallel` - a mapset of files to compile in parallel

    * `:preload` - any code that must be preloaded if any pending
      entry needs to be compiled

  ## Examples

  For example, a simple compiler for Lisp Flavored Erlang
  would be implemented like:

      manifest = Path.join(Mix.Project.manifest_path(), "compile.lfe")
      dest = Mix.Project.compile_path()

      compile(manifest, [{"src", dest}], :lfe, :beam, opts, fn input, output ->
        :lfe_comp.file(
          to_erl_file(input),
          [{:outdir, Path.dirname(output)}, :return, :report]
        )
      end)

  The command above will:

    1. look for files ending with the `lfe` extension in `src` path
       and their `beam` counterpart in `ebin` path

    2. for each stale file (or for all if `force` is `true`),
       invoke the callback passing the calculated input
       and output

    3. update the manifest with the newly compiled outputs

    4. remove any output in the manifest that does not
       have an equivalent source

  The callback must return `{:ok, term, warnings}` or
  `{:error, errors, warnings}` in case of error. This function returns
  `{status, diagnostics}` as specified in `Mix.Task.Compiler`.
  """
  def compile(manifest, mappings, src_ext, dest_ext, opts, callback)
      when is_atom(src_ext) and is_atom(dest_ext) and is_list(opts) do
    force = opts[:force]

    entries =
      for {src, dest} <- mappings,
          target <- extract_entries(src, src_ext, dest, dest_ext, force),
          do: target

    if preload = entries != [] && opts[:preload] do
      preload.()
    end

    compile_entries(manifest, entries, src_ext, dest_ext, opts, callback)
  end

  # TODO: remove me on v1.22
  @deprecated "Use compile/6 or open an up an issue"
  def compile(manifest, entries, opts \\ [], callback) do
    compile_entries(manifest, entries, :erl, :beam, opts, callback)
  end

  @doc false
  def compile_entries(manifest, mappings, src_ext, dest_ext, opts, callback) do
    stale = for {:stale, src, dest} <- mappings, do: {src, dest}

    # Get the previous entries from the manifest
    timestamp = System.os_time(:second)
    old_entries = entries = read_manifest(manifest)

    # Files to remove are the ones in the manifest but they no longer have a source
    removed =
      for {dest, _} <- entries, not List.keymember?(mappings, dest, 2), do: dest

    # Remove manifest entries with no source
    Enum.each(removed, &File.rm/1)
    verbose = opts[:verbose]

    # Clear stale and removed files from manifest
    entries =
      Enum.reject(entries, fn {dest, _warnings} ->
        dest in removed || List.keymember?(stale, dest, 1)
      end)

    if Keyword.get(opts, :all_warnings, true), do: show_warnings(entries)

    if stale == [] and removed == [] do
      {:noop, manifest_warnings(entries)}
    else
      Mix.Utils.compiling_n(length(stale), src_ext)
      Mix.Project.ensure_structure()

      # Let's prepend the newly created path so compiled files
      # can be accessed still during compilation (for behaviours
      # and what not). Note we don't want to cache this path as
      # we will write to it.
      Code.prepend_path(Mix.Project.compile_path())

      {parallel, serial} =
        case opts[:parallel] || false do
          true -> {stale, []}
          false -> {[], stale}
          parallel -> Enum.split_with(stale, fn {source, _target} -> source in parallel end)
        end

      serial_results = Enum.map(serial, &do_compile(&1, callback, timestamp, verbose))

      parallel_results =
        parallel
        |> Task.async_stream(&do_compile(&1, callback, timestamp, verbose),
          timeout: :infinity,
          ordered: false
        )
        |> Enum.map(fn {:ok, result} -> result end)

      # Compile stale files and print the results
      {status, new_entries, warnings, errors} =
        Enum.reduce(serial_results ++ parallel_results, {:ok, [], [], []}, &combine_results/2)

      write_manifest(manifest, entries ++ new_entries, timestamp)

      # Return status and diagnostics
      warnings = manifest_warnings(entries) ++ to_diagnostics(warnings, :warning)

      if dest_ext == :beam do
        lazy_modules_diff = fn ->
          {changed, added} =
            stale
            |> Enum.map(&elem(&1, 1))
            |> Enum.split_with(fn dest -> List.keymember?(old_entries, dest, 0) end)

          modules_diff(added, changed, removed, timestamp)
        end

        Mix.Task.Compiler.notify_modules_compiled(lazy_modules_diff)
      end

      case status do
        :ok ->
          {:ok, warnings}

        :error ->
          errors = to_diagnostics(errors, :error)
          {:error, warnings ++ errors}
      end
    end
  end

  # TODO: Deprecate this in favor of `Mix.ensure_application!/1` in Elixir v1.19.
  @doc false
  def ensure_application!(app, _input) do
    Mix.ensure_application!(app)
    {:ok, _} = Application.ensure_all_started(app)
  end

  @doc """
  Removes compiled files for the given `manifest`.
  """
  def clean(manifest) do
    Enum.each(read_manifest(manifest), fn {file, _} -> File.rm(file) end)
    File.rm(manifest)
  end

  @doc """
  Converts the given `file` to a format accepted by
  the Erlang compilation tools.
  """
  def to_erl_file(file) do
    to_charlist(file)
  end

  @doc """
  Asserts that the `:erlc_paths` configuration option that many Mix tasks
  rely on is valid.

  Raises a `Mix.Error` exception if the option is not valid, returns `:ok`
  otherwise.
  """
  def assert_valid_erlc_paths(erlc_paths) do
    if is_list(erlc_paths) do
      :ok
    else
      Mix.raise(":erlc_paths should be a list of paths, got: #{inspect(erlc_paths)}")
    end
  end

  @doc """
  Returns the output paths in the manifest.
  """
  def outputs(manifest) do
    manifest |> read_manifest() |> Enum.map(&elem(&1, 0))
  end

  @doc """
  Retrieves all diagnostics from the given manifest.
  """
  def diagnostics(manifest) do
    entries = read_manifest(manifest)
    manifest_warnings(entries)
  end

  defp extract_entries(src_dir, src_ext, dest_dir, dest_ext, force) do
    files = Mix.Utils.extract_files(List.wrap(src_dir), List.wrap(src_ext))

    for file <- files do
      module = module_from_artifact(file)
      target = Path.join(dest_dir, module <> "." <> to_string(dest_ext))

      if force || Mix.Utils.stale?([file], [target]) do
        {:stale, file, target}
      else
        {:ok, file, target}
      end
    end
  end

  defp module_from_artifact(artifact) do
    artifact |> Path.basename() |> Path.rootname()
  end

  # The manifest file contains a list of {dest, warnings} tuples
  defp read_manifest(file) do
    try do
      file |> File.read!() |> :erlang.binary_to_term()
    rescue
      _ -> []
    else
      {@manifest_vsn, data} when is_list(data) -> data
      _ -> []
    end
  end

  defp write_manifest(file, entries, timestamp) do
    File.mkdir_p!(Path.dirname(file))
    File.write!(file, :erlang.term_to_binary({@manifest_vsn, entries}))
    File.touch!(file, timestamp)
  end

  defp do_compile({input, output}, callback, timestamp, verbose) do
    case callback.(input, output) do
      {:ok, _, warnings} ->
        File.touch!(output, timestamp)
        verbose && Mix.shell().info("Compiled #{input}")
        {:ok, [{output, warnings}], warnings, []}

      {:error, errors, warnings} ->
        {:error, [], warnings, errors}

      {:ok, _} ->
        IO.warn(
          "returning {:ok, contents} in the Mix.Compilers.Erlang.compile/6 callback is deprecated " <>
            "The callback should return {:ok, contents, warnings} or {:error, errors, warnings}"
        )

        {:ok, [], [], []}

      :error ->
        IO.warn(
          "returning :error in the Mix.Compilers.Erlang.compile/6 callback is deprecated " <>
            "The callback should return {:ok, contents, warnings} or {:error, errors, warnings}"
        )

        {:error, [], [], []}
    end
  end

  defp combine_results(result1, result2) do
    {status1, new_entries1, warnings1, errors1} = result1
    {status2, new_entries2, warnings2, errors2} = result2
    status = if status1 == :error or status2 == :error, do: :error, else: :ok
    {status, new_entries1 ++ new_entries2, warnings1 ++ warnings2, errors1 ++ errors2}
  end

  defp manifest_warnings(entries) do
    Enum.flat_map(entries, fn {_, warnings} ->
      to_diagnostics(warnings, :warning)
    end)
  end

  defp to_diagnostics(warnings_or_errors, severity) do
    for {file, issues} <- warnings_or_errors,
        {location, module, data} <- issues do
      file = Path.absname(file)

      %Mix.Task.Compiler.Diagnostic{
        file: file,
        source: file,
        position: location_normalize(location),
        message: to_string(module.format_error(data)),
        severity: severity,
        compiler_name: to_string(module),
        details: data
      }
    end
  end

  defp show_warnings(entries) do
    for {_, warnings} <- entries,
        {file, issues} <- warnings,
        {location, module, message} <- issues do
      IO.puts("#{file}:#{location_to_string(location)} warning: #{module.format_error(message)}")
    end
  end

  defp location_normalize({line, column})
       when is_integer(line) and line >= 1 and is_integer(column) and column >= 0,
       do: {line, column}

  defp location_normalize(line) when is_integer(line) and line >= 1, do: line
  defp location_normalize(_), do: 0

  defp location_to_string({line, column}), do: "#{line}:#{column}:"
  defp location_to_string(0), do: ""
  defp location_to_string(line), do: "#{line}:"

  defp modules_diff(added, changed, removed, timestamp) do
    %{
      added: modules_from_paths(added),
      changed: modules_from_paths(changed),
      removed: modules_from_paths(removed),
      timestamp: timestamp
    }
  end

  defp modules_from_paths(paths) do
    Enum.map(paths, &module_from_path/1)
  end

  defp module_from_path(path) do
    path |> Path.basename() |> Path.rootname() |> String.to_atom()
  end
end
