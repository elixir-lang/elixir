defmodule Mix.Compilers.Erlang do
  @moduledoc false

  @manifest_vsn 1

  @doc """
  Compiles the files in `mappings` with given extensions into
  the destination, automatically invoking the callback for each
  stale input and output pair (or for all if `force` is `true`) and
  removing files that no longer have a source, while keeping the
  `manifest` up to date.

  `mappings` should be a list of tuples in the form of `{src, dest}` paths.

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
  def compile(manifest, mappings, src_ext, dest_ext, opts, callback) when is_list(opts) do
    force = opts[:force]

    files =
      for {src, dest} <- mappings,
          target <- extract_targets(src, src_ext, dest, dest_ext, force),
          do: target

    compile(manifest, files, src_ext, opts, callback)
  end

  def compile(manifest, mappings, src_ext, dest_ext, force, callback)
      when is_boolean(force) or is_nil(force) do
    IO.warn(
      "Mix.Compilers.Erlang.compile/6 with a boolean or nil as 5th argument is deprecated, " <>
        "please pass [force: true] or [] instead"
    )

    compile(manifest, mappings, src_ext, dest_ext, [force: force], callback)
  end

  @doc """
  Compiles the given `mappings`.

  `mappings` should be a list of tuples in the form of `{src, dest}`.

  A `manifest` file and a `callback` to be invoked for each src/dest pair
  must be given. A src/dest pair where destination is `nil` is considered
  to be up to date and won't be (re-)compiled.
  """
  def compile(manifest, mappings, opts \\ [], callback) do
    compile(manifest, mappings, :erl, opts, callback)
  end

  defp compile(manifest, mappings, ext, opts, callback) do
    stale = for {:stale, src, dest} <- mappings, do: {src, dest}

    # Get the previous entries from the manifest
    timestamp = System.os_time(:second)
    entries = read_manifest(manifest)

    # Files to remove are the ones in the manifest
    # but they no longer have a source
    removed =
      Enum.filter(entries, fn {dest, _} ->
        not Enum.any?(mappings, fn {_status, _mapping_src, mapping_dest} ->
          mapping_dest == dest
        end)
      end)
      |> Enum.map(&elem(&1, 0))

    # Remove manifest entries with no source
    Enum.each(removed, &File.rm/1)
    verbose = opts[:verbose]

    # Clear stale and removed files from manifest
    entries =
      Enum.reject(entries, fn {dest, _warnings} ->
        dest in removed || Enum.any?(stale, fn {_, stale_dest} -> dest == stale_dest end)
      end)

    if opts[:all_warnings], do: show_warnings(entries)

    if stale == [] && removed == [] do
      {:noop, manifest_warnings(entries)}
    else
      Mix.Utils.compiling_n(length(stale), ext)
      Mix.Project.ensure_structure()

      # Let's prepend the newly created path so compiled files
      # can be accessed still during compilation (for behaviours
      # and what not).
      Code.prepend_path(Mix.Project.compile_path())

      # Compile stale files and print the results
      {status, new_entries, warnings, errors} =
        stale
        |> Enum.map(&do_compile(&1, callback, timestamp, verbose))
        |> Enum.reduce({:ok, [], [], []}, &combine_results/2)

      write_manifest(manifest, entries ++ new_entries, timestamp)

      # Return status and diagnostics
      warnings = manifest_warnings(entries) ++ to_diagnostics(warnings, :warning)

      case status do
        :ok ->
          {:ok, warnings}

        :error ->
          errors = to_diagnostics(errors, :error)
          {:error, warnings ++ errors}
      end
    end
  end

  @doc """
  Ensures the native OTP application is available.
  """
  def ensure_application!(app, input) do
    case Application.ensure_all_started(app) do
      {:ok, _} ->
        :ok

      {:error, _} ->
        Mix.raise(
          "Could not compile #{inspect(Path.relative_to_cwd(input))} because " <>
            "the application \"#{app}\" could not be found. This may happen if " <>
            "your package manager broke Erlang into multiple packages and may " <>
            "be fixed by installing the missing \"erlang-dev\" and \"erlang-#{app}\" packages"
        )
    end
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

  defp extract_targets(src_dir, src_ext, dest_dir, dest_ext, force) do
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
        {line, module, data} <- issues do
      %Mix.Task.Compiler.Diagnostic{
        file: Path.absname(file),
        position: line,
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
        {line, module, message} <- issues do
      IO.puts("#{file}:#{line}: Warning: #{module.format_error(message)}")
    end
  end
end
