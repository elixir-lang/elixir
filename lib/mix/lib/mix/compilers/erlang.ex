defmodule Mix.Compilers.Erlang do
  @moduledoc false

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

      manifest = Path.join Mix.Project.manifest_path, ".compile.lfe"
      dest = Mix.Project.compile_path

      compile manifest, [{"src", dest}], :lfe, :beam, opts, fn
        input, output ->
          :lfe_comp.file(to_erl_file(input),
                         [output_dir: Path.dirname(output)])
      end

  The command above will:

    1. look for files ending with the `lfe` extension in `src` path
       and their `beam` counterpart in `ebin` path

    2. for each stale file (or for all if `force` is `true`),
       invoke the callback passing the calculated input
       and output

    3. update the manifest with the newly compiled outputs

    4. remove any output in the manifest that does not
       have an equivalent source

  The callback must return `{:ok, mod}` or `:error` in case
  of error. An error is raised at the end if any of the
  files failed to compile.
  """
  def compile(manifest, mappings, src_ext, dest_ext, opts, callback) when is_list(opts) do
    force = opts[:force]
    files =
      for {src, dest} <- mappings do
        extract_targets(src, src_ext, dest, dest_ext, force)
      end |> Enum.concat
    compile(manifest, files, src_ext, opts, callback)
  end

  def compile(manifest, mappings, src_ext, dest_ext, force, callback) when is_boolean(force) or is_nil(force) do
    IO.warn "Mix.Compilers.Erlang.compile/6 with a boolean or nil as 5th argument is deprecated, " <>
            "please pass [force: true] or [] instead"
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
    timestamp = :calendar.universal_time()
    entries = read_manifest(manifest)

    # Files to remove are the ones in the manifest
    # but they no longer have a source
    removed = Enum.filter(entries, fn entry ->
      not Enum.any?(mappings, fn {_status, _src, dest} -> dest == entry end)
    end)

    if stale == [] && removed == [] do
      :noop
    else
      Mix.Utils.compiling_n(length(stale), ext)
      Mix.Project.ensure_structure()

      # Let's prepend the newly created path so compiled files
      # can be accessed still during compilation (for behaviours
      # and what not).
      Code.prepend_path(Mix.Project.compile_path)

      # Remove manifest entries with no source
      Enum.each(removed, &File.rm/1)
      verbose = opts[:verbose]

      # Compile stale files and print the results
      results =
        for {input, output} <- stale do
          result = callback.(input, output)

          with {:ok, _} <- result do
            File.touch!(output, timestamp)
            verbose && Mix.shell.info "Compiled #{input}"
          end

          result
        end

      # Write final entries to manifest
      entries = (entries -- removed) ++ Enum.map(stale, &elem(&1, 1))
      write_manifest(manifest, :lists.usort(entries), timestamp)

      # Raise if any error, return :ok otherwise
      if :error in results do
        Mix.raise "Encountered compilation errors"
      end
      :ok
    end
  end

  @doc """
  Ensures the native Erlang application is available.
  """
  def ensure_application!(app, input) do
    case Application.ensure_all_started(app) do
      {:ok, _} ->
        :ok
      {:error, _} ->
        Mix.raise "Could not compile #{inspect Path.relative_to_cwd(input)} because " <>
                  "the application \"#{app}\" could not be found. This may happen if " <>
                  "your package manager broke Erlang into multiple packages and may " <>
                  "be fixed by installing the missing \"erlang-dev\" and \"erlang-#{app}\" packages"
    end
  end

  @doc """
  Removes compiled files for the given `manifest`.
  """
  def clean(manifest) do
    Enum.each read_manifest(manifest), &File.rm/1
    File.rm manifest
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
      Mix.raise ":erlc_paths should be a list of paths, got: #{inspect(erlc_paths)}"
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
    artifact |> Path.basename |> Path.rootname
  end

  defp read_manifest(file) do
    case File.read(file) do
      {:ok, contents} -> String.split(contents, "\n")
      {:error, _} -> []
    end
  end

  defp write_manifest(file, entries, timestamp) do
    Path.dirname(file) |> File.mkdir_p!
    File.write!(file, Enum.join(entries, "\n"))
    File.touch!(file, timestamp)
  end
end
