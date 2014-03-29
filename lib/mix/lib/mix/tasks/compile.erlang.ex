defmodule Mix.Tasks.Compile.Erlang do
  alias :epp, as: Epp
  alias :digraph, as: Graph
  alias :digraph_utils, as: GraphUtils

  use Mix.Task

  @recursive true
  @manifest ".compile.erlang"

  @moduledoc """
  Compile Erlang source files.

  When this task runs, it will first check the modification times of
  all files to be compiled and if they haven't been
  changed since the last compilation, it will not compile
  them. If any of them have changed, it compiles
  everything.

  For this reason, the task touches your `:compile_path`
  directory and sets the modification time to the current
  time and date at the end of each compilation. You can
  force compilation regardless of modification times by passing
  the `--force` option.

  ## Command line options

  * `--force` - forces compilation regardless of modification times

  ## Configuration

  * `ERL_COMPILER_OPTIONS` - can be used to give default compile options.
     The value must be a valid Erlang term. If the value is a list, it will
     be used as is. If it is not a list, it will be put into a list.

  * `:erlc_paths` - directories to find source files.
    Defaults to `["src"]`, can be configured as:

    ```
    [erlc_paths: ["src", "other"]]
    ```

  * `:erlc_include_path` - directory for adding include files.
    Defaults to `"include"`, can be configured as:

    ```
    [erlc_include_path: "other"]
    ```

  * `:erlc_options` - compilation options that apply to Erlang's
     compiler. `:debug_info` is enabled by default.

     There are many available options here:
     http://www.erlang.org/doc/man/compile.html#file-2
  """

  defrecord Erl, file: nil, module: nil, behaviours: [], compile: [],
    includes: [], mtime: nil, invalid: false

  @doc """
  Runs this task.
  """
  def run(args) do
    { opts, _, _ } = OptionParser.parse(args, switches: [force: :boolean])

    project      = Mix.project
    source_paths = project[:erlc_paths]
    include_path = to_erl_file project[:erlc_include_path]
    compile_path = to_erl_file Mix.Project.compile_path(project)
    files        = Mix.Utils.extract_files(source_paths, [:erl])

    erlc_options = project[:erlc_options] || []
    erlc_options = erlc_options ++ [{:outdir, compile_path}, {:i, include_path}, :report]
    erlc_options = Enum.map erlc_options, fn
      { kind, dir } when kind in [:i, :outdit] ->
        { kind, to_erl_file(dir) }
      opt ->
        opt
    end

    tuples = files
             |> scan_sources(include_path, source_paths)
             |> sort_dependencies
             |> Enum.map(&annotate_target(&1, compile_path, opts[:force]))

    compile_mappings(manifest(), tuples, fn
      input, _output ->
        file = to_erl_file(Path.rootname(input, ".erl"))
        :compile.file(file, erlc_options)
    end)
  end

  @doc """
  Returns Erlang manifests.
  """
  def manifests, do: [manifest]
  defp manifest, do: Path.join(Mix.Project.manifest_path, @manifest)

  @doc """
  Extracts the extensions from the mappings, automatically
  invoking the callback for each stale input and output pair
  (or for all if `force` is true) and removing files that no
  longer have a source, while keeping the manifest up
  to date.

  ## Examples

  For example, a simple compiler for Lisp Flavored Erlang
  would be implemented like:

      compile_mappings ".compile.lfe",
                       [{ "src", "ebin" }],
                       :lfe, :beam, opts[:force], fn
        input, output ->
          :lfe_comp.file(to_erl_file(input),
                         [output_dir: Path.dirname(output)])
      end

  The command above will:

  1. Look for files ending with the `lfe` extension in `src`
     and their `beam` counterpart in `ebin`;
  2. For each stale file (or for all if `force` is true),
     invoke the callback passing the calculated input
     and output;
  3. Update the manifest with the newly compiled outputs;
  4. Remove any output in the manifest that that does not
     have an equivalent source;

  The callback must return `{ :ok, mod }` or `:error` in case
  of error. An error is raised at the end if any of the
  files failed to compile.
  """
  def compile_mappings(manifest, mappings, src_ext, dest_ext, force, callback) do
    files = for { src, dest } <- mappings do
              extract_targets(src, src_ext, dest, dest_ext, force)
            end |> Enum.concat

    compile_mappings(manifest, files, callback)
  end

  @doc """
  Converts the given file to a format accepted by
  the Erlang compilation tools.
  """
  def to_erl_file(file) do
    to_char_list(file)
  end

  ## Internal helpers

  defp scan_sources(files, include_path, source_paths) do
    include_paths = [include_path | source_paths]
    Enum.reduce(files, [], &scan_source(&2, &1, include_paths)) |> Enum.reverse
  end

  defp scan_source(acc, file, include_paths) do
    erl_file = Erl[file: file, module: module_from_artifact(file)]

    case Epp.parse_file(to_erl_file(file), include_paths, []) do
      { :ok, forms } ->
        [List.foldl(tl(forms), erl_file, &do_form(file, &1, &2)) | acc]
      { :error, _error } ->
        acc
    end
  end

  defp do_form(file, form, Erl[] = erl) do
    case form do
      {:attribute, _, :file, {include_file, _}} when file != include_file ->
        if File.regular?(include_file) do
          erl.update_includes &[include_file|&1]
        else
          erl
        end
      {:attribute, _, :behaviour, behaviour} ->
        erl.update_behaviours &[behaviour|&1]
      {:attribute, _, :compile, value} ->
        erl.update_compile &[value|&1]
      _ ->
        erl
    end
  end

  defp sort_dependencies(erls) do
    graph = Graph.new

    for erl <- erls do
      Graph.add_vertex(graph, erl.module, erl)
    end

    for erl <- erls do
      for b <- erl.behaviours, do: Graph.add_edge(graph, b, erl.module)
      for c <- erl.compile do
        case c do
          {:parse_transform, transform} -> Graph.add_edge(graph, transform, erl.module)
          _ -> :ok
        end
      end
    end

    result =
      case GraphUtils.topsort(graph) do
        false -> erls
        mods  ->
          for m <- mods, do: elem(Graph.vertex(graph, m), 1)
      end

    Graph.delete(graph)
    result
  end

  defp annotate_target(erl, compile_path, force) do
    beam   = Path.join(compile_path, "#{erl.module}#{:code.objfile_extension}")

    if force || Mix.Utils.stale?([erl.file|erl.includes], [beam]) do
      { erl.file, erl.module, beam }
    else
      { erl.file, erl.module, nil }
    end
  end

  defp module_from_artifact(artifact) do
    artifact |> Path.basename |> Path.rootname
  end

  defp extract_targets(dir1, src_ext, dir2, dest_ext, force) do
    files = Mix.Utils.extract_files([dir1], List.wrap(src_ext))

    for file <- files do
      module = module_from_artifact(file)
      target = Path.join(dir2, module <> "." <> to_string(dest_ext))

      if force || Mix.Utils.stale?([file], [target]) do
        { file, module, target }
      else
        { file, module, nil }
      end
    end
  end

  defp compile_mappings(manifest, tuples, callback) do
    # Stale files are the ones with a destination
    stale = for { src, _mod, dest } <- tuples, dest != nil, do: { src, dest }

    # Get the previous entries from the manifest
    entries = Mix.Utils.read_manifest(manifest)

    # Files to remove are the ones in the
    # manifest but they no longer have a source
    removed = Enum.filter(entries, fn entry ->
      module = module_from_artifact(entry)
      not Enum.any?(tuples, fn { _src, mod, _dest } -> module == mod end)
    end)

    if stale == [] && removed == [] do
      :noop
    else
      # Build the project structure so we can write down compiled files.
      Mix.Project.build_structure

      # Remove manifest entries with no source
      Enum.each(removed, &File.rm/1)

      # Compile stale files and print the results
      results = for { input, output } <- stale do
        interpret_result(input, callback.(input, output))
      end

      # Write final entries to manifest
      entries = (entries -- removed) ++ Enum.map(stale, &elem(&1, 1))
      Mix.Utils.write_manifest(manifest, :lists.usort(entries))

      # Raise if any error, return :ok otherwise
      if :error in results, do: raise CompileError
      :ok
    end
  end

  defp interpret_result(file, result) do
    case result do
      { :ok, _ } -> Mix.shell.info "Compiled #{file}"
      :error -> :error
    end
    result
  end
end
