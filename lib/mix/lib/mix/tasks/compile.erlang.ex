defmodule Mix.Tasks.Compile.Erlang do
  alias :epp, as: Epp
  alias :digraph, as: Graph
  alias :digraph_utils, as: GraphUtils

  use Mix.Task

  @hidden true
  @shortdoc "Compile Erlang source files"
  @recursive true
  @manifest ".compile.erlang"

  @moduledoc """
  A task to compile Erlang source files.

  When this task runs, it will first check the modification times of
  all of the files to be compiled and if they haven't been
  changed since the last compilation, it will not compile
  them at all. If any one of them has changed, it compiles
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

  def run(args) do
    { opts, _ } = OptionParser.parse(args, switches: [force: :boolean])

    project      = Mix.project
    source_paths = project[:erlc_paths]
    files        = Mix.Utils.extract_files(source_paths, [:erl])
    compile_path = to_erl_file project[:compile_path]
    include_path = to_erl_file project[:erlc_include_path]

    erlc_options = project[:erlc_options] || []
    erlc_options = erlc_options ++ [{:outdir, compile_path}, {:i, include_path}, :report]
    erlc_options = Enum.map erlc_options, fn
      { kind, dir } when kind in [:i, :outdit] ->
        { kind, to_erl_file(dir) }
      opt ->
        opt
    end

    files = files |> scan_sources(include_path, source_paths) |> sort_dependencies

    if opts[:force] do
      filtered = files
    else
      filtered = Enum.filter(files, requires_compilation?(compile_path, &1))
    end

    if filtered == [] do
      :noop
    else
      File.mkdir_p! compile_path
      compile_files filtered, files, compile_path, erlc_options
      :ok
    end
  end

  def manifest do
    Path.join(Mix.project[:compile_path], @manifest)
  end

  defp scan_sources(files, include_path, source_paths) do
    include_paths = [include_path | source_paths]
    Enum.reduce(files, [], scan_source(&2, &1, include_paths)) |> Enum.reverse
  end

  defp scan_source(acc, file, include_paths) do
    erl_file = Erl[file: file, module: module_from_artifact(file)]

    case Epp.parse_file(to_erl_file(file), include_paths, []) do
      { :ok, forms } ->
        [List.foldl(tl(forms), erl_file, do_form(file, &1, &2)) | acc]
      { :error, _error } ->
        acc
    end
  end

  defp do_form(file, form, Erl[] = erl) do
    case form do
      {:attribute, _, :file, {include_file, _}} when file != include_file ->
        if File.regular?(include_file) do
          erl.update_includes [include_file|&1]
        else
          erl
        end
      {:attribute, _, :behaviour, behaviour} ->
        erl.update_behaviours [behaviour|&1]
      {:attribute, _, :compile, value} ->
        erl.update_compile [value|&1]
      _ ->
        erl
    end
  end

  defp sort_dependencies(erls) do
    graph = Graph.new

    lc erl inlist erls do
      Graph.add_vertex(graph, erl.module, erl)
    end

    lc erl inlist erls do
      lc b inlist erl.behaviours, do: Graph.add_edge(graph, b, erl.module)
      lc c inlist erl.compile do
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
          lc m inlist mods, do: elem(Graph.vertex(graph, m), 1)
      end

    Graph.delete(graph)
    result
  end

  defp requires_compilation?(compile_path, erl) do
    beam = Path.join(compile_path, "#{erl.module}#{:code.objfile_extension}")
    Mix.Utils.stale?([erl.file|erl.includes], [beam])
  end

  defp compile_files(files, all, compile_path, erlc_options) do
    manifest_path = Path.join(compile_path, @manifest)

    modules  = lc erl inlist all, do: erl.module
    previous = Mix.Utils.read_manifest(manifest_path)

    lc beam inlist previous, not (module_from_artifact(beam) in modules) do
      File.rm(beam)
    end

    results  = Enum.map(files, compile_file(&1, erlc_options))
    compiled = lc { :ok, mod } inlist results do
      Path.join(compile_path, "#{mod}.beam")
    end

    Mix.Utils.update_manifest(manifest_path, compiled)
    if Enum.any?(results, &1 == :error), do: raise CompileError
  end

  defp compile_file(erl, erlc_options) do
    file = to_erl_file(Path.rootname(erl.file, ".erl"))
    interpret_result(file, :compile.file(file, erlc_options), ".erl")
  end

  defp module_from_artifact(artifact) do
    artifact |> Path.basename |> Path.rootname
  end

  # Helpers shared across erlang compilers

  @doc """
  Extract stale pairs considering the set of directories
  and filename extensions. It first looks in `dir1`
  for files with `ext1` extensions and then recursively
  attempts to find matching pairs in `dir2` with `ext2`
  extensions.
  """
  def extract_stale_pairs(dir1, ext1, dir2, ext2, force) do
    files = Mix.Utils.extract_files([dir1], List.wrap(ext1))
    Enum.reduce files, [], fn(file, acc) ->
      compiled_file = module_from_artifact(file)
      compiled_file = Path.join(dir2, compiled_file <> "." <> to_binary(ext2))
      if force or Mix.Utils.stale?([file], [compiled_file]) do
        [{file, compiled_file} | acc]
      else
        acc
      end
    end
  end

  @doc """
  Interprets compilation results and prints them to the console.
  """
  def interpret_result(file, result, ext // "") do
    case result do
      { :ok, _ } -> Mix.shell.info "Compiled #{file}#{ext}"
      :error -> :error
    end
    result
  end

  @doc """
  Converts the given file to a format accepted by
  the Erlang compilation tools.
  """
  def to_erl_file(file) do
    to_char_list(file)
  end
end
