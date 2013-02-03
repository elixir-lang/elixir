defmodule Mix.Tasks.Compile.Erlang do

  alias :epp, as: Epp
  alias :digraph, as: Graph
  alias :digraph_utils, as: GraphUtils
  alias :code, as: Code
  alias :compile, as: Compiler
  alias Mix.Utils
  use Mix.Task

  @hidden true
  @shortdoc "Compile Erlang source files"

  @moduledoc """
  A task to compile Erlang source files.

  When this task runs, it will first check the mod times of
  all of the files. Every file will checked, if file or
  file dependencies, like include files, was changed.
  If file of his dependencies haven't been changed since the
  last compilation, it will not compile. If file or one of his
  dependency has changed, it will compile.

  For this reason, this task touches your `:compile_path`
  directory and sets the modification time to the current
  time and date at the end of each compilation. You can
  force compilation regardless of mod times by passing
  the `--force` option.

  ## Command line options

  * `--force` - forces compilation regardless of module times;

  ## Configuration

  * `ERL_COMPILER_OPTIONS` - can be used to give default compile options.
     It's value must be a valid Erlang term. If the value is a list, it will
     be used as is. If it is not a list, it will be put into a list.

  * `:erlc_paths` - directories to find source files.
    Defaults to `["src"]`, can be configured as:

        [erlc_paths: ["src", "other"]]

  * `:erlc_include_path` - directory for adding include files.
    Defaults to `"include"`, can be configured as:

        [`erlc_include_path`: "other"]

  * `:erlc_options` - compilation options that applies
     to Erlang's compiler.
     This options are setted:

     :outdir to a configured :compile_path
     :i to a configured :include_path
     :report

     and :debug_info in project configuration

     There are many other available options here:
     http://www.erlang.org/doc/man/compile.html#file-2

  """

  defrecord Erl, file: nil, module: nil, behaviours: [], compile: [],
    includes: [], mtime: nil, invalid: false

  def run(args) do
    { opts, _ } = OptionParser.parse(args, switches: [force: :boolean])

    project = Mix.project
    source_paths = project[:erlc_paths]
    files        = Mix.Utils.extract_files(source_paths, [:erl])
    compile_path = to_erl_file project[:compile_path]
    include_path = to_erl_file project[:erlc_include_path]

    erlc_options = [{:outdir, compile_path}, {:i, include_path}, :report
                    | project[:erlc_options] || []]
    erlc_options = Enum.map erlc_options, fn(opt) ->
      case opt do
        { :i, dir } -> { :i, to_erl_file(dir) }
        _           -> opt
      end
    end

    files = files |> scan_sources(include_path, source_paths) |> sort_dependency
    unless opts[:force], do: files = Enum.filter(files, check_file(compile_path, &1))

    if files == [] do
      :noop
    else
      Utils.preserving_mtime(compile_path, fn ->
        File.mkdir_p! compile_path
        compile_files files, compile_path, erlc_options
      end)

      :ok
    end
  end

  defp scan_sources(files, include_path, source_paths) do
    include_pathes = [include_path | source_paths]
    List.foldl(files, [], fn(file, acc) -> scan_source(acc, file, include_pathes) end) |> Enum.reverse
  end

  defp scan_source(acc, file, include_pathes) do
    erl_file = Erl[mtime: Utils.last_modified(file),
                   file: file,
                   module: Path.basename(file, ".erl")]
    case Epp.parse_file(to_erl_file(file), include_pathes, []) do
      {:ok, forms} ->
        [List.foldl(tl(forms), erl_file, fn(f, acc) -> do_form(file, f, acc) end) | acc]
      {:error, _error} ->
        acc
    end
  end

  defp do_form(file, form, erl) do
    case form do
      {:attribute, _, :file, {include_file, _}} when file != include_file ->
        erl.update(includes: [include_file | erl.includes])
      {:attribute, _, :behaviour, behaviour} ->
        erl.update(behaviour: [behaviour | erl.behaviours])
      {:attribute, _, :compile, value} ->
        erl.update(compile: [value | erl.compile])
      _ ->
        erl
    end
  end

  defp sort_dependency(erls) do
    graph = Graph.new
    lc erl inlist erls do
      Graph.add_vertex(graph, erl.module, erl)
    end
    lc erl inlist erls do
      lc b inlist erl.behaviours, do: Graph.add_edge(graph, b, erl.module)
      lc a inlist erl.compile do
        case a do
          {:parse_transform, transform} -> Graph.add_edge(graph, transform, erl.module);
          _ -> :ok
        end
      end
    end
    result =
      case GraphUtils.topsort(graph) do
        :false -> erls;
        mods   ->
          lc m inlist mods, do: elem(Graph.vertex(graph, m), 1)
      end
    Graph.delete(graph)
    result
  end

  defp check_file(compile_path, erl) do
    beam = Path.join(compile_path, "#{erl.module}#{Code.objfile_extension}")
    case File.regular?(beam) do
      :false -> :true
      :true  ->
        beammtime = Utils.last_modified(beam)
        (beammtime <= erl.mtime) or Utils.check_mtime(beammtime, erl.includes)
    end
  end

  defp compile_files(files, compile_path, erlc_options) do
    File.mkdir_p!(compile_path)
    Enum.each files, compile_file(&1, erlc_options)
  end

  defp compile_file(erl, erlc_options) do
    file = to_erl_file Path.rootname(erl.file, ".erl")
    interpret_result file, :compile.file(file, erlc_options), ".erl"
  end

  def interpret_result(file, result, ext // "") do
    case result do
      { :ok, _} ->
        Mix.shell.info  "Compiled #{file}#{ext}"
      :error     ->
        :ok
    end
  end

  def to_erl_file file do
    to_char_list(file)
  end

end
