defmodule Mix.Tasks.Compile.Erlang do
  use Mix.Task
  import Mix.Compilers.Erlang

  @recursive true
  @manifest ".compile.erlang"

  @moduledoc """
  Compiles Erlang source files.

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
      Defaults to `["src"]`.

    * `:erlc_include_path` - directory for adding include files.
      Defaults to `"include"`.

    * `:erlc_options` - compilation options that apply to Erlang's compiler.
      `:debug_info` is enabled by default.

      For a list of the many more available options,
      see [`:compile.file/2`](http://www.erlang.org/doc/man/compile.html#file-2).

  For example, to configure the `erlc_options` for your Erlang project you
  may run:

      erlc_options: [:debug_info, {:i, 'path/to/include'}]

  """

  @doc """
  Runs this task.
  """
  @spec run(OptionParser.argv) :: :ok | :noop
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: [force: :boolean])

    project      = Mix.Project.config
    source_paths = project[:erlc_paths]
    include_path = to_erl_file project[:erlc_include_path]
    compile_path = to_erl_file Mix.Project.compile_path(project)
    files        = Mix.Utils.extract_files(source_paths, [:erl])

    erlc_options = project[:erlc_options] || []
    erlc_options = erlc_options ++ [{:outdir, compile_path}, {:i, include_path}, :report]
    erlc_options = Enum.map erlc_options, fn
      {kind, dir} when kind in [:i, :outdir] ->
        {kind, to_erl_file(dir)}
      opt ->
        opt
    end

    compile_path = Path.relative_to(compile_path, File.cwd!)

    tuples = files
             |> scan_sources(include_path, source_paths)
             |> sort_dependencies
             |> Enum.map(&annotate_target(&1, compile_path, opts[:force]))

    Mix.Compilers.Erlang.compile(manifest(), tuples, fn
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
  Cleans up compilation artifacts.
  """
  def clean do
    Mix.Compilers.Erlang.clean(manifest())
  end

  ## Internal helpers

  defp scan_sources(files, include_path, source_paths) do
    include_paths = [include_path | source_paths]
    Enum.reduce(files, [], &scan_source(&2, &1, include_paths)) |> Enum.reverse
  end

  defp scan_source(acc, file, include_paths) do
    erl_file = %{file: file, module: module_from_artifact(file),
                 behaviours: [], compile: [], includes: [], invalid: false}
    case :epp.parse_file(to_erl_file(file), include_paths, []) do
      {:ok, forms} ->
        [List.foldl(tl(forms), erl_file, &do_form(file, &1, &2)) | acc]
      {:error, _error} ->
        acc
    end
  end

  defp do_form(file, form, erl) do
    case form do
      {:attribute, _, :file, {include_file, _}} when file != include_file ->
        if File.regular?(include_file) do
          %{erl | includes: [include_file|erl.includes]}
        else
          erl
        end
      {:attribute, _, :behaviour, behaviour} ->
        %{erl | behaviours: [behaviour|erl.behaviours]}
      {:attribute, _, :compile, value} ->
        %{erl | compile: [value|erl.compile]}
      _ ->
        erl
    end
  end

  defp sort_dependencies(erls) do
    graph = :digraph.new

    _ = for erl <- erls do
      :digraph.add_vertex(graph, erl.module, erl)
    end

    _ = for erl <- erls do
      _ = for b <- erl.behaviours, do: :digraph.add_edge(graph, b, erl.module)
      _ = for c <- erl.compile do
        case c do
          {:parse_transform, transform} -> :digraph.add_edge(graph, transform, erl.module)
          _ -> :ok
        end
      end
      :ok
    end

    result =
      case :digraph_utils.topsort(graph) do
        false -> erls
        mods  ->
          for m <- mods, do: elem(:digraph.vertex(graph, m), 1)
      end

    :digraph.delete(graph)
    result
  end

  defp annotate_target(erl, compile_path, force) do
    beam = Path.join(compile_path, "#{erl.module}#{:code.objfile_extension}")

    if force || Mix.Utils.stale?([erl.file|erl.includes], [beam]) do
      {:stale, erl.file, beam}
    else
      {:ok, erl.file, beam}
    end
  end

  defp module_from_artifact(artifact) do
    artifact |> Path.basename |> Path.rootname |> String.to_atom
  end
end
