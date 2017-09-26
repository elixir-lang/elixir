defmodule Mix.Tasks.Xref do
  use Mix.Task

  alias Mix.Tasks.Compile.Elixir, as: E
  import Mix.Compilers.Elixir, only: [read_manifest: 2, source: 1, source: 2, module: 1]

  @shortdoc "Performs cross reference checks"
  @recursive true

  @moduledoc """
  Performs cross reference checks between modules.

  ## Xref modes

  The `xref` task expects a mode as first argument:

      mix xref MODE

  All available modes are discussed below.

  ### warnings

  Prints warnings for violated cross reference checks:

      mix xref warnings

  This is the mode used by Mix during compilation.

  ### unreachable

  Prints all unreachable "file:line: module.function/arity" entries:

      mix xref unreachable

  The "file:line" represents the file and line a call to an unknown
  "module.function/arity" is made.

  ### callers CALLEE

  Prints all callers of the given `CALLEE`, which can be one of: `Module`,
  `Module.function`, or `Module.function/arity`. Examples:

      mix xref callers MyMod
      mix xref callers MyMod.fun
      mix xref callers MyMod.fun/3

  ### graph

  Prints a file dependency graph where an edge from `A` to `B` indicates
  that `A` depends on `B`.

      mix xref graph --format dot

  The following options are accepted:

    * `--exclude` - paths to exclude

    * `--source` - displays all files that the given source file references (directly or indirectly)

    * `--sink` - displays all files that reference the given file (directly or indirectly)

    * `--format` - can be set to one of:

      * `pretty` - uses Unicode codepoints for formatting the graph.
        This is the default except on Windows

      * `plain` - does not use Unicode codepoints for formatting the graph.
        This is the default on Windows

      * `dot` - produces a DOT graph description in `xref_graph.dot` in the
        current directory. Warning: this will override any previously generated file

  The `--source` and `--sink` options are particularly useful when trying to understand how
  the modules in a particular file interact with the whole system.

  ## Shared options

  Those options are shared across all modes:

    * `--no-compile` - does not compile even if files require compilation

    * `--no-deps-check` - does not check dependencies

    * `--no-archives-check` - does not check archives

    * `--no-elixir-version-check` - does not check the Elixir version from mix.exs

  ## Configuration

  All configuration for Xref should be placed under the key `:xref`.

    * `:exclude` - a list of modules and `{module, function, arity}` tuples to ignore when checking
      cross references. For example: `[MissingModule, {MissingModule2, :missing_func, 2}]`

  """

  @switches [compile: :boolean, deps_check: :boolean, archives_check: :boolean,
             elixir_version_check: :boolean, exclude: :keep, format: :string,
             source: :string, sink: :string]

  @doc """
  Runs this task.
  """
  @spec run(OptionParser.argv) :: :ok | :error
  def run(args) do
    {opts, args} =
      OptionParser.parse!(args, strict: @switches)

    Mix.Task.run("loadpaths")

    if Keyword.get(opts, :compile, true) do
      Mix.Task.run("compile")
    end

    case args do
      ["warnings"] ->
        warnings()
      ["unreachable"] ->
        unreachable()
      ["callers", callee] ->
        callers(callee)
      ["graph"] ->
        graph(opts)
      _ ->
        Mix.raise "xref doesn't support this command. For more information run \"mix help xref\""
    end
  end

  ## Modes

  defp warnings() do
    List.flatten(unreachable(&warnings/2))
  end

  defp unreachable() do
    if unreachable(&print_entry/2) == [] do
      :ok
    else
      :error
    end
  end

  defp callers(callee) do
    callee
    |> filter_for_callee()
    |> do_callers()

    :ok
  end

  defp graph(opts) do
    write_graph(file_references(), excluded(opts), opts)

    :ok
  end

  ## Unreachable

  defp unreachable(pair_fun) do
    excludes = excludes()
    each_source_entries(&source_warnings(&1, excludes), pair_fun)
  end

  defp source_warnings(source, excludes) do
    source(runtime_dispatches: runtime_dispatches) = source

    for {module, func_arity_lines} <- runtime_dispatches,
        exports = load_exports(module),
        {{func, arity}, lines} <- func_arity_lines,
        warning = unreachable_mfa(exports, module, func, arity, lines, excludes),
        do: warning
  end

  defp load_exports(module) do
    if :code.is_loaded(module) do
      # If the module is loaded, we will use the faster function_exported?/3 check
      module
    else
      # Otherwise we get all exports from :beam_lib to avoid loading modules
      with file when is_list(file) <- :code.which(module),
           {:ok, {^module, [exports: exports]}} <- :beam_lib.chunks(file, [:exports]) do
        exports
      else
        _ -> :unknown_module
      end
    end
  end

  defp unreachable_mfa(exports, module, func, arity, lines, excludes) do
    cond do
      excluded?(module, func, arity, excludes) ->
        nil
      skip?(module, func, arity) ->
        nil
      exports == :unknown_module ->
        {Enum.sort(lines), :unknown_module, module, func, arity, nil}
      is_atom(exports) and not function_exported?(module, func, arity) ->
        {Enum.sort(lines), :unknown_function, module, func, arity, nil}
      is_list(exports) and {func, arity} not in exports ->
        {Enum.sort(lines), :unknown_function, module, func, arity, exports}
      true ->
        nil
    end
  end

  ## Print entries

  defp print_entry(file, entries) do
    entries
    |> Enum.sort()
    |> Enum.each(&IO.write(format_entry(file, &1)))
  end

  defp format_entry(file, {lines, _, module, function, arity, _}) do
    for line <- lines do
      [Exception.format_file_line(file, line), ?\s, Exception.format_mfa(module, function, arity), ?\n]
    end
  end

  ## Print warnings

  defp warnings(file, entries) do
    prefix = IO.ANSI.format([:yellow, "warning: "])
    Enum.map(Enum.sort(entries), fn entry ->
      message = message(entry)
      lines = elem(entry, 0)
      IO.write(:stderr, [prefix, message, ?\n, format_file_lines(file, lines), ?\n])
      {file, lines, message}
    end)
  end

  defp message({_lines, :unknown_function, module, function, arity, exports}) do
    UndefinedFunctionError.function_not_exported(module, function, arity, exports)
  end

  defp message({_lines, :unknown_module, module, function, arity, _}) do
    ["function ", Exception.format_mfa(module, function, arity),
     " is undefined (module #{inspect module} is not available)"]
  end

  defp format_file_lines(file, [line]) do
    format_file_line(file, line)
  end

  defp format_file_lines(file, lines) do
    ["Found at #{length(lines)} locations:\n" |
     Enum.map(lines, &format_file_line(file, &1))]
  end

  defp format_file_line(file, line) do
    ["  ", file, ?:, Integer.to_string(line), ?\n]
  end

  ## "Unreachable" helpers

  @protocol_builtins for {_, type} <- Protocol.__builtin__(), do: type

  defp skip?(:erlang, func, 2) when func in [:andalso, :orelse] do
    true
  end

  defp skip?(module, :__impl__, 1) do
    {maybe_protocol, maybe_builtin} = module |> Module.split() |> Enum.split(-1)
    maybe_protocol = Module.concat(maybe_protocol)
    maybe_builtin = Module.concat(maybe_builtin)

    maybe_builtin in @protocol_builtins and
      Code.ensure_loaded?(maybe_protocol) and
      function_exported?(maybe_protocol, :__protocol__, 1)
  end

  defp skip?(_, _, _) do
    false
  end

  defp excludes() do
    Mix.Project.config()
    |> Keyword.get(:xref, [])
    |> Keyword.get(:exclude, [])
    |> MapSet.new()
  end

  defp excluded?(module, func, arity, excludes) do
    MapSet.member?(excludes, module) or MapSet.member?(excludes, {module, func, arity})
  end

  ## Callers

  defp do_callers(filter) do
    each_source_entries(&source_calls_for_filter(&1, filter), &print_calls/2)
  end

  defp source_calls_for_filter(source, filter) do
    runtime_dispatches = source(source, :runtime_dispatches)
    compile_dispatches = source(source, :compile_dispatches)
    dispatches = runtime_dispatches ++ compile_dispatches

    calls =
      for {module, func_arity_lines} <- dispatches,
          {{func, arity}, lines} <- func_arity_lines,
          filter.({module, func, arity}),
          do: {module, func, arity, lines}

    Enum.reduce calls, %{}, fn {module, func, arity, lines}, merged_calls ->
      lines = MapSet.new(lines)
      Map.update(merged_calls, {module, func, arity}, lines, &MapSet.union(&1, lines))
    end
  end

  ## Print callers

  defp print_calls(file, calls) do
    calls
    |> Enum.sort()
    |> Enum.each(&IO.write(format_call(file, &1)))
  end

  defp format_call(file, {{module, func, arity}, lines}) do
    for line <- Enum.sort(lines),
      do: [file, ":", to_string(line), ": ", Exception.format_mfa(module, func, arity), ?\n]
  end

  ## "Callers" helpers

  defp filter_for_callee(callee) do
    case Mix.Utils.parse_mfa(callee) do
      {:ok, mfa_list} ->
        mfa_list_length = length(mfa_list)
        fn {module, function, arity} ->
          mfa_list == Enum.take([module, function, arity], mfa_list_length)
        end
      :error ->
        Mix.raise "xref callers CALLEE expects Module, Module.function, or Module.function/arity, " <>
                  "got: " <> callee
    end
  end

  ## Graph helpers

  defp excluded(opts) do
    opts
    |> Keyword.get_values(:exclude)
    |> Enum.flat_map(&[{&1, nil}, {&1, "(compile)"}, {&1, "(runtime)"}])
  end

  defp file_references() do
    module_sources =
      for manifest <- E.manifests(),
          manifest_data = read_manifest(manifest, ""),
          module(module: module, sources: sources) <- manifest_data,
          source <- sources,
          source = Enum.find(manifest_data, &match?(source(source: ^source), &1)),
          do: {module, source}

    all_modules = MapSet.new(module_sources, &elem(&1, 0))

    Map.new module_sources, fn {module, source} ->
      source(runtime_references: runtime, compile_references: compile, source: file) = source
      compile_references =
        compile
        |> MapSet.new()
        |> MapSet.delete(module)
        |> MapSet.intersection(all_modules)
        |> Enum.filter(&module_sources[&1] != source)
        |> Enum.map(&{source(module_sources[&1], :source), "(compile)"})

      runtime_references =
        runtime
        |> MapSet.new()
        |> MapSet.delete(module)
        |> MapSet.intersection(all_modules)
        |> Enum.filter(&module_sources[&1] != source)
        |> Enum.map(&{source(module_sources[&1], :source), nil})

      {file, compile_references ++ runtime_references}
    end
  end

  defp write_graph(file_references, excluded, opts) do
    {root, file_references} =
      case {opts[:source], opts[:sink]} do
        {nil, nil} ->
          {Enum.map(file_references, &{elem(&1, 0), nil}) -- excluded, file_references}

        {source, nil} ->
          if file_references[source] do
            {[{source, nil}], file_references}
          else
            Mix.raise "Source could not be found: #{source}"
          end

        {nil, sink} ->
          if file_references[sink] do
            file_references = filter_for_sink(file_references, sink)
            roots =
              file_references
              |> Map.delete(sink)
              |> Enum.map(&{elem(&1, 0), nil})
            {roots -- excluded, file_references}
          else
            Mix.raise "Sink could not be found: #{sink}"
          end

        {_, _} ->
          Mix.raise "mix xref graph expects only one of --source and --sink"
      end

    callback =
      fn {file, type} ->
        children = Map.get(file_references, file, [])
        {{file, type}, children -- excluded}
      end

    if opts[:format] == "dot" do
      Mix.Utils.write_dot_graph!("xref_graph.dot", "xref graph",
                                 root, callback, opts)
      """
      Generated "xref_graph.dot" in the current directory. To generate a PNG:

         dot -Tpng xref_graph.dot -o xref_graph.png

      For more options see http://www.graphviz.org/.
      """
      |> String.trim_trailing()
      |> Mix.shell.info()
    else
      Mix.Utils.print_tree(root, callback, opts)
    end
  end

  defp filter_for_sink(file_references, sink) do
    file_references
    |> invert_references()
    |> do_filter_for_sink([{sink, nil}], %{})
    |> invert_references()
  end

  defp do_filter_for_sink(file_references, new_nodes, acc) do
    Enum.reduce new_nodes, acc, fn {new_node_name, _type}, acc ->
      new_nodes = file_references[new_node_name]
      if acc[new_node_name] || !new_nodes do
        acc
      else
        do_filter_for_sink(file_references, new_nodes, Map.put(acc, new_node_name, new_nodes))
      end
    end
  end

  defp invert_references(file_references) do
    Enum.reduce file_references, %{}, fn {file, references}, acc ->
      Enum.reduce references, acc, fn {reference, type}, acc ->
        Map.update(acc, reference, [{file, type}], &[{file, type} | &1])
      end
    end
  end

  ## Helpers

  defp each_source_entries(entries_fun, pair_fun) do
    for manifest <- E.manifests(),
        source(source: file) = source <- read_manifest(manifest, ""),
        entries = entries_fun.(source),
        entries != [] and entries != %{},
        do: pair_fun.(file, entries)
  end
end
