defmodule Mix.Tasks.Xref do
  use Mix.Task

  import Mix.Compilers.Elixir,
    only: [read_manifest: 2, source: 0, source: 1, source: 2, module: 1]

  @shortdoc "Performs cross reference checks"
  @recursive true
  @manifest ".compile.elixir"

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

    * `--label` - only shows relationships with the given label
      The labels are "compile", "struct" and "runtime" (runtime is now shown on the graph)

    * `--only-nodes` - only show the node names (no edges)

    * `--source` - displays all files that the given source file references (directly or indirectly)

    * `--sink` - displays all files that reference the given file (directly or indirectly)

    * `--format` - can be set to one of:

      * `pretty` - prints the graph to the terminal using Unicode characters.
        Each prints each file followed by the files it depends on. This is the
        default except on Windows;

      * `plain` - the same as pretty except ASCII characters is used instead of
        Unicode characters. This is the default on Windows;

      * `stats` - prints general statistics about the graph;

      * `dot` - produces a DOT graph description in `xref_graph.dot` in the
        current directory. Warning: this will override any previously generated file

  The `--source` and `--sink` options are particularly useful when trying to understand
  how the modules in a particular file interact with the whole system. You can combine
  those options with `--label` and `--only-nodes` to get all files that exhibit a certain
  property, for example:

      # To get all files that depend on lib/foo.ex
      mix xref graph --sink lib/foo.ex --only-nodes

      # To get all files that depend on lib/foo.ex at compile time
      mix xref graph --label compile --sink lib/foo.ex --only-nodes

      # To show general statistics about the graph
      mix xref graph --format stats

      # To limit statistics only to certain labels
      mix xref graph --format stats --label compile

  ## Shared options

  Those options are shared across all modes:

    * `--include-siblings` - include dependencies that have `:in_umbrella` set
      to true in the current project in the reports. This can be used to find
      callers or analyze graphs between projects

    * `--no-compile` - does not compile even if files require compilation

    * `--no-deps-check` - does not check dependencies

    * `--no-archives-check` - does not check archives

    * `--no-elixir-version-check` - does not check the Elixir version from mix.exs

  ## Configuration

  All configuration for Xref should be placed under the key `:xref`.

    * `:exclude` - a list of modules and `{module, function, arity}` tuples to ignore when checking
      cross references. For example: `[MissingModule, {MissingModule2, :missing_func, 2}]`

  """

  @switches [
    archives_check: :boolean,
    compile: :boolean,
    deps_check: :boolean,
    elixir_version_check: :boolean,
    exclude: :keep,
    format: :string,
    include_siblings: :boolean,
    label: :string,
    only_nodes: :boolean,
    sink: :string,
    source: :string
  ]

  def run(args) do
    {opts, args} = OptionParser.parse!(args, strict: @switches)

    Mix.Task.run("loadpaths")

    if Keyword.get(opts, :compile, true) do
      Mix.Task.run("compile")
    end

    case args do
      ["warnings"] ->
        warnings(opts)

      ["unreachable"] ->
        unreachable(opts)

      ["callers", callee] ->
        callers(callee, opts)

      ["graph"] ->
        graph(opts)

      _ ->
        Mix.raise("xref doesn't support this command. For more information run \"mix help xref\"")
    end
  end

  ## Modes

  defp warnings(opts) do
    warnings(&print_warnings/1, opts)
  end

  defp unreachable(opts) do
    case warnings(&print_unreachables/1, opts) do
      {:ok, []} -> :ok
      _ -> :error
    end
  end

  defp callers(callee, opts) do
    callee
    |> filter_for_callee()
    |> source_callers(opts)
    |> merge_entries()
    |> sort_entries()
    |> print_calls()

    :ok
  end

  defp graph(opts) do
    write_graph(file_references(opts), excluded(opts), opts)

    :ok
  end

  ## Warnings

  defp warnings(print_warnings, opts) do
    case source_warnings(excludes(), opts) do
      [] ->
        {:ok, []}

      entries ->
        entries =
          entries
          |> merge_entries()
          |> sort_entries()
          |> print_warnings.()

        {:ok, entries}
    end
  end

  defp source_warnings(excludes, opts) do
    Enum.flat_map(sources(opts), fn source ->
      file = source(source, :source)
      runtime_dispatches = source(source, :runtime_dispatches)

      for {module, func_arity_locations} <- runtime_dispatches,
          exports = load_exports(module),
          {{func, arity}, locations} <- func_arity_locations,
          unreachable_mfa = unreachable_mfa(exports, module, func, arity, excludes),
          do: {unreachable_mfa, absolute_locations(locations, file)}
    end)
  end

  defp load_exports(module) do
    if :code.is_loaded(module) do
      # If the module is loaded, we will use the faster function_exported?/3 check
      module
    else
      # Otherwise we get all exports from :beam_lib to avoid loading modules
      with [_ | _] = file <- :code.which(module),
           {:ok, {^module, [exports: exports]}} <- :beam_lib.chunks(file, [:exports]) do
        exports
      else
        _ -> :unknown_module
      end
    end
  end

  defp unreachable_mfa(exports, module, func, arity, excludes) do
    cond do
      excluded?(module, func, arity, excludes) ->
        nil

      skip?(module, func, arity) ->
        nil

      exports == :unknown_module ->
        {:unknown_module, module, func, arity, nil}

      is_atom(exports) and not function_exported?(module, func, arity) ->
        {:unknown_function, module, func, arity, nil}

      is_list(exports) and {func, arity} not in exports ->
        {:unknown_function, module, func, arity, exports}

      true ->
        nil
    end
  end

  ## Print unreachable

  defp print_unreachables(entries) do
    Enum.each(entries, &print_unreachable/1)
    entries
  end

  defp print_unreachable({{_, module, function, arity, _}, locations}) do
    shell = Mix.shell()

    for {file, line} <- locations do
      shell.info([
        Exception.format_file_line(file, line, " "),
        Exception.format_mfa(module, function, arity)
      ])
    end
  end

  ## Print warnings

  defp print_warnings(entries) do
    prefix = IO.ANSI.format([:yellow, "warning: "])

    Enum.map(entries, fn {type, locations} ->
      message = warning_message(type)
      IO.write(:stderr, [prefix, message, ?\n, format_locations(locations), ?\n])
      {message, locations}
    end)
  end

  defp warning_message({:unknown_function, module, function, arity, exports}) do
    UndefinedFunctionError.function_not_exported(module, function, arity, exports)
  end

  defp warning_message({:unknown_module, module, function, arity, _}) do
    [
      "function ",
      Exception.format_mfa(module, function, arity),
      " is undefined (module #{inspect(module)} is not available)"
    ]
  end

  defp format_locations([location]) do
    format_location(location)
  end

  defp format_locations(locations) do
    [
      "Found at #{length(locations)} locations:\n",
      Enum.map(locations, &format_location/1)
    ]
  end

  defp format_location({file, line}) do
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

    maybe_builtin in @protocol_builtins and Code.ensure_loaded?(maybe_protocol) and
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

  defp source_callers(filter, opts) do
    Enum.flat_map(sources(opts), fn source ->
      file = source(source, :source)
      runtime_dispatches = source(source, :runtime_dispatches)
      compile_dispatches = source(source, :compile_dispatches)
      dispatches = runtime_dispatches ++ compile_dispatches

      for {module, func_arity_locations} <- dispatches,
          {{func, arity}, locations} <- func_arity_locations,
          filter.({module, func, arity}),
          do: {{module, func, arity}, absolute_locations(locations, file)}
    end)
  end

  ## Print callers

  defp print_calls(calls) do
    Enum.each(calls, &print_call/1)
    calls
  end

  defp print_call({{module, func, arity}, locations}) do
    shell = Mix.shell()

    for {file, line} <- locations do
      shell.info([
        Exception.format_file_line(file, line, " "),
        Exception.format_mfa(module, func, arity)
      ])
    end
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
        Mix.raise(
          "xref callers CALLEE expects Module, Module.function, or Module.function/arity, " <>
            "got: " <> callee
        )
    end
  end

  ## Graph helpers

  defp excluded(opts) do
    opts
    |> Keyword.get_values(:exclude)
    |> Enum.flat_map(&[{&1, nil}, {&1, :compile}, {&1, :struct}])
  end

  defp label_filter(nil), do: :all
  defp label_filter("compile"), do: :compile
  defp label_filter("struct"), do: :struct
  defp label_filter("runtime"), do: nil
  defp label_filter(other), do: Mix.raise("unknown --label #{other}")

  defp file_references(opts) do
    filter = label_filter(opts[:label])

    module_sources =
      for manifest_path <- manifests(opts),
          manifest_data = read_manifest(manifest_path, ""),
          module(module: module, sources: sources) <- manifest_data,
          source <- sources,
          source = Enum.find(manifest_data, &match?(source(source: ^source), &1)),
          do: {module, source}

    all_modules = MapSet.new(module_sources, &elem(&1, 0))

    Map.new(module_sources, fn {current, source} ->
      source(
        runtime_references: runtime,
        struct_references: structs,
        compile_references: compile,
        source: file
      ) = source

      compile_references =
        modules_to_nodes(compile, :compile, current, source, module_sources, all_modules, filter)

      struct_references =
        modules_to_nodes(structs, :struct, current, source, module_sources, all_modules, filter)

      runtime_references =
        modules_to_nodes(runtime, nil, current, source, module_sources, all_modules, filter)

      references =
        runtime_references
        |> Map.merge(struct_references)
        |> Map.merge(compile_references)
        |> Enum.to_list()

      {file, references}
    end)
  end

  defp modules_to_nodes(_, label, _, _, _, _, filter) when filter != :all and label != filter do
    %{}
  end

  defp modules_to_nodes(modules, label, current, source, module_sources, all_modules, _filter) do
    for module <- modules,
        module != current,
        module in all_modules,
        module_sources[module] != source,
        do: {source(module_sources[module], :source), label},
        into: %{}
  end

  defp write_graph(file_references, excluded, opts) do
    {root, file_references} =
      case {opts[:source], opts[:sink]} do
        {nil, nil} ->
          {Enum.map(file_references, &{elem(&1, 0), nil}) -- excluded, file_references}

        {source, nil} ->
          if file_references[source] do
            {Map.get(file_references, source, []), file_references}
          else
            Mix.raise("Source could not be found: #{source}")
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
            Mix.raise("Sink could not be found: #{sink}")
          end

        {_, _} ->
          Mix.raise("mix xref graph expects only one of --source and --sink")
      end

    callback = fn {file, type} ->
      children = if opts[:only_nodes], do: [], else: Map.get(file_references, file, [])
      type = type && "(#{type})"
      {{file, type}, children -- excluded}
    end

    case opts[:format] do
      "dot" ->
        Mix.Utils.write_dot_graph!("xref_graph.dot", "xref graph", root, callback, opts)

        """
        Generated "xref_graph.dot" in the current directory. To generate a PNG:

           dot -Tpng xref_graph.dot -o xref_graph.png

        For more options see http://www.graphviz.org/.
        """
        |> String.trim_trailing()
        |> Mix.shell().info()

      "stats" ->
        stats(file_references)

      _ ->
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
    Enum.reduce(new_nodes, acc, fn {new_node_name, _type}, acc ->
      new_nodes = file_references[new_node_name]

      if acc[new_node_name] || !new_nodes do
        acc
      else
        do_filter_for_sink(file_references, new_nodes, Map.put(acc, new_node_name, new_nodes))
      end
    end)
  end

  defp invert_references(file_references) do
    Enum.reduce(file_references, %{}, fn {file, references}, acc ->
      Enum.reduce(references, acc, fn {reference, type}, acc ->
        Map.update(acc, reference, [{file, type}], &[{file, type} | &1])
      end)
    end)
  end

  defp stats(references) do
    shell = Mix.shell()

    counters =
      Enum.reduce(references, %{compile: 0, struct: 0, nil: 0}, fn {_, deps}, acc ->
        Enum.reduce(deps, acc, fn {_, value}, acc ->
          Map.update!(acc, value, &(&1 + 1))
        end)
      end)

    shell.info("Tracked files: #{map_size(references)} (nodes)")
    shell.info("Compile dependencies: #{counters.compile} (edges)")
    shell.info("Structs dependencies: #{counters.struct} (edges)")
    shell.info("Runtime dependencies: #{counters.nil} (edges)")

    outgoing =
      references
      |> Enum.map(fn {file, deps} -> {length(deps), file} end)
      |> Enum.sort()
      |> Enum.take(-10)
      |> Enum.reverse()

    shell.info("\nTop #{length(outgoing)} files with most outgoing dependencies:")
    for {count, file} <- outgoing, do: shell.info("  * #{file} (#{count})")

    incoming =
      references
      |> Enum.reduce(%{}, fn {_, deps}, acc ->
           Enum.reduce(deps, acc, fn {file, _}, acc ->
             Map.update(acc, file, 1, &(&1 + 1))
           end)
         end)
      |> Enum.map(fn {file, count} -> {count, file} end)
      |> Enum.sort()
      |> Enum.take(-10)
      |> Enum.reverse()

    shell.info("\nTop #{length(incoming)} files with most incoming dependencies:")
    for {count, file} <- incoming, do: shell.info("  * #{file} (#{count})")
  end

  ## Helpers

  defp sources(opts) do
    for manifest <- manifests(opts),
        source() = source <- read_manifest(manifest, ""),
        do: source
  end

  defp manifests(opts) do
    siblings =
      if opts[:include_siblings] do
        for %{scm: Mix.SCM.Path, opts: opts} <- Mix.Dep.cached(),
            opts[:in_umbrella],
            do: Path.join(opts[:build], @manifest)
      else
        []
      end

    [Path.join(Mix.Project.manifest_path(), @manifest) | siblings]
  end

  defp merge_entries(entries) do
    Enum.reduce(entries, %{}, fn {type, locations}, merged_entries ->
      locations = MapSet.new(locations)
      Map.update(merged_entries, type, locations, &MapSet.union(&1, locations))
    end)
  end

  defp sort_entries(entries) do
    entries
    |> Enum.map(fn {type, locations} -> {type, Enum.sort(locations)} end)
    |> Enum.sort()
  end

  defp absolute_locations(locations, base) do
    Enum.map(locations, &absolute_location(&1, base))
  end

  defp absolute_location({_, _} = location, _), do: location
  defp absolute_location(line, base), do: {base, line}
end
