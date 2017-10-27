defmodule Mix.Tasks.Xref do
  use Mix.Task

  alias Mix.Tasks.Compile.Elixir, as: E

  import Mix.Compilers.Elixir,
    only: [read_manifest: 2, source: 0, source: 1, source: 2, module: 1]

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

  @switches [
    compile: :boolean,
    deps_check: :boolean,
    archives_check: :boolean,
    elixir_version_check: :boolean,
    exclude: :keep,
    format: :string,
    source: :string,
    sink: :string
  ]

  def run(args) do
    {opts, args} = OptionParser.parse!(args, strict: @switches)

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
        Mix.raise("xref doesn't support this command. For more information run \"mix help xref\"")
    end
  end

  ## Modes

  defp warnings() do
    warnings(&print_warnings/1)
  end

  defp unreachable() do
    case warnings(&print_unreachables/1) do
      {:ok, []} -> :ok
      _ -> :error
    end
  end

  defp callers(callee) do
    callee
    |> filter_for_callee()
    |> source_callers()
    |> merge_entries()
    |> sort_entries()
    |> print_calls()

    :ok
  end

  defp graph(opts) do
    write_graph(file_references(), excluded(opts), opts)

    :ok
  end

  ## Warnings

  defp warnings(print_warnings) do
    case source_warnings(excludes()) do
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

  defp source_warnings(excludes) do
    Enum.flat_map(sources(), &source_warnings(&1, excludes))
  end

  defp source_warnings(source, excludes) do
    file = source(source, :source)
    runtime_dispatches = source(source, :runtime_dispatches)

    for {module, func_arity_locations} <- runtime_dispatches,
        exports = load_exports(module),
        {{func, arity}, locations} <- func_arity_locations,
        unreachable_mfa = unreachable_mfa(exports, module, func, arity, excludes),
        do: {unreachable_mfa, absolute_locations(locations, file)}
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

  defp source_callers(filter) do
    Enum.flat_map(sources(), &source_callers(&1, filter))
  end

  defp source_callers(source, filter) do
    file = source(source, :source)
    runtime_dispatches = source(source, :runtime_dispatches)
    compile_dispatches = source(source, :compile_dispatches)
    dispatches = runtime_dispatches ++ compile_dispatches

    for {module, func_arity_locations} <- dispatches,
        {{func, arity}, locations} <- func_arity_locations,
        filter.({module, func, arity}),
        do: {{module, func, arity}, absolute_locations(locations, file)}
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

    Map.new(module_sources, fn {current, source} ->
      source(runtime_references: runtime, compile_references: compile, source: file) = source

      compile_references =
        for module <- compile,
            module != current,
            module in all_modules,
            module_sources[module] != source,
            do: {source(module_sources[module], :source), "(compile)"},
            into: %{}

      runtime_references =
        for module <- runtime,
            module != current,
            module in all_modules,
            module_sources[module] != source,
            do: {source(module_sources[module], :source), nil},
            into: %{}

      {file, runtime_references |> Map.merge(compile_references) |> Enum.to_list()}
    end)
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
      children = Map.get(file_references, file, [])
      {{file, type}, children -- excluded}
    end

    if opts[:format] == "dot" do
      Mix.Utils.write_dot_graph!("xref_graph.dot", "xref graph", root, callback, opts)

      """
      Generated "xref_graph.dot" in the current directory. To generate a PNG:

         dot -Tpng xref_graph.dot -o xref_graph.png

      For more options see http://www.graphviz.org/.
      """
      |> String.trim_trailing()
      |> Mix.shell().info()
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

  ## Helpers

  defp sources() do
    for manifest <- E.manifests(),
        source() = source <- read_manifest(manifest, ""),
        do: source
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
