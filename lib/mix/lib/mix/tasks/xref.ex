defmodule Mix.Tasks.Xref do
  use Mix.Task

  import Mix.Compilers.Elixir,
    only: [read_manifest: 1, source: 0, source: 1, source: 2, module: 1]

  @shortdoc "Performs cross reference checks"
  @recursive true
  @manifest "compile.elixir"

  @moduledoc """
  Performs cross reference checks between modules.

  The deprecated checks below happen every time
  your project is compiled via `mix compile.xref`. See
  `Mix.Tasks.Compile.Xref` for more information.

  This task is automatically reenabled, so you can perform multiple
  cross reference checks in the same Mix invocation.

  ## Xref modes

  The `xref` task expects a mode as first argument:

      mix xref MODE

  All available modes are discussed below.

  ## deprecated

  Prints all deprecated "file:line: module.function/arity" entries:

      mix xref deprecated

  The "file:line" represents the file and line a call to a deprecated
  "module.function/arity" is made. This operation does not show deprecated
  local calls (a call to a deprecated function or macro in the same module)
  nor calls to deprecated functionality in Elixir itself.

  The option `--abort-if-any` can be used for the command to fail if
  deprecated calls exist.

  ### callers CALLEE

  Prints all callers of the given `CALLEE`, which can be one of: `Module`,
  `Module.function`, or `Module.function/arity`. Examples:

      mix xref callers MyMod
      mix xref callers MyMod.fun
      mix xref callers MyMod.fun/3

  ### graph

  Prints a file dependency graph where an edge from `A` to `B` indicates
  that `A` (source) depends on `B` (sink).

      mix xref graph --format stats

  The following options are accepted:

    * `--exclude` - paths to exclude

    * `--label` - only shows relationships with the given label
      The labels are "compile", "struct" and "runtime"

    * `--only-nodes` - only shows the node names (no edges)

    * `--source` - displays all files that the given source file
      references (directly or indirectly)

    * `--sink` - displays all files that reference the given file
      (directly or indirectly)

    * `--format` - can be set to one of:

      * `pretty` - prints the graph to the terminal using Unicode characters.
        Each prints each file followed by the files it depends on. This is the
        default except on Windows;

      * `plain` - the same as pretty except ASCII characters are used instead of
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

    * `--include-siblings` - includes dependencies that have `:in_umbrella` set
      to true in the current project in the reports. This can be used to find
      callers or to analyze graphs between projects

    * `--no-compile` - does not compile even if files require compilation

    * `--no-deps-check` - does not check dependencies

    * `--no-archives-check` - does not check archives

    * `--no-elixir-version-check` - does not check the Elixir version from mix.exs

  ## Configuration

  All configuration for Xref should be placed under the key `:xref`.

    * `:exclude` - a list of modules and `{module, function, arity}`
      tuples to ignore when checking cross references. For example:
      `[MissingModule, {MissingModule2, :missing_func, 2}]`

  """

  @switches [
    abort_if_any: :boolean,
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

  @impl true
  def run(args) do
    {opts, args} = OptionParser.parse!(args, strict: @switches)
    Mix.Task.run("loadpaths")

    if Keyword.get(opts, :compile, true) do
      Mix.Task.run("compile")
    end

    Mix.Task.reenable("xref")

    case args do
      ["deprecated"] ->
        deprecated(opts)

      ["callers", callee] ->
        callers(callee, opts)

      ["graph"] ->
        graph(opts)

      _ ->
        Mix.raise("xref doesn't support this command. For more information run \"mix help xref\"")
    end
  end

  @doc false
  def warnings(opts \\ []) do
    source_warnings(excludes(), opts)
    |> merge_entries(:all)
    |> sort_entries()
    |> print_warnings()
  end

  @doc """
  Returns a list of information of all the function calls in the project.

  Each item in the list is a map with the following keys:

    * `:callee` - a tuple containing the module, function, and arity of the call
    * `:line` - an integer representing the line where the function is called
    * `:file` - a binary representing the file where the function is called
    * `:caller_module` - the module where the function is called

  This function returns an empty list when used at the root of an umbrella
  project because there is no compile manifest to extract the function call
  information from. To get the function calls of each child in an umbrella,
  execute the function at the root of each individual application.
  """
  @spec calls(keyword()) :: [
          %{
            callee: {module(), atom(), arity()},
            caller_module: module(),
            line: integer(),
            file: String.t()
          }
        ]
  def calls(opts \\ []) do
    module_sources =
      for manifest <- manifests(opts),
          manifest_data = read_manifest(manifest),
          module(module: module, sources: sources) <- manifest_data,
          source <- sources,
          source = Enum.find(manifest_data, &match?(source(source: ^source), &1)),
          do: {module, source}

    Enum.flat_map(module_sources, fn {caller_module, source} ->
      source(
        runtime_dispatches: runtime_nested,
        compile_dispatches: compile_nested,
        source: rel_file
      ) = source

      runtime_function_calls =
        dispatches_to_function_calls(caller_module, rel_file, runtime_nested)

      compile_function_calls =
        dispatches_to_function_calls(caller_module, rel_file, compile_nested)

      runtime_function_calls ++ compile_function_calls
    end)
  end

  defp dispatches_to_function_calls(caller_module, file, dispatches) do
    for {module, function_calls} <- dispatches,
        {{function, arity}, lines} <- function_calls,
        line <- lines do
      %{
        callee: {module, function, arity},
        file: file,
        line: line,
        caller_module: caller_module
      }
    end
  end

  ## Modes

  defp deprecated(opts) do
    source_warnings(excludes(), opts)
    |> merge_entries(&(&1 == :deprecated))
    |> sort_entries()
    |> print_mfas()
    |> return_ok_error_or_abort(:deprecated, opts)
  end

  defp callers(callee, opts) do
    callee
    |> filter_for_callee()
    |> source_callers(opts)
    |> merge_entries(:all)
    |> sort_entries()
    |> print_calls()

    :ok
  end

  defp graph(opts) do
    write_graph(file_references(opts), excluded(opts), opts)

    :ok
  end

  ## Warnings deprecated

  defp source_warnings(excludes, opts) do
    sources = sources(opts)

    deprecated =
      Enum.reduce(sources, %{}, fn source, acc ->
        runtime = source(source, :runtime_dispatches)
        compile = source(source, :compile_dispatches)

        acc = Enum.reduce(runtime, acc, &load_deprecated_into_acc/2)
        acc = Enum.reduce(compile, acc, &load_deprecated_into_acc/2)
        acc
      end)

    runtime_warnings =
      for source <- sources,
          {module, func_arity_locations} <- source(source, :runtime_dispatches),
          deprecated = deprecated[module],
          {{func, arity}, locations} <- func_arity_locations,
          not excluded?(excludes, module, func, arity),
          warning = deprecated_mfa(deprecated, module, func, arity),
          do: {warning, absolute_locations(locations, source(source, :source))}

    compile_warnings =
      for source <- sources,
          {module, func_arity_locations} <- source(source, :compile_dispatches),
          deprecated = deprecated[module],
          {{func, arity}, locations} <- func_arity_locations,
          not excluded?(excludes, module, func, arity),
          warning = deprecated_mfa(deprecated, module, func, arity),
          do: {warning, absolute_locations(locations, source(source, :source))}

    runtime_warnings ++ compile_warnings
  end

  defp load_deprecated_into_acc({module, _}, acc) do
    case acc do
      %{^module => _} -> acc
      _ -> Map.put(acc, module, load_deprecated(module))
    end
  end

  defp load_deprecated(module) do
    if :code.is_loaded(module) do
      # If the module is loaded, we will use the faster function_exported?/3
      # check for exports and __info__/1 for deprecated
      if function_exported?(module, :__info__, 1) do
        load_deprecated_from_module(module)
      else
        []
      end
    else
      # Otherwise we get the information from :beam_lib to avoid loading modules
      with [_ | _] = file <- :code.which(module),
           {:ok, {^module, [{'ExDp', deprecated}]}} <-
             :beam_lib.chunks(file, ['ExDp'], [:allow_missing_chunks]) do
        load_deprecated_from_chunk(deprecated)
      else
        _ -> []
      end
    end
  end

  defp load_deprecated_from_chunk(chunk) do
    if is_binary(chunk) do
      {:elixir_deprecated_v1, deprecated} = :erlang.binary_to_term(chunk)
      deprecated
    else
      []
    end
  end

  defp load_deprecated_from_module(module) do
    try do
      module.__info__(:deprecated)
    rescue
      _ -> []
    end
  end

  defp deprecated_mfa(deprecated, module, func, arity) do
    case List.keyfind(deprecated, {func, arity}, 0) do
      {_, reason} ->
        {:deprecated, module, func, arity, reason}

      nil ->
        nil
    end
  end

  defp excludes() do
    Mix.Project.config()
    |> Keyword.get(:xref, [])
    |> Keyword.get(:exclude, [])
    |> MapSet.new()
  end

  defp excluded?(excludes, module, func, arity) do
    MapSet.member?(excludes, module) or MapSet.member?(excludes, {module, func, arity})
  end

  ## Print mfas (unreachable / deprecated)

  defp print_mfas(entries) do
    Enum.each(entries, &print_mfa/1)
    entries
  end

  defp print_mfa({{_, module, function, arity, _}, locations}) do
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

  defp warning_message({:deprecated, module, function, arity, reason}) do
    [
      Exception.format_mfa(module, function, arity),
      " is deprecated. ",
      reason
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

  ## Callers

  defp source_callers(filter, opts) do
    for source <- sources(opts),
        file = source(source, :source),
        {module, func_arity_locations} <- dispatches(source),
        {{func, arity}, locations} <- func_arity_locations,
        filter.({module, func, arity}),
        do: {{module, func, arity}, absolute_locations(locations, file)}
  end

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

  ## Graph

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
          manifest_data = read_manifest(manifest_path),
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
    |> apply_filter_for_sink([{sink, nil}], %{})
    |> invert_references()
  end

  defp apply_filter_for_sink(file_references, new_nodes, acc) do
    Enum.reduce(new_nodes, acc, fn {new_node_name, _type}, acc ->
      new_nodes = file_references[new_node_name]

      if acc[new_node_name] || !new_nodes do
        acc
      else
        apply_filter_for_sink(file_references, new_nodes, Map.put(acc, new_node_name, new_nodes))
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
        source() = source <- read_manifest(manifest),
        do: source
  end

  defp manifests(opts) do
    siblings =
      if opts[:include_siblings] do
        for %{scm: Mix.SCM.Path, opts: opts} <- Mix.Dep.cached(),
            opts[:in_umbrella],
            do: Path.join([opts[:build], ".mix", @manifest])
      else
        []
      end

    [Path.join(Mix.Project.manifest_path(), @manifest) | siblings]
  end

  defp merge_entries(entries, filter) do
    Enum.reduce(entries, %{}, fn {type, locations}, merged_entries ->
      if filter == :all or filter.(elem(type, 0)) do
        locations = MapSet.new(locations)
        Map.update(merged_entries, type, locations, &MapSet.union(&1, locations))
      else
        merged_entries
      end
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

  defp dispatches(source) do
    source(source, :runtime_dispatches) ++ source(source, :compile_dispatches)
  end

  defp return_ok_error_or_abort(entries, kind, opts) do
    cond do
      entries == [] -> :ok
      opts[:abort_if_any] -> Mix.raise("mix xref #{kind} failed: #{kind} calls were found")
      true -> :error
    end
  end
end
