defmodule Mix.Tasks.Xref do
  use Mix.Task

  import Mix.Compilers.Elixir,
    only: [read_manifest: 1, source: 0, source: 1, source: 2, module: 1]

  @shortdoc "Prints cross reference information"
  @recursive true
  @manifest "compile.elixir"

  @moduledoc """
  Prints cross reference information between modules.

  The `xref` task expects a mode as first argument:

      $ mix xref MODE

  All available modes are discussed below.

  This task is automatically re-enabled, so you can print
  information multiple times in the same Mix invocation.

  ## mix xref callers MODULE

  Prints all callers of the given `MODULE`. Example:

      $ mix xref callers MyMod

  ## mix xref trace FILE

  Compiles the given file listing all dependencies within the same app.
  It includes the type and line for each one. Example:

      $ mix xref trace lib/my_app/router.ex

  The `--label` option may be given to keep only certain traces
  (compile, runtime or export):

      $ mix xref trace lib/my_app/router.ex --label compile

  If you have an umbrella application, we also recommend using the
  `--include-siblings` flag to see the dependencies on other
  umbrella applications.

  ### Example

  Imagine the given file lib/b.ex:

      defmodule B do
        import A
        A.macro()
        macro()
        A.fun()
        fun()
        def calls_macro, do: A.macro()
        def calls_fun, do: A.fun()
        def calls_struct, do: %A{}
      end

  `mix xref trace` will print:

      lib/b.ex:2: require A (export)
      lib/b.ex:3: call A.macro/0 (compile)
      lib/b.ex:4: import A.macro/0 (compile)
      lib/b.ex:5: call A.fun/0 (compile)
      lib/b.ex:6: call A.fun/0 (compile)
      lib/b.ex:6: import A.fun/0 (compile)
      lib/b.ex:7: call A.macro/0 (compile)
      lib/b.ex:8: call A.fun/0 (runtime)
      lib/b.ex:9: struct A (export)

  ## mix xref graph

  Prints a file dependency graph where an edge from `A` to `B` indicates
  that `A` (source) depends on `B` (sink).

      $ mix xref graph --format stats

  The following options are accepted:

    * `--exclude` - path to exclude. Can be repeated to exclude multiple paths.

    * `--label` - only shows relationships with the given label.
      The labels are "compile", "export" and "runtime". By default,
      the `--label` option simply filters the printed graph to show
      only relationships with the given label. You can pass `--only-direct`
      to trim the graph to only the nodes that have the direct
      relationship given by label. There is also a special label
      called "compile-connected" that keeps only compile-time files
      with at least one transitive dependency. See "Dependencies types"
      section below.

    * `--group` - provide comma-separated paths to consider as a group. Dependencies
      from and into multiple files of the group are considered a single dependency.
      Dependencies between the group elements are ignored. This is useful when you
      are computing compile and compile-connected dependencies and you want a
      series of files to be treated as one. The group is printed using the first path,
      with a `+` suffix. Can be repeated to create multiple groups.

    * `--only-direct` - keeps only files with the direct relationship
      given by `--label`

    * `--only-nodes` - only shows the node names (no edges).
      Generally useful with the `--sink` flag

    * `--source` - displays all files that the given source file
      references (directly or indirectly). Can be repeated to display
      references from multiple sources.

    * `--sink` - displays all files that reference the given file
      (directly or indirectly). Can be repeated.

    * `--min-cycle-size` - controls the minimum cycle size on formats
      like `stats` and `cycles`

    * `--format` - can be set to one of:

      * `pretty` - prints the graph to the terminal using Unicode characters.
        Each prints each file followed by the files it depends on. This is the
        default except on Windows;

      * `plain` - the same as pretty except ASCII characters are used instead of
        Unicode characters. This is the default on Windows;

      * `stats` - prints general statistics about the graph;

      * `cycles` - prints all cycles in the graph;

      * `dot` - produces a DOT graph description in `xref_graph.dot` in the
        current directory. Warning: this will override any previously generated file

  The `--source` and `--sink` options are particularly useful when trying to understand
  how the modules in a particular file interact with the whole system. You can combine
  those options with `--label` and `--only-nodes` to get all files that exhibit a certain
  property, for example:

      # To show all compile-time relationships
      mix xref graph --label compile

      # To get the tree that depend on lib/foo.ex at compile time
      mix xref graph --label compile --sink lib/foo.ex

      # To get all files that depend on lib/foo.ex at compile time
      mix xref graph --label compile --sink lib/foo.ex --only-nodes

      # To get all paths between two files
      mix xref graph --source lib/foo.ex --sink lib/bar.ex

      # To show general statistics about the graph
      mix xref graph --format stats

  ### Understanding the printed graph

  When `mix xref graph` runs, it will print a tree of the following
  format. Imagine the following code:

      # lib/a.ex
      defmodule A do
        IO.puts B.hello()
      end

      # lib/b.ex
      defmodule B do
        def hello, do: C.world()
      end

      # lib/c.ex
      defmodule C do
        def world, do: "hello world"
      end

  It will print:

      $ mix xref graph
      lib/a.ex
      └── lib/b.ex (compile)
      lib/b.ex
      └── lib/c.ex
      lib/c.ex

  This tree means that `lib/a.ex` depends on `lib/b.ex` at compile
  time. And `lib/b.ex` depends on `lib/c.ex` at runtime. This is often
  problematic because if `lib/c.ex` changes, `lib/a.ex` also has to
  recompile due to this indirect compile time dependency. When you pass
  `--label compile`, the graph shows only the compile-time dependencies:

      $ mix xref graph --label compile
      lib/a.ex
      └── lib/b.ex (compile)

  The `--label compile` flag removes all non-compile dependencies. However,
  this can be misleading because having direct compile time dependencies is
  not necessarily an issue. The biggest concern, as mentioned above, are the
  transitive compile time dependencies. You can get all compile time
  dependencies that cause transitive compile time dependencies by using
  `--label compile-connected`:

      $ mix xref graph --label compile-connected
      lib/a.ex
      └── lib/b.ex (compile)

  The above says `lib/a.ex` depends on `lib/b.ex` and that causes transitive
  compile time dependencies - as we know, `lib/a.ex` also depends on `lib/c.ex`.
  We can retrieve those transitive dependencies by passing `lib/b.ex` as
  `--source` to `mix xref graph`:

      $ mix xref graph --source lib/b.ex
      lib/b.ex
      └── lib/c.ex

  Similarly, you can use the `--label compile` and the `--sink` flag to find
  all compile time dependencies that will recompile once the sink changes:

      $ mix xref graph --label compile --sink lib/c.ex
      lib/a.ex
      └── lib/b.ex (compile)

  ### Dependencies types

  Elixir tracks three types of dependencies between modules: compile,
  exports, and runtime. If a module has a compile time dependency on
  another module, the caller module has to be recompiled whenever the
  callee changes. Compile-time dependencies are typically added when
  using macros or when invoking functions in the module body (outside
  of functions). You can list all dependencies in a file by running
  `mix xref trace path/to/file.ex`.

  Exports dependencies are compile time dependencies on the module API,
  namely structs and its public definitions. For example, if you import
  a module but only use its functions, it is an export dependency. If
  you use a struct, it is an export dependency too. Export dependencies
  are only recompiled if the module API changes. Note, however, that compile
  time dependencies have higher precedence than exports. Therefore if
  you import a module and use its macros, it is a compile time dependency.

  Runtime dependencies are added whenever you invoke another module
  inside a function. Modules with runtime dependencies do not have
  to be compiled when the callee changes, unless there is a transitive
  compile or an outdated export time dependency between them. The option
  `--label compile-connected` can be used to find the first case.

  ## Shared options

  Those options are shared across all modes:

    * `--fail-above` - generates a failure if the relevant metric is above the
      given threshold. Applies to all modes except `mix xref graph --format stats`.

    * `--include-siblings` - includes dependencies that have `:in_umbrella` set
      to true in the current project in the reports. This can be used to find
      callers or to analyze graphs between projects

    * `--no-compile` - does not compile even if files require compilation

    * `--no-deps-check` - does not check dependencies

    * `--no-archives-check` - does not check archives

    * `--no-elixir-version-check` - does not check the Elixir version from mix.exs

  """

  @switches [
    archives_check: :boolean,
    compile: :boolean,
    deps_check: :boolean,
    elixir_version_check: :boolean,
    exclude: :keep,
    fail_above: :integer,
    format: :string,
    group: :keep,
    include_siblings: :boolean,
    label: :string,
    only_nodes: :boolean,
    only_direct: :boolean,
    sink: :keep,
    source: :keep,
    min_cycle_size: :integer
  ]

  @impl true
  def run(args) do
    Mix.Task.run("compile", args)
    Mix.Task.reenable("xref")

    {opts, args} = OptionParser.parse!(args, strict: @switches)

    case args do
      ["callers", module] ->
        handle_callers(module, opts)

      ["trace", file] ->
        handle_trace(file, opts)

      ["graph"] ->
        handle_graph(opts)

      # TODO: Remove on v2.0
      ["deprecated"] ->
        Mix.shell().error(
          "The deprecated check has been moved to the compiler and has no effect now"
        )

      # TODO: Remove on v2.0
      ["unreachable"] ->
        Mix.shell().error(
          "The unreachable check has been moved to the compiler and has no effect now"
        )

      _ ->
        Mix.raise("xref doesn't support this command. For more information run \"mix help xref\"")
    end
  end

  @doc """
  Returns a list of information of all the runtime function calls in the project.

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
  @deprecated "Use compilation tracers described in the Code module"
  @spec calls(keyword()) :: [
          %{
            callee: {module(), atom(), arity()},
            line: integer(),
            file: String.t()
          }
        ]
  def calls(opts \\ []) do
    for manifest <- manifests(opts),
        source(source: source, modules: modules) <- read_manifest(manifest) |> elem(1),
        module <- modules,
        call <- collect_calls(source, module),
        do: call
  end

  defp collect_calls(source, module) do
    with [_ | _] = path <- :code.which(module),
         {:ok, {_, [debug_info: debug_info]}} <- :beam_lib.chunks(path, [:debug_info]),
         {:debug_info_v1, backend, data} <- debug_info,
         {:ok, %{definitions: defs}} <- backend.debug_info(:elixir_v1, module, data, []),
         do: walk_definitions(module, source, defs),
         else: (_ -> [])
  end

  defp walk_definitions(module, file, definitions) do
    state = %{
      file: file,
      module: module,
      calls: []
    }

    state = Enum.reduce(definitions, state, &walk_definition/2)
    state.calls
  end

  defp walk_definition({_function, _kind, meta, clauses}, state) do
    with_file_meta(state, meta, fn state ->
      Enum.reduce(clauses, state, &walk_clause/2)
    end)
  end

  defp with_file_meta(%{file: original_file} = state, meta, fun) do
    case Keyword.fetch(meta, :file) do
      {:ok, {meta_file, _}} ->
        state = fun.(%{state | file: meta_file})
        %{state | file: original_file}

      :error ->
        fun.(state)
    end
  end

  defp walk_clause({_meta, args, _guards, body}, state) do
    state = walk_expr(args, state)
    walk_expr(body, state)
  end

  # &Mod.fun/arity
  defp walk_expr({:&, meta, [{:/, _, [{{:., _, [module, fun]}, _, []}, arity]}]}, state)
       when is_atom(module) and is_atom(fun) do
    add_call(module, fun, arity, meta, state)
  end

  # Mod.fun(...)
  defp walk_expr({{:., _, [module, fun]}, meta, args}, state)
       when is_atom(module) and is_atom(fun) do
    state = add_call(module, fun, length(args), meta, state)
    walk_expr(args, state)
  end

  # %Module{...}
  defp walk_expr({:%, meta, [module, {:%{}, _meta, args}]}, state)
       when is_atom(module) and is_list(args) do
    state = add_call(module, :__struct__, 0, meta, state)
    walk_expr(args, state)
  end

  # Function call
  defp walk_expr({left, _meta, right}, state) when is_list(right) do
    state = walk_expr(right, state)
    walk_expr(left, state)
  end

  # {x, y}
  defp walk_expr({left, right}, state) do
    state = walk_expr(right, state)
    walk_expr(left, state)
  end

  # [...]
  defp walk_expr(list, state) when is_list(list) do
    Enum.reduce(list, state, &walk_expr/2)
  end

  defp walk_expr(_other, state) do
    state
  end

  defp add_call(module, fun, arity, meta, state) do
    call = %{
      callee: {module, fun, arity},
      caller_module: state.module,
      file: state.file,
      line: meta[:line]
    }

    %{state | calls: [call | state.calls]}
  end

  ## Modes

  defp handle_callers(module, opts) do
    module = parse_module(module)

    file_callers =
      for source <- sources(opts),
          reference = reference(module, source),
          do: {source(source, :source), reference}

    for {file, type} <- Enum.sort(file_callers) do
      Mix.shell().info([file, " (", type, ")"])
    end

    check_failure(:references, length(file_callers), opts[:fail_above])
  end

  defp handle_trace(file, opts) do
    set =
      for app <- apps(opts),
          modules = Application.spec(app, :modules),
          module <- modules,
          into: MapSet.new(),
          do: module

    new = [ignore_already_consolidated: true, ignore_module_conflict: true, tracers: [__MODULE__]]
    old = Code.compiler_options(new)
    ets = :ets.new(__MODULE__, [:named_table, :duplicate_bag, :public])
    :ets.insert(ets, [{:config, set, trace_label(opts[:label])}])

    try do
      Code.compile_file(file)
    else
      _ ->
        :ets.delete(ets, :modules)

        traces =
          try do
            print_traces(Enum.sort(:ets.lookup_element(__MODULE__, :entry, 2)))
          rescue
            _ -> []
          end

        check_failure(:traces, length(traces), opts[:fail_above])
    after
      :ets.delete(ets)
      Code.compiler_options(old)
    end
  end

  defp handle_graph(opts) do
    label = label_filter(opts[:label])

    {direct_filter, transitive_filter} =
      if opts[:only_direct], do: {label, :all}, else: {:all, label}

    write_graph(file_references(direct_filter, opts), transitive_filter, opts)
  end

  ## Callers

  defp parse_module(module) do
    case Mix.Utils.parse_mfa(module) do
      {:ok, [module]} -> module
      _ -> Mix.raise("xref callers MODULE expects a MODULE, got: " <> module)
    end
  end

  defp reference(module, source) do
    cond do
      module in source(source, :compile_references) -> "compile"
      module in source(source, :export_references) -> "export"
      module in source(source, :runtime_references) -> "runtime"
      true -> nil
    end
  end

  ## Trace

  @doc false
  def trace({:require, meta, module, _opts}, env),
    do: add_trace(require_mode(meta), :require, module, module, meta, env)

  def trace({:struct_expansion, meta, module, _keys}, env),
    do: add_trace(:export, :struct, module, module, meta, env)

  def trace({:alias_reference, meta, module}, env) when env.module != module,
    do: add_trace(mode(env), :alias, module, module, meta, env)

  def trace({:remote_function, meta, module, function, arity}, env),
    do: add_trace(mode(env), :call, module, {module, function, arity}, meta, env)

  def trace({:remote_macro, meta, module, function, arity}, env),
    do: add_trace(:compile, :call, module, {module, function, arity}, meta, env)

  def trace({:imported_function, meta, module, function, arity}, env),
    do: add_trace(mode(env), :import, module, {module, function, arity}, meta, env)

  def trace({:imported_macro, meta, module, function, arity}, env),
    do: add_trace(:compile, :import, module, {module, function, arity}, meta, env)

  def trace(_event, _env),
    do: :ok

  defp require_mode(meta), do: if(meta[:from_macro], do: :compile, else: :export)

  defp mode(%{function: nil}), do: :compile
  defp mode(_), do: :runtime

  defp add_trace(mode, type, module, module_or_mfa, meta, env) do
    [{:config, modules, label}] = :ets.lookup(__MODULE__, :config)

    if module in modules and (label == nil or mode == label) do
      line = meta[:line] || env.line
      :ets.insert(__MODULE__, {:entry, {env.file, line, module_or_mfa, mode, type}})
    end

    :ok
  end

  defp print_traces(entries) do
    # We don't want to show aliases if there is an entry of the same type
    non_aliases =
      for {_file, _line, module_or_mfa, mode, type} <- entries,
          type != :alias,
          into: %{},
          do: {{trace_module(module_or_mfa), mode}, []}

    shell = Mix.shell()

    for {file, line, module_or_mfa, mode, type} <- entries,
        type != :alias or not Map.has_key?(non_aliases, {module_or_mfa, mode}) do
      shell.info([
        Exception.format_file_line(Path.relative_to_cwd(file), line),
        ?\s,
        Atom.to_string(type),
        ?\s,
        format_module_or_mfa(module_or_mfa),
        " (#{mode})"
      ])

      :ok
    end
  end

  defp trace_label(nil), do: nil
  defp trace_label("compile"), do: :compile
  defp trace_label("export"), do: :export
  defp trace_label("runtime"), do: :runtime
  defp trace_label(other), do: Mix.raise("Unknown --label #{other} in mix xref trace")

  defp trace_module({m, _, _}), do: m
  defp trace_module(m), do: m

  defp format_module_or_mfa({m, f, a}), do: Exception.format_mfa(m, f, a)
  defp format_module_or_mfa(m), do: inspect(m)

  ## Graph

  defp merge_groups(file_references, comma_separated_groups) do
    for group_paths <- comma_separated_groups,
        reduce: {file_references, %{}} do
      {file_references, aliases} ->
        group_paths
        |> String.split(",")
        |> check_files(file_references, :group)
        |> group(file_references, aliases)
    end
  end

  @type_order %{
    compile: 0,
    export: 1,
    nil: 2
  }

  # Group the given paths.
  # In graph theory vocabulary, this is done by vertex identification
  # and removal of edges between contracting vertices.
  defp group(paths, file_references, aliases) do
    group_name = hd(paths) <> "+"
    aliases = paths |> Map.new(&{&1, group_name}) |> Map.merge(aliases)

    # Merge the references *from* the paths to group
    {from_group, file_references} = Map.split(file_references, paths)

    file_references =
      Map.put(file_references, group_name, merge_references_from_group(from_group))

    # Remap the references *to* the merged group
    file_references =
      Map.new(file_references, fn {file, references} ->
        {file, remap_references_to_group(references, aliases, group_name)}
      end)

    # Remove the resulting reference from the merged group to itself, if there is one
    file_references = Map.update!(file_references, group_name, &List.keydelete(&1, group_name, 0))

    {file_references, aliases}
  end

  # Calculate the references from the merged group by concatenating all the references
  # from its components; in case of duplicates keep the one with the most important type.
  defp merge_references_from_group(file_references_to_merge) do
    file_references_to_merge
    |> Map.values()
    |> Enum.concat()
    |> Enum.sort_by(fn {_ref, type} -> @type_order[type] end)
    |> Enum.uniq_by(fn {ref, _type} -> ref end)
    |> Enum.sort()
  end

  defp remap_references_to_group(references, aliases, group_name) do
    case Enum.split_with(references, fn {ref, _type} -> Map.has_key?(aliases, ref) end) do
      {[], _all_references} ->
        references

      {refs_to_merge, other_refs} ->
        type =
          refs_to_merge
          |> Enum.map(fn {_ref, type} -> type end)
          |> Enum.min_by(&@type_order[&1])

        Enum.sort([{group_name, type} | other_refs])
    end
  end

  defp exclude(file_references, nil), do: file_references

  defp exclude(file_references, excluded) do
    excluded_set = MapSet.new(excluded)

    file_references
    |> Map.drop(excluded)
    |> Map.new(fn {key, list} ->
      {key, Enum.reject(list, fn {ref, _kind} -> MapSet.member?(excluded_set, ref) end)}
    end)
  end

  defp label_filter(nil), do: :all
  defp label_filter("compile"), do: :compile
  defp label_filter("export"), do: :export
  defp label_filter("runtime"), do: nil
  defp label_filter("compile-connected"), do: :compile_connected
  defp label_filter(other), do: Mix.raise("Unknown --label #{other} in mix xref graph")

  defp file_references(:compile_connected, _opts) do
    Mix.raise("Cannot use --only-direct with --label=compile-connected")
  end

  defp file_references(filter, opts) do
    module_sources =
      for manifest_path <- manifests(opts),
          {manifest_modules, manifest_sources} = read_manifest(manifest_path),
          module(module: module, sources: sources) <- manifest_modules,
          source <- sources,
          source = Enum.find(manifest_sources, &match?(source(source: ^source), &1)),
          do: {module, source}

    all_modules = MapSet.new(module_sources, &elem(&1, 0))

    Map.new(module_sources, fn {current, source} ->
      source(
        runtime_references: runtime,
        export_references: exports,
        compile_references: compile,
        source: file
      ) = source

      compile_references =
        modules_to_nodes(compile, :compile, current, source, module_sources, all_modules, filter)

      export_references =
        modules_to_nodes(exports, :export, current, source, module_sources, all_modules, filter)

      runtime_references =
        modules_to_nodes(runtime, nil, current, source, module_sources, all_modules, filter)

      references =
        runtime_references
        |> Map.merge(export_references)
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

  @humanize_option %{
    group: "Group files",
    source: "Sources",
    sink: "Sinks",
    exclude: "Excluded files"
  }

  defp get_files(what, opts, file_references, aliases) do
    files =
      for file <- Keyword.get_values(opts, what) do
        Map.get(aliases, file, file)
      end

    check_files(files, file_references, what)
  end

  defp check_files(files, file_references, what) do
    case files -- Map.keys(file_references) do
      [_ | _] = missing ->
        Mix.raise("#{@humanize_option[what]} could not be found: #{Enum.join(missing, ", ")}")

      _ ->
        :ok
    end

    if files == [], do: nil, else: files
  end

  defp write_graph(file_references, filter, opts) do
    {file_references, aliases} = merge_groups(file_references, Keyword.get_values(opts, :group))

    file_references =
      exclude(file_references, get_files(:exclude, opts, file_references, aliases))

    sources = get_files(:source, opts, file_references, aliases)
    sinks = get_files(:sink, opts, file_references, aliases)

    file_references =
      cond do
        sinks -> sink_tree(file_references, sinks)
        sources -> source_tree(file_references, sources)
        true -> file_references
      end

    # Filter according to non direct label
    file_references = filter(file_references, filter)

    # If a label is given, remove empty root nodes
    file_references =
      if opts[:label] do
        for {_, [_ | _]} = pair <- file_references, into: %{}, do: pair
      else
        file_references
      end

    roots =
      if sources do
        Enum.map(sources, &{&1, nil})
      else
        file_references
        |> Map.drop(sinks || [])
        |> Enum.map(&{elem(&1, 0), nil})
      end

    callback = fn {file, type} ->
      children = if opts[:only_nodes], do: [], else: Map.get(file_references, file, [])
      type = type && "(#{type})"
      {{file, type}, Enum.sort(children)}
    end

    {found, count} =
      case opts[:format] do
        "dot" ->
          Mix.Utils.write_dot_graph!(
            "xref_graph.dot",
            "xref graph",
            Enum.sort(roots),
            callback,
            opts
          )

          """
          Generated "xref_graph.dot" in the current directory. To generate a PNG:

             dot -Tpng xref_graph.dot -o xref_graph.png

          For more options see http://www.graphviz.org/.
          """
          |> String.trim_trailing()
          |> Mix.shell().info()

          {:references, count_references(file_references)}

        "stats" ->
          print_stats(file_references, opts)
          {:stats, 0}

        "cycles" ->
          {:cycles, print_cycles(file_references, opts)}

        other when other in [nil, "plain", "pretty"] ->
          Mix.Utils.print_tree(Enum.sort(roots), callback, opts)

          {:references, count_references(file_references)}

        other ->
          Mix.raise("Unknown --format #{other} in mix xref graph")
      end

    check_failure(found, count, opts[:fail_above])
  end

  defp count_references(file_references) do
    Enum.reduce(file_references, 0, fn {_, refs}, total -> total + length(refs) end)
  end

  defp filter_fn(file_references, :compile_connected),
    do: fn {key, type} ->
      type == :compile and match?([_ | _], file_references[key] || [])
    end

  defp filter_fn(_file_references, filter),
    do: fn {_key, type} -> type == filter end

  defp filter(file_references, :all), do: file_references

  defp filter(file_references, filter) do
    filter_fn = filter_fn(file_references, filter)

    for {key, children} <- file_references,
        into: %{},
        do: {key, Enum.filter(children, filter_fn)}
  end

  defp source_tree(file_references, keys) do
    keys
    |> Enum.reduce({%{}, %{}}, fn key, {acc, seen} ->
      source_tree(file_references, key, acc, seen)
    end)
    |> elem(0)
  end

  defp source_tree(file_references, key, acc, seen) do
    nodes = file_references[key]

    if is_nil(nodes) or Map.has_key?(seen, key) do
      {acc, seen}
    else
      acc = Map.put(acc, key, nodes)
      seen = Map.put(seen, key, true)

      Enum.reduce(nodes, {acc, seen}, fn {key, _type}, {acc, seen} ->
        source_tree(file_references, key, acc, seen)
      end)
    end
  end

  defp sink_tree(file_references, keys) do
    file_references
    |> invert_references()
    |> source_tree(keys)
    |> invert_references()
  end

  defp invert_references(file_references) do
    Enum.reduce(file_references, %{}, fn {file, references}, acc ->
      Enum.reduce(references, acc, fn {file_reference, type}, acc ->
        Map.update(acc, file_reference, [{file, type}], &[{file, type} | &1])
      end)
    end)
  end

  defp print_stats(references, opts) do
    with_digraph(references, fn graph ->
      shell = Mix.shell()

      counters =
        Enum.reduce(references, %{compile: 0, export: 0, nil: 0}, fn {_, deps}, acc ->
          Enum.reduce(deps, acc, fn {_, value}, acc ->
            Map.update!(acc, value, &(&1 + 1))
          end)
        end)

      shell.info("Tracked files: #{map_size(references)} (nodes)")
      shell.info("Compile dependencies: #{counters.compile} (edges)")
      shell.info("Exports dependencies: #{counters.export} (edges)")
      shell.info("Runtime dependencies: #{counters.nil} (edges)")
      shell.info("Cycles: #{length(cycles(graph, opts))}")

      outgoing =
        references
        |> Enum.map(fn {file, _} -> {:digraph.out_degree(graph, file), file} end)
        |> Enum.sort(:desc)
        |> Enum.take(10)

      shell.info("\nTop #{length(outgoing)} files with most outgoing dependencies:")
      for {count, file} <- outgoing, do: shell.info("  * #{file} (#{count})")

      incoming =
        references
        |> Enum.map(fn {file, _} -> {:digraph.in_degree(graph, file), file} end)
        |> Enum.sort(:desc)
        |> Enum.take(10)

      shell.info("\nTop #{length(incoming)} files with most incoming dependencies:")
      for {count, file} <- incoming, do: shell.info("  * #{file} (#{count})")
    end)
  end

  defp with_digraph(references, callback) do
    graph = :digraph.new()

    try do
      for {file, _} <- references do
        :digraph.add_vertex(graph, file)
      end

      for {file, deps} <- references, {dep, label} <- deps do
        :digraph.add_edge(graph, file, dep, label)
      end

      callback.(graph)
    after
      :digraph.delete(graph)
    end
  end

  defp cycles(graph, opts) do
    cycles =
      graph
      |> :digraph_utils.cyclic_strong_components()
      |> Enum.reduce([], &inner_cycles(graph, &1, &2))
      |> Enum.map(&{length(&1), &1})

    if min = opts[:min_cycle_size], do: Enum.filter(cycles, &(elem(&1, 0) > min)), else: cycles
  end

  defp inner_cycles(_graph, [], acc), do: acc

  defp inner_cycles(graph, [v | vertices], acc) do
    cycle = :digraph.get_cycle(graph, v)
    inner_cycles(graph, vertices -- cycle, [cycle | acc])
  end

  defp print_cycles(references, opts) do
    with_digraph(references, fn graph ->
      shell = Mix.shell()

      case graph |> cycles(opts) |> Enum.sort(:desc) do
        [] ->
          shell.info("No cycles found")
          0

        cycles ->
          shell.info("#{length(cycles)} cycles found. Showing them in decreasing size:\n")

          for {length, cycle} <- cycles do
            shell.info("Cycle of length #{length}:\n")

            for node <- cycle do
              shell.info("    " <> node)
            end

            shell.info("")
          end

          length(cycles)
      end
    end)
  end

  ## Helpers

  defp sources(opts) do
    for manifest <- manifests(opts),
        source() = source <- read_manifest(manifest) |> elem(1),
        do: source
  end

  defp apps(opts) do
    siblings =
      if opts[:include_siblings] do
        for %{scm: Mix.SCM.Path, opts: opts, app: app} <- Mix.Dep.cached(),
            opts[:in_umbrella],
            do: app
      else
        []
      end

    [Mix.Project.config()[:app] | siblings]
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

  defp check_failure(found, count, max_count)
       when not is_nil(max_count) and count > max_count do
    Mix.raise("Too many #{found} (found: #{count}, permitted: #{max_count})")
  end

  defp check_failure(_, _, _) do
    :ok
  end
end
