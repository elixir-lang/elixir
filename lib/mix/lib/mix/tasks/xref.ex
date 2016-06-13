defmodule Mix.Tasks.Xref do
  use Mix.Task

  alias Mix.Tasks.Compile.Elixir, as: E
  import Mix.Compilers.Elixir, only: [read_manifest: 2, source: 1, source: 2, module: 1]

  @shortdoc "Performs cross reference checks"
  @recursive true

  @moduledoc """
  Performs cross reference checks between modules.

  ## Xref modes

  The following commands and options are available:

    * `warnings` - prints warnings for violated cross reference checks

    * `unreachable` - prints all unreachable "file:line: module.function/arity" entries

    * `callers CALLEE` - prints all references of `CALLEE`, which can be one of: `Module`,
      `Module.function`, or `Module.function/arity`

    * `graph` - prints the file reference graph. By default, an edge from `A` to `B` indicates
      that `A` depends on `B`

      * `--exclude` - paths to exclude

      * `--source` - display only modules for which there is a path from the
        given source module

      * `--sink` - display only modules for which there is a path to the
        given sink module. In this mode, an edge from `A` to `B` indicates `B` depends on `A`

      * `--format` - can be set to one of:

        * `pretty` - use Unicode codepoints for formatting the graph. This is the default except on
          Windows

        * `plain` - do not use Unicode codepoints for formatting the graph. This is the default on
          Windows

        * `dot` - produces a DOT graph description in `xref_graph.dot` in the
          current directory. Warning: this will override any previously generated file

  ## Options for all commands

    * `--no-compile` - do not compile even if files require compilation

    * `--no-deps-check` - do not check dependencies

    * `--no-archives-check` - do not check archives

    * `--no-elixir-version-check` - do not check the Elixir version from mix.exs

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
        Mix.raise "xref doesn't support this command, see mix help xref for more information"
    end
  end

  ## Modes

  defp warnings() do
    if unreachable(&print_warnings/2) == [] do
      :ok
    else
      :error
    end
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
        {{func, arity}, lines} <- func_arity_lines,
        warning = unreachable_mfa(module, func, arity, lines, excludes),
        do: warning
  end

  defp unreachable_mfa(module, func, arity, lines, excludes) do
    cond do
      excluded?(module, func, arity, excludes) ->
        nil
      skip?(module, func, arity) ->
        nil
      not Code.ensure_loaded?(module) ->
        {Enum.sort(lines), :unknown_module, module, func, arity}
      not function_exported?(module, func, arity) ->
        {Enum.sort(lines), :unknown_function, module, func, arity}
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

  defp format_entry(file, {lines, _, module, function, arity}) do
    for line <- lines do
      [Exception.format_file_line(file, line), ?\s, Exception.format_mfa(module, function, arity), ?\n]
    end
  end

  ## Print warnings

  defp print_warnings(file, entries) do
    prefix = IO.ANSI.format([:yellow, "warning: "])
    entries
    |> Enum.sort()
    |> Enum.each(&IO.write(:stderr, [prefix, format_warning(file, &1), ?\n]))
  end

  defp format_warning(file, {lines, :unknown_function, module, function, arity}) do
    ["function ", Exception.format_mfa(module, function, arity),
     " is undefined or private\n" | format_file_lines(file, lines)]
  end

  defp format_warning(file, {lines, :unknown_module, module, function, arity}) do
    ["function ", Exception.format_mfa(module, function, arity),
     " is undefined (module #{inspect module} is not available)\n" | format_file_lines(file, lines)]
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
    dispatches = Stream.concat(runtime_dispatches, compile_dispatches)

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
    mfa_list =
      case Code.string_to_quoted(callee) do
        {:ok, quoted_callee} -> quoted_to_mfa_list(quoted_callee)
        {:error, _} -> raise_invalid_callee(callee)
      end

    mfa_list_length = length(mfa_list)

    fn {module, function, arity} ->
      mfa_list == Enum.take([module, function, arity], mfa_list_length)
    end
  end

  defp quoted_to_mfa_list(quoted) do
    quoted
    |> do_quoted_to_mfa_list()
    |> Enum.reverse()
  end

  defp do_quoted_to_mfa_list({:__aliases__, _, aliases}) do
    [Module.concat(aliases)]
  end

  defp do_quoted_to_mfa_list({{:., _, [module, func]}, _, []}) when is_atom(func) do
    [func | do_quoted_to_mfa_list(module)]
  end

  defp do_quoted_to_mfa_list({:/, _, [dispatch, arity]}) when is_integer(arity) do
    [arity | do_quoted_to_mfa_list(dispatch)]
  end

  defp do_quoted_to_mfa_list(other) do
    other
    |> Macro.to_string()
    |> raise_invalid_callee()
  end

  defp raise_invalid_callee(callee) do
    message =
      "xref callers CALLEE expects Module, Module.function, or Module.function/arity, got: " <>
      callee

    Mix.raise message
  end

  ## Graph helpers

  defp excluded(opts) do
    Keyword.get_values(opts, :exclude)
    |> Enum.map(&{&1, nil})
  end

  defp file_references() do
    module_sources =
      for manifest <- E.manifests(),
          manifest_data = read_manifest(manifest, ""),
          module(module: module, source: source) <- manifest_data,
          source = Enum.find(manifest_data, &match?(source(source: ^source), &1)),
          do: {module, source},
          into: %{}

    all_modules = MapSet.new(module_sources, &elem(&1, 0))

    Map.new module_sources, fn {module, source} ->
      source(runtime_references: runtime, compile_references: compile, source: file) = source
      references =
        runtime ++ compile
        |> MapSet.new()
        |> MapSet.intersection(all_modules)
        |> MapSet.delete(module)
        |> Enum.map(&{source(module_sources[&1], :source), nil})

      {{file, nil}, references}
    end
  end

  defp write_graph(file_references, excluded, opts) do
    app_label = "#{Mix.Project.config[:app]} application"
    {root, file_references} =
      case {opts[:source], opts[:sink]} do
        {nil, nil} ->
          {{app_label, nil}, file_references}

        {source, nil} ->
          if file_references[{source, nil}] do
            {{source, nil}, file_references}
          else
            Mix.raise "Source could not be found: #{source}"
          end

        {nil, sink} ->
          if file_references[{sink, nil}] do
            {{sink, nil}, file_references |> invert_references()}
          else
            Mix.raise "Sink could not be found: #{sink}"
          end

        {_, _} ->
          Mix.raise "mix xref graph expects only one of --source and --sink"
      end

    callback =
      fn
        {^app_label, nil} ->
          {{app_label, nil}, Enum.map(file_references, &elem(&1, 0)) -- excluded}

        {file, nil} ->
          children = Map.get(file_references, {file, nil}, [])
          {{file, nil}, children -- excluded}
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

  defp invert_references(file_references) do
    Enum.reduce file_references, %{}, fn {file, references}, acc ->
      Enum.reduce references, acc, fn reference, acc ->
        Map.update(acc, reference, [file], &[file | &1])
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
