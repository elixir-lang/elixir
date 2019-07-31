defmodule Mix.Compilers.Elixir do
  @moduledoc false

  @manifest_vsn 4

  import Record

  defrecord :module, [:module, :kind, :sources, :struct]

  defrecord :source,
    source: nil,
    size: 0,
    compile_references: [],
    struct_references: [],
    runtime_references: [],
    external: [],
    warnings: [],
    modules: []

  @doc """
  Compiles stale Elixir files.

  It expects a `manifest` file, the source directories, the destination
  directory, an option to know if compilation is being forced or not, and a
  list of any additional compiler options.

  The `manifest` is written down with information including dependencies
  between modules, which helps it recompile only the modules that
  have changed at runtime.
  """
  def compile(manifest, srcs, dest, exts, force, opts) do
    # We fetch the time from before we read files so any future
    # change to files are still picked up by the compiler. This
    # timestamp is used when writing BEAM files and the manifest.
    timestamp = System.os_time(:second)
    all_paths = MapSet.new(Mix.Utils.extract_files(srcs, exts))

    {all_modules, all_sources} = parse_manifest(manifest, dest)
    modified = Mix.Utils.last_modified(manifest)
    stale_local_deps = stale_local_deps(manifest, modified)
    prev_paths = for source(source: source) <- all_sources, into: MapSet.new(), do: source

    removed =
      prev_paths
      |> MapSet.difference(all_paths)
      |> MapSet.to_list()

    {modules, structs, changed, sources_stats} =
      if force do
        # A config, path dependency or manifest has changed, let's just compile everything
        all_paths = MapSet.to_list(all_paths)

        for module(module: module) <- all_modules,
            do: remove_and_purge(beam_path(dest, module), module)

        sources_stats =
          for path <- all_paths,
              into: %{},
              do: {path, Mix.Utils.last_modified_and_size(path)}

        # Now that we have deleted all beams, remember to remove the manifest.
        # This is important in case mix compile --force fails, otherwise we
        # would have an outdated manifest.
        File.rm(manifest)

        {[], %{}, all_paths, sources_stats}
      else
        # Otherwise let's start with the new sources
        new_paths =
          all_paths
          |> MapSet.difference(prev_paths)
          |> MapSet.to_list()

        sources_stats =
          for path <- new_paths,
              into: mtimes_and_sizes(all_sources),
              do: {path, Mix.Utils.last_modified_and_size(path)}

        # Plus the sources that have changed in disk
        changed =
          for source(source: source, external: external, size: size) <- all_sources,
              {last_mtime, last_size} = Map.fetch!(sources_stats, source),
              times = Enum.map(external, &(sources_stats |> Map.fetch!(&1) |> elem(0))),
              size != last_size or Mix.Utils.stale?([last_mtime | times], [modified]),
              do: source

        changed = new_paths ++ changed

        {modules, structs, changed} =
          update_stale_entries(
            all_modules,
            all_sources,
            removed ++ changed,
            stale_local_deps,
            stale_local_deps,
            dest
          )

        {modules, structs, changed, sources_stats}
      end

    stale = changed -- removed

    sources =
      removed
      |> Enum.reduce(all_sources, &List.keydelete(&2, &1, source(:source)))
      |> update_stale_sources(stale, sources_stats)

    if opts[:all_warnings], do: show_warnings(sources)

    cond do
      stale != [] ->
        compile_manifest(manifest, exts, modules, structs, sources, stale, dest, timestamp, opts)

      # We need to return ok if stale_local_deps changed
      # because we want that to propagate to compile.protocols
      removed != [] or stale_local_deps != %{} ->
        write_manifest(manifest, modules, sources, timestamp)
        {:ok, warning_diagnostics(sources)}

      true ->
        {:noop, warning_diagnostics(sources)}
    end
  end

  defp mtimes_and_sizes(sources) do
    Enum.reduce(sources, %{}, fn source(source: source, external: external), map ->
      Enum.reduce([source | external], map, fn file, map ->
        Map.put_new_lazy(map, file, fn -> Mix.Utils.last_modified_and_size(file) end)
      end)
    end)
  end

  @doc """
  Removes compiled files for the given `manifest`.
  """
  def clean(manifest, compile_path) do
    Enum.each(read_manifest(manifest), fn
      module(module: module) -> File.rm(beam_path(compile_path, module))
      _ -> :ok
    end)
  end

  @doc """
  Returns protocols and implementations for the given `manifest`.
  """
  def protocols_and_impls(manifest, compile_path) do
    for module(module: module, kind: kind) <- read_manifest(manifest),
        match?(:protocol, kind) or match?({:impl, _}, kind),
        do: {module, kind, beam_path(compile_path, module)}
  end

  @doc """
  Reads the manifest.
  """
  def read_manifest(manifest) do
    try do
      manifest |> File.read!() |> :erlang.binary_to_term()
    rescue
      _ -> []
    else
      [@manifest_vsn | data] -> data
      _ -> []
    end
  end

  defp compile_manifest(manifest, exts, modules, structs, sources, stale, dest, timestamp, opts) do
    Mix.Utils.compiling_n(length(stale), hd(exts))
    Mix.Project.ensure_structure()
    true = Code.prepend_path(dest)
    set_compiler_opts(opts)
    cwd = File.cwd!()

    # Stores state for keeping track which files were compiled
    # and the dependencies between them.
    put_compiler_info({modules, structs, sources, modules, %{}})
    long_compilation_threshold = opts[:long_compilation_threshold] || 15
    verbose = opts[:verbose] || false

    compile_opts = [
      each_cycle: fn -> each_cycle(dest) end,
      each_file: &each_file(&1, &2, cwd, verbose),
      each_module: &each_module(&1, &2, &3, cwd),
      each_long_compilation: &each_long_compilation(&1, cwd, long_compilation_threshold),
      long_compilation_threshold: long_compilation_threshold,
      beam_timestamp: timestamp
    ]

    try do
      Kernel.ParallelCompiler.compile_to_path(stale, dest, compile_opts)
    else
      {:ok, _, warnings} ->
        {modules, _structs, sources, _pending_modules, _pending_structs} = get_compiler_info()
        sources = apply_warnings(sources, warnings)
        write_manifest(manifest, modules, sources, timestamp)
        {:ok, warning_diagnostics(sources)}

      {:error, errors, warnings} ->
        errors = Enum.map(errors, &diagnostic(&1, :error))
        {_, _, sources, _, _} = get_compiler_info()
        warnings = Enum.map(warnings, &diagnostic(&1, :warning)) ++ warning_diagnostics(sources)
        {:error, warnings ++ errors}
    after
      Code.purge_compiler_modules()
      delete_compiler_info()
    end
  end

  defp get_compiler_info(), do: Process.get(__MODULE__)
  defp put_compiler_info(value), do: Process.put(__MODULE__, value)
  defp delete_compiler_info(), do: Process.delete(__MODULE__)

  defp set_compiler_opts(opts) do
    opts
    |> Keyword.take(Code.available_compiler_options())
    |> Code.compiler_options()
  end

  defp each_cycle(compile_path) do
    {modules, _structs, sources, pending_modules, pending_structs} = get_compiler_info()

    {pending_modules, structs, changed} =
      update_stale_entries(pending_modules, sources, [], %{}, pending_structs, compile_path)

    if changed == [] do
      {:runtime, dependent_runtime_modules(sources, modules, pending_modules)}
    else
      modules =
        for module(sources: source_files) = module <- modules do
          module(module, sources: source_files -- changed)
        end

      sources = update_stale_sources(sources, changed)
      put_compiler_info({modules, structs, sources, pending_modules, %{}})
      {:compile, changed}
    end
  end

  defp dependent_runtime_modules(sources, all_modules, pending_modules) do
    changed_modules =
      for module <- all_modules,
          module not in pending_modules,
          into: %{},
          do: {module, true}

    fixpoint_runtime_modules(sources, changed_modules, %{}, pending_modules)
  end

  defp fixpoint_runtime_modules(sources, changed, dependent, not_dependent) do
    {new_dependent, not_dependent} =
      Enum.reduce(not_dependent, {dependent, []}, fn module, {new_dependent, not_dependent} ->
        depending? =
          Enum.any?(module(module, :sources), fn file ->
            source(runtime_references: runtime_refs) =
              List.keyfind(sources, file, source(:source))

            Enum.any?(changed, fn {module(module: module), true} -> module in runtime_refs end)
          end)

        if depending? do
          {Map.put(new_dependent, module, true), not_dependent}
        else
          {new_dependent, [module | not_dependent]}
        end
      end)

    if map_size(dependent) != map_size(new_dependent) do
      fixpoint_runtime_modules(sources, new_dependent, new_dependent, not_dependent)
    else
      Enum.map(new_dependent, fn {module(module: module), true} -> module end)
    end
  end

  defp each_module(file, module, _binary, cwd) do
    {modules, structs, sources, pending_modules, pending_structs} = get_compiler_info()
    kind = detect_kind(module)
    file = Path.relative_to(file, cwd)
    external = get_external_resources(module, cwd)

    struct =
      case Module.get_attribute(module, :struct) do
        %{} = struct -> {struct, List.wrap(Module.get_attribute(module, :enforce_keys))}
        _ -> nil
      end

    old_struct = Map.get(structs, module)

    pending_structs =
      if old_struct && struct != old_struct do
        Map.put(pending_structs, module, true)
      else
        pending_structs
      end

    {module_sources, existing_module?} =
      case List.keyfind(modules, module, module(:module)) do
        module(sources: old_sources) -> {[file | List.delete(old_sources, file)], true}
        nil -> {[file], false}
      end

    {source, sources} =
      List.keytake(sources, file, source(:source)) ||
        Mix.raise(
          "Could not find source for #{inspect(file)}. Make sure the :elixirc_paths configuration " <>
            "is a list of relative paths to the current project or absolute paths to external directories"
        )

    source =
      source(
        source,
        external: external ++ source(source, :external),
        modules: [module | source(source, :modules)]
      )

    module =
      module(
        module: module,
        kind: kind,
        sources: module_sources,
        struct: struct
      )

    modules = prepend_or_merge(modules, module, module(:module), module, existing_module?)
    put_compiler_info({modules, structs, [source | sources], pending_modules, pending_structs})
    :ok
  end

  defp prepend_or_merge(collection, key, pos, value, true) do
    List.keystore(collection, key, pos, value)
  end

  defp prepend_or_merge(collection, _key, _pos, value, false) do
    [value | collection]
  end

  defp detect_kind(module) do
    protocol_metadata = Module.get_attribute(module, :protocol_impl)

    cond do
      is_list(protocol_metadata) and protocol_metadata[:protocol] ->
        {:impl, protocol_metadata[:protocol]}

      is_list(Module.get_attribute(module, :protocol)) ->
        :protocol

      true ->
        :module
    end
  end

  defp get_external_resources(module, cwd) do
    for file <- Module.get_attribute(module, :external_resource), do: Path.relative_to(file, cwd)
  end

  defp each_file(file, lexical, cwd, verbose) do
    file = Path.relative_to(file, cwd)

    if verbose do
      Mix.shell().info("Compiled #{file}")
    end

    {modules, structs, sources, pending_modules, pending_structs} = get_compiler_info()
    {source, sources} = List.keytake(sources, file, source(:source))

    {compile_references, struct_references, runtime_references} =
      Kernel.LexicalTracker.remote_references(lexical)

    compile_references =
      Enum.reject(compile_references, &match?("elixir_" <> _, Atom.to_string(&1)))

    source(modules: source_modules) = source
    compile_references = compile_references -- source_modules
    struct_references = struct_references -- source_modules
    runtime_references = runtime_references -- source_modules

    source =
      source(
        source,
        compile_references: compile_references,
        struct_references: struct_references,
        runtime_references: runtime_references
      )

    put_compiler_info({modules, structs, [source | sources], pending_modules, pending_structs})
    :ok
  end

  defp each_long_compilation(file, cwd, threshold) do
    Mix.shell().info(
      "Compiling #{Path.relative_to(file, cwd)} (it's taking more than #{threshold}s)"
    )
  end

  ## Resolution

  # Store empty sources for the changed ones as the compiler appends data
  defp update_stale_sources(sources, changed) do
    Enum.reduce(changed, sources, fn file, acc ->
      {source(size: size), acc} = List.keytake(acc, file, source(:source))
      [source(source: file, size: size) | acc]
    end)
  end

  defp update_stale_sources(sources, stale, sources_stats) do
    Enum.reduce(stale, sources, fn file, acc ->
      %{^file => {_, size}} = sources_stats
      List.keystore(acc, file, source(:source), source(source: file, size: size))
    end)
  end

  # This function receives the manifest entries and some source
  # files that have changed. It then, recursively, figures out
  # all the files that changed (via the module dependencies) and
  # return the non-changed entries and the removed sources.
  defp update_stale_entries(modules, _sources, [], stale_files, stale_structs, _compile_path)
       when stale_files == %{} and stale_structs == %{} do
    {modules, %{}, []}
  end

  defp update_stale_entries(modules, sources, changed, stale_files, stale_structs, compile_path) do
    changed = Enum.into(changed, %{}, &{&1, true})
    reducer = &remove_stale_entry(&1, &2, sources, stale_structs, compile_path)
    remove_stale_entries(modules, %{}, changed, stale_files, reducer)
  end

  defp remove_stale_entries(modules, structs, old_changed, old_stale, reducer) do
    {pending_modules, structs, new_changed, new_stale} =
      Enum.reduce(modules, {[], structs, old_changed, old_stale}, reducer)

    if map_size(new_stale) > map_size(old_stale) or map_size(new_changed) > map_size(old_changed) do
      remove_stale_entries(pending_modules, structs, new_changed, new_stale, reducer)
    else
      {pending_modules, structs, Map.keys(new_changed)}
    end
  end

  defp remove_stale_entry(entry, acc, sources, stale_structs, compile_path) do
    module(module: module, sources: source_files, struct: struct) = entry
    {rest, structs, changed, stale} = acc

    {compile_references, struct_references, runtime_references} =
      Enum.reduce(source_files, {[], [], []}, fn file, {compile_acc, struct_acc, runtime_acc} ->
        source(
          compile_references: compile_refs,
          struct_references: struct_refs,
          runtime_references: runtime_refs
        ) = List.keyfind(sources, file, source(:source))

        {compile_acc ++ compile_refs, struct_acc ++ struct_refs, runtime_acc ++ runtime_refs}
      end)

    cond do
      # If I changed in disk or have a compile time reference to
      # something stale or have a reference to an old struct,
      # I need to be recompiled.
      has_any_key?(changed, source_files) or has_any_key?(stale, compile_references) or
          has_any_key?(stale_structs, struct_references) ->
        remove_and_purge(beam_path(compile_path, module), module)
        changed = Enum.reduce(source_files, changed, &Map.put(&2, &1, true))
        {rest, Map.put(structs, module, struct), changed, Map.put(stale, module, true)}

      # If I have a runtime references to something stale,
      # I am stale too.
      has_any_key?(stale, runtime_references) ->
        {[entry | rest], structs, changed, Map.put(stale, module, true)}

      # Otherwise, we don't store it anywhere
      true ->
        {[entry | rest], structs, changed, stale}
    end
  end

  defp has_any_key?(map, enumerable) do
    Enum.any?(enumerable, &Map.has_key?(map, &1))
  end

  defp stale_local_deps(manifest, modified) do
    base = Path.basename(manifest)

    for %{scm: scm, opts: opts} = dep <- Mix.Dep.cached(),
        not scm.fetchable?,
        Mix.Utils.last_modified(Path.join([opts[:build], ".mix", base])) > modified,
        path <- Mix.Dep.load_paths(dep),
        beam <- Path.wildcard(Path.join(path, "*.beam")),
        Mix.Utils.last_modified(beam) > modified,
        do: {beam |> Path.basename() |> Path.rootname() |> String.to_atom(), true},
        into: %{}
  end

  defp remove_and_purge(beam, module) do
    _ = File.rm(beam)
    _ = :code.purge(module)
    _ = :code.delete(module)
  end

  defp show_warnings(sources) do
    for source(source: source, warnings: warnings) <- sources do
      file = Path.absname(source)

      for {line, message} <- warnings do
        :elixir_errors.erl_warn(line, file, message)
      end
    end
  end

  defp apply_warnings(sources, warnings) do
    warnings = Enum.group_by(warnings, &elem(&1, 0), &{elem(&1, 1), elem(&1, 2)})

    for source(source: source_path, warnings: source_warnings) = s <- sources do
      source(s, warnings: Map.get(warnings, Path.absname(source_path), source_warnings))
    end
  end

  defp warning_diagnostics(sources) do
    for source(source: source, warnings: warnings) <- sources,
        {line, message} <- warnings,
        do: diagnostic({Path.absname(source), line, message}, :warning)
  end

  defp diagnostic({file, line, message}, severity) do
    %Mix.Task.Compiler.Diagnostic{
      file: file,
      position: line,
      message: message,
      severity: severity,
      compiler_name: "Elixir"
    }
  end

  ## Manifest handling

  # Similar to read_manifest, but supports data migration.
  defp parse_manifest(manifest, compile_path) do
    try do
      manifest |> File.read!() |> :erlang.binary_to_term()
    rescue
      _ ->
        {[], []}
    else
      [@manifest_vsn | data] ->
        split_manifest(data)

      [v | data] when is_integer(v) ->
        for module <- data, elem(module, 0) == :module do
          module = elem(module, 1)
          File.rm(beam_path(compile_path, module))
          :code.purge(module)
          :code.delete(module)
        end

        {[], []}

      _ ->
        {[], []}
    end
  end

  defp split_manifest(data) do
    Enum.reduce(data, {[], []}, fn
      module() = module, {modules, sources} ->
        {[module | modules], sources}

      source() = source, {modules, sources} ->
        {modules, [source | sources]}
    end)
  end

  defp write_manifest(manifest, [], [], _timestamp) do
    File.rm(manifest)
    :ok
  end

  defp write_manifest(manifest, modules, sources, timestamp) do
    File.mkdir_p!(Path.dirname(manifest))

    manifest_data =
      [@manifest_vsn | modules ++ sources]
      |> :erlang.term_to_binary([:compressed])

    File.write!(manifest, manifest_data)
    File.touch!(manifest, timestamp)

    # Since Elixir is a dependency itself, we need to touch the lock
    # so the current Elixir version, used to compile the files above,
    # is properly stored.
    Mix.Dep.ElixirSCM.update()
  end

  defp beam_path(compile_path, module) do
    Path.join(compile_path, Atom.to_string(module) <> ".beam")
  end
end
