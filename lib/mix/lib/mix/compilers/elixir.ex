defmodule Mix.Compilers.Elixir do
  @moduledoc false

  @manifest_vsn 1

  import Record

  defrecord :module, [:module, :kind, :sources, :beam, :binary, :struct]

  defrecord :source,
    source: nil,
    size: 0,
    compile_references: [],
    struct_references: [],
    runtime_references: [],
    compile_dispatches: [],
    runtime_dispatches: [],
    external: [],
    warnings: []

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

        for module(module: module, beam: beam) <- all_modules,
            do: remove_and_purge(beam, module)

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
            stale_local_deps
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
        write_manifest(manifest, modules, sources, dest, timestamp)
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
    Enum.each(read_manifest(manifest, compile_path), fn
      module(beam: beam) -> File.rm(beam)
      _ -> :ok
    end)
  end

  @doc """
  Returns protocols and implementations for the given `manifest`.
  """
  def protocols_and_impls(manifest, compile_path) do
    for module(beam: beam, module: module, kind: kind) <- read_manifest(manifest, compile_path),
        match?(:protocol, kind) or match?({:impl, _}, kind),
        do: {module, kind, beam}
  end

  @doc """
  Reads the manifest.
  """
  def read_manifest(manifest, compile_path) do
    try do
      manifest |> File.read!() |> :erlang.binary_to_term()
    rescue
      _ -> []
    else
      [@manifest_vsn | data] -> expand_beam_paths(data, compile_path)
      _ -> []
    end
  end

  defp compile_manifest(manifest, exts, modules, structs, sources, stale, dest, timestamp, opts) do
    Mix.Utils.compiling_n(length(stale), hd(exts))
    Mix.Project.ensure_structure()
    true = Code.prepend_path(dest)
    set_compiler_opts(opts)
    cwd = File.cwd!()

    extra =
      if opts[:verbose] do
        [each_file: &each_file/1]
      else
        []
      end

    # Stores state for keeping track which files were compiled
    # and the dependencies between them.
    put_compiler_info({modules, structs, sources, modules, %{}})
    long_compilation_threshold = opts[:long_compilation_threshold] || 15

    compile_opts = [
      each_cycle: &each_cycle/0,
      each_module: &each_module(cwd, &1, &2, &3),
      each_long_compilation: &each_long_compilation(&1, long_compilation_threshold),
      long_compilation_threshold: long_compilation_threshold,
      dest: dest
    ]

    try do
      Kernel.ParallelCompiler.compile(stale, compile_opts ++ extra)
    else
      {:ok, _, warnings} ->
        {modules, _structs, sources, _pending_modules, _pending_structs} = get_compiler_info()
        sources = apply_warnings(sources, warnings)
        write_manifest(manifest, modules, sources, dest, timestamp)
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

  defp each_cycle() do
    {modules, _structs, sources, pending_modules, pending_structs} = get_compiler_info()

    {pending_modules, structs, changed} =
      update_stale_entries(pending_modules, sources, [], %{}, pending_structs)

    if changed == [] do
      []
    else
      modules =
        for module(sources: source_files) = module <- modules do
          module(module, sources: source_files -- changed)
        end

      sources = update_stale_sources(sources, changed)
      put_compiler_info({modules, structs, sources, pending_modules, %{}})
      changed
    end
  end

  defp each_module(cwd, source, module, binary) do
    {compile_references, struct_references, runtime_references} =
      Kernel.LexicalTracker.remote_references(module)

    {elixir_references, compile_references} =
      Enum.split_with(compile_references, &match?("elixir_" <> _, Atom.to_string(&1)))

    compile_references = List.delete(compile_references, module)
    struct_references = List.delete(struct_references, module)
    runtime_references = List.delete(runtime_references, module)
    {compile_dispatches, runtime_dispatches} = Kernel.LexicalTracker.remote_dispatches(module)

    compile_dispatches =
      compile_dispatches
      |> Map.drop(elixir_references)
      |> Enum.to_list()

    runtime_dispatches =
      runtime_dispatches
      |> Enum.to_list()

    struct =
      case Module.get_attribute(module, :struct) do
        %{} = struct -> {struct, List.wrap(Module.get_attribute(module, :enforce_keys))}
        _ -> nil
      end

    kind = detect_kind(module)
    source = Path.relative_to(source, cwd)
    external = get_external_resources(module, cwd)

    {modules, structs, sources, pending_modules, pending_structs} = get_compiler_info()

    {module_sources, existing_module?} =
      case List.keyfind(modules, module, module(:module)) do
        module(sources: old_sources) -> {[source | List.delete(old_sources, source)], true}
        nil -> {[source], false}
      end

    # They are calculated when writing the manifest
    new_module =
      module(
        module: module,
        kind: kind,
        sources: module_sources,
        beam: nil,
        struct: struct,
        binary: binary
      )

    source(size: size, external: old_external) = List.keyfind(sources, source, source(:source))

    new_source =
      source(
        source: source,
        size: size,
        compile_references: compile_references,
        struct_references: struct_references,
        runtime_references: runtime_references,
        compile_dispatches: compile_dispatches,
        runtime_dispatches: runtime_dispatches,
        external: external ++ old_external
      )

    old_struct = Map.get(structs, module)

    pending_structs =
      if old_struct && struct != old_struct do
        Map.put(pending_structs, module, true)
      else
        pending_structs
      end

    modules = prepend_or_merge(modules, module, module(:module), new_module, existing_module?)
    sources = prepend_or_merge(sources, source, source(:source), new_source, true)
    put_compiler_info({modules, structs, sources, pending_modules, pending_structs})
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

  defp each_file(source) do
    Mix.shell().info("Compiled #{source}")
  end

  defp each_long_compilation(source, threshold) do
    Mix.shell().info("Compiling #{source} (it's taking more than #{threshold}s)")
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
  defp update_stale_entries(modules, _sources, [], stale_files, stale_structs)
       when stale_files == %{} and stale_structs == %{} do
    {modules, %{}, []}
  end

  defp update_stale_entries(modules, sources, changed, stale_files, stale_structs) do
    changed = Enum.into(changed, %{}, &{&1, true})
    reducer = &remove_stale_entry(&1, &2, sources, stale_structs)
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

  defp remove_stale_entry(entry, {rest, structs, changed, stale}, sources, stale_structs) do
    module(module: module, beam: beam, sources: source_files, struct: struct) = entry

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
        remove_and_purge(beam, module)
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
        split_manifest(data, compile_path)

      [v | data] when is_integer(v) ->
        for module <- data,
            is_record(module, :module),
            do: File.rm(Path.join(compile_path, module(module, :beam)))

        {[], []}

      _ ->
        {[], []}
    end
  end

  defp split_manifest(data, compile_path) do
    Enum.reduce(data, {[], []}, fn
      module() = module, {modules, sources} ->
        {[expand_beam_path(module, compile_path) | modules], sources}

      source() = source, {modules, sources} ->
        {modules, [source | sources]}
    end)
  end

  defp expand_beam_path(module(beam: beam) = module, compile_path) do
    module(module, beam: Path.join(compile_path, beam))
  end

  defp expand_beam_paths(modules, ""), do: modules

  defp expand_beam_paths(modules, compile_path) do
    Enum.map(modules, fn
      module() = module -> expand_beam_path(module, compile_path)
      other -> other
    end)
  end

  defp write_manifest(manifest, [], [], _compile_path, _timestamp) do
    File.rm(manifest)
    :ok
  end

  defp write_manifest(manifest, modules, sources, compile_path, timestamp) do
    File.mkdir_p!(Path.dirname(manifest))

    modules =
      for module(binary: binary, module: module) = entry <- modules do
        beam = Atom.to_string(module) <> ".beam"

        if binary do
          beam_path = Path.join(compile_path, beam)
          File.write!(beam_path, binary)
          File.touch!(beam_path, timestamp)
        end

        module(entry, binary: nil, beam: beam)
      end

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
end
