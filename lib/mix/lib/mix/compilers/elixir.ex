defmodule Mix.Compilers.Elixir do
  @moduledoc false

  @manifest_vsn 14
  @checkpoint_vsn 2

  import Record

  defrecord :module, [:module, :kind, :sources, :export, :recompile?]

  defrecord :source,
    source: nil,
    size: 0,
    digest: nil,
    compile_references: [],
    export_references: [],
    runtime_references: [],
    compile_env: [],
    external: [],
    warnings: [],
    modules: []

  @doc """
  Compiles stale Elixir files.

  It expects a `manifest` file, the source directories, the destination
  directory, the cache key based on compiler configuration, external
  manifests, and external modules, followed by opts.

  The `manifest` is written down with information including dependencies
  between modules, which helps it recompile only the modules that
  have changed at runtime.
  """
  def compile(manifest, srcs, dest, new_cache_key, new_parent_manifests, new_parents, opts) do
    modified = Mix.Utils.last_modified(manifest)
    new_parents = :ordsets.from_list(new_parents)

    # We fetch the time from before we read files so any future
    # change to files are still picked up by the compiler. This
    # timestamp is used when writing BEAM files and the manifest.
    timestamp = System.os_time(:second)
    all_paths = Mix.Utils.extract_files(srcs, [:ex])

    {all_modules, all_sources, all_local_exports, old_parents, old_cache_key, old_deps_config} =
      parse_manifest(manifest, dest)

    # If modules have been added or removed from the Erlang compiler,
    # we need to recompile all references to old and new modules.
    stale =
      if old_parents != new_parents or
           Mix.Utils.stale?(new_parent_manifests, [modified]) do
        :ordsets.union(old_parents, new_parents)
      else
        []
      end

    # If mix.exs has changed, recompile anything that calls Mix.Project.
    stale =
      if Mix.Utils.stale?([Mix.Project.project_file()], [modified]),
        do: [Mix.Project | stale],
        else: stale

    # If the dependencies have changed, we need to traverse lock/config files.
    deps_changed? = Mix.Utils.stale?([Mix.Project.config_mtime()], [modified])

    # If a configuration is only accessed at compile-time, we don't need to
    # track modules, only the compile env. So far this is only true for Elixir's
    # dbg callback.
    compile_env_apps = deps_config_compile_env_apps(old_deps_config)

    # The app tracer will return information about apps before this compilation.
    app_tracer = Mix.Compilers.ApplicationTracer.init()

    {force?, stale, new_deps_config} =
      cond do
        !!opts[:force] or is_nil(old_deps_config) or old_cache_key != new_cache_key ->
          {true, stale, deps_config()}

        deps_changed? or compile_env_apps != [] ->
          new_deps_config = deps_config()
          config_apps = merge_appset(old_deps_config.config, new_deps_config.config, [])
          apps = merge_appset(old_deps_config.lock, new_deps_config.lock, config_apps)

          if Mix.Project.config()[:app] in apps do
            {true, stale, new_deps_config}
          else
            apps_stale =
              apps
              |> deps_on()
              |> Enum.flat_map(fn {app, _} ->
                new_modules = Application.spec(app, :modules) || []

                if old_modules = Mix.Compilers.ApplicationTracer.app_modules(app_tracer, app) do
                  :ordsets.union(old_modules, :ordsets.from_list(new_modules))
                else
                  new_modules
                end
              end)

            compile_env_apps = compile_env_apps ++ config_apps

            compile_env_stale =
              for source(compile_env: compile_env, modules: modules) <- all_sources,
                  Enum.any?(compile_env_apps, &List.keymember?(compile_env, &1, 0)),
                  module <- modules,
                  do: module

            stale = (stale ++ compile_env_stale) ++ apps_stale
            {false, stale, new_deps_config}
          end

        true ->
          {false, stale, old_deps_config}
      end

    {stale_modules, stale_exports, all_local_exports} =
      stale_local_deps(manifest, stale, modified, all_local_exports)

    prev_paths = for source(source: source) <- all_sources, do: source
    removed = prev_paths -- all_paths
    {sources, removed_modules} = remove_removed_sources(all_sources, removed)

    {modules, exports, changed, sources_stats} =
      if force? do
        compiler_info_from_force(manifest, all_paths, all_modules, dest)
      else
        compiler_info_from_updated(
          manifest,
          modified,
          all_paths -- prev_paths,
          all_modules,
          all_sources,
          removed,
          Map.merge(stale_modules, removed_modules),
          Map.merge(stale_exports, removed_modules),
          dest
        )
      end

    stale = changed -- removed

    {sources, removed_modules} =
      update_stale_sources(sources, stale, removed_modules, sources_stats)

    if stale != [] do
      Mix.Utils.compiling_n(length(stale), :ex)
      Mix.Project.ensure_structure()
      true = Code.prepend_path(dest)

      {pending_tracer, tracer_opts} = Mix.Compilers.ApplicationTracer.prepare(app_tracer, opts)
      previous_opts = set_compiler_opts(tracer_opts)

      # Stores state for keeping track which files were compiled
      # and the dependencies between them.
      put_compiler_info({[], exports, sources, modules, removed_modules})

      try do
        compile_path(stale, dest, timestamp, opts)
      else
        {:ok, _, warnings} ->
          {modules, _exports, sources, pending_modules, _pending_exports} = get_compiler_info()

          # We only collect the warnings if --all-warnings is given.
          # In this case, we print them too. Then we apply the new warnings.
          previous_warnings =
            if opts[:all_warnings], do: previous_warnings(sources, true), else: []

          sources = apply_warnings(sources, warnings)

          write_manifest(
            manifest,
            modules ++ pending_modules,
            sources,
            all_local_exports,
            new_parents,
            new_cache_key,
            new_deps_config,
            timestamp
          )

          put_compile_env(sources)
          all_warnings = previous_warnings ++ Enum.map(warnings, &diagnostic(&1, :warning))
          unless_previous_warnings_as_errors(previous_warnings, opts, {:ok, all_warnings})

        {:error, errors, warnings} ->
          # In case of errors, we show all previous warnings and all new ones.
          # Print the new ones if --all-warnings was given.
          {_, _, sources, _, _} = get_compiler_info()
          errors = Enum.map(errors, &diagnostic(&1, :error))
          warnings = Enum.map(warnings, &diagnostic(&1, :warning))
          {:error, previous_warnings(sources, opts[:all_warnings]) ++ warnings ++ errors}
      after
        Code.compiler_options(previous_opts)
        Mix.Compilers.ApplicationTracer.stop(pending_tracer)
        Code.purge_compiler_modules()
        delete_compiler_info()
      end
    else
      # We need to return ok if deps_changed? or stale_modules changed,
      # even if no code was compiled, because we need to propagate the changed
      # status to compile.protocols. This will be the case whenever:
      #
      #   * the lock file or a config changes
      #   * any module in a path dependency changes
      #   * the mix.exs changes
      #   * the Erlang manifest updates (Erlang files are compiled)
      #
      # In the first case, we will consolidate from scratch. In the remaining, we
      # will only compute the diff with current protocols. In fact, there is no
      # need to reconsolidate if an Erlang file changes and it doesn't trigger
      # any other change, but the diff check should be reasonably fast anyway.
      status = if removed != [] or deps_changed? or stale_modules != %{}, do: :ok, else: :noop

      # If nothing changed but there is one more recent mtime, bump the manifest
      if status != :noop or Enum.any?(Map.values(sources_stats), &(elem(&1, 0) > modified)) do
        write_manifest(
          manifest,
          modules,
          sources,
          all_local_exports,
          new_parents,
          new_cache_key,
          new_deps_config,
          timestamp
        )
      end

      previous_warnings = previous_warnings(sources, opts[:all_warnings])
      unless_previous_warnings_as_errors(previous_warnings, opts, {status, previous_warnings})
    end
  after
    Mix.Compilers.ApplicationTracer.stop()
  end

  defp deps_config do
    # If you change this config, you need to bump @manifest_vsn
    %{
      lock: Enum.sort(Mix.Dep.Lock.read()),
      config: Enum.sort(Mix.Tasks.Loadconfig.read_compile()),
      dbg: Application.fetch_env!(:elixir, :dbg_callback)
    }
  end

  defp deps_config_compile_env_apps(deps_config) do
    if deps_config[:dbg] != Application.fetch_env!(:elixir, :dbg_callback) do
      [:elixir]
    else
      []
    end
  end

  @doc """
  Removes compiled files for the given `manifest`.
  """
  def clean(manifest, compile_path) do
    {modules, _} = read_manifest(manifest)

    Enum.each(modules, fn module(module: module) ->
      File.rm(beam_path(compile_path, module))
    end)
  end

  @doc """
  Returns protocols and implementations for the given `manifest`.
  """
  def protocols_and_impls(manifest, compile_path) do
    {modules, _} = read_manifest(manifest)

    for module(module: module, kind: kind) <- modules,
        match?(:protocol, kind) or match?({:impl, _}, kind),
        do: {module, kind, beam_path(compile_path, module)}
  end

  @doc """
  Reads the manifest for external consumption.
  """
  def read_manifest(manifest) do
    try do
      manifest |> File.read!() |> :erlang.binary_to_term()
    rescue
      _ -> {[], []}
    else
      {@manifest_vsn, modules, sources, _, _, _, _} -> {modules, sources}
      _ -> {[], []}
    end
  end

  defp compiler_info_from_force(manifest, all_paths, all_modules, dest) do
    # A config, path dependency or manifest has changed, let's just compile everything
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
  end

  # If any .beam file is missing, the first one will the first to miss,
  # so we always check that. If there are no modules, then we can rely
  # purely on digests.
  defp missing_beam_file?(dest, [mod | _]), do: not File.exists?(beam_path(dest, mod))
  defp missing_beam_file?(_dest, []), do: false

  defp compiler_info_from_updated(
         manifest,
         modified,
         new_paths,
         all_modules,
         all_sources,
         removed,
         stale_modules,
         stale_exports,
         dest
       ) do
    modules_to_recompile =
      for module(module: module, recompile?: recompile?) <- all_modules,
          recompile_module?(module, recompile?) or Map.has_key?(stale_modules, module),
          do: module

    {checkpoint_stale_modules, checkpoint_stale_exports, checkpoint_modules} =
      parse_checkpoint(manifest)

    modules_to_recompile =
      Map.merge(checkpoint_modules, Map.from_keys(modules_to_recompile, true))

    stale_modules = Map.merge(checkpoint_stale_modules, stale_modules)
    stale_exports = Map.merge(checkpoint_stale_exports, stale_exports)

    if map_size(stale_modules) != map_size(checkpoint_stale_modules) or
         map_size(stale_exports) != map_size(checkpoint_stale_exports) or
         map_size(modules_to_recompile) != map_size(checkpoint_modules) do
      write_checkpoint(manifest, stale_modules, stale_exports, modules_to_recompile)
    end

    sources_stats =
      for path <- new_paths,
          into: mtimes_and_sizes(all_sources),
          do: {path, Mix.Utils.last_modified_and_size(path)}

    # Sources that have changed on disk or
    # any modules associated with them need to be recompiled
    changed =
      for source(source: source, external: external, size: size, digest: digest, modules: modules) <-
            all_sources,
          {last_mtime, last_size} = Map.fetch!(sources_stats, source),
          # If the user does a change, compilation fails, and then they revert
          # the change, the mtime will have changed but the .beam files will
          # be missing and the digest is the same, so we need to check if .beam
          # files are available.
          size != last_size or
            Enum.any?(modules, &Map.has_key?(modules_to_recompile, &1)) or
            Enum.any?(external, &stale_external?(&1, modified, sources_stats)) or
            (last_mtime > modified and
               (missing_beam_file?(dest, modules) or digest != digest(source))),
          do: source

    changed = new_paths ++ changed

    {modules, exports, changed} =
      update_stale_entries(
        all_modules,
        all_sources,
        removed ++ changed,
        stale_modules,
        stale_exports,
        dest
      )

    # Now sort the files so the ones changed more recently come first.
    # We do an optimized version of sort_by since we don't care about
    # stable sorting.
    changed =
      changed
      |> Enum.map(&{-elem(Map.fetch!(sources_stats, &1), 0), &1})
      |> Enum.sort()
      |> Enum.map(&elem(&1, 1))

    {modules, exports, changed, sources_stats}
  end

  defp stale_external?({external, existed?}, modified, sources_stats) do
    case sources_stats do
      %{^external => {0, 0}} -> existed?
      %{^external => {mtime, _}} -> mtime > modified
    end
  end

  defp mtimes_and_sizes(sources) do
    Enum.reduce(sources, %{}, fn source(source: source, external: external), map ->
      map = Map.put_new_lazy(map, source, fn -> Mix.Utils.last_modified_and_size(source) end)

      Enum.reduce(external, map, fn {file, _}, map ->
        Map.put_new_lazy(map, file, fn -> Mix.Utils.last_modified_and_size(file) end)
      end)
    end)
  end

  defp digest(file) do
    file
    |> File.read!()
    |> :erlang.md5()
  end

  defp compile_path(stale, dest, timestamp, opts) do
    cwd = File.cwd!()
    long_compilation_threshold = opts[:long_compilation_threshold] || 10
    verbose = opts[:verbose] || false

    compile_opts = [
      each_cycle: fn -> each_cycle(dest, timestamp) end,
      each_file: &each_file(&1, &2, cwd, verbose),
      each_module: &each_module(&1, &2, &3, cwd),
      each_long_compilation: &each_long_compilation(&1, cwd, long_compilation_threshold),
      long_compilation_threshold: long_compilation_threshold,
      profile: opts[:profile],
      beam_timestamp: timestamp
    ]

    Kernel.ParallelCompiler.compile_to_path(stale, dest, compile_opts)
  end

  defp get_compiler_info(), do: Process.get(__MODULE__)
  defp put_compiler_info(value), do: Process.put(__MODULE__, value)
  defp delete_compiler_info(), do: Process.delete(__MODULE__)

  defp set_compiler_opts(opts) do
    opts
    |> Keyword.take(Code.available_compiler_options())
    |> Code.compiler_options()
  end

  defp put_compile_env(sources) do
    all_compile_env =
      Enum.reduce(sources, :ordsets.new(), fn source(compile_env: compile_env), acc ->
        :ordsets.union(compile_env, acc)
      end)

    Mix.ProjectStack.compile_env(all_compile_env)
  end

  defp each_cycle(compile_path, timestamp) do
    {modules, _exports, sources, pending_modules, pending_exports} = get_compiler_info()

    {pending_modules, exports, changed} =
      update_stale_entries(pending_modules, sources, [], %{}, pending_exports, compile_path)

    # For each changed file, mark it as changed.
    # If compilation fails mid-cycle, they will
    # be picked next time around.
    for file <- changed do
      File.touch!(file, timestamp)
    end

    if changed == [] do
      modules = Enum.map(modules, &module(&1, :module))
      warnings = Mix.Compilers.ApplicationTracer.warnings(modules)

      modules_set = Map.from_keys(modules, true)
      {_, runtime_modules} = fixpoint_runtime_modules(sources, modules_set)
      {:runtime, runtime_modules, warnings}
    else
      Mix.Utils.compiling_n(length(changed), :ex)

      # If we have a compile time dependency to a module, as soon as its file
      # change, we will detect the compile time dependency and recompile. However,
      # the whole goal of pending exports is to delay this decision, so we need to
      # track which modules were removed and start them as our pending exports and
      # remove the pending exports as we notice they have not gone stale.
      {sources, removed_modules} = update_stale_sources(sources, changed)
      put_compiler_info({modules, exports, sources, pending_modules, removed_modules})
      {:compile, changed, []}
    end
  end

  defp each_module(file, module, _binary, cwd) do
    {modules, exports, sources, pending_modules, pending_exports} = get_compiler_info()

    kind = detect_kind(module)
    file = Path.relative_to(file, cwd)
    external = get_external_resources(module, cwd)

    old_export = Map.get(exports, module)
    new_export = exports_md5(module, true)

    pending_exports =
      if old_export && old_export != new_export do
        pending_exports
      else
        Map.delete(pending_exports, module)
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
        export: new_export,
        recompile?: function_exported?(module, :__mix_recompile__?, 0)
      )

    modules = prepend_or_merge(modules, module, module(:module), module, existing_module?)
    put_compiler_info({modules, exports, [source | sources], pending_modules, pending_exports})
    :ok
  end

  defp recompile_module?(module, recompile?) do
    recompile? and Code.ensure_loaded?(module) and
      function_exported?(module, :__mix_recompile__?, 0) and
      module.__mix_recompile__?()
  end

  defp prepend_or_merge(collection, key, pos, value, true) do
    List.keystore(collection, key, pos, value)
  end

  defp prepend_or_merge(collection, _key, _pos, value, false) do
    [value | collection]
  end

  defp detect_kind(module) do
    protocol_metadata = Module.get_attribute(module, :__impl__)

    cond do
      is_list(protocol_metadata) and protocol_metadata[:protocol] ->
        {:impl, protocol_metadata[:protocol]}

      is_list(Module.get_attribute(module, :__protocol__)) ->
        :protocol

      true ->
        :module
    end
  end

  defp get_external_resources(module, cwd) do
    for file <- Module.get_attribute(module, :external_resource) do
      {Path.relative_to(file, cwd), File.exists?(file)}
    end
  end

  defp each_file(file, lexical, cwd, verbose) do
    file = Path.relative_to(file, cwd)

    if verbose do
      Mix.shell().info("Compiled #{file}")
    end

    {modules, exports, sources, pending_modules, pending_exports} = get_compiler_info()
    {source, sources} = List.keytake(sources, file, source(:source))

    {compile_references, export_references, runtime_references, compile_env} =
      Kernel.LexicalTracker.references(lexical)

    compile_references =
      Enum.reject(compile_references, &match?("elixir_" <> _, Atom.to_string(&1)))

    source(modules: source_modules) = source
    compile_references = compile_references -- source_modules
    export_references = export_references -- source_modules
    runtime_references = runtime_references -- source_modules

    source =
      source(
        source,
        # We preserve the digest if the file is recompiled but not changed
        digest: source(source, :digest) || digest(file),
        compile_references: compile_references,
        export_references: export_references,
        runtime_references: runtime_references,
        compile_env: compile_env
      )

    put_compiler_info({modules, exports, [source | sources], pending_modules, pending_exports})
    :ok
  end

  defp each_long_compilation(file, cwd, threshold) do
    Mix.shell().info(
      "Compiling #{Path.relative_to(file, cwd)} (it's taking more than #{threshold}s)"
    )
  end

  ## Resolution

  defp remove_removed_sources(sources, removed) do
    Enum.reduce(removed, {sources, %{}}, fn file, {acc_sources, acc_modules} ->
      {source(modules: modules), acc_sources} = List.keytake(acc_sources, file, source(:source))

      acc_modules = Enum.reduce(modules, acc_modules, &Map.put(&2, &1, true))
      {acc_sources, acc_modules}
    end)
  end

  # Initial definition of empty records for changed sources
  # as the compiler appends data. This may include new files,
  # so we rely on sources_stats to avoid multiple FS lookups.
  defp update_stale_sources(sources, stale, removed_modules, sources_stats) do
    Enum.reduce(stale, {sources, removed_modules}, fn file, {acc_sources, acc_modules} ->
      %{^file => {_, size}} = sources_stats

      {modules, acc_sources} =
        case List.keytake(acc_sources, file, source(:source)) do
          {source(modules: modules), acc_sources} -> {modules, acc_sources}
          nil -> {[], acc_sources}
        end

      acc_modules = Enum.reduce(modules, acc_modules, &Map.put(&2, &1, true))
      {[source(source: file, size: size) | acc_sources], acc_modules}
    end)
  end

  # Define empty records for the sources that needs
  # to be recompiled (but were not changed on disk)
  defp update_stale_sources(sources, changed) do
    Enum.reduce(changed, {sources, %{}}, fn file, {acc_sources, acc_modules} ->
      {source(size: size, digest: digest, modules: modules), acc_sources} =
        List.keytake(acc_sources, file, source(:source))

      acc_modules = Enum.reduce(modules, acc_modules, &Map.put(&2, &1, true))
      {[source(source: file, size: size, digest: digest) | acc_sources], acc_modules}
    end)
  end

  # This function receives the manifest entries and some source
  # files that have changed. Then it recursively figures out
  # all the files that changed (via the module dependencies) and
  # return the non-changed entries and the removed sources.
  defp update_stale_entries(modules, _sources, [], stale_modules, stale_exports, _compile_path)
       when stale_modules == %{} and stale_exports == %{} do
    {modules, %{}, []}
  end

  defp update_stale_entries(modules, sources, changed, stale_modules, stale_exports, compile_path) do
    changed = Map.from_keys(changed, true)
    reducer = &remove_stale_entry(&1, &2, sources, stale_exports, compile_path)
    remove_stale_entries(modules, %{}, changed, stale_modules, reducer)
  end

  defp remove_stale_entries(modules, exports, old_changed, old_stale, reducer) do
    {pending_modules, exports, new_changed, new_stale} =
      Enum.reduce(modules, {[], exports, old_changed, old_stale}, reducer)

    if map_size(new_stale) > map_size(old_stale) or map_size(new_changed) > map_size(old_changed) do
      remove_stale_entries(pending_modules, exports, new_changed, new_stale, reducer)
    else
      {pending_modules, exports, Map.keys(new_changed)}
    end
  end

  defp remove_stale_entry(entry, acc, sources, stale_exports, compile_path) do
    module(module: module, sources: source_files, export: export) = entry
    {rest, exports, changed, stale} = acc

    {compile_references, export_references, runtime_references} =
      Enum.reduce(source_files, {[], [], []}, fn file, {compile_acc, export_acc, runtime_acc} ->
        source(
          compile_references: compile_refs,
          export_references: export_refs,
          runtime_references: runtime_refs
        ) = List.keyfind(sources, file, source(:source))

        {compile_acc ++ compile_refs, export_acc ++ export_refs, runtime_acc ++ runtime_refs}
      end)

    cond do
      # If I changed in disk or have a compile time reference to
      # something stale or have a reference to an old export,
      # I need to be recompiled.
      has_any_key?(changed, source_files) or has_any_key?(stale, compile_references) or
          has_any_key?(stale_exports, export_references) ->
        remove_and_purge(beam_path(compile_path, module), module)
        changed = Enum.reduce(source_files, changed, &Map.put(&2, &1, true))
        {rest, Map.put(exports, module, export), changed, Map.put(stale, module, true)}

      # If I have a runtime references to something stale,
      # I am stale too.
      has_any_key?(stale, runtime_references) ->
        {[entry | rest], exports, changed, Map.put(stale, module, true)}

      # Otherwise, we don't store it anywhere
      true ->
        {[entry | rest], exports, changed, stale}
    end
  end

  defp has_any_key?(map, enumerable) do
    Enum.any?(enumerable, &Map.has_key?(map, &1))
  end

  defp stale_local_deps(manifest, stale_modules, modified, old_exports) do
    base = Path.basename(manifest)

    # The stale modules so far will become both stale_modules and stale_exports,
    # as any export from a dependency needs to be recompiled.
    stale_modules = Map.from_keys(stale_modules, true)

    for %{scm: scm, opts: opts} = dep <- Mix.Dep.cached(),
        not scm.fetchable?,
        manifest = Path.join([opts[:build], ".mix", base]),
        Mix.Utils.last_modified(manifest) > modified,
        reduce: {stale_modules, stale_modules, old_exports} do
      {modules, exports, new_exports} ->
        {_manifest_modules, dep_sources} = read_manifest(manifest)

        dep_modules =
          for path <- Mix.Dep.load_paths(dep),
              beam <- Path.wildcard(Path.join(path, "*.beam")),
              Mix.Utils.last_modified(beam) > modified,
              do: beam |> Path.basename() |> Path.rootname() |> String.to_atom()

        # If any module has a compile time dependency on a changed module
        # within the dependnecy, they will be recompiled. However, export
        # and runtime dependencies won't have recompiled so we need to
        # propagate them to the parent app.
        {dep_modules, _} = fixpoint_runtime_modules(dep_sources, Map.from_keys(dep_modules, true))

        # Update exports
        {exports, new_exports} =
          for {module, _} <- dep_modules, reduce: {exports, new_exports} do
            {exports, new_exports} ->
              export = exports_md5(module, false)

              # If the exports are the same, then the API did not change,
              # so we do not mark the export as stale. Note this has to
              # be very conservative. If the module is not loaded or if
              # the exports were not there, we need to consider it a stale
              # export.
              exports =
                if export && old_exports[module] == export,
                  do: exports,
                  else: Map.put(exports, module, true)

              # In any case, we always store it as the most update export
              # that we have, otherwise we delete it.
              new_exports =
                if export,
                  do: Map.put(new_exports, module, export),
                  else: Map.delete(new_exports, module)

              {exports, new_exports}
          end

        {Map.merge(modules, dep_modules), exports, new_exports}
    end
  end

  defp fixpoint_runtime_modules(sources, modules) when modules != %{} do
    fixpoint_runtime_modules(sources, modules, false, [], [])
  end

  defp fixpoint_runtime_modules(_sources, modules) do
    {modules, []}
  end

  defp fixpoint_runtime_modules([source | sources], modules, new?, acc_modules, acc_sources) do
    source(export_references: export_refs, runtime_references: runtime_refs) = source

    if has_any_key?(modules, export_refs) or has_any_key?(modules, runtime_refs) do
      new_modules = Enum.reject(source(source, :modules), &Map.has_key?(modules, &1))
      modules = Enum.reduce(new_modules, modules, &Map.put(&2, &1, true))
      new? = new? or new_modules != []
      acc_modules = new_modules ++ acc_modules
      fixpoint_runtime_modules(sources, modules, new?, acc_modules, acc_sources)
    else
      fixpoint_runtime_modules(sources, modules, new?, acc_modules, [source | acc_sources])
    end
  end

  defp fixpoint_runtime_modules([], modules, new?, acc_modules, acc_sources)
       when new? == false or acc_sources == [],
       do: {modules, acc_modules}

  defp fixpoint_runtime_modules([], modules, true, acc_modules, acc_sources),
    do: fixpoint_runtime_modules(acc_sources, modules, false, acc_modules, [])

  defp exports_md5(module, use_attributes?) do
    cond do
      function_exported?(module, :__info__, 1) ->
        module.__info__(:exports_md5)

      use_attributes? ->
        defs = :lists.sort(Module.definitions_in(module, :def))
        defmacros = :lists.sort(Module.definitions_in(module, :defmacro))

        struct =
          case Module.get_attribute(module, :__struct__) do
            %{} = entry -> {entry, List.wrap(Module.get_attribute(module, :enforce_keys))}
            _ -> nil
          end

        {defs, defmacros, struct} |> :erlang.term_to_binary() |> :erlang.md5()

      true ->
        nil
    end
  end

  defp remove_and_purge(beam, module) do
    _ = File.rm(beam)
    _ = :code.purge(module)
    _ = :code.delete(module)
  end

  defp previous_warnings(sources, print?) do
    for source(source: source, warnings: warnings) <- sources,
        file = Path.absname(source),
        {location, message} <- warnings do
      warning = {file, location, message}
      print? && Kernel.ParallelCompiler.print_warning(warning)
      diagnostic(warning, :warning)
    end
  end

  defp apply_warnings(sources, warnings) do
    warnings = Enum.group_by(warnings, &elem(&1, 0), &{elem(&1, 1), elem(&1, 2)})

    for source(source: source_path, warnings: source_warnings) = s <- sources do
      source(s, warnings: Map.get(warnings, Path.absname(source_path), source_warnings))
    end
  end

  defp diagnostic({file, location, message}, severity) do
    %Mix.Task.Compiler.Diagnostic{
      file: file,
      position: location,
      message: message,
      severity: severity,
      compiler_name: "Elixir"
    }
  end

  ## Merging of lock and config files

  # Value for app didn't change
  defp merge_appset([{app, value} | old_set], [{app, value} | new_set], apps),
    do: merge_appset(old_set, new_set, apps)

  # Value for app changed
  defp merge_appset([{app, _} | old_set], [{app, _} | new_set], apps),
    do: merge_appset(old_set, new_set, [app | apps])

  # Added value for app
  defp merge_appset([{app1, _} | _] = old_set, [{app2, _} | new_set], apps)
       when app1 > app2,
       do: merge_appset(old_set, new_set, [app2 | apps])

  # Removed value for app
  defp merge_appset([{app1, _} | old_set], [{app2, _} | _] = new_set, apps)
       when app1 < app2,
       do: merge_appset(old_set, new_set, [app1 | apps])

  # One of them is done, add the others
  defp merge_appset(old_set, new_set, apps) do
    apps = Enum.reduce(old_set, apps, fn {app, _}, apps -> [app | apps] end)
    Enum.reduce(new_set, apps, fn {app, _}, apps -> [app | apps] end)
  end

  defp deps_on(apps) do
    apps = Map.from_keys(apps, true)
    deps_on(Mix.Dep.cached(), apps, [], false)
  end

  defp deps_on([%{app: app, deps: deps} = dep | cached_deps], apps, acc, stored?) do
    cond do
      # We have already seen this dep
      Map.has_key?(apps, app) ->
        deps_on(cached_deps, apps, acc, stored?)

      # It depends on one of the apps, store it
      Enum.any?(deps, &Map.has_key?(apps, &1.app)) ->
        deps_on(cached_deps, Map.put(apps, app, true), acc, true)

      # Otherwise we will check it later
      true ->
        deps_on(cached_deps, apps, [dep | acc], stored?)
    end
  end

  defp deps_on([], apps, cached_deps, true), do: deps_on(cached_deps, apps, [], false)
  defp deps_on([], apps, _cached_deps, false), do: apps

  ## Manifest handling

  @default_manifest {[], [], %{}, [], nil, nil}

  # Similar to read_manifest, but for internal consumption and with data migration support.
  defp parse_manifest(manifest, compile_path) do
    try do
      manifest |> File.read!() |> :erlang.binary_to_term()
    rescue
      _ ->
        @default_manifest
    else
      {@manifest_vsn, modules, sources, local_exports, parent, cache_key, deps_config} ->
        {modules, sources, local_exports, parent, cache_key, deps_config}

      # {vsn, modules, sources} v5-v7 (v1.10)
      # {vsn, modules, sources, local_exports} v8-v10 (v1.11)
      manifest when is_tuple(manifest) and is_integer(elem(manifest, 0)) ->
        purge_old_manifest(compile_path, elem(manifest, 1))

      # v1-v4
      [vsn | data] when is_integer(vsn) ->
        purge_old_manifest(compile_path, data)

      _ ->
        @default_manifest
    end
  end

  defp purge_old_manifest(compile_path, data) do
    try do
      for module <- data, elem(module, 0) == :module do
        module = elem(module, 1)
        File.rm(beam_path(compile_path, module))
        :code.purge(module)
        :code.delete(module)
      end
    rescue
      _ ->
        Mix.raise(
          "Cannot clean-up stale manifest, please run \"mix clean --deps\" manually before proceeding"
        )
    end

    @default_manifest
  end

  defp write_manifest(manifest, [], [], _exports, _parents, _cache_key, _deps_config, _timestamp) do
    File.rm(manifest)
    :ok
  end

  defp write_manifest(
         manifest,
         modules,
         sources,
         exports,
         parents,
         cache_key,
         deps_config,
         timestamp
       ) do
    File.mkdir_p!(Path.dirname(manifest))

    term = {@manifest_vsn, modules, sources, exports, parents, cache_key, deps_config}
    manifest_data = :erlang.term_to_binary(term, [:compressed])
    File.write!(manifest, manifest_data)
    File.touch!(manifest, timestamp)
    delete_checkpoint(manifest)

    # Since Elixir is a dependency itself, we need to touch the lock
    # so the current Elixir version, used to compile the files above,
    # is properly stored.
    Mix.Dep.ElixirSCM.update()
  end

  defp beam_path(compile_path, module) do
    Path.join(compile_path, Atom.to_string(module) <> ".beam")
  end

  # Once we added semantic recompilation, the following can happen:
  #
  # 1. The user changes config/mix.exs/__mix_recompile__?
  # 2. We detect the change, remove .beam files and start recompilation
  # 3. Recompilation fails
  # 4. The user reverts the change
  # 5. The compiler no longer recompiles and the .beam files are missing
  #
  # Therefore, it is important for us to checkpoint any state that may
  # have lead to a compilation and which can now be reverted.

  defp parse_checkpoint(manifest) do
    try do
      (manifest <> ".checkpoint") |> File.read!() |> :erlang.binary_to_term()
    rescue
      _ ->
        {%{}, %{}, %{}}
    else
      {@checkpoint_vsn, stale_modules, stale_exports, recompile_modules} ->
        {stale_modules, stale_exports, recompile_modules}

      _ ->
        {%{}, %{}, %{}}
    end
  end

  defp write_checkpoint(manifest, stale_modules, stale_exports, recompile_modules) do
    File.mkdir_p!(Path.dirname(manifest))
    term = {@checkpoint_vsn, stale_modules, stale_exports, recompile_modules}
    checkpoint_data = :erlang.term_to_binary(term, [:compressed])
    File.write!(manifest <> ".checkpoint", checkpoint_data)
  end

  defp delete_checkpoint(manifest) do
    File.rm(manifest <> ".checkpoint")
  end

  defp unless_previous_warnings_as_errors(previous_warnings, opts, {status, all_warnings}) do
    if previous_warnings != [] and opts[:warnings_as_errors] do
      message = "Compilation failed due to warnings while using the --warnings-as-errors option"
      IO.puts(:stderr, message)
      {:error, all_warnings}
    else
      {status, all_warnings}
    end
  end
end
