defmodule Mix.Compilers.Elixir do
  @moduledoc false

  @manifest_vsn 25
  @checkpoint_vsn 2

  import Record

  defrecord :module, [:kind, :sources, :export, :recompile?, :timestamp]

  defrecord :source,
    size: 0,
    mtime: 0,
    digest: nil,
    compile_references: [],
    export_references: [],
    runtime_references: [],
    compile_env: [],
    external: [],
    compile_warnings: [],
    runtime_warnings: [],
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
    Mix.ensure_application!(:crypto)
    modified = Mix.Utils.last_modified(manifest)
    config_mtime = Mix.Project.config_mtime()
    project_mtime = Mix.Utils.last_modified(Mix.Project.project_file())
    new_parents = :ordsets.from_list(new_parents)

    # We fetch the time from before we read files so any future
    # change to files are still picked up by the compiler. This
    # timestamp is used when writing BEAM files and the manifest.
    timestamp = System.os_time(:second)
    all_paths = Mix.Utils.extract_files(srcs, [:ex])

    {all_modules, all_sources, all_local_exports, old_parents, old_cache_key, old_deps_config,
     old_project_mtime,
     old_config_mtime} =
      parse_manifest(manifest, dest)

    # Prepend ourselves early because of __mix_recompile__? checks
    # and also that, in case of nothing compiled, we already need
    # ourselves available in the path.
    Code.prepend_path(dest)

    # If modules have been added or removed from the Erlang compiler,
    # we need to recompile all references to old and new modules.
    stale =
      if old_parents != new_parents or
           Mix.Utils.stale?(new_parent_manifests, [modified]) do
        :ordsets.union(old_parents, new_parents)
      else
        []
      end

    local_deps = Enum.reject(Mix.Dep.cached(), & &1.scm.fetchable?())

    # If mix.exs has changed, recompile anything that calls Mix.Project.
    stale =
      if project_mtime > old_project_mtime,
        do: [Mix.Project | stale],
        else: stale

    # If the lock has changed or a local dependency was added or removed,
    # we need to traverse lock/config files.
    deps_changed? =
      config_mtime > old_config_mtime or
        local_deps_changed?(old_deps_config, local_deps)

    # If a configuration is only accessed at compile-time, we don't need to
    # track modules, only the compile env. So far this is only true for Elixir's
    # dbg callback.
    compile_env_apps = deps_config_compile_env_apps(old_deps_config)

    {force?, stale, new_deps_config} =
      cond do
        !!opts[:force] or is_nil(old_deps_config) or old_cache_key != new_cache_key ->
          {true, stale, deps_config(local_deps)}

        deps_changed? or compile_env_apps != [] ->
          new_deps_config = deps_config(local_deps)
          local_apps = merge_appset(old_deps_config.local, new_deps_config.local, [])
          config_apps = merge_appset(old_deps_config.config, new_deps_config.config, local_apps)
          apps = merge_appset(old_deps_config.lock, new_deps_config.lock, config_apps)

          if Mix.Project.config()[:app] in apps do
            {true, stale, new_deps_config}
          else
            app_modules = Mix.AppLoader.read_cache()

            apps_stale =
              apps
              |> deps_on()
              |> Enum.flat_map(fn {app, _} ->
                new_modules = Application.spec(app, :modules) || []

                if old_modules = app_modules[app] do
                  :ordsets.union(old_modules, :ordsets.from_list(new_modules))
                else
                  new_modules
                end
              end)

            compile_env_apps = compile_env_apps ++ config_apps

            compile_env_stale =
              for {_, source(compile_env: compile_env, modules: modules)} <- all_sources,
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
      stale_local_deps(local_deps, manifest, stale, modified, all_local_exports)

    prev_paths = Map.keys(all_sources)
    removed = prev_paths -- all_paths
    {sources, removed_modules} = remove_removed_sources(all_sources, removed)

    {modules, exports, changed, sources_stats} =
      if force? do
        compiler_info_from_force(manifest, all_paths, all_modules, dest)
      else
        compiler_info_from_updated(
          manifest,
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

    if stale != [] or stale_modules != %{} do
      path = opts[:purge_consolidation_path_if_stale]

      if is_binary(path) and Code.delete_path(path) do
        purge_modules_in_path(path)
      end

      Mix.Utils.compiling_n(length(stale), :ex)
      Mix.Project.ensure_structure()

      # We don't want to cache this path as we will write to it
      true = Code.prepend_path(dest)
      previous_opts = set_compiler_opts(opts)

      try do
        state = {%{}, exports, sources, [], modules, removed_modules}
        compiler_loop(stale, stale_modules, dest, timestamp, opts, state)
      else
        {:ok, %{runtime_warnings: runtime_warnings, compile_warnings: compile_warnings}, state} ->
          {modules, _exports, sources, _changed, pending_modules, _stale_exports} = state

          previous_warnings =
            if Keyword.get(opts, :all_warnings, true),
              do: previous_warnings(sources, true),
              else: []

          runtime_warnings = Enum.map(runtime_warnings, &diagnostic/1)
          compile_warnings = Enum.map(compile_warnings, &diagnostic/1)
          sources = apply_warnings(sources, runtime_warnings, compile_warnings)

          write_manifest(
            manifest,
            Map.merge(modules, pending_modules),
            sources,
            all_local_exports,
            new_parents,
            new_cache_key,
            new_deps_config,
            project_mtime,
            config_mtime,
            timestamp
          )

          put_compile_env(sources)
          all_warnings = previous_warnings ++ runtime_warnings ++ compile_warnings

          lazy_modules_diff = fn ->
            modules_diff(modules, removed_modules, all_modules, timestamp)
          end

          Mix.Task.Compiler.notify_modules_compiled(lazy_modules_diff)

          unless_previous_warnings_as_errors(previous_warnings, opts, {:ok, all_warnings})

        {:error, errors, %{runtime_warnings: r_warnings, compile_warnings: c_warnings}, state} ->
          # In case of errors, we show all previous warnings and all new ones.
          {_, _, sources, _, _, _} = state
          errors = Enum.map(errors, &diagnostic/1)
          warnings = Enum.map(r_warnings ++ c_warnings, &diagnostic/1)
          all_warnings = Keyword.get(opts, :all_warnings, errors == [])
          {:error, previous_warnings(sources, all_warnings) ++ warnings ++ errors}
      after
        Code.compiler_options(previous_opts)
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

      if status != :noop do
        write_manifest(
          manifest,
          modules,
          sources,
          all_local_exports,
          new_parents,
          new_cache_key,
          new_deps_config,
          project_mtime,
          config_mtime,
          timestamp
        )
      end

      all_warnings = Keyword.get(opts, :all_warnings, true)
      previous_warnings = previous_warnings(sources, all_warnings)

      unless_previous_warnings_as_errors(previous_warnings, opts, {status, previous_warnings})
    end
  end

  defp deps_config(local_deps) do
    # If you change this config, you need to bump @manifest_vsn
    %{
      local: Enum.sort(Enum.map(local_deps, &{&1.app, true})),
      lock: Enum.sort(Mix.Dep.Lock.read()),
      config: Enum.sort(Mix.Tasks.Loadconfig.read_compile()),
      dbg: Application.fetch_env!(:elixir, :dbg_callback)
    }
  end

  defp local_deps_changed?(deps_config, local_deps) do
    is_map(deps_config) and Enum.sort(Enum.map(local_deps, &{&1.app, true})) != deps_config.local
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

    Enum.each(modules, fn {module, _} ->
      File.rm(beam_path(compile_path, module))
    end)
  end

  @doc """
  Returns protocols and implementations for the given `manifest`.
  """
  def protocols_and_impls(paths) do
    Enum.reduce(paths, {%{}, %{}}, fn path, acc ->
      {modules, _} = read_manifest(Path.join(path, ".mix/compile.elixir"))

      Enum.reduce(modules, acc, fn
        {module, module(kind: kind, timestamp: timestamp)}, {protocols, impls} ->
          case kind do
            :protocol -> {Map.put(protocols, module, timestamp), impls}
            {:impl, protocol} -> {protocols, Map.put(impls, module, protocol)}
            _ -> {protocols, impls}
          end
      end)
    end)
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
      {@manifest_vsn, modules, sources, _, _, _, _, _, _} -> {modules, sources}
      _ -> {[], []}
    end
  end

  @doc """
  Retrieves all diagnostics from the given manifest.
  """
  def diagnostics(manifest, dest) do
    {_, all_sources, _, _, _, _, _, _} = parse_manifest(manifest, dest)
    previous_warnings(all_sources, false)
  end

  defp compiler_info_from_force(manifest, all_paths, all_modules, dest) do
    # A config, path dependency or manifest has changed, let's just compile everything
    for {module, _} <- all_modules,
        do: remove_and_purge(beam_path(dest, module), module)

    sources_stats =
      for path <- all_paths,
          into: %{},
          do: {path, Mix.Utils.last_modified_and_size(path)}

    # Now that we have deleted all beams, remember to remove the manifest.
    # This is important in case mix compile --force fails, otherwise we
    # would have an outdated manifest.
    File.rm(manifest)

    {%{}, %{}, all_paths, sources_stats}
  end

  # If any .beam file is missing, the first one will the first to miss,
  # so we always check that. If there are no modules, then we can rely
  # purely on digests.
  defp missing_beam_file?(dest, [mod | _]), do: not File.exists?(beam_path(dest, mod))
  defp missing_beam_file?(_dest, []), do: false

  defp compiler_info_from_updated(
         manifest,
         new_paths,
         all_modules,
         all_sources,
         removed,
         stale_modules,
         stale_exports,
         dest
       ) do
    {modules_to_recompile, modules_to_mix_check} =
      for {module, module(recompile?: recompile?)} <- all_modules, reduce: {[], []} do
        {modules_to_recompile, modules_to_mix_check} ->
          cond do
            Map.has_key?(stale_modules, module) ->
              {[module | modules_to_recompile], modules_to_mix_check}

            recompile? and Code.ensure_loaded?(module) and
                function_exported?(module, :__mix_recompile__?, 0) ->
              {modules_to_recompile, [module | modules_to_mix_check]}

            true ->
              {modules_to_recompile, modules_to_mix_check}
          end
      end

    modules_to_recompile =
      modules_to_recompile ++
        for {:ok, {module, true}} <-
              Task.async_stream(modules_to_mix_check, &{&1, &1.__mix_recompile__?()},
                ordered: false,
                timeout: :infinity
              ) do
          module
        end

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
      for {source,
           source(external: external, size: size, mtime: mtime, digest: digest, modules: modules)} <-
            all_sources,
          {last_mtime, last_size} = Map.fetch!(sources_stats, source),
          # If the user does a change, compilation fails, and then they revert
          # the change, the mtime will have changed but the .beam files will
          # be missing and the digest is the same, so we need to check if .beam
          # files are available.
          size != last_size or
            Enum.any?(modules, &Map.has_key?(modules_to_recompile, &1)) or
            Enum.any?(external, &stale_external?(&1, sources_stats)) or
            (last_mtime > mtime and
               (missing_beam_file?(dest, modules) or digest_changed?(source, digest))),
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

  defp stale_external?({external, {mtime, size}, digest}, sources_stats) do
    case sources_stats do
      %{^external => {0, 0}} ->
        digest != nil

      %{^external => {last_mtime, last_size}} ->
        size != last_size or (last_mtime > mtime and digest_changed?(external, digest))
    end
  end

  defp mtimes_and_sizes(sources) do
    Enum.reduce(sources, %{}, fn {source, source(external: external)}, map ->
      map = Map.put_new_lazy(map, source, fn -> Mix.Utils.last_modified_and_size(source) end)

      Enum.reduce(external, map, fn {file, _, _}, map ->
        Map.put_new_lazy(map, file, fn -> Mix.Utils.last_modified_and_size(file) end)
      end)
    end)
  end

  defp digest_changed?(file, digest) do
    case File.read(file) do
      {:ok, binary} -> digest != digest_contents(binary)
      {:error, _} -> true
    end
  end

  defp digest_contents(contents) do
    case :erlang.system_info(:wordsize) do
      8 -> :crypto.hash(:blake2b, contents)
      _ -> :crypto.hash(:blake2s, contents)
    end
  rescue
    # Blake may not be available on all OpenSSL distribution
    _ -> :erlang.md5(contents)
  end

  defp set_compiler_opts(opts) do
    opts
    |> Keyword.take(Code.available_compiler_options())
    |> Code.compiler_options()
  end

  defp put_compile_env(sources) do
    all_compile_env =
      Enum.reduce(sources, :ordsets.new(), fn {_, source(compile_env: compile_env)}, acc ->
        :ordsets.union(compile_env, acc)
      end)

    Mix.ProjectStack.compile_env(all_compile_env)
  end

  ## Resolution

  defp remove_removed_sources(sources, removed) do
    Enum.reduce(removed, {sources, %{}}, fn file, {acc_sources, acc_modules} ->
      {source(modules: modules), acc_sources} = Map.pop(acc_sources, file)
      acc_modules = Enum.reduce(modules, acc_modules, &Map.put(&2, &1, true))
      {acc_sources, acc_modules}
    end)
  end

  # Initial definition of empty records for changed sources
  # as the compiler appends data. This may include new files,
  # so we rely on sources_stats to avoid multiple FS lookups.
  defp update_stale_sources(sources, stale, removed_modules, sources_stats) do
    Enum.reduce(stale, {sources, removed_modules}, fn file, {acc_sources, acc_modules} ->
      %{^file => {mtime, size}} = sources_stats

      modules =
        case acc_sources do
          %{^file => source(modules: modules)} -> modules
          %{} -> []
        end

      acc_modules = Enum.reduce(modules, acc_modules, &Map.put(&2, &1, true))
      {Map.put(acc_sources, file, source(size: size, mtime: mtime)), acc_modules}
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
      Enum.reduce(modules, {modules, exports, old_changed, old_stale}, reducer)

    if map_size(new_stale) > map_size(old_stale) or map_size(new_changed) > map_size(old_changed) do
      remove_stale_entries(pending_modules, exports, new_changed, new_stale, reducer)
    else
      {pending_modules, exports, Map.keys(new_changed)}
    end
  end

  defp remove_stale_entry(entry, acc, sources, stale_exports, compile_path) do
    {module, module(sources: source_files, export: export)} = entry
    {pending_modules, exports, changed, stale_modules} = acc

    {compile_references, export_references, runtime_references} =
      Enum.reduce(source_files, {[], [], []}, fn file, {compile_acc, export_acc, runtime_acc} ->
        source(
          compile_references: compile_refs,
          export_references: export_refs,
          runtime_references: runtime_refs
        ) = Map.fetch!(sources, file)

        {compile_acc ++ compile_refs, export_acc ++ export_refs, runtime_acc ++ runtime_refs}
      end)

    cond do
      # If I changed in disk or have a compile time reference to
      # something stale or have a reference to an old export,
      # I need to be recompiled.
      has_any_key?(changed, source_files) or has_any_key?(stale_modules, compile_references) or
          has_any_key?(stale_exports, export_references) ->
        remove_and_purge(beam_path(compile_path, module), module)
        changed = Enum.reduce(source_files, changed, &Map.put(&2, &1, true))

        {Map.delete(pending_modules, module), Map.put(exports, module, export), changed,
         Map.put(stale_modules, module, true)}

      # If I have a runtime references to something stale,
      # I am stale too.
      has_any_key?(stale_modules, runtime_references) ->
        {pending_modules, exports, changed, Map.put(stale_modules, module, true)}

      # Otherwise, we don't store it anywhere
      true ->
        {pending_modules, exports, changed, stale_modules}
    end
  end

  defp has_any_key?(map, enumerable) do
    Enum.any?(enumerable, &Map.has_key?(map, &1))
  end

  defp stale_local_deps(local_deps, manifest, stale_modules, modified, deps_exports) do
    base = Path.basename(manifest)

    # The stale modules so far will become both stale_modules and stale_exports,
    # as any export from a dependency needs to be recompiled.
    stale_modules = Map.from_keys(stale_modules, true)

    for %{app: app, opts: opts} <- local_deps,
        manifest = Path.join([opts[:build], ".mix", base]),
        Mix.Utils.last_modified(manifest) > modified,
        reduce: {stale_modules, stale_modules, deps_exports} do
      {modules, exports, deps_exports} ->
        {manifest_modules, manifest_sources} = read_manifest(manifest)

        dep_modules =
          for {module, module(timestamp: timestamp)} <- manifest_modules,
              timestamp > modified,
              do: module

        # If any module has a compile time dependency on a changed module
        # within the dependency, they will be recompiled. However, export
        # and runtime dependencies won't have recompiled so we need to
        # propagate them to the parent app.
        dep_modules =
          fixpoint_non_compile_modules(manifest_sources, Map.from_keys(dep_modules, true))

        old_exports = Map.get(deps_exports, app, %{})

        # Update exports
        {exports, new_exports} =
          for {module, _} <- dep_modules, reduce: {exports, []} do
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

              # Then we store the new export if any
              new_exports =
                if export,
                  do: [{module, export} | new_exports],
                  else: new_exports

              {exports, new_exports}
          end

        new_exports = Map.new(new_exports)

        removed =
          for {module, _} <- old_exports,
              not is_map_key(new_exports, module),
              do: {module, true},
              into: %{}

        modules = modules |> Map.merge(dep_modules) |> Map.merge(removed)
        exports = Map.merge(exports, removed)
        deps_exports = Map.put(deps_exports, app, new_exports)
        {modules, exports, deps_exports}
    end
  end

  defp fixpoint_non_compile_modules(sources, modules) when modules != %{} do
    fixpoint_non_compile_modules(Map.to_list(sources), modules, false, [])
  end

  defp fixpoint_non_compile_modules(_sources, modules) do
    modules
  end

  defp fixpoint_non_compile_modules(
         [{_source_path, source_entry} = pair | sources],
         modules,
         new?,
         pending_sources
       ) do
    source(export_references: export_refs, runtime_references: runtime_refs) = source_entry

    if has_any_key?(modules, export_refs) or has_any_key?(modules, runtime_refs) do
      new_modules = Enum.reject(source(source_entry, :modules), &Map.has_key?(modules, &1))
      modules = Enum.reduce(new_modules, modules, &Map.put(&2, &1, true))
      new? = new? or new_modules != []
      fixpoint_non_compile_modules(sources, modules, new?, pending_sources)
    else
      pending_sources = [pair | pending_sources]
      fixpoint_non_compile_modules(sources, modules, new?, pending_sources)
    end
  end

  defp fixpoint_non_compile_modules([], modules, new?, pending_sources)
       when new? == false or pending_sources == [],
       do: modules

  defp fixpoint_non_compile_modules([], modules, true, pending_sources),
    do: fixpoint_non_compile_modules(pending_sources, modules, false, [])

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
    :code.purge(module)
    :code.delete(module)
  end

  defp purge_modules_in_path(path) do
    with {:ok, beams} <- File.ls(path) do
      Enum.each(beams, fn beam ->
        module = beam |> Path.rootname() |> String.to_atom()
        :code.purge(module)
        :code.delete(module)
      end)
    end
  end

  defp previous_warnings(sources, print?) do
    for {_, source(compile_warnings: compile_warnings, runtime_warnings: runtime_warnings)} <-
          sources,
        diagnostic <- compile_warnings ++ runtime_warnings do
      if print? do
        Mix.shell().print_app()
        Code.print_diagnostic(diagnostic)
      end

      diagnostic
    end
  end

  defp apply_warnings(sources, [], []) do
    sources
  end

  defp apply_warnings(sources, runtime_warnings, compile_warnings) do
    cwd = File.cwd!()
    runtime_group = Enum.group_by(runtime_warnings, & &1.source)
    compile_group = Enum.group_by(compile_warnings, & &1.source)

    for {source_path, source_entry} <- sources, into: %{} do
      source(
        runtime_warnings: runtime_warnings,
        compile_warnings: compile_warnings,
        external: external
      ) = source_entry

      keys = [
        Path.absname(source_path, cwd)
        | Enum.map(external, &(&1 |> elem(0) |> Path.absname(cwd)))
      ]

      runtime_warnings =
        case Enum.flat_map(keys, &Map.get(runtime_group, &1, [])) do
          [] -> runtime_warnings
          runtime_warnings -> runtime_warnings
        end

      compile_warnings =
        case Enum.flat_map(keys, &Map.get(compile_group, &1, [])) do
          [] -> compile_warnings
          compile_warnings -> compile_warnings
        end

      {source_path,
       source(source_entry,
         runtime_warnings: runtime_warnings,
         compile_warnings: compile_warnings
       )}
    end
  end

  defp diagnostic(
         %{
           file: file,
           position: position,
           message: message,
           severity: severity,
           stacktrace: stacktrace,
           span: span,
           source: source
         } = diagnostic
       ) do
    %Mix.Task.Compiler.Diagnostic{
      file: file,
      source: source,
      position: position,
      message: message,
      severity: severity,
      compiler_name: "Elixir",
      stacktrace: stacktrace,
      span: span,
      details: Map.get(diagnostic, :details, nil)
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

  @default_manifest {%{}, %{}, %{}, [], nil, nil, 0, 0}

  # Similar to read_manifest, but for internal consumption and with data migration support.
  defp parse_manifest(manifest, compile_path) do
    try do
      manifest |> File.read!() |> :erlang.binary_to_term()
    rescue
      _ ->
        @default_manifest
    else
      {@manifest_vsn, modules, sources, local_exports, parent, cache_key, deps_config,
       project_mtime, config_mtime} ->
        {modules, sources, local_exports, parent, cache_key, deps_config, project_mtime,
         config_mtime}

      # {vsn, %{module => record}, sources, ...} v22-?
      # {vsn, [module_record], sources, ...} v5-v21
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
      # If data is a list, we have an old manifest and
      # we convert it to the same format as maps.
      data =
        if is_list(data) do
          for entry <- data, elem(entry, 0) == :module do
            {elem(entry, 1), entry}
          end
        else
          data
        end

      for {module, _} <- data do
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

  defp write_manifest(
         manifest,
         %{} = modules,
         %{} = sources,
         exports,
         parents,
         cache_key,
         deps_config,
         project_mtime,
         config_mtime,
         timestamp
       ) do
    if modules == %{} and sources == %{} do
      File.rm(manifest)
    else
      File.mkdir_p!(Path.dirname(manifest))

      term =
        {@manifest_vsn, modules, sources, exports, parents, cache_key, deps_config, project_mtime,
         config_mtime}

      manifest_data = :erlang.term_to_binary(term, [:compressed])
      File.write!(manifest, manifest_data)
      File.touch!(manifest, timestamp)
      delete_checkpoint(manifest)

      # Since Elixir is a dependency itself, we need to touch the lock
      # so the current Elixir version, used to compile the files above,
      # is properly stored.
      Mix.Dep.ElixirSCM.update()
    end

    :ok
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

  defp modules_diff(compiled_modules, removed_modules, all_modules, timestamp) do
    {changed, added} =
      compiled_modules
      |> Map.keys()
      |> Enum.split_with(&Map.has_key?(all_modules, &1))

    # Note that removed_modules may also include changed modules
    removed =
      for {module, _} <- removed_modules, not Map.has_key?(compiled_modules, module), do: module

    %{
      added: added,
      changed: changed,
      removed: removed,
      timestamp: timestamp
    }
  end

  ## Compiler loop
  # The compiler is invoked in a separate process so we avoid blocking its main loop.

  defp compiler_loop(stale, stale_modules, dest, timestamp, opts, state) do
    ref = make_ref()
    parent = self()
    threshold = opts[:long_compilation_threshold] || 10
    profile = opts[:profile]
    verbose = opts[:verbose] || false

    pid =
      spawn_link(fn ->
        compile_opts = [
          each_cycle: fn ->
            compiler_call(parent, ref, {:each_cycle, stale_modules, dest, timestamp})
          end,
          each_file: fn file, lexical ->
            compiler_call(parent, ref, {:each_file, file, lexical, verbose})
          end,
          each_module: fn file, module, _binary ->
            compiler_call(parent, ref, {:each_module, file, module, System.os_time(:second)})
          end,
          each_long_compilation: fn file ->
            Mix.shell().info(
              "Compiling #{Path.relative_to(file, File.cwd!())} (it's taking more than #{threshold}s)"
            )
          end,
          long_compilation_threshold: threshold,
          profile: profile,
          beam_timestamp: timestamp,
          return_diagnostics: true
        ]

        response = Kernel.ParallelCompiler.compile_to_path(stale, dest, compile_opts)
        send(parent, {ref, response})
      end)

    compiler_loop(ref, pid, state, File.cwd!())
  end

  defp compiler_call(parent, ref, info) do
    send(parent, {ref, info})

    receive do
      {^ref, response} -> response
    end
  end

  defp compiler_loop(ref, pid, state, cwd) do
    receive do
      {^ref, {:each_cycle, stale_modules, dest, timestamp}} ->
        {response, state} = each_cycle(stale_modules, dest, timestamp, state)
        send(pid, {ref, response})
        compiler_loop(ref, pid, state, cwd)

      {^ref, {:each_file, file, lexical, verbose}} ->
        # Read the relevant file information and unblock the compiler
        references = Kernel.LexicalTracker.references(lexical)
        send(pid, {ref, :ok})
        state = each_file(file, references, verbose, state, cwd)
        compiler_loop(ref, pid, state, cwd)

      {^ref, {:each_module, file, module, timestamp}} ->
        # Read the relevant module information and unblock the compiler
        kind = detect_kind(module)
        external = Module.get_attribute(module, :external_resource)
        new_export = exports_md5(module, true)
        send(pid, {ref, :ok})
        state = each_module(file, module, kind, external, new_export, state, timestamp, cwd)
        compiler_loop(ref, pid, state, cwd)

      {^ref, {:ok, _modules, info}} ->
        {:ok, info, state}

      {^ref, {:error, errors, info}} ->
        {:error, errors, info, state}
    end
  end

  defp each_cycle(stale_modules, compile_path, timestamp, state) do
    {modules, _exports, sources, changed, pending_modules, stale_exports} = state

    {pending_modules, exports, changed} =
      update_stale_entries(pending_modules, sources, changed, %{}, stale_exports, compile_path)

    # For each changed file, mark it as changed.
    # If compilation fails mid-cycle, they will be picked next time around.
    for file <- changed do
      File.touch!(file, timestamp)
    end

    if changed == [] do
      # We merge stale_modules (which is a map of %{module => true} that the user changed)
      # into a map of modules we compiled (which is a map of %{module => record}). This is
      # fine because we only care about the keys.
      changed_modules = Map.merge(modules, stale_modules)

      # Now we do a simple pass finding anything that directly depends on the modules that
      # changed. We don't need to compute a fixpoint, because now only the directly affected
      # matter.
      {sources, runtime_modules} =
        Enum.reduce(sources, {sources, []}, fn
          {source_path, source_entry}, {acc_sources, acc_modules} ->
            source(export_references: export_refs, runtime_references: runtime_refs) =
              source_entry

            if has_any_key?(changed_modules, export_refs) or
                 has_any_key?(changed_modules, runtime_refs) do
              acc_sources =
                Map.replace!(acc_sources, source_path, source(source_entry, runtime_warnings: []))

              new_modules =
                Enum.reject(source(source_entry, :modules), &Map.has_key?(changed_modules, &1))

              {acc_sources, new_modules ++ acc_modules}
            else
              {acc_sources, acc_modules}
            end
        end)

      runtime_paths =
        Enum.map(runtime_modules, &{&1, Path.join(compile_path, Atom.to_string(&1) <> ".beam")})

      state = {modules, exports, sources, [], pending_modules, stale_exports}
      {{:runtime, runtime_paths, []}, state}
    else
      Mix.Utils.compiling_n(length(changed), :ex)

      # If we have a compile time dependency to a module, as soon as its file
      # change, we will detect the compile time dependency and recompile. However,
      # the whole goal of pending exports is to delay this decision, so we need to
      # track which modules were removed and start them as our pending exports and
      # remove the pending exports as we notice they have not gone stale.
      {sources, removed_modules} =
        Enum.reduce(changed, {sources, %{}}, fn file, {acc_sources, acc_modules} ->
          source(size: size, digest: digest, modules: modules) = Map.fetch!(acc_sources, file)
          acc_modules = Enum.reduce(modules, acc_modules, &Map.put(&2, &1, true))

          # Define empty records for the sources that needs
          # to be recompiled (but were not changed on disk)
          {Map.replace!(acc_sources, file, source(size: size, digest: digest)), acc_modules}
        end)

      state = {modules, exports, sources, [], pending_modules, removed_modules}
      {{:compile, changed, []}, state}
    end
  end

  defp each_file(file, references, verbose, state, cwd) do
    {compile_references, export_references, runtime_references, compile_env} = references
    {modules, exports, sources, changed, pending_modules, stale_exports} = state

    file = Path.relative_to(file, cwd)

    if verbose do
      Mix.shell().info("Compiled #{file}")
    end

    compile_references =
      Enum.reject(compile_references, &match?("elixir_" <> _, Atom.to_string(&1)))

    source(modules: source_modules) = source = Map.fetch!(sources, file)
    compile_references = compile_references -- source_modules
    export_references = export_references -- source_modules
    runtime_references = runtime_references -- source_modules

    source =
      source(
        source,
        # We preserve the digest if the file is recompiled but not changed
        digest: source(source, :digest) || file |> File.read!() |> digest_contents(),
        compile_references: compile_references,
        export_references: export_references,
        runtime_references: runtime_references,
        compile_env: compile_env
      )

    sources = Map.replace!(sources, file, source)
    {modules, exports, sources, changed, pending_modules, stale_exports}
  end

  defp each_module(file, module, kind, external, new_export, state, timestamp, cwd) do
    {modules, exports, sources, changed, pending_modules, stale_exports} = state

    file = Path.relative_to(file, cwd)
    external = process_external_resources(external, cwd)
    old_export = Map.get(exports, module)

    stale_exports =
      if old_export && old_export != new_export do
        stale_exports
      else
        Map.delete(stale_exports, module)
      end

    module_sources =
      case modules do
        %{^module => module(sources: old_sources)} -> [file | List.delete(old_sources, file)]
        %{} -> [file]
      end

    source =
      Map.get(sources, file) ||
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

    entry =
      module(
        kind: kind,
        sources: module_sources,
        export: new_export,
        timestamp: timestamp,
        recompile?: function_exported?(module, :__mix_recompile__?, 0)
      )

    modules = Map.put(modules, module, entry)
    sources = Map.replace!(sources, file, source)

    # In case the module defined is pending, this is a source conflict.
    # So we need to compile all duplicates.
    changed =
      case pending_modules do
        %{^module => module(sources: sources)} -> sources ++ changed
        %{} -> changed
      end

    {modules, exports, sources, changed, pending_modules, stale_exports}
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

  defp process_external_resources(external, cwd) do
    for file <- external do
      digest =
        case File.read(file) do
          {:ok, binary} -> digest_contents(binary)
          {:error, _} -> nil
        end

      {Path.relative_to(file, cwd), Mix.Utils.last_modified_and_size(file), digest}
    end
  end
end
