defmodule Mix.Compilers.Elixir do
  @moduledoc false

  @manifest_vsn :v2

  @doc """
  Compiles stale Elixir files.

  It expects a manifest file, the source directories, the extensions
  to read in sources, the destination directory, a flag to know if
  compilation is being forced or not and a callback to be invoked
  once (and only if) compilation starts.

  The manifest is written down with information including dependencies
  between modules, which helps it recompile only the modules that
  have changed at runtime.
  """
  def compile(manifest_map, srcs, skip, exts, dest, force, on_start) do
    keep = srcs -- skip

    universe_files_map = for(path <- srcs,
      files = Mix.Utils.extract_files([path], exts),
      into: %{},
      do: {path, files}) |> dedupe_files_map

    all_files_map = Map.take(universe_files_map, keep)

    all = for {_, files} <- all_files_map,
      file <- files,
      do: file

    keep_manifest_map = Map.take(manifest_map, keep)

    all_entries = entries_from_manifest_map(keep_manifest_map)

    universe_entries = entries_from_manifest_map(manifest_map)

    universe_file_manifest = for {_, manifest} <- manifest_map,
                          file <- read_manifest(manifest),
                          into: %{},
                          do: {elem(file,3), manifest}

    universe_module_map = for {_, module, _, file, _, _, _, _} <- universe_entries,
                          into: %{},
                          do: {module, file}

    file_manifest_path_map = for {path, files} <- all_files_map,
                          file <- files,
                          manifest = Map.fetch!(manifest_map, path),
                          into: %{},
                          do: {file, {path, manifest}}

    file_manifest_modified_map = for {file, {_, manifest}} <- file_manifest_path_map,
                          mtime = Mix.Utils.last_modified(manifest),
                          into: %{},
                          do: {file, mtime}

    removed =
      for {_b, _m, _k, source, _cd, _rd, _f, _bin} <- all_entries,
          not(source in all),
          do: source

    changed =
      if force do
        # A config, path dependency or manifest has
        # changed, let's just compile everything
        all
      else
        all_mtimes = mtimes(universe_entries)

        # Otherwise let's start with the new ones
        # plus the ones that have changed
        new = for(source <- all,
            not Enum.any?(all_entries, fn {_b, _m, _k, s, _cd, _rd, _f, _bin} -> s == source end),
            do: source)
        modified = for({_b, _m, _k, source, compile, _rd, files, _bin} <- all_entries,
            not(source in removed),
            source_mtimes = Enum.map([source|files], &Map.fetch!(all_mtimes, &1)),
            # Some compile time dependencies might be outside of `elixirc_paths`
            compile_dependency_manifests_mtimes = for(
              {module, file} <- universe_module_map,
              module in compile,
              manifest = Dict.fetch!(universe_file_manifest, file),
              do: Mix.Utils.last_modified(manifest)),
            times = source_mtimes ++ compile_dependency_manifests_mtimes,
            Mix.Utils.stale?(times, [Map.fetch!(file_manifest_modified_map, source)]),
            do: source)
        new ++ modified
      end

    {entries, changed} = remove_stale_entries(all_entries, removed ++ changed)
    stale = changed -- removed

    cond do
      stale != [] ->
        compile_manifest({keep_manifest_map, file_manifest_path_map}, entries, stale, dest, on_start)
        :ok
      removed != [] ->
        write_manifests({keep_manifest_map, file_manifest_path_map}, entries)
        :ok
      true ->
        :noop
    end
  end

  defp mtimes(entries) do
    Enum.reduce(entries, %{}, fn {_b, _m, _k, source, _cd, _rd, files, _bin}, dict ->
      Enum.reduce([source|files], dict, fn file, dict ->
        if Map.has_key?(dict, file) do
          dict
        else
          Map.put(dict, file, Mix.Utils.last_modified(file))
        end
      end)
    end)
  end

  @doc """
  Removes compiled files.
  """
  def clean(manifest) do
    Enum.map read_manifest(manifest), fn {beam, _, _, _, _, _, _, _} ->
      File.rm(beam)
    end
    :ok
  end

  @doc """
  Returns protocols and implementations for the given manifest.
  """
  def protocols_and_impls(manifests) do
    for manifest <- manifests,
      {_, module, kind, _, _, _, _, _} <- read_manifest(manifest),
        match?(:protocol, kind) or match?({:impl, _}, kind),
        do: {module, kind}
  end

  defp compile_manifest(file_manifest_path_map, entries, stale, dest, on_start) do
    Mix.Project.ensure_structure()
    true = Code.prepend_path(dest)

    on_start.()
    cwd = File.cwd!

    # Starts a server responsible for keeping track which files
    # were compiled and the dependencies between them.
    {:ok, pid} = Agent.start_link(fn -> entries end)

    try do
      _ = Kernel.ParallelCompiler.files :lists.usort(stale),
        each_module: &each_module(pid, dest, cwd, &1, &2, &3),
        each_file: &each_file(&1),
        dest: dest
      Agent.cast pid, fn entries ->
        write_manifests(file_manifest_path_map, entries)
        entries
      end
    after
      Agent.stop(pid, :normal, :infinity)
    end

    :ok
  end

  defp each_module(pid, dest, cwd, source, module, binary) do
    beam =
      dest
      |> Path.join(Atom.to_string(module) <> ".beam")
      |> Path.relative_to(cwd)

    {compile, runtime} = Kernel.LexicalTracker.remotes(module)

    compile =
      compile
      |> List.delete(module)
      |> Enum.reject(&match?("elixir_" <> _, Atom.to_string(&1)))

    runtime =
      runtime
      |> List.delete(module)
      |> Enum.reject(&match?("elixir_" <> _, Atom.to_string(&1)))

    kind   = detect_kind(module)
    source = Path.relative_to(source, cwd)
    files  = get_external_resources(module, cwd)
    tuple  = {beam, module, kind, source, compile, runtime, files, binary}
    Agent.cast pid, &:lists.keystore(beam, 1, &1, tuple)
  end

  defp detect_kind(module) do
    cond do
      impl = Module.get_attribute(module, :impl) ->
        {:impl, impl[:protocol]}
      Module.get_attribute(module, :protocol) ->
        :protocol
      true ->
        :module
    end
  end

  defp get_external_resources(module, cwd) do
    for file <- Module.get_attribute(module, :external_resource),
        File.regular?(file),
        relative = Path.relative_to(file, cwd),
        Path.type(relative) == :relative,
        do: relative
  end

  defp each_file(file) do
    Mix.shell.info "Compiled #{file}"
  end

  ## Resolution

  # This function receives the manifest entries and some source
  # files that have changed. It then, recursively, figures out
  # all the files that changed (via the module dependencies) and
  # return the non-changed entries and the removed sources.
  defp remove_stale_entries(all, []) do
    {all, []}
  end

  defp remove_stale_entries(all, changed) do
    remove_stale_entries(all, %{}, Enum.into(changed, %{}, &{&1, true}))
  end

  defp remove_stale_entries(entries, old_stale, old_removed) do
    {rest, new_stale, new_removed} =
      Enum.reduce entries, {[], old_stale, old_removed}, &remove_stale_entry/2

    if map_size(new_stale) > map_size(old_stale) or
       map_size(new_removed) > map_size(old_removed) do
      remove_stale_entries(rest, new_stale, new_removed)
    else
      {rest, Map.keys(new_removed)}
    end
  end

  defp remove_stale_entry({beam, module, _kind, source, compile, runtime, _f, _bin} = entry,
                          {rest, stale, removed}) do
    cond do
      # If I changed in disk or have a compile time dependency
      # on something stale, I need to be recompiled.
      Map.has_key?(removed, source) or Enum.any?(compile, &Map.has_key?(stale, &1)) ->
        _ = File.rm(beam)
        _ = :code.purge(module)
        _ = :code.delete(module)
        {rest, Map.put(stale, module, true), Map.put(removed, source, true)}

      # If I have a runtime time dependency on something stale,
      # I am stale too.
      Enum.any?(runtime, &Map.has_key?(stale, &1)) ->
        {[entry|rest], Map.put(stale, module, true), removed}

      # Otherwise, we don't store it anywhere
      true ->
        {[entry|rest], stale, removed}
    end
  end

  ## Manifest handling

  defp read_manifest(manifest) do
    case :file.consult(manifest) do
      {:ok, [@manifest_vsn|t]} -> t
      _ -> []
    end
  end

  defp write_manifests({manifest_map, file_manifest_path_map}, entries) do
    files_by_manifest = Enum.group_by(entries, fn (entry) ->
      Map.fetch!(file_manifest_path_map, elem(entry,3)) |> elem(1)
    end)
    for {_, manifest} <- manifest_map,
        entries = Map.get(files_by_manifest, manifest, []),
        do: write_manifest(manifest, entries)
  end

  defp write_manifest(manifest, entries) do
    File.mkdir_p!(Path.dirname(manifest))

    File.open!(manifest, [:write], fn device ->
      :io.format(device, '~p.~n', [@manifest_vsn])

      Enum.map entries, fn {beam, _, _, _, _, _, _, binary} = entry ->
        if binary, do: File.write!(beam, binary)
        :io.format(device, '~p.~n', [put_elem(entry, 7, nil)])
      end

      :ok
    end)

    # Since Elixir is a dependency itself, we need to touch the lock
    # so the current Elixir version, used to compile the files above,
    # is properly stored.
    Mix.Dep.ElixirSCM.update
  end

  defp entries_from_manifest_map(manifest_map) do
    for {_, manifest} <- manifest_map,
        entry <- read_manifest(manifest),
        do: entry
  end

  # Since we can have nested paths, we might have duplicates here.
  defp dedupe_files_map(map) do
    dupes = for {path, files} <- map,
        {path2, files2} <- map,
        file <- files,
        file2 <- files2,
        file == file2, # same file
        String.length(path) < String.length(path2), # dupe if there's a path that's longer
        do: {path, file}
    Enum.reduce(dupes, map, fn {path, file}, map ->
      Map.update!(map, path, &List.delete(&1, file))
    end)
  end
end
