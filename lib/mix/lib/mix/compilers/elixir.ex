defmodule Mix.Compilers.Elixir do
  @moduledoc false

  @manifest_vsn :v3

  @doc """
  Compiles stale Elixir files.

  It expects a `manifest` file, the source directories, the source directories to skip,
  the extensions to read in sources, the destination directory, a flag to know if
  compilation is being forced or not and a callback to be invoked
  once (and only if) compilation starts.

  The `manifest` is written down with information including dependencies
  between modules, which helps it recompile only the modules that
  have changed at runtime.
  """
  def compile(manifest, srcs, dest, force, opts) do
    all  = Mix.Utils.extract_files(srcs, [:ex])
    {all_entries, all_sources} = parse_manifest(manifest)

    modified = Mix.Utils.last_modified(manifest)

    removed =
      for {source, _files} <- all_sources,
          not(source in all),
          do: source

    changed =
      if force do
        # A config, path dependency or manifest has
        # changed, let's just compile everything
        all
      else
        sources_mtimes = mtimes(all_sources)

        # Otherwise let's start with the new sources
        for(source <- all,
            not Map.has_key?(all_sources, source),
            do: source)
          ++
        # Plus the sources that have changed in disk
        for({source, files} <- all_sources,
            times = Enum.map([source | files], &Map.fetch!(sources_mtimes, &1)),
            Mix.Utils.stale?(times, [modified]),
            do: source)
      end

    sources = update_stale_sources(all_sources, removed, changed)
    {entries, changed} = update_stale_entries(all_entries, removed ++ changed,
                                              stale_local_deps(manifest, modified))
    stale = changed -- removed

    cond do
      stale != [] ->
        compile_manifest(manifest, entries, sources, stale, dest, opts)
        :ok
      removed != [] ->
        write_manifest(manifest, entries, sources)
        :ok
      true ->
        :noop
    end
  end

  defp mtimes(sources) do
    Enum.reduce(sources, %{}, fn {source, files}, map ->
      Enum.reduce([source | files], map, fn file, map ->
        Map.put_new_lazy(map, file, fn -> Mix.Utils.last_modified(file) end)
      end)
    end)
  end

  @doc """
  Removes compiled files for the given `manifest`.
  """
  def clean(manifest) do
    Enum.each read_manifest(manifest), fn
      {beam, _, _, _, _, _, _} ->
        File.rm(beam)
      {_, _} ->
        :ok
    end
  end

  @doc """
  Returns protocols and implementations for the given `manifest`.
  """
  def protocols_and_impls(manifest) do
    for {beam, module, kind, _, _, _, _} <- read_manifest(manifest),
        match?(:protocol, kind) or match?({:impl, _}, kind),
        do: {module, kind, beam}
  end

  defp compile_manifest(manifest, entries, sources, stale, dest, opts) do
    Mix.Utils.compiling_n(length(stale), :ex)
    Mix.Project.ensure_structure()
    true = Code.prepend_path(dest)

    set_compiler_opts(opts)
    cwd = File.cwd!

    extra =
      if opts[:verbose] do
        [each_file: &each_file/1]
      else
        []
      end

    # Starts a server responsible for keeping track which files
    # were compiled and the dependencies between them.
    {:ok, pid} = Agent.start_link(fn -> {entries, sources} end)

    try do
      _ = Kernel.ParallelCompiler.files :lists.usort(stale),
            [each_module: &each_module(pid, dest, cwd, &1, &2, &3),
             each_long_compilation: &each_long_compilation(&1),
             timeout: 5_000,
             dest: dest] ++ extra
      Agent.cast pid, fn {entries, sources} ->
        write_manifest(manifest, entries, sources)
        {entries, sources}
      end
    after
      Agent.stop(pid, :normal, :infinity)
    end

    :ok
  end

  defp set_compiler_opts(opts) do
    opts = Keyword.take(opts, Code.available_compiler_options)
    opts = Keyword.merge(Mix.Project.config[:elixirc_options] || [], opts)
    Code.compiler_options opts
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

    Agent.cast pid, fn {entries, sources} ->
      entries = List.keystore(entries, beam, 0, {beam, module, kind, source, compile, runtime, binary})
      sources = Map.update(sources, source, files, & files ++ &1)
      {entries, sources}
    end
  end

  defp detect_kind(module) do
    impl = Module.get_attribute(module, :impl)

    cond do
      is_list(impl) and impl[:protocol] ->
        {:impl, impl[:protocol]}
      is_list(Module.get_attribute(module, :protocol)) ->
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

  defp each_file(source) do
    Mix.shell.info "Compiled #{source}"
  end

  defp each_long_compilation(source) do
    Mix.shell.info "Compiling #{source} (it's taking more than 5s)"
  end

  ## Resolution

  defp update_stale_sources(sources, removed, changed) do
    Enum.reduce changed, Map.drop(sources, removed), &Map.put(&2, &1, [])
  end

  # This function receives the manifest entries and some source
  # files that have changed. It then, recursively, figures out
  # all the files that changed (via the module dependencies) and
  # return the non-changed entries and the removed sources.
  defp update_stale_entries(all, [], stale) when stale == %{} do
    {all, []}
  end

  defp update_stale_entries(all, changed, stale) do
    removed = Enum.into(changed, %{}, &{&1, true})
    remove_stale_entries(all, stale, removed)
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

  defp remove_stale_entry({beam, module, _kind, source, compile, runtime, _bin} = entry,
                          {rest, stale, removed}) do
    cond do
      # If I changed in disk or have a compile time dependency
      # on something stale, I need to be recompiled.
      Map.has_key?(removed, source) or Enum.any?(compile, &Map.has_key?(stale, &1)) ->
        remove_and_purge(beam, module)
        {rest, Map.put(stale, module, true), Map.put(removed, source, true)}

      # If I have a runtime time dependency on something stale,
      # I am stale too.
      Enum.any?(runtime, &Map.has_key?(stale, &1)) ->
        {[entry | rest], Map.put(stale, module, true), removed}

      # Otherwise, we don't store it anywhere
      true ->
        {[entry | rest], stale, removed}
    end
  end

  defp stale_local_deps(manifest, modified) do
    base = Path.basename(manifest)
    for %{scm: scm, opts: opts} = dep <- Mix.Dep.cached(),
        not scm.fetchable?,
        Mix.Utils.last_modified(Path.join(opts[:build], base)) > modified,
        path <- Mix.Dep.load_paths(dep),
        beam <- Path.wildcard(Path.join(path, "*.beam")),
        Mix.Utils.last_modified(beam) > modified,
        do: {beam |> Path.basename |> Path.rootname |> String.to_atom, true},
        into: %{}
  end

  defp remove_and_purge(beam, module) do
    _ = File.rm(beam)
    _ = :code.purge(module)
    _ = :code.delete(module)
  end

  ## Manifest handling

  defp read_manifest(manifest) do
    case :file.consult(manifest) do
      {:ok, [@manifest_vsn | t]} -> t
      _ -> []
    end
  end

  # Similar to read manifest but supports data migration.
  defp parse_manifest(manifest) do
    state = {[], %{}}

    case :file.consult(manifest) do
      {:ok, [@manifest_vsn | data]} ->
        parse_manifest(data, state)
      {:ok, [:v2 | data]} ->
        for {beam, module, _, _, _, _, _, _} <- data do
          remove_and_purge(beam, module)
        end
        state
      _ ->
        state
    end
  end

  defp parse_manifest(data, state) do
    Enum.reduce data, state, fn
      entry, {entries, sources} when tuple_size(entry) == 7 ->
        {[entry | entries], sources}
      {source, files}, {entries, sources} ->
        {entries, Map.put(sources, source, files)}
    end
  end

  defp write_manifest(manifest, [], sources) when sources == %{} do
    File.rm(manifest)
    :ok
  end

  defp write_manifest(manifest, entries, sources) do
    File.mkdir_p!(Path.dirname(manifest))

    File.open!(manifest, [:write], fn device ->
      :io.format(device, '~p.~n', [@manifest_vsn])

      Enum.each entries, fn {beam, _, _, _, _, _, binary} = entry ->
        if binary, do: File.write!(beam, binary)
        :io.format(device, '~p.~n', [put_elem(entry, 6, nil)])
      end

      Enum.each sources, fn {_, _} = entry ->
        :io.format(device, '~p.~n', [entry])
      end

      :ok
    end)

    # Since Elixir is a dependency itself, we need to touch the lock
    # so the current Elixir version, used to compile the files above,
    # is properly stored.
    Mix.Dep.ElixirSCM.update
  end
end
