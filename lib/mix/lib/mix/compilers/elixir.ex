defmodule Mix.Compilers.Elixir do
  @moduledoc false

  @manifest_vsn :v4

  import Record

  defrecordp :module, [:module, :kind, :source, :beam, :binary]

  defrecordp :file, [:file, :compile, :runtime]

  defrecordp :source, [:source, :files]

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
    {all_modules, all_files, all_sources} = parse_manifest(manifest)

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

    {modules, files, changed} =
      update_stale_entries(
        all_modules,
        all_files,
        removed ++ changed,
        stale_local_deps(manifest, modified)
      )

    stale = changed -- removed

    cond do
      stale != [] ->
        compile_manifest(manifest, modules, files, sources, stale, dest, opts)
        :ok
      removed != [] ->
        write_manifest(manifest, modules, files, sources)
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
      module(beam: beam) ->
        File.rm(beam)
      _ ->
        :ok
    end
  end

  @doc """
  Returns protocols and implementations for the given `manifest`.
  """
  def protocols_and_impls(manifest) do
    for module(beam: beam, module: module, kind: kind) <- read_manifest(manifest),
        match?(:protocol, kind) or match?({:impl, _}, kind),
        do: {module, kind, beam}
  end

  defp compile_manifest(manifest, modules, files, sources, stale, dest, opts) do
    Mix.Utils.compiling_n(length(stale), :ex)

    config = Mix.Project.config()
    Mix.Project.ensure_structure(config)
    true = Code.prepend_path(dest)

    opts = Keyword.merge(config[:elixirc_options] || [], opts)
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
    {:ok, pid} = Agent.start_link(fn -> {modules, files, sources} end)
    long_compilation_threshold = opts[:long_compilation_threshold] || 5

    try do
      _ = Kernel.ParallelCompiler.files stale,
            [each_module: &each_module(pid, dest, cwd, &1, &2, &3),
             each_long_compilation: &each_long_compilation(&1, long_compilation_threshold),
             long_compilation_threshold: long_compilation_threshold,
             dest: dest] ++ extra
      Agent.cast pid, fn {modules, files, sources} ->
        write_manifest(manifest, modules, files, sources)
        {modules, files, sources}
      end
    after
      Agent.stop(pid, :normal, :infinity)
    end

    :ok
  end

  defp set_compiler_opts(opts) do
    opts
    |> Keyword.take(Code.available_compiler_options)
    |> Code.compiler_options()
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
    source_files  = get_external_resources(module, cwd)

    Agent.cast pid, fn {modules, files, sources} ->
      new_module =
        module(
          module: module,
          kind: kind,
          source: source,
          beam: beam,
          binary: binary
        )
      modules = List.keystore(modules, module, 1, new_module)

      new_file = file(file: source, compile: compile, runtime: runtime)
      files = List.keystore(files, source, 1, new_file)

      sources = Map.update(sources, source, source_files, & source_files ++ &1)

      {modules, files, sources}
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

  defp each_long_compilation(source, threshold) do
    Mix.shell.info "Compiling #{source} (it's taking more than #{threshold}s)"
  end

  ## Resolution

  defp update_stale_sources(sources, removed, changed) do
    Enum.reduce changed, Map.drop(sources, removed), &Map.put(&2, &1, [])
  end

  # This function receives the manifest entries and some source
  # files that have changed. It then, recursively, figures out
  # all the files that changed (via the module dependencies) and
  # return the non-changed entries and the removed sources.
  defp update_stale_entries(modules, files, [], stale) when stale == %{} do
    {modules, files, []}
  end

  defp update_stale_entries(modules, files, changed, stale) do
    removed = Enum.into(changed, %{}, &{&1, true})
    remove_stale_entries(modules, files, stale, removed)
  end

  defp remove_stale_entries(modules, files, old_stale, old_removed) do
    file_modules =
      Enum.reduce modules, %{}, fn module(source: source) = module, file_modules ->
        file = Enum.find(files, & file(&1, :file) == source)
        Map.update(file_modules, file, [module], &[module | &1])
      end

    {rest_modules, rest_files, new_stale, new_removed} =
      Enum.reduce file_modules, {[], [], old_stale, old_removed}, &remove_stale_entry/2

    if map_size(new_stale) > map_size(old_stale) or
       map_size(new_removed) > map_size(old_removed) do
      remove_stale_entries(rest_modules, rest_files, new_stale, new_removed)
    else
      {rest_modules, rest_files, Map.keys(new_removed)}
    end
  end

  defp remove_stale_entry({file, modules}, acc) do
    file(file: source, compile: compile, runtime: runtime) = file
    {rest_modules, rest_files, stale, removed} = acc

    cond do
      # If I changed in disk or have a compile time dependency
      # on something stale, I need to be recompiled.
      Map.has_key?(removed, source) or Enum.any?(compile, &Map.has_key?(stale, &1)) ->
        stale =
          Enum.reduce modules, stale, fn module(beam: beam, module: module), stale ->
            remove_and_purge(beam, module)

            Map.put(stale, module, true)
          end

        removed = Map.put(removed, source, true)
        {rest_modules, rest_files, stale, removed}

      # If I have a runtime time dependency on something stale,
      # I am stale too.
      Enum.any?(runtime, &Map.has_key?(stale, &1)) ->
        rest_modules = modules ++ rest_modules
        rest_files = [file | rest_files]
        stale = Enum.reduce(modules, stale, &Map.put(&2, &1, true))
        {rest_modules, rest_files, stale, removed}

      # Otherwise, we don't store it anywhere
      true ->
        {modules ++ rest_modules, [file | rest_files], stale, removed}
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
    state = {[], [], %{}}

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
      module() = module, {modules, files, sources} ->
        {[module | modules], files, sources}
      file() = file, {modules, files, sources} ->
        {modules, [file | files], sources}
      source(source: source, files: source_files), {modules, files, sources} ->
        {modules, files, Map.put(sources, source, source_files)}
    end
  end

  defp write_manifest(manifest, [], [], sources) when sources == %{} do
    File.rm(manifest)
    :ok
  end

  defp write_manifest(manifest, modules, files, sources) do
    File.mkdir_p!(Path.dirname(manifest))

    File.open!(manifest, [:write], fn device ->
      :io.format(device, '~p.~n', [@manifest_vsn])

      Enum.each modules, fn module(beam: beam, binary: binary) = module ->
        if binary, do: File.write!(beam, binary)
        :io.format(device, '~p.~n', [module(module, binary: nil)])
      end

      Enum.each files, fn file ->
        :io.format(device, '~p.~n', [file])
      end

      Enum.each sources, fn {source, files} ->
        :io.format(device, '~p.~n', [source(source: source, files: files)])
      end

      :ok
    end)

    # Since Elixir is a dependency itself, we need to touch the lock
    # so the current Elixir version, used to compile the files above,
    # is properly stored.
    Mix.Dep.ElixirSCM.update
  end
end
