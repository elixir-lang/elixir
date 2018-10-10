defmodule Mix.Compilers.Test do
  @moduledoc false

  require Mix.Compilers.Elixir, as: CE

  import Record

  defrecordp :source,
    source: nil,
    compile_references: [],
    runtime_references: [],
    external: []

  @stale_manifest "compile.test_stale"
  @manifest_vsn 1

  @doc """
  Requires and runs test files.

  It expects all of the test patterns, the test files that were matched for the
  test patterns, the test paths, and the opts from the test task.
  """
  def require_and_run(matched_test_files, test_paths, opts) do
    stale = opts[:stale]

    {test_files, stale_manifest_pid, parallel_require_callbacks} =
      if stale do
        set_up_stale(matched_test_files, test_paths, opts)
      else
        {matched_test_files, nil, []}
      end

    if test_files == [] do
      :noop
    else
      task = Task.async(ExUnit, :run, [])

      try do
        case Kernel.ParallelCompiler.require(test_files, parallel_require_callbacks) do
          {:ok, _, _} -> :ok
          {:error, _, _} -> exit({:shutdown, 1})
        end

        ExUnit.Server.modules_loaded()
        %{failures: failures} = results = Task.await(task, :infinity)

        if failures == 0 do
          agent_write_manifest(stale_manifest_pid)
        end

        {:ok, results}
      catch
        kind, reason ->
          # In case there is an error, shut down the runner task
          # before the error propagates up and trigger links.
          Task.shutdown(task)
          :erlang.raise(kind, reason, __STACKTRACE__)
      after
        agent_stop(stale_manifest_pid)
      end
    end
  end

  defp set_up_stale(matched_test_files, test_paths, opts) do
    manifest = manifest()
    modified = Mix.Utils.last_modified(manifest)
    all_sources = read_manifest()

    removed =
      for source(source: source) <- all_sources, source not in matched_test_files, do: source

    config_mtime = Mix.Project.config_mtime()
    test_helpers = Enum.map(test_paths, &Path.join(&1, "test_helper.exs"))
    force = opts[:force] || Mix.Utils.stale?([config_mtime | test_helpers], [modified])

    changed =
      if force do
        # Let's just require everything
        matched_test_files
      else
        sources_mtimes = mtimes(all_sources)

        # Otherwise let's start with the new sources
        # Plus the sources that have changed in disk
        for(
          source <- matched_test_files,
          not List.keymember?(all_sources, source, source(:source)),
          do: source
        ) ++
          for(
            source(source: source, external: external) <- all_sources,
            times = Enum.map([source | external], &Map.fetch!(sources_mtimes, &1)),
            Mix.Utils.stale?(times, [modified]),
            do: source
          )
      end

    stale = MapSet.new(changed -- removed)
    sources = update_stale_sources(all_sources, removed, changed)

    test_files_to_run =
      sources
      |> tests_with_changed_references()
      |> MapSet.union(stale)
      |> MapSet.to_list()

    if test_files_to_run == [] do
      write_manifest(sources)
      {[], nil, nil}
    else
      {:ok, pid} = Agent.start_link(fn -> sources end)
      cwd = File.cwd!()
      parallel_require_callbacks = [each_module: &each_module(pid, cwd, &1, &2, &3)]
      {test_files_to_run, pid, parallel_require_callbacks}
    end
  end

  defp agent_write_manifest(nil), do: :noop

  defp agent_write_manifest(pid) do
    Agent.cast(pid, fn sources ->
      write_manifest(sources)
      sources
    end)
  end

  defp agent_stop(nil), do: :noop

  defp agent_stop(pid) do
    Agent.stop(pid, :normal, :infinity)
  end

  ## Setup helpers

  defp mtimes(sources) do
    Enum.reduce(sources, %{}, fn source(source: source, external: external), map ->
      Enum.reduce([source | external], map, fn file, map ->
        Map.put_new_lazy(map, file, fn -> Mix.Utils.last_modified(file) end)
      end)
    end)
  end

  defp update_stale_sources(sources, removed, changed) do
    sources = Enum.reject(sources, fn source(source: source) -> source in removed end)

    sources =
      Enum.reduce(changed, sources, &List.keystore(&2, &1, source(:source), source(source: &1)))

    sources
  end

  ## Manifest

  defp manifest, do: Path.join(Mix.Project.manifest_path(), @stale_manifest)

  defp read_manifest() do
    try do
      [@manifest_vsn | sources] = manifest() |> File.read!() |> :erlang.binary_to_term()
      sources
    rescue
      _ -> []
    end
  end

  defp write_manifest([]) do
    File.rm(manifest())
    :ok
  end

  defp write_manifest(sources) do
    manifest = manifest()
    File.mkdir_p!(Path.dirname(manifest))

    manifest_data = :erlang.term_to_binary([@manifest_vsn | sources], [:compressed])
    File.write!(manifest, manifest_data)
  end

  ## Test changed dependency resolution

  defp tests_with_changed_references(test_sources) do
    test_manifest = manifest()
    [elixir_manifest] = Mix.Tasks.Compile.Elixir.manifests()

    if Mix.Utils.stale?([elixir_manifest], [test_manifest]) do
      elixir_manifest_entries =
        CE.read_manifest(elixir_manifest, Mix.Project.compile_path())
        |> Enum.group_by(&elem(&1, 0))

      stale_modules =
        for CE.module(module: module, beam: beam) <- elixir_manifest_entries.module,
            Mix.Utils.stale?([beam], [test_manifest]),
            do: module,
            into: MapSet.new()

      stale_modules =
        find_all_dependent_on(
          stale_modules,
          elixir_manifest_entries.source,
          elixir_manifest_entries.module
        )

      for module <- stale_modules,
          source(source: source, runtime_references: r, compile_references: c) <- test_sources,
          module in r or module in c,
          do: source,
          into: MapSet.new()
    else
      MapSet.new()
    end
  end

  defp find_all_dependent_on(modules, sources, all_modules, resolved \\ MapSet.new()) do
    new_modules =
      for module <- modules,
          module not in resolved,
          dependent_module <- dependent_modules(module, all_modules, sources),
          do: dependent_module,
          into: modules

    if MapSet.size(new_modules) == MapSet.size(modules) do
      new_modules
    else
      find_all_dependent_on(new_modules, sources, all_modules, modules)
    end
  end

  defp dependent_modules(module, modules, sources) do
    for CE.source(source: source, runtime_references: r, compile_references: c) <- sources,
        module in r or module in c,
        CE.module(sources: sources, module: dependent_module) <- modules,
        source in sources,
        do: dependent_module
  end

  ## ParallelRequire callback

  defp each_module(pid, cwd, source, module, _binary) do
    {compile_references, struct_references, runtime_references} =
      Kernel.LexicalTracker.remote_references(module)

    external = get_external_resources(module, cwd)
    source = Path.relative_to(source, cwd)

    Agent.cast(pid, fn sources ->
      external =
        case List.keyfind(sources, source, source(:source)) do
          source(external: old_external) -> external ++ old_external
          nil -> external
        end

      new_source =
        source(
          source: source,
          compile_references: compile_references ++ struct_references,
          runtime_references: runtime_references,
          external: external
        )

      List.keystore(sources, source, source(:source), new_source)
    end)
  end

  defp get_external_resources(module, cwd) do
    for file <- Module.get_attribute(module, :external_resource), do: Path.relative_to(file, cwd)
  end
end
