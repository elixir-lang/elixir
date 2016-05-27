defmodule Mix.Tasks.Test.Stale do
  @moduledoc false

  require Mix.Compilers.Elixir
  alias Mix.Compilers.Elixir, as: CE

  import Record

  defrecordp :source, [
    source: nil,
    compile_references: [],
    runtime_references: [],
    external: []
  ]

  @stale_manifest ".compile.test_stale"
  @manifest_vsn :v1


  @doc """
  Set up the `--stale` run for given matched test files, test paths, and options.

  If there are tests to run, a tuple containing the test files to run, the PID
  of the agent used in the each_module callback, and the callback options for
  ParallelRequire is returned.

  If there are no tests to run, the pid and callbacks are not returned, since the
  test task should not perform a ParallelRequire if there are no tests to run.

  The test task is responsible for running the returned tests, using the returned
  ParallelRequire callbacks, calling the agent_write_manifest/1 function with the
  provided PID after performing the ParallelRequire sucessfully, and calling the
  agent_stop/1 function with the same PID after the ParallelRequire, whether it
  succeeds or not.
  """
  def set_up(matched_test_files, test_paths, opts) do
    manifest = manifest()
    modified = Mix.Utils.last_modified(manifest)
    all_sources = read_manifest()

    removed =
      for source(source: source) <- all_sources,
          not(source in matched_test_files),
          do: source

    configs = Mix.Project.config_files
    force = opts[:force] || Mix.Utils.stale?(configs, [manifest]) || test_helper_stale?(test_paths)

    changed =
      if force do
        # let's just require everything
        matched_test_files
      else
        sources_mtimes = mtimes(all_sources)

        # Otherwise let's start with the new sources
        for(source <- matched_test_files,
            not List.keymember?(all_sources, source, source(:source)),
            do: source)
          ++
        # Plus the sources that have changed in disk
        for(source(source: source, external: external) <- all_sources,
            times = Enum.map([source | external], &Map.fetch!(sources_mtimes, &1)),
            Mix.Utils.stale?(times, [modified]),
            do: source)
      end

    stale   = MapSet.new(changed -- removed)
    sources = update_stale_sources(all_sources, removed, changed)

    test_files_to_run =
      sources
      |> tests_with_changed_references()
      |> MapSet.union(stale)
      |> MapSet.to_list()

    if test_files_to_run == [] do
      write_manifest(sources)
      Mix.shell.info "mix test --stale did not find any tests to run, add --force if you want to run all tests"

      {[], nil, nil}
    else
      {:ok, pid} = Agent.start_link(fn -> sources end)

      cwd = File.cwd!()
      parallel_require_callbacks = [each_module: &each_module(pid, cwd, &1, &2, &3)]

      {test_files_to_run, pid, parallel_require_callbacks}
    end
  end

  @doc """
  Should be called with the PID returned from `set_up`, after a successful
  ParallelRequire.
  """
  def agent_write_manifest(nil),
    do: :noop

  def agent_write_manifest(pid) do
    Agent.cast pid, fn sources ->
      write_manifest(sources)
      sources
    end
  end

  @doc """
  Should be called with the PID returned from `set_up`, after the ParallelRequire
  is complete, whether it succeeded or not.
  """
  def agent_stop(nil),
    do: :noop

  def agent_stop(pid) do
    Agent.stop(pid, :normal, :infinity)
  end

  ## Setup helpers

  defp test_helper_stale?(test_paths) do
    test_paths
    |> Stream.map(&Path.join(&1, "test_helper.exs"))
    |> Mix.Utils.stale?([manifest()])
  end

  defp mtimes(sources) do
    Enum.reduce(sources, %{}, fn source(source: source, external: external), map ->
      Enum.reduce([source | external], map, fn file, map ->
        Map.put_new_lazy(map, file, fn -> Mix.Utils.last_modified(file) end)
      end)
    end)
  end

  defp update_stale_sources(sources, removed, changed) do
    sources =
      Enum.reject(sources, fn source(source: source) -> source in removed end)
    sources =
      Enum.reduce(changed, sources, &List.keystore(&2, &1, source(:source), source(source: &1)))
    sources
  end

  ## Manifest

  defp manifest, do: Path.join(Mix.Project.manifest_path, @stale_manifest)

  defp read_manifest() do
    try do
      manifest() |> File.read!() |> :erlang.binary_to_term()
    else
      [@manifest_vsn | t] -> t
      _ -> []
    rescue
      _ -> []
    end
  end

  defp write_manifest([]) do
    manifest()
    |> File.rm()

    :ok
  end

  defp write_manifest(sources) do
    manifest = manifest()

    manifest
    |> Path.dirname()
    |> File.mkdir_p!()

    manifest_data =
      [@manifest_vsn | sources]
      |> :erlang.term_to_binary(compressed: 9)

    File.write!(manifest, manifest_data)
  end

  ## Test changed dependency resolution

  defp tests_with_changed_references(test_sources) do
    test_manifest = manifest()
    [elixir_manifest] = Mix.Tasks.Compile.Elixir.manifests()

    if Mix.Utils.stale?([elixir_manifest], [test_manifest]) do
      elixir_manifest_entries =
        Mix.Compilers.Elixir.read_manifest(elixir_manifest)
        |> Enum.group_by(&elem(&1, 0))

      stale_modules =
        for CE.module(module: module, beam: beam) <- elixir_manifest_entries.module,
            Mix.Utils.stale?([beam], [test_manifest]),
            do: module,
            into: MapSet.new()

      stale_modules = find_all_dependant_on(stale_modules, elixir_manifest_entries.source, elixir_manifest_entries.module)

      for module <- stale_modules,
          source(source: source, runtime_references: r, compile_references: c) <- test_sources,
          module in r ++ c,
          do: source,
          into: MapSet.new()
    else
      MapSet.new()
    end
  end

  defp find_all_dependant_on(modules, sources, all_modules, resolved \\ MapSet.new())

  defp find_all_dependant_on(modules, _sources, _all_modules, modules),
    do: modules

  defp find_all_dependant_on(modules, sources, all_modules, resolved) do
    new_modules =
      for module <- modules,
          not module in resolved,
          dependant_module <- dependant_modules(module, all_modules, sources),
          do: dependant_module,
          into: modules

    find_all_dependant_on(new_modules, sources, all_modules, modules)
  end

  defp dependant_modules(module, modules, sources) do
    for CE.source(source: source, runtime_references: r, compile_references: c) <- sources,
        module in r ++ c,
        CE.module(source: ^source, module: dependant_module) <- modules,
        do: dependant_module
  end

  ## ParallelRequire callback

  defp each_module(pid, cwd, source, module, _binary) do
    {compile_references, runtime_references} = Kernel.LexicalTracker.remote_references(module)
    external = get_external_resources(module, cwd)
    source = Path.relative_to(source, cwd)

    Agent.cast pid, fn sources ->
      external =
        case List.keyfind(sources, source, source(:source)) do
          source(external: old_external) -> external ++ old_external
          nil -> external
        end

      new_source = source(
        source: source,
        compile_references: compile_references,
        runtime_references: runtime_references,
        external: external
      )

      List.keystore(sources, source, source(:source), new_source)
    end
  end

  defp get_external_resources(module, cwd) do
    for file <- Module.get_attribute(module, :external_resource),
        File.regular?(file),
        relative = Path.relative_to(file, cwd),
        Path.type(relative) == :relative,
        do: relative
  end
end
