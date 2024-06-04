defmodule Mix.Compilers.Test do
  @moduledoc false

  require Mix.Compilers.Elixir, as: CE

  import Record

  defrecordp :source,
    compile_references: [],
    runtime_references: [],
    external: []

  # Necessary to avoid warnings during bootstrap
  @compile {:no_warn_undefined, ExUnit}
  @stale_manifest "compile.test_stale"
  @manifest_vsn 2

  @doc """
  Requires and runs test files.

  It expects all of the test patterns, the test files that were matched for the
  test patterns, the test paths, and the opts from the test task.
  """
  def require_and_run(matched_test_files, test_paths, elixirc_opts, opts) do
    elixirc_opts = Keyword.merge([docs: false, debug_info: false], elixirc_opts)
    previous_opts = Code.compiler_options(elixirc_opts)

    try do
      require_and_run(matched_test_files, test_paths, opts)
    after
      Code.compiler_options(previous_opts)
    end
  end

  defp require_and_run(matched_test_files, test_paths, opts) do
    stale = opts[:stale]
    max_requires = opts[:max_requires]

    {test_files, stale_manifest_pid, parallel_require_callbacks} =
      if stale do
        set_up_stale(matched_test_files, test_paths, opts)
      else
        {matched_test_files, nil, []}
      end

    shared_require_options =
      if max_requires do
        [max_concurrency: max_requires]
      else
        []
      end

    Application.ensure_all_started(:ex_unit)

    cond do
      test_files == [] ->
        # Make sure we run the after_suite callbacks but with no feedback
        formatters = Application.fetch_env!(:ex_unit, :formatters)

        try do
          Application.put_env(:ex_unit, :formatters, [])
          _ = ExUnit.run()
          :noop
        after
          Application.put_env(:ex_unit, :formatters, formatters)
        end

      Keyword.get(opts, :profile_require) == "time" ->
        Kernel.ParallelCompiler.require(test_files, [profile: :time] ++ shared_require_options)
        :noop

      true ->
        task = ExUnit.async_run()
        warnings_as_errors? = Keyword.get(opts, :warnings_as_errors, false)
        seed = Application.fetch_env!(:ex_unit, :seed)
        rand_algorithm = Application.fetch_env!(:ex_unit, :rand_algorithm)
        test_files = shuffle(seed, rand_algorithm, test_files)

        try do
          parallel_require_options = shared_require_options ++ parallel_require_callbacks

          failed? =
            case Kernel.ParallelCompiler.require(test_files, parallel_require_options) do
              {:ok, _, [_ | _]} when warnings_as_errors? -> true
              {:ok, _, _} -> false
              {:error, _, _} -> exit({:shutdown, 1})
            end

          %{failures: failures} = results = ExUnit.await_run(task)

          if failures == 0 do
            if failed? do
              message =
                "\nERROR! Test suite aborted after successful execution due to warnings while using the --warnings-as-errors option"

              IO.puts(:stderr, IO.ANSI.format([:red, message]))
              exit({:shutdown, 1})
            end

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
    test_sources = read_manifest()

    removed =
      for {source, _} <- test_sources, source not in matched_test_files, do: source

    test_helpers = Enum.map(test_paths, &Path.join(&1, "test_helper.exs"))
    sources = [Mix.Project.config_mtime(), Mix.Project.project_file() | test_helpers]
    force = opts[:force] || Mix.Utils.stale?(sources, [modified])

    changed =
      if force do
        # Let's just require everything
        matched_test_files
      else
        sources_mtimes = mtimes(test_sources)

        # Otherwise let's start with the new sources
        # Plus the sources that have changed in disk
        for(
          source <- matched_test_files,
          not is_map_key(test_sources, source),
          do: source
        ) ++
          for(
            {source, source(external: external)} <- test_sources,
            times = Enum.map([source | external], &Map.fetch!(sources_mtimes, &1)),
            Mix.Utils.stale?(times, [modified]),
            do: source
          )
      end

    stale = MapSet.new(changed -- removed)
    sources = update_stale_sources(test_sources, removed, changed)

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

      parallel_require_callbacks = [
        each_module: &each_module(pid, cwd, &1, &2, &3),
        each_file: &each_file(pid, cwd, &1, &2)
      ]

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
    Enum.reduce(sources, %{}, fn {source, source(external: external)}, map ->
      Enum.reduce([source | external], map, fn file, map ->
        Map.put_new_lazy(map, file, fn -> Mix.Utils.last_modified(file) end)
      end)
    end)
  end

  defp update_stale_sources(sources, removed, changed) do
    sources = Map.drop(sources, removed)
    sources = Enum.reduce(changed, sources, &Map.put(&2, &1, source()))
    sources
  end

  ## Manifest

  defp manifest, do: Path.join(Mix.Project.manifest_path(), @stale_manifest)

  defp read_manifest() do
    try do
      [@manifest_vsn | sources] = manifest() |> File.read!() |> :erlang.binary_to_term()
      sources
    rescue
      _ -> %{}
    end
  end

  defp write_manifest(test_sources) when test_sources == %{} do
    File.rm(manifest())
    :ok
  end

  defp write_manifest(test_sources = %{}) do
    manifest = manifest()
    File.mkdir_p!(Path.dirname(manifest))

    manifest_data = :erlang.term_to_binary([@manifest_vsn | test_sources], [:compressed])
    File.write!(manifest, manifest_data)
  end

  ## Test changed dependency resolution

  defp tests_with_changed_references(test_sources) do
    test_manifest = manifest()
    [elixir_manifest] = Mix.Tasks.Compile.Elixir.manifests()

    if Mix.Utils.stale?([elixir_manifest], [test_manifest]) do
      compile_path = Mix.Project.compile_path()
      {elixir_modules, elixir_sources} = CE.read_manifest(elixir_manifest)

      stale_modules =
        for {module, _} <- elixir_modules,
            beam = Path.join(compile_path, Atom.to_string(module) <> ".beam"),
            Mix.Utils.stale?([beam], [test_manifest]),
            do: module,
            into: MapSet.new()

      stale_modules = find_all_dependent_on(stale_modules, elixir_modules, elixir_sources)

      for {source, source(runtime_references: r, compile_references: c)} <- test_sources,
          Enum.any?(r, &(&1 in stale_modules)) or Enum.any?(c, &(&1 in stale_modules)),
          do: source,
          into: MapSet.new()
    else
      MapSet.new()
    end
  end

  defp find_all_dependent_on(modules, elixir_modules, elixir_sources, resolved \\ MapSet.new()) do
    new_modules =
      for module <- modules,
          module not in resolved,
          dependent_module <- dependent_modules(module, elixir_modules, elixir_sources),
          do: dependent_module,
          into: modules

    if MapSet.size(new_modules) == MapSet.size(modules) do
      new_modules
    else
      find_all_dependent_on(new_modules, elixir_modules, elixir_sources, modules)
    end
  end

  defp dependent_modules(module, modules, sources) do
    for {source,
         CE.source(
           runtime_references: r,
           compile_references: c,
           export_references: e
         )} <- sources,
        module in r or module in c or module in e,
        {dependent_module, CE.module(sources: sources)} <- modules,
        source in sources,
        do: dependent_module
  end

  ## ParallelRequire callback

  defp each_module(pid, cwd, file, module, _binary) do
    external = get_external_resources(module, cwd)

    if external != [] do
      Agent.update(pid, fn sources ->
        file = Path.relative_to(file, cwd)
        source(external: current_external) = source = Map.fetch!(sources, file)
        Map.put(sources, file, source(source, external: external ++ current_external))
      end)
    end

    :ok
  end

  defp each_file(pid, cwd, file, lexical) do
    Agent.update(pid, fn sources ->
      file = Path.relative_to(file, cwd)

      {compile_references, export_references, runtime_references, _compile_env} =
        Kernel.LexicalTracker.references(lexical)

      source =
        source(
          Map.fetch!(sources, file),
          compile_references: compile_references ++ export_references,
          runtime_references: runtime_references
        )

      Map.put(sources, file, source)
    end)
  end

  defp get_external_resources(module, cwd) do
    for file <- Module.get_attribute(module, :external_resource), do: Path.relative_to(file, cwd)
  end

  defp shuffle(_seed = 0, _rand_algorithm, list) do
    list
  end

  defp shuffle(seed, rand_algorithm, list) do
    _ = :rand.seed(rand_algorithm, {seed, seed, seed})
    Enum.shuffle(list)
  end
end
