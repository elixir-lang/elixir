# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

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
  @manifest_vsn 3

  @doc """
  Requires and runs test files.

  It expects all of the test patterns, the test files that were matched for the
  test patterns, the test paths, and the opts from the test task.
  """
  def require_and_run(matched_test_files, test_paths, elixirc_opts, opts) do
    elixirc_opts =
      Keyword.merge([docs: false, debug_info: false, infer_signatures: false], elixirc_opts)

    previous_opts = Code.compiler_options(elixirc_opts)

    try do
      require_and_run(matched_test_files, test_paths, opts)
    after
      Code.compiler_options(previous_opts)
    end
  end

  defp require_and_run(matched_test_files, test_paths, opts) do
    dry_run = Keyword.get(opts, :dry_run, false)
    stale = Keyword.get(opts, :stale, false)
    max_requires = opts[:max_requires]

    {test_files, stale_manifest_pid, parallel_require_callbacks} =
      if stale and not dry_run do
        set_up_stale(matched_test_files, test_paths, opts)
      else
        {matched_test_files, nil, []}
      end

    shared_require_options =
      if max_requires do
        [max_concurrency: max_requires, return_diagnostics: true]
      else
        [return_diagnostics: true]
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
        seed = Application.fetch_env!(:ex_unit, :seed)
        rand_algorithm = Application.fetch_env!(:ex_unit, :rand_algorithm)
        test_files = shuffle(seed, rand_algorithm, test_files)

        try do
          parallel_require_options = shared_require_options ++ parallel_require_callbacks

          warnings? =
            case Kernel.ParallelCompiler.require(test_files, parallel_require_options) do
              {:ok, _, %{compile_warnings: c, runtime_warnings: r}}
              when c != [] or r != [] ->
                true

              {:ok, _, _} ->
                false

              {:error, _, _} ->
                exit({:shutdown, 1})
            end

          %{failures: failures} = results = ExUnit.await_run(task)

          if failures == 0 do
            agent_write_manifest(stale_manifest_pid)
          end

          {:ok, Map.put(results, :warnings?, warnings?)}
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
    {test_sources, test_modules} = read_manifest()

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

    {changed_references, modules} = tests_with_changed_references(sources, test_modules)

    test_files_to_run =
      changed_references
      |> MapSet.union(stale)
      |> MapSet.to_list()

    if test_files_to_run == [] do
      write_manifest(sources, modules)
      {[], nil, nil}
    else
      {:ok, pid} = Agent.start_link(fn -> {sources, modules} end)
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
    Agent.cast(pid, fn {sources, modules} = state ->
      write_manifest(sources, modules)
      state
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
      [@manifest_vsn, modules | sources] = manifest() |> File.read!() |> :erlang.binary_to_term()
      {sources, modules}
    rescue
      _ -> {%{}, []}
    end
  end

  defp write_manifest(test_sources, _modules) when test_sources == %{} do
    File.rm(manifest())
    :ok
  end

  defp write_manifest(test_sources = %{}, modules) do
    manifest = manifest()
    File.mkdir_p!(Path.dirname(manifest))

    manifest_data =
      :erlang.term_to_binary([@manifest_vsn, modules | test_sources], [:compressed])

    File.write!(manifest, manifest_data)
  end

  ## Test changed dependency resolution

  # Returns the test files that reference changed modules and the
  # list of modules currently in the project and its local dependencies.
  # Modules that were in the previous run but no longer exist are also
  # considered changed.
  defp tests_with_changed_references(test_sources, previous_modules) do
    test_manifest = manifest()
    manifests = elixir_manifests()

    if Mix.Utils.stale?(Enum.map(manifests, &elem(&1, 0)), [test_manifest]) do
      {stale_modules, current_modules, all_sources} =
        Enum.reduce(manifests, {MapSet.new(), [], []}, fn {manifest, ebin}, acc ->
          {stale_modules, current_modules, all_sources} = acc
          {modules, sources, parents} = CE.read_manifest_with_parents(manifest)

          # Elixir modules and Erlang modules (parents)
          all_modules = for({module, CE.module(kind: kind)} <- modules, do: {module, kind})
          all_modules = for(module <- parents, do: {module, :erlang}) ++ all_modules

          stale_modules =
            for {module, kind} <- all_modules,
                beam = Path.join(ebin, Atom.to_string(module) <> ".beam"),
                Mix.Utils.stale?([beam], [test_manifest]),
                changed <- [module | impl_modules(module, kind)],
                do: changed,
                into: stale_modules

          current_modules = for({module, _} <- all_modules, do: module) ++ current_modules
          {stale_modules, current_modules, Enum.to_list(sources) ++ all_sources}
        end)

      removed_modules = previous_modules -- current_modules
      stale_modules = MapSet.union(stale_modules, MapSet.new(removed_modules))
      stale_modules = find_all_dependent_on(stale_modules, all_sources)

      changed =
        for {source, source(runtime_references: r, compile_references: c)} <- test_sources,
            Enum.any?(r, &(&1 in stale_modules)) or Enum.any?(c, &(&1 in stale_modules)),
            do: source,
            into: MapSet.new()

      {changed, current_modules}
    else
      {MapSet.new(), previous_modules}
    end
  end

  # The manifest and ebin of the project and its local dependencies
  defp elixir_manifests() do
    [elixir_manifest] = Mix.Tasks.Compile.Elixir.manifests()
    base = Path.basename(elixir_manifest)

    deps =
      for %{scm: scm, opts: opts} <- Mix.Dep.cached(),
          not scm.fetchable?(),
          do: {Path.join([opts[:build], ".mix", base]), Path.join(opts[:build], "ebin")}

    [{elixir_manifest, Mix.Project.compile_path()} | deps]
  end

  # Tests depend on protocol implementations through the protocol and
  # the implemented module, so we consider those changed as well.
  defp impl_modules(impl, {:impl, protocol}) do
    prefix = Atom.to_string(protocol) <> "."
    for_module = impl |> Atom.to_string() |> String.replace_prefix(prefix, "Elixir.")
    [protocol, String.to_atom(for_module)]
  end

  defp impl_modules(_module, _kind), do: []

  defp find_all_dependent_on(modules, elixir_sources, resolved \\ MapSet.new()) do
    new_modules =
      for module <- modules,
          module not in resolved,
          dependent_module <- dependent_modules(module, elixir_sources),
          do: dependent_module,
          into: modules

    if MapSet.size(new_modules) == MapSet.size(modules) do
      new_modules
    else
      find_all_dependent_on(new_modules, elixir_sources, modules)
    end
  end

  defp dependent_modules(module, sources) do
    for {_,
         CE.source(
           runtime_references: r,
           compile_references: c,
           export_references: e,
           modules: modules
         )} <- sources,
        module in r or module in c or module in e,
        dependent_module <- modules,
        do: dependent_module
  end

  ## ParallelRequire callback

  defp each_module(pid, cwd, file, module, _binary) do
    external = get_external_resources(module, cwd)

    if external != [] do
      Agent.update(pid, fn {sources, modules} ->
        file = Path.relative_to(file, cwd)
        source(external: current_external) = source = Map.fetch!(sources, file)
        {Map.put(sources, file, source(source, external: external ++ current_external)), modules}
      end)
    end

    :ok
  end

  defp each_file(pid, cwd, file, lexical) do
    Agent.update(pid, fn {sources, modules} ->
      file = Path.relative_to(file, cwd)

      {compile_references, export_references, runtime_references, _compile_env} =
        Kernel.LexicalTracker.references(lexical)

      source =
        source(
          Map.fetch!(sources, file),
          compile_references: compile_references ++ export_references,
          runtime_references: runtime_references
        )

      {Map.put(sources, file, source), modules}
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
