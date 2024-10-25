defmodule Module.ParallelChecker do
  @moduledoc false

  import Kernel, except: [spawn: 3]

  @type cache() :: {pid(), :ets.tid()}
  @type warning() :: term()
  @type kind() :: :def | :defmacro
  @type mode() :: :elixir | :erlang

  @doc """
  Initializes the parallel checker process.
  """
  def start_link(schedulers \\ nil) do
    :gen_server.start_link(__MODULE__, schedulers, [])
  end

  @doc """
  Stops the parallel checker process.
  """
  def stop(checker) do
    send(checker, {__MODULE__, :stop})
    :ok
  end

  @doc """
  Gets the parallel checker data from pdict.
  """
  def get do
    case :erlang.get(:elixir_checker_info) do
      {parent, nil} ->
        {:ok, checker} = start_link()
        put(parent, checker)
        {parent, checker}

      {parent, checker} ->
        {parent, checker}
    end
  end

  @doc """
  Stores the parallel checker information.
  """
  def put(pid, checker) when is_pid(pid) and is_pid(checker) do
    :erlang.put(:elixir_checker_info, {pid, checker})
  end

  @doc """
  Spawns a process that runs the parallel checker.
  """
  def spawn({pid, checker}, module, info, log?) do
    ref = make_ref()

    spawned =
      spawn(fn ->
        mon_ref = Process.monitor(pid)

        receive do
          {^ref, :cache, ets} ->
            Process.link(pid)

            module_map =
              if is_map(info) do
                info
              else
                case File.read(info) do
                  {:ok, binary} -> maybe_module_map(binary, module)
                  {:error, _} -> nil
                end
              end

            module_map && cache_from_module_map(ets, module_map)
            send(checker, {ref, :cached})

            receive do
              {^ref, :check} ->
                # Set the compiler info so we can collect warnings
                :erlang.put(:elixir_compiler_info, {pid, self()})

                warnings =
                  if module_map do
                    check_module(module_map, {checker, ets}, log?)
                  else
                    []
                  end

                send(pid, {__MODULE__, module, warnings})
                send(checker, {__MODULE__, :done})
            end

          {:DOWN, ^mon_ref, _, _, _} ->
            :ok
        end
      end)

    register(checker, spawned, ref)
    :ok
  end

  @doc """
  Verifies the given compilation function
  by starting a checker if one does not exist.

  See `verify/3`.
  """
  def verify(fun) do
    case :erlang.get(:elixir_compiler_info) do
      :undefined ->
        previous = :erlang.put(:elixir_checker_info, {self(), nil})

        try do
          result = fun.()

          case :erlang.get(:elixir_checker_info) do
            {_, nil} -> :ok
            {_, checker} -> verify(checker, [])
          end

          result
        after
          {_, checker} = :erlang.get(:elixir_checker_info)

          if previous != :undefined do
            :erlang.put(:elixir_checker_info, previous)
          else
            :erlang.erase(:elixir_checker_info)
          end

          checker && stop(checker)
        end

      _ ->
        # If we are during compilation, then they will be
        # reported to the compiler, which will validate them.
        fun.()
    end
  end

  @doc """
  Receives pairs of module maps and BEAM binaries. In parallel it verifies
  the modules and adds the ExCk chunk to the binaries. Returns the updated
  list of warnings from the verification.
  """
  @spec verify(pid(), [{module(), Path.t()}]) :: [warning()]
  def verify(checker, runtime_files) do
    value = :erlang.get(:elixir_code_diagnostics)
    log? = not match?({_, false}, value)

    for {module, file} <- runtime_files do
      spawn({self(), checker}, module, file, log?)
    end

    count = :gen_server.call(checker, :start, :infinity)
    diagnostics = collect_results(count, [])

    case :erlang.get(:elixir_code_diagnostics) do
      :undefined -> :ok
      {tail, log?} -> :erlang.put(:elixir_code_diagnostics, {diagnostics ++ tail, log?})
    end

    diagnostics
  end

  defp collect_results(0, diagnostics) do
    diagnostics
  end

  defp collect_results(count, diagnostics) do
    receive do
      {:diagnostic, %{file: file} = diagnostic, read_snippet} ->
        :elixir_errors.print_diagnostic(diagnostic, read_snippet)
        diagnostic = %{diagnostic | file: file && Path.absname(file)}
        collect_results(count, [diagnostic | diagnostics])

      {__MODULE__, _module, new_diagnostics} ->
        collect_results(count - 1, new_diagnostics ++ diagnostics)
    end
  end

  @doc """
  Test cache.
  """
  def test_cache do
    {:ok, checker} = start_link()
    {checker, :gen_server.call(checker, :ets, :infinity)}
  end

  @doc """
  Returns the export kind and deprecation reason for the given MFA from
  the cache. If the module does not exist return `{:error, :module}`,
  or if the function does not exist return `{:error, :function}`.
  """
  @spec fetch_export(cache(), module(), atom(), arity()) ::
          {:ok, mode(), kind(), binary() | nil} | {:error, :function | :module}
  def fetch_export({server, ets}, module, fun, arity) do
    case :ets.lookup(ets, {:cached, module}) do
      [] ->
        cache_module({server, ets}, module)
        fetch_export({server, ets}, module, fun, arity)

      [{_key, false}] ->
        {:error, :module}

      [{_key, mode}] ->
        case :ets.lookup(ets, {:export, module, {fun, arity}}) do
          [{_key, kind, reason}] -> {:ok, mode, kind, reason}
          [] -> {:error, :function}
        end
    end
  end

  @doc """
  Returns all exported functions and macros for the given module from
  the cache.
  """
  @spec all_exports(cache(), module()) :: [{atom(), arity()}]
  def all_exports({_server, ets}, module) do
    # This is only called after we get a deprecation notice
    # so we can assume it's a cached module
    ets
    |> :ets.match({{:export, module, :"$1"}, :_, :_})
    |> Enum.flat_map(& &1)
    |> Enum.sort()
  end

  ## Module checking

  defp check_module(module_map, cache, log?) do
    %{
      module: module,
      file: file,
      line: line,
      compile_opts: compile_opts,
      definitions: definitions,
      uses_behaviours: uses_behaviours,
      impls: impls
    } = module_map

    no_warn_undefined =
      compile_opts
      |> extract_no_warn_undefined()
      |> merge_compiler_no_warn_undefined()

    behaviour_warnings =
      Module.Behaviour.check_behaviours_and_impls(
        module,
        file,
        line,
        uses_behaviours,
        impls,
        definitions
      )

    diagnostics =
      module
      |> Module.Types.warnings(file, definitions, no_warn_undefined, cache)
      |> Kernel.++(behaviour_warnings)
      |> group_warnings()
      |> emit_warnings(log?)

    module_map
    |> Map.get(:after_verify, [])
    |> Enum.each(fn {verify_mod, verify_fun} -> apply(verify_mod, verify_fun, [module]) end)

    diagnostics
  end

  defp extract_no_warn_undefined(compile_opts) do
    for(
      {:no_warn_undefined, values} <- compile_opts,
      value <- List.wrap(values),
      do: value
    )
  end

  defp merge_compiler_no_warn_undefined(no_warn_undefined) do
    case Code.get_compiler_option(:no_warn_undefined) do
      :all -> :all
      list when is_list(list) -> no_warn_undefined ++ list
    end
  end

  ## Warning helpers

  defp group_warnings(warnings) do
    {ungrouped, grouped} =
      Enum.reduce(warnings, {[], %{}}, fn {module, warning, location}, {ungrouped, grouped} ->
        %{message: _} = diagnostic = module.format_diagnostic(warning)

        if Map.get(diagnostic, :group, false) do
          locations = MapSet.new([location])

          grouped =
            Map.update(grouped, warning, {locations, diagnostic}, fn
              {locations, diagnostic} -> {MapSet.put(locations, location), diagnostic}
            end)

          {ungrouped, grouped}
        else
          {[{[location], diagnostic} | ungrouped], grouped}
        end
      end)

    grouped =
      Enum.map(grouped, fn {_warning, {locations, diagnostic}} ->
        {Enum.sort(locations), diagnostic}
      end)

    Enum.sort(ungrouped ++ grouped)
  end

  defp emit_warnings(warnings, log?) do
    Enum.flat_map(warnings, fn {locations, diagnostic} ->
      diagnostics = Enum.map(locations, &to_diagnostic(diagnostic, &1))
      log? and print_diagnostics(diagnostics)
      diagnostics
    end)
  end

  defp print_diagnostics([diagnostic]) do
    :elixir_errors.print_diagnostic(diagnostic, true)
  end

  defp print_diagnostics(diagnostics) do
    :elixir_errors.print_diagnostics(diagnostics)
  end

  defp to_diagnostic(diagnostic, {file, position, mfa}) when is_list(position) do
    %{
      severity: :warning,
      source: file,
      file: file,
      position: position_to_tuple(position),
      stacktrace: [to_stacktrace(file, position, mfa)],
      span: nil
    }
    |> Map.merge(diagnostic)
  end

  defp position_to_tuple(position) do
    case position[:column] do
      nil -> position[:line] || 0
      col -> {position[:line], col}
    end
  end

  defp to_stacktrace(file, pos, {module, fun, arity}),
    do: {module, fun, arity, location(file, pos)}

  defp to_stacktrace(file, pos, nil),
    do: {:elixir_compiler, :__FILE__, 1, location(file, pos)}

  defp to_stacktrace(file, pos, module),
    do: {module, :__MODULE__, 0, location(file, pos)}

  defp location(file, position) do
    [{:file, String.to_charlist(Path.relative_to_cwd(file))} | position]
  end

  ## Cache

  defp cache_module({server, ets}, module) do
    if lock(server, module) do
      object_code = :code.get_object_code(module)

      # The chunk has more information, so that's our preference
      with {^module, binary, _filename} <- object_code,
           {:ok, {^module, [{~c"ExCk", chunk}]}} <- :beam_lib.chunks(binary, [~c"ExCk"]),
           {:elixir_checker_v1, contents} <- :erlang.binary_to_term(chunk) do
        cache_chunk(ets, module, contents.exports)
      else
        _ ->
          # Otherwise, if the module is loaded, use its info
          case :erlang.module_loaded(module) do
            true ->
              {mode, exports} = info_exports(module)
              deprecated = info_deprecated(module)
              cache_info(ets, module, exports, deprecated, mode)

            false ->
              # Or load exports from chunk
              with {^module, binary, _filename} <- object_code,
                   {:ok, {^module, [exports: exports]}} <- :beam_lib.chunks(binary, [:exports]) do
                exports = Map.new(Enum.map(exports, &{&1, :def}))
                cache_info(ets, module, exports, %{}, :erlang)
              else
                _ ->
                  :ets.insert(ets, {{:cached, module}, false})
              end
          end
      end

      unlock(server, module)
    end
  end

  defp info_exports(module) do
    map =
      Map.new(
        [{{:__info__, 1}, :def}] ++
          behaviour_exports(module) ++
          Enum.map(module.__info__(:macros), &{&1, :defmacro}) ++
          Enum.map(module.__info__(:functions), &{&1, :def})
      )

    {:elixir, map}
  rescue
    _ -> {:erlang, Map.new(Enum.map(module.module_info(:exports), &{&1, :def}))}
  end

  defp info_deprecated(module) do
    Map.new(module.__info__(:deprecated))
  rescue
    _ -> %{}
  end

  defp maybe_module_map(binary, module) when is_binary(binary) do
    # If a module was compiled without debug_info,
    # then there is no module_map for further verification.
    with {:ok, {_, [debug_info: chunk]}} <- :beam_lib.chunks(binary, [:debug_info]),
         {:debug_info_v1, backend, data} = chunk,
         {:ok, module_map} <- backend.debug_info(:elixir_v1, module, data, []) do
      module_map
    else
      _ -> nil
    end
  end

  defp cache_from_module_map(ets, map) do
    exports =
      [{{:__info__, 1}, :def}] ++
        behaviour_exports(map) ++
        definitions_to_exports(map.definitions)

    deprecated = Map.new(map.deprecated)
    cache_info(ets, map.module, exports, deprecated, :elixir)
  end

  defp cache_info(ets, module, exports, deprecated, mode) do
    Enum.each(exports, fn {{fun, arity}, kind} ->
      reason = Map.get(deprecated, {fun, arity})
      :ets.insert(ets, {{:export, module, {fun, arity}}, kind, reason})
      {{fun, arity}, kind}
    end)

    :ets.insert(ets, {{:cached, module}, mode})
  end

  defp cache_chunk(ets, module, exports) do
    Enum.each(exports, fn {{fun, arity}, %{kind: kind, deprecated_reason: reason}} ->
      :ets.insert(ets, {{:export, module, {fun, arity}}, kind, reason})

      {{fun, arity}, kind}
    end)

    :ets.insert(ets, {{:export, module, {:__info__, 1}}, :def, nil})
    :ets.insert(ets, {{:cached, module}, :elixir})
  end

  defp behaviour_exports(%{defines_behaviour: true}), do: [{{:behaviour_info, 1}, :def}]
  defp behaviour_exports(%{defines_behaviour: false}), do: []

  defp behaviour_exports(module) when is_atom(module) do
    if function_exported?(module, :behaviour_info, 1) do
      [{{:behaviour_info, 1}, :def}]
    else
      []
    end
  end

  defp definitions_to_exports(definitions) do
    Enum.flat_map(definitions, fn {function, kind, _meta, _clauses} ->
      if kind in [:def, :defmacro] do
        [{function, kind}]
      else
        []
      end
    end)
  end

  defp lock(server, module) do
    :gen_server.call(server, {:lock, module}, :infinity)
  end

  defp unlock(server, module) do
    :gen_server.call(server, {:unlock, module}, :infinity)
  end

  defp register(server, pid, ref) do
    :gen_server.cast(server, {:register, pid, ref})
  end

  ## Server callbacks

  def init(schedulers) do
    ets = :ets.new(__MODULE__, [:set, :public, {:read_concurrency, true}])

    state = %{
      ets: ets,
      waiting: %{},
      modules: [],
      spawned: 0,
      schedulers: schedulers || max(:erlang.system_info(:schedulers_online), 2)
    }

    {:ok, state}
  end

  def handle_call(:start, _from, %{ets: ets, modules: modules} = state) do
    for {pid, ref} <- modules do
      send(pid, {ref, :cache, ets})
    end

    for {_pid, ref} <- modules do
      receive do
        {^ref, :cached} -> :ok
      end
    end

    {:reply, length(modules), run_checkers(state)}
  end

  def handle_call(:ets, _from, state) do
    {:reply, state.ets, state}
  end

  def handle_call({:lock, module}, from, %{waiting: waiting} = state) do
    case waiting do
      %{^module => froms} ->
        waiting = Map.put(state.waiting, module, [from | froms])
        {:noreply, %{state | waiting: waiting}}

      %{} ->
        waiting = Map.put(state.waiting, module, [])
        {:reply, true, %{state | waiting: waiting}}
    end
  end

  def handle_call({:unlock, module}, _from, %{waiting: waiting} = state) do
    froms = Map.fetch!(waiting, module)
    Enum.each(froms, &:gen_server.reply(&1, false))
    waiting = Map.delete(waiting, module)
    {:reply, :ok, %{state | waiting: waiting}}
  end

  def handle_info({__MODULE__, :done}, state) do
    state = %{state | spawned: state.spawned - 1}
    {:noreply, run_checkers(state)}
  end

  def handle_info({__MODULE__, :stop}, state) do
    {:stop, :normal, state}
  end

  def handle_cast({:register, pid, ref}, %{modules: modules} = state) do
    {:noreply, %{state | modules: [{pid, ref} | modules]}}
  end

  defp run_checkers(%{modules: []} = state) do
    state
  end

  defp run_checkers(%{spawned: spawned, schedulers: schedulers} = state)
       when spawned >= schedulers do
    state
  end

  defp run_checkers(%{modules: [{pid, ref} | modules]} = state) do
    send(pid, {ref, :check})
    run_checkers(%{state | modules: modules, spawned: state.spawned + 1})
  end
end
