defmodule Module.ParallelChecker do
  @moduledoc false

  import Kernel, except: [spawn: 3]

  @type cache() :: {pid(), :ets.tid()}
  @type warning() :: term()
  @type mode() :: :elixir | :erlang

  @doc """
  Initializes the parallel checker process.
  """
  def start_link(schedulers \\ nil) do
    :proc_lib.start_link(__MODULE__, :init, [schedulers])
  end

  @doc """
  Stops the parallel checker process.
  """
  def stop({checker, _table}) do
    send(checker, {__MODULE__, :stop})
    :ok
  end

  @doc """
  Gets the parallel checker data from pdict.
  """
  def get do
    case :erlang.get(:elixir_checker_info) do
      {parent, nil} ->
        {:ok, cache} = start_link()
        put(parent, cache)
        {parent, cache}

      {parent, cache} ->
        {parent, cache}
    end
  end

  @doc """
  Stores the parallel checker information.
  """
  def put(pid, {checker, table}) when is_pid(pid) do
    :erlang.put(:elixir_checker_info, {pid, {checker, table}})
  end

  @doc """
  Spawns a process that runs the parallel checker.
  """
  def spawn({pid, {checker, table}}, module, module_map, beam_location, log?) do
    # Protocols may have been consolidated. So if we know their beam location,
    # we discard their module map on purpose and start from file.
    info =
      if beam_location != [] and List.keymember?(module_map.attributes, :__protocol__, 0) do
        List.to_string(beam_location)
      else
        cache_from_module_map(table, module_map)
      end

    inner_spawn(pid, checker, table, module, info, log?)
  end

  defp inner_spawn(pid, checker, table, module, info, log?) do
    ref = make_ref()

    spawned =
      spawn(fn ->
        mon_ref = Process.monitor(pid)

        receive do
          {^ref, :cache} ->
            Process.link(pid)

            module_tuple =
              cond do
                is_tuple(info) ->
                  info

                is_binary(info) ->
                  with {:ok, binary} <- File.read(info),
                       {:ok, {_, [debug_info: chunk]}} <- :beam_lib.chunks(binary, [:debug_info]),
                       {:debug_info_v1, backend, data} = chunk,
                       {:ok, module_map} <- backend.debug_info(:elixir_v1, module, data, []) do
                    cache_from_module_map(table, module_map)
                  else
                    _ -> nil
                  end
              end

            send(checker, {ref, :cached})

            receive do
              {^ref, :check} ->
                # Set the compiler info so we can collect warnings
                :erlang.put(:elixir_compiler_info, {pid, self()})

                warnings =
                  if module_tuple do
                    check_module(module_tuple, {checker, table}, log?)
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
            {_, cache} -> verify(cache, [])
          end

          result
        after
          {_, cache} = :erlang.get(:elixir_checker_info)

          if previous != :undefined do
            :erlang.put(:elixir_checker_info, previous)
          else
            :erlang.erase(:elixir_checker_info)
          end

          cache && stop(cache)
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
  @spec verify(cache(), [{module(), Path.t()}]) :: [warning()]
  def verify({checker, table}, runtime_files) do
    value = :erlang.get(:elixir_code_diagnostics)
    log? = not match?({_, false}, value)

    for {module, file} <- runtime_files do
      inner_spawn(self(), checker, table, module, file, log?)
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
    {:ok, cache} = start_link()
    cache
  end

  @doc """
  Returns the export kind and deprecation reason for the given MFA from
  the cache. If the module does not exist return `{:error, :module}`,
  or if the function does not exist return `{:error, :function}`.
  """
  @spec fetch_export(cache(), module(), atom(), arity()) ::
          {:ok, mode(), binary() | nil, {:infer, [term()]} | :none}
          | :badmodule
          | {:badfunction, mode()}
  def fetch_export({checker, table}, module, fun, arity) do
    case :ets.lookup(table, module) do
      [] ->
        cache_module({checker, table}, module)
        fetch_export({checker, table}, module, fun, arity)

      [{_key, false}] ->
        :badmodule

      [{_key, mode}] ->
        case :ets.lookup(table, {module, {fun, arity}}) do
          [{_key, reason, signature}] -> {:ok, mode, reason, signature}
          [] -> {:badfunction, mode}
        end
    end
  end

  ## Module checking

  defp check_module(module_tuple, cache, log?) do
    {module, file, line, definitions, no_warn_undefined, behaviours, impls, attrs, after_verify} =
      module_tuple

    behaviour_warnings =
      Module.Behaviour.check_behaviours_and_impls(
        module,
        file,
        line,
        behaviours,
        impls,
        definitions
      )

    diagnostics =
      module
      |> Module.Types.warnings(file, attrs, definitions, no_warn_undefined, cache)
      |> Kernel.++(behaviour_warnings)
      |> group_warnings()
      |> emit_warnings(file, log?)

    Enum.each(after_verify, fn {verify_mod, verify_fun} ->
      apply(verify_mod, verify_fun, [module])
    end)

    diagnostics
  end

  defp module_map_to_module_tuple(module_map) do
    %{
      module: module,
      file: file,
      compile_opts: compile_opts,
      definitions: definitions,
      attributes: attributes,
      impls: impls,
      after_verify: after_verify
    } = module_map

    # TODO: Match on anno directly in Elixir v1.22+
    line =
      case module_map do
        %{anno: anno} -> :erl_anno.line(anno)
        %{line: line} -> line
      end

    behaviours = for {:behaviour, module} <- attributes, do: module

    no_warn_undefined =
      compile_opts
      |> extract_no_warn_undefined()
      |> merge_compiler_no_warn_undefined()

    attributes = Keyword.take(attributes, [:__protocol__, :__impl__])

    {module, file, line, definitions, no_warn_undefined, behaviours, impls, attributes,
     after_verify}
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

  defp emit_warnings(warnings, file, log?) do
    Enum.flat_map(warnings, fn {locations, diagnostic} ->
      diagnostics = Enum.map(locations, &to_diagnostic(diagnostic, file, &1))
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

  defp to_diagnostic(diagnostic, source, {file, position, mfa}) when is_list(position) do
    file = Path.absname(file)

    %{
      severity: :warning,
      source: source,
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

  defp cache_module({checker, table}, module) do
    if lock(checker, module) do
      object_code = :code.get_object_code(module)

      # The chunk has more information, so that's our preference
      with {^module, binary, _filename} <- object_code,
           {:ok, {^module, [{~c"ExCk", chunk}]}} <- :beam_lib.chunks(binary, [~c"ExCk"]),
           {:elixir_checker_v1, contents} <- :erlang.binary_to_term(chunk) do
        cache_chunk(table, module, contents.exports)
      else
        _ ->
          # Otherwise, if the module is loaded, use its info
          case :erlang.module_loaded(module) do
            true ->
              {mode, exports} = info_exports(module)
              deprecated = info_deprecated(module)
              cache_info(table, module, exports, deprecated, %{}, mode)

            false ->
              # Or load exports from chunk
              with {^module, binary, _filename} <- object_code,
                   {:ok, {^module, [exports: exports]}} <- :beam_lib.chunks(binary, [:exports]) do
                cache_info(table, module, exports, %{}, %{}, :erlang)
              else
                _ ->
                  :ets.insert(table, {module, false})
              end
          end
      end

      unlock(checker, module)
    end
  end

  defp info_exports(module) do
    {:elixir, behaviour_exports(module) ++ module.__info__(:functions)}
  rescue
    _ -> {:erlang, module.module_info(:exports)}
  end

  defp info_deprecated(module) do
    Map.new(module.__info__(:deprecated))
  rescue
    _ -> %{}
  end

  defp cache_from_module_map(table, map) do
    exports =
      behaviour_exports(map) ++
        for({function, :def, _meta, _clauses} <- map.definitions, do: function)

    cache_info(table, map.module, exports, Map.new(map.deprecated), map.signatures, :elixir)
    module_map_to_module_tuple(map)
  end

  defp cache_info(table, module, exports, deprecated, sigs, mode) do
    Enum.each(exports, fn fa ->
      reason = Map.get(deprecated, fa)
      :ets.insert(table, {{module, fa}, reason, Map.get(sigs, fa, :none)})
    end)

    :ets.insert(table, {module, mode})
  end

  defp cache_chunk(table, module, exports) do
    Enum.each(exports, fn {{fun, arity}, info} ->
      # TODO: Match on signature directly in Elixir v1.22+
      :ets.insert(
        table,
        {{module, {fun, arity}}, Map.get(info, :deprecated), Map.get(info, :sig, :none)}
      )
    end)

    :ets.insert(table, {module, :elixir})
  end

  defp behaviour_exports(%{defines_behaviour: true}), do: [{:behaviour_info, 1}]
  defp behaviour_exports(%{defines_behaviour: false}), do: []

  defp behaviour_exports(module) when is_atom(module) do
    if function_exported?(module, :behaviour_info, 1) do
      [{:behaviour_info, 1}]
    else
      []
    end
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
    table = :ets.new(__MODULE__, [:set, :public, {:read_concurrency, true}])
    :proc_lib.init_ack({:ok, {self(), table}})

    state = %{
      waiting: %{},
      modules: [],
      spawned: 0,
      schedulers: schedulers || max(:erlang.system_info(:schedulers_online), 2)
    }

    :gen_server.enter_loop(__MODULE__, [], state)
  end

  def handle_call(:start, _from, %{modules: modules} = state) do
    for {pid, ref} <- modules do
      send(pid, {ref, :cache})
    end

    for {_pid, ref} <- modules do
      receive do
        {^ref, :cached} -> :ok
      end
    end

    {:reply, length(modules), run_checkers(state)}
  end

  def handle_call({:lock, module}, from, %{waiting: waiting} = state) do
    case waiting do
      %{^module => from_list} ->
        waiting = Map.put(state.waiting, module, [from | from_list])
        {:noreply, %{state | waiting: waiting}}

      %{} ->
        waiting = Map.put(state.waiting, module, [])
        {:reply, true, %{state | waiting: waiting}}
    end
  end

  def handle_call({:unlock, module}, _from, %{waiting: waiting} = state) do
    from_list = Map.fetch!(waiting, module)
    Enum.each(from_list, &:gen_server.reply(&1, false))
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
