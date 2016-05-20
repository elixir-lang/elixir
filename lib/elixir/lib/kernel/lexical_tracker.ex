# This is an Elixir module responsible for tracking
# the usage of aliases, imports and requires in the Elixir scope.
#
# Note that since this is required for bootstrap, we can't use
# any of the `GenServer.Behaviour` conveniences.
defmodule Kernel.LexicalTracker do
  @moduledoc false
  @timeout 30_000
  @behaviour :gen_server

  @doc """
  Returns all remotes referenced in this lexical scope.
  """
  def remote_references(arg) do
    :gen_server.call(to_pid(arg), :remote_references, @timeout)
  end

  @doc """
  Returns all remote dispatches in this lexical scope.
  """
  def remote_dispatches(arg) do
    :gen_server.call(to_pid(arg), :remote_dispatches, @timeout)
  end

  @doc """
  Gets the destination the lexical scope is meant to
  compile to.
  """
  def dest(arg) do
    :gen_server.call(to_pid(arg), :dest, @timeout)
  end

  defp to_pid(pid) when is_pid(pid),  do: pid
  defp to_pid(mod) when is_atom(mod) do
    table = :elixir_module.data_table(mod)
    [{_, val}] = :ets.lookup(table, {:elixir, :lexical_tracker})
    val
  end

  # Internal API

  # Starts the tracker and returns its pid.
  @doc false
  def start_link(dest) do
    :gen_server.start_link(__MODULE__, dest, [])
  end

  @doc false
  def stop(pid) do
    :gen_server.cast(pid, :stop)
  end

  @doc false
  def add_import(pid, module, fas, line, warn) do
    :gen_server.cast(pid, {:add_import, module, fas, line, warn})
  end

  @doc false
  def add_alias(pid, module, line, warn) do
    :gen_server.cast(pid, {:add_alias, module, line, warn})
  end

  @doc false
  def remote_reference(pid, module, mode) do
    :gen_server.cast(pid, {:remote_reference, module, mode})
  end

  @doc false
  def remote_dispatch(pid, module, fa, line, mode) do
    :gen_server.cast(pid, {:remote_dispatch, module, fa, line, mode})
  end

  @doc false
  def import_dispatch(pid, {module, function, arity}) do
    :gen_server.cast(pid, {:import_dispatch, {module, function, arity}})
  end

  @doc false
  def alias_dispatch(pid, module) do
    :gen_server.cast(pid, {:alias_dispatch, module})
  end

  @doc false
  def collect_unused_imports(pid) do
    unused(pid, :import)
  end

  @doc false
  def collect_unused_aliases(pid) do
    unused(pid, :alias)
  end

  defp unused(pid, tag) do
    :gen_server.call(pid, {:unused, tag}, @timeout)
  end

  # Callbacks

  def init(dest) do
    {:ok, %{directives: %{}, references: %{}, dispatches: %{}, dest: dest}}
  end

  @doc false
  def handle_call({:unused, tag}, _from, state) do
    directives =
      for {{^tag, module_or_mfa}, marker} <- state.directives,
          is_integer(marker),
          do: {module_or_mfa, marker}

    {:reply, Enum.sort(directives), state}
  end

  def handle_call(:remote_references, _from, state) do
    {:reply, partition(Enum.to_list(state.references), [], []), state}
  end

  def handle_call(:remote_dispatches, _from, state) do
    compile = state.dispatches[:compile] || %{}
    runtime = state.dispatches[:runtime] || %{}
    {:reply, {compile, runtime}, state}
  end

  def handle_call(:dest, _from, state) do
    {:reply, state.dest, state}
  end

  def handle_cast({:remote_reference, module, mode}, state) do
    {:noreply, %{state | references: add_reference(state.references, module, mode)}}
  end

  def handle_cast({:remote_dispatch, module, fa, line, mode}, state) do
    references = add_reference(state.references, module, mode)
    dispatches = add_remote_dispatch(state.dispatches, module, fa, line, mode)
    {:noreply, %{state | references: references, dispatches: dispatches}}
  end

  def handle_cast({:import_dispatch, {module, function, arity}}, state) do
    directives =
      add_dispatch(state.directives, module, :import)
      |> add_dispatch({module, function, arity}, :import)
    # Always compile time because we depend
    # on the module at compile time
    references = add_reference(state.references, module, :compile)
    {:noreply, %{state | directives: directives, references: references}}
  end

  def handle_cast({:alias_dispatch, module}, state) do
    {:noreply, %{state | directives: add_dispatch(state.directives, module, :alias)}}
  end

  def handle_cast({:add_import, module, fas, line, warn}, state) when is_atom(module) do
    directives =
      for {{:import, {import_module, _, _}}, _} = directive <- state.directives,
          module != import_module,
          do: directive,
          into: %{}

    directives = add_directive(directives, module, line, warn, :import)

    directives =
      Enum.reduce fas, directives, fn {function, arity}, directives ->
        add_directive(directives, {module, function, arity}, line, warn, :import)
      end

    {:noreply, %{state | directives: directives}}
  end

  def handle_cast({:add_alias, module, line, warn}, state) do
    {:noreply, %{state | directives: add_directive(state.directives, module, line, warn, :alias)}}
  end

  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  @doc false
  def handle_info(_msg, state) do
    {:noreply, state}
  end

  @doc false
  def terminate(_reason, _state) do
    :ok
  end

  @doc false
  def code_change(_old, state, _extra) do
    {:ok, state}
  end


  defp partition([{remote, :compile} | t], compile, runtime),
    do: partition(t, [remote | compile], runtime)
  defp partition([{remote, :runtime} | t], compile, runtime),
    do: partition(t, compile, [remote | runtime])
  defp partition([], compile, runtime),
    do: {compile, runtime}

  # Callbacks helpers

  defp add_reference(references, module, :runtime) when is_atom(module),
    do: map_put_new(references, module, :runtime)
  defp add_reference(references, module, :compile) when is_atom(module),
    do: map_put(references, module, :compile)

  defp add_remote_dispatch(dispatches, module, fa, line, mode) when is_atom(module) do
    map_update dispatches, mode, %{module => %{fa => [line]}}, fn mode_dispatches ->
      map_update mode_dispatches, module, %{fa => [line]}, fn module_dispatches ->
        map_update module_dispatches, fa, [line], &[line | List.delete(&1, line)]
      end
    end
  end

  # In the map we keep imports and aliases.
  # If the value is a line, it was imported/aliased and has a pending warning
  # If the value is true, it was imported/aliased and used
  defp add_directive(directives, module_or_mfa, line, warn, tag) do
    marker = if warn, do: line, else: true
    map_put(directives, {tag, module_or_mfa}, marker)
  end

  defp add_dispatch(directives, module_or_mfa, tag) do
    map_put(directives, {tag, module_or_mfa}, true)
  end

  defp map_update(map, key, initial, fun) do
    case map_find(map, key) do
      {:ok, val} -> map_put(map, key, fun.(val))
      :error -> map_put(map, key, initial)
    end
  end

  defp map_put_new(map, key, value) do
    case map_find(map, key) do
      {:ok, _} -> map
      :error -> map_put(map, key, value)
    end
  end

  defp map_find(map, key),
    do: :maps.find(key, map)

  defp map_put(map, key, value),
    do: :maps.put(key, value, map)
end
