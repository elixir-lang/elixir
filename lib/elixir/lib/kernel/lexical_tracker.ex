# This is an Elixir module responsible for tracking references
# to modules, remote dispatches, and the usage of
# aliases/imports/requires in the Elixir scope.
#
# Note that since this is required for bootstrap, we can't use
# any of the `GenServer.Behaviour` conveniences.
defmodule Kernel.LexicalTracker do
  @moduledoc false
  @timeout 30000
  @behaviour :gen_server

  @doc """
  Returns all remotes referenced in this lexical scope.
  """
  def remote_references(pid) do
    :gen_server.call(pid, :remote_references, @timeout)
  end

  @doc """
  Returns all remote dispatches in this lexical scope.
  """
  def remote_dispatches(pid) do
    :gen_server.call(pid, :remote_dispatches, @timeout)
  end

  # Internal API

  # Starts the tracker and returns its PID.
  @doc false
  def start_link() do
    :gen_server.start_link(__MODULE__, :ok, [])
  end

  @doc false
  def stop(pid) do
    :gen_server.cast(pid, :stop)
  end

  @doc false
  def add_import(pid, module, fas, line, warn) when is_atom(module) do
    :gen_server.cast(pid, {:add_import, module, fas, line, warn})
  end

  @doc false
  def add_alias(pid, module, line, warn) when is_atom(module) do
    :gen_server.cast(pid, {:add_alias, module, line, warn})
  end

  @doc false
  def remote_reference(pid, module, mode) when is_atom(module) do
    :gen_server.cast(pid, {:remote_reference, module, mode})
  end

  @doc false
  def remote_dispatch(pid, module, fa, line, mode) when is_atom(module) do
    :gen_server.cast(pid, {:remote_dispatch, module, fa, line, mode})
  end

  @doc false
  def remote_struct(pid, module, line) when is_atom(module) do
    :gen_server.cast(pid, {:remote_struct, module, line})
  end

  @doc false
  def import_dispatch(pid, module, fa, line, mode) when is_atom(module) do
    :gen_server.cast(pid, {:import_dispatch, module, fa, line, mode})
  end

  @doc false
  def alias_dispatch(pid, module) when is_atom(module) do
    :gen_server.cast(pid, {:alias_dispatch, module})
  end

  @doc false
  def set_file(pid, file) do
    :gen_server.cast(pid, {:set_file, file})
  end

  @doc false
  def reset_file(pid) do
    :gen_server.cast(pid, :reset_file)
  end

  @doc false
  def write_cache(pid, value) do
    key = :erlang.unique_integer()
    :gen_server.cast(pid, {:write_cache, key, value})
    key
  end

  @doc false
  def read_cache(pid, key) do
    :gen_server.call(pid, {:read_cache, key}, @timeout)
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

  def init(:ok) do
    state = %{
      directives: %{},
      references: %{},
      compile: %{},
      runtime: %{},
      structs: %{},
      cache: %{},
      file: nil
    }

    {:ok, state}
  end

  @doc false
  def handle_call({:unused, tag}, _from, state) do
    directives =
      for {{^tag, module_or_mfa}, marker} <- state.directives, is_integer(marker) do
        {module_or_mfa, marker}
      end

    {:reply, Enum.sort(directives), state}
  end

  def handle_call(:remote_references, _from, state) do
    {compile, runtime} = partition(:maps.to_list(state.references), [], [])
    {:reply, {compile, :maps.keys(state.structs), runtime}, state}
  end

  def handle_call(:remote_dispatches, _from, state) do
    {:reply, {state.compile, state.runtime}, state}
  end

  def handle_call({:read_cache, key}, _from, %{cache: cache} = state) do
    {:reply, :maps.get(key, cache), state}
  end

  def handle_cast({:write_cache, key, value}, %{cache: cache} = state) do
    {:noreply, %{state | cache: :maps.put(key, value, cache)}}
  end

  def handle_cast({:remote_reference, module, mode}, state) do
    {:noreply, %{state | references: add_reference(state.references, module, mode)}}
  end

  def handle_cast({:remote_struct, module, line}, state) do
    state = add_remote_dispatch(state, module, {:__struct__, 0}, line, :compile)
    structs = :maps.put(module, true, state.structs)
    {:noreply, %{state | structs: structs}}
  end

  def handle_cast({:remote_dispatch, module, fa, line, mode}, state) do
    references = add_reference(state.references, module, mode)
    state = add_remote_dispatch(state, module, fa, line, mode)
    {:noreply, %{state | references: references}}
  end

  def handle_cast({:import_dispatch, module, {function, arity} = fa, line, mode}, state) do
    state =
      state
      |> add_import_dispatch(module, function, arity)
      |> add_remote_dispatch(module, fa, line, mode)

    {:noreply, state}
  end

  def handle_cast({:alias_dispatch, module}, state) do
    {:noreply, %{state | directives: add_dispatch(state.directives, module, :alias)}}
  end

  def handle_cast({:set_file, file}, state) do
    {:noreply, %{state | file: file}}
  end

  def handle_cast(:reset_file, state) do
    {:noreply, %{state | file: nil}}
  end

  def handle_cast({:add_import, module, fas, line, warn}, state) do
    directives =
      state.directives
      |> Enum.reject(&match?({{:import, {^module, _, _}}, _}, &1))
      |> :maps.from_list()
      |> add_directive(module, line, warn, :import)

    directives =
      Enum.reduce(fas, directives, fn {function, arity}, directives ->
        add_directive(directives, {module, function, arity}, line, warn, :import)
      end)

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

  defp partition([], compile, runtime), do: {compile, runtime}

  # Callbacks helpers

  defp add_reference(references, module, :compile) when is_atom(module),
    do: :maps.put(module, :compile, references)

  defp add_reference(references, module, :runtime) when is_atom(module) do
    case :maps.find(module, references) do
      {:ok, _} -> references
      :error -> :maps.put(module, :runtime, references)
    end
  end

  defp add_remote_dispatch(state, module, fa, line, mode) when is_atom(module) do
    location = location(state.file, line)

    map_update(mode, %{module => %{fa => [location]}}, state, fn mode_dispatches ->
      map_update(module, %{fa => [location]}, mode_dispatches, fn module_dispatches ->
        map_update(fa, [location], module_dispatches, &[location | List.delete(&1, location)])
      end)
    end)
  end

  defp location(nil, line), do: line
  defp location(file, line), do: {file, line}

  defp add_import_dispatch(state, module, function, arity) do
    directives =
      add_dispatch(state.directives, module, :import)
      |> add_dispatch({module, function, arity}, :import)

    # Always compile time because we depend
    # on the module at compile time
    references = add_reference(state.references, module, :compile)

    %{state | directives: directives, references: references}
  end

  # In the map we keep imports and aliases.
  # If the value is a line, it was imported/aliased and has a pending warning
  # If the value is true, it was imported/aliased and used
  defp add_directive(directives, module_or_mfa, line, warn, tag) do
    marker = if warn, do: line, else: true
    :maps.put({tag, module_or_mfa}, marker, directives)
  end

  defp add_dispatch(directives, module_or_mfa, tag) do
    :maps.put({tag, module_or_mfa}, true, directives)
  end

  defp map_update(key, initial, map, fun) do
    case :maps.find(key, map) do
      {:ok, val} -> :maps.put(key, fun.(val), map)
      :error -> :maps.put(key, initial, map)
    end
  end
end
