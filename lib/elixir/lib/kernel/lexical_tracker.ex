# This is an Elixir module responsible for tracking references
# to modules, remote dispatches, and the usage of
# aliases/imports/requires in the Elixir scope.
#
# Note that since this is required for bootstrap, we can't use
# any of the `GenServer.Behaviour` conveniences.
defmodule Kernel.LexicalTracker do
  @moduledoc false
  @timeout :infinity
  @behaviour :gen_server

  @doc """
  Returns all references in this lexical scope.
  """
  def references(pid) do
    :gen_server.call(pid, :references, @timeout)
  end

  # Internal API

  # Starts the tracker and returns its PID.
  @doc false
  def start_link() do
    :gen_server.start_link(__MODULE__, :ok, [])
  end

  @doc false
  def stop(pid) do
    :gen_server.call(pid, :stop)
  end

  @doc false
  def add_require(pid, module) when is_atom(module) do
    :gen_server.cast(pid, {:add_require, module})
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
  def remote_dispatch(pid, module, mode) when is_atom(module) do
    :gen_server.cast(pid, {:remote_dispatch, module, mode})
  end

  @doc false
  def import_dispatch(pid, module, fa, mode) when is_atom(module) do
    :gen_server.cast(pid, {:import_dispatch, module, fa, mode})
  end

  @doc false
  def alias_dispatch(pid, module) when is_atom(module) do
    :gen_server.cast(pid, {:alias_dispatch, module})
  end

  @doc false
  def add_compile_env(pid, app, path, return) do
    :gen_server.cast(pid, {:compile_env, app, path, return})
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
      exports: %{},
      cache: %{},
      compile_env: :ordsets.new(),
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

  def handle_call(:references, _from, state) do
    {compile, runtime} = partition(Map.to_list(state.references), [], [])
    {:reply, {compile, Map.keys(state.exports), runtime, state.compile_env}, state}
  end

  def handle_call({:read_cache, key}, _from, %{cache: cache} = state) do
    {:reply, Map.get(cache, key), state}
  end

  def handle_call(:stop, _from, state) do
    {:stop, :normal, :ok, state}
  end

  def handle_cast({:write_cache, key, value}, %{cache: cache} = state) do
    {:noreply, %{state | cache: Map.put(cache, key, value)}}
  end

  def handle_cast({:remote_dispatch, module, mode}, state) do
    references = add_reference(state.references, module, mode)
    {:noreply, %{state | references: references}}
  end

  def handle_cast({:import_dispatch, module, {function, arity}, mode}, state) do
    state = add_import_dispatch(state, module, function, arity, mode)
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

  def handle_cast({:compile_env, app, path, return}, state) do
    {:noreply, update_in(state.compile_env, &:ordsets.add_element({app, path, return}, &1))}
  end

  def handle_cast({:add_require, module}, state) do
    {:noreply, put_in(state.exports[module], true)}
  end

  def handle_cast({:add_import, module, fas, line, warn}, state) do
    to_remove = for {{:import, {^module, _, _}} = key, _} <- state.directives, do: key

    directives =
      state.directives
      |> Map.drop(to_remove)
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
    do: Map.put(references, module, :compile)

  defp add_reference(references, module, :runtime) when is_atom(module) do
    case Map.fetch(references, module) do
      {:ok, _} -> references
      :error -> Map.put(references, module, :runtime)
    end
  end

  defp add_import_dispatch(state, module, function, arity, mode) do
    directives =
      state.directives
      |> add_dispatch(module, :import)
      |> add_dispatch({module, function, arity}, :import)

    references = add_reference(state.references, module, mode)
    %{state | directives: directives, references: references}
  end

  # In the map we keep imports and aliases.
  # If the value is a line, it was imported/aliased and has a pending warning
  # If the value is true, it was imported/aliased and used
  defp add_directive(directives, module_or_mfa, line, warn, tag) do
    marker = if warn, do: line, else: true
    Map.put(directives, {tag, module_or_mfa}, marker)
  end

  defp add_dispatch(directives, module_or_mfa, tag) do
    Map.put(directives, {tag, module_or_mfa}, true)
  end
end
