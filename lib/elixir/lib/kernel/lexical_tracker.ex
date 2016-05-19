# This is an Elixir module responsible for tracking
# the usage of aliases, imports and requires in the Elixir scope.
#
# The implementation simply stores dispatch information in an
# ETS table and then consults this table once compilation is done.
#
# Note that since this is required for bootstrap, we can't use
# any of the `GenServer.Behaviour` conveniences.
defmodule Kernel.LexicalTracker do
  @moduledoc false
  @timeout 30_000
  @behaviour :gen_server

  @doc """
  Returns all remotes linked to in this lexical scope.
  """
  def remotes(arg) do
    references = :gen_server.call(to_pid(arg), :references, @timeout)
    for({{:mode, module}, mode} <- references, do: {module, mode})
    |> partition([], [])
  end

  defp partition([{remote, :compile} | t], compile, runtime),
    do: partition(t, [remote | compile], runtime)
  defp partition([{remote, :runtime} | t], compile, runtime),
    do: partition(t, compile, [remote | runtime])
  defp partition([], compile, runtime),
    do: {compile, runtime}

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
  def remote_dispatch(pid, module, mode) do
    :gen_server.cast(pid, {:remote_dispatch, module, mode})
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
    directives = :gen_server.call(pid, :directives, @timeout)

    directives =
      for {{^tag, module_or_mfa}, marker} <- directives,
          is_integer(marker),
          do: {module_or_mfa, marker}

    Enum.sort(directives)
  end

  # Callbacks

  def init(dest) do
    {:ok, %{directives: %{}, references: %{}, dest: dest}}
  end

  @doc false
  def handle_call(:directives, _from, state) do
    {:reply, state.directives, state}
  end

  def handle_call(:references, _from, state) do
    {:reply, state.references, state}
  end

  def handle_call(:dest, _from, state) do
    {:reply, state.dest, state}
  end

  def handle_cast({:remote_dispatch, module, mode}, state) do
    {:noreply, %{state | references: add_reference(state.references, module, mode)}}
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

  # Callbacks helpers

  defp add_reference(references, module, :runtime) when is_atom(module) do
    key = {:mode, module}

    case :maps.find(key, references) do
      {:ok, _} -> references
      :error -> :maps.put(key, :runtime, references)
    end
   end

  defp add_reference(references, module, :compile) when is_atom(module),
    do: :maps.put({:mode, module}, :compile, references)

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
end
