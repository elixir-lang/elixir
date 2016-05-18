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
    :gen_server.call(to_pid(arg), :ets, @timeout)
    |> :ets.match({{:mode, :'$1'}, :'$2'})
    |> partition([], [])
  end

  defp partition([[remote, :compile] | t], compile, runtime),
    do: partition(t, [remote | compile], runtime)
  defp partition([[remote, :runtime] | t], compile, runtime),
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
    :gen_server.call(pid, :ets, @timeout)
    |> :ets.select([{{{tag, :"$1"}, :"$2"}, [is_integer: :"$2"], [{{:"$1", :"$2"}}]}])
    |> Enum.sort
  end

  # Callbacks

  def init(dest) do
    {:ok, {:ets.new(__MODULE__, [:protected]), dest}}
  end

  @doc false
  def handle_call(:ets, _from, {d, dest}) do
    {:reply, d, {d, dest}}
  end

  def handle_call(:dest, _from, {d, dest}) do
    {:reply, dest, {d, dest}}
  end

  def handle_cast({:remote_dispatch, module, mode}, {d, dest}) do
    add_reference(d, module, mode)
    {:noreply, {d, dest}}
  end

  def handle_cast({:import_dispatch, {module, function, arity}}, {d, dest}) do
    add_dispatch(d, module, :import)
    add_dispatch(d, {module, function, arity}, :import)
    # Always compile time because we depend
    # on the module at compile time
    add_reference(d, module, :compile)
    {:noreply, {d, dest}}
  end

  def handle_cast({:alias_dispatch, module}, {d, dest}) do
    add_dispatch(d, module, :alias)
    {:noreply, {d, dest}}
  end

  def handle_cast({:add_import, module, fas, line, warn}, {d, dest}) when is_atom(module) do
    :ets.match_delete(d, {{:import, {module, :_, :_}}, :_})

    add_directive(d, module, line, warn, :import)
    for {function, arity} <- fas do
      add_directive(d, {module, function, arity}, line, warn, :import)
    end

    {:noreply, {d, dest}}
  end

  def handle_cast({:add_alias, module, line, warn}, {d, dest}) do
    add_directive(d, module, line, warn, :alias)
    {:noreply, {d, dest}}
  end

  def handle_cast(:stop, {d, dest}) do
    {:stop, :normal, {d, dest}}
  end

  @doc false
  def handle_info(_msg, {d, dest}) do
    {:noreply, {d, dest}}
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

  defp add_reference(d, module, :runtime) when is_atom(module),
    do: :ets.insert_new(d, {{:mode, module}, :runtime})
  defp add_reference(d, module, :compile) when is_atom(module),
    do: :ets.insert(d, {{:mode, module}, :compile})

  # In the table we keep imports and aliases.
  # If the marker is a line, it was imported/aliased and has a pending warning
  # If the marker is true, it was imported/aliased and used
  defp add_directive(d, module_or_mfa, line, warn, tag) do
    marker = if warn, do: line, else: true
    :ets.insert(d, {{tag, module_or_mfa}, marker})
  end

  defp add_dispatch(d, module_or_mfa, tag) do
    :ets.insert(d, {{tag, module_or_mfa}, true})
  end
end
