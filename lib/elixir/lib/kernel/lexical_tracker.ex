# This is a module Elixir responsible for tracking
# the usage of aliases, imports and requires in the Elixir scope.
#
# The implementation simply stores dispatch information in an
# ETS table and then consults this table once compilation is done.
#
# Note that since this is required for bootstrap, we can't use
# any of the `GenServer.Behaviour` conveniences.
defmodule Kernel.LexicalTracker do
  @timeout  30_000
  @behavior :gen_server

  @import 2
  @alias  3

  @doc """
  Returns all remotes linked to in this lexical scope.
  """
  def remotes(pid) do
    ets = :gen_server.call(to_pid(pid), :ets, @timeout)
    :ets.match(ets, { :"$1", :_, :_ }) |> List.flatten
  end

  defp to_pid(pid) when is_pid(pid),  do: pid
  defp to_pid(mod) when is_atom(mod) do
    table = :elixir_module.data_table(mod)
    [{ _, val }] = :ets.lookup(table, :__lexical_tracker)
    val
  end

  # Internal API

  # Starts the tracker and returns its pid.
  @doc false
  def start_link do
    { :ok, pid } = :gen_server.start_link(__MODULE__, [], [])
    pid
  end

  @doc false
  def stop(pid) do
    :gen_server.cast(pid, :stop)
  end

  @doc false
  def add_import(pid, module, line, warn) do
    :gen_server.cast(pid, { :add_import, module, line, warn })
  end

  @doc false
  def add_alias(pid, module, line, warn) do
    :gen_server.cast(pid, { :add_alias, module, line, warn })
  end

  @doc false
  def remote_dispatch(pid, module) do
    :gen_server.cast(pid, { :remote_dispatch, module })
  end

  @doc false
  def import_dispatch(pid, module) do
    :gen_server.cast(pid, { :import_dispatch, module })
  end

  @doc false
  def alias_dispatch(pid, module) do
    :gen_server.cast(pid, { :alias_dispatch, module })
  end

  @doc false
  def collect_unused_imports(pid) do
    unused(pid, @import)
  end

  @doc false
  def collect_unused_aliases(pid) do
    unused(pid, @alias)
  end

  defp unused(pid, pos) do
    ets = :gen_server.call(pid, :ets, @timeout)
    :ets.foldl(fn
      { module, _, _ } = tuple, acc when is_integer(:erlang.element(pos, tuple)) ->
        [{ module, :erlang.element(pos, tuple) }|acc]
      _, acc ->
        acc
    end, [], ets) |> Enum.sort
  end

  # Callbacks


  def init([]) do
    { :ok, :ets.new(:lexical, [:protected]) }
  end

  def handle_call(:ets, _from, d) do
    { :reply, d, d }
  end

  def handle_call(_request, _from, d) do
    { :noreply, d }
  end

  def handle_cast({ :remote_dispatch, module }, d) do
    add_module(d, module)
    { :noreply, d }
  end

  def handle_cast({ :import_dispatch, module }, d) do
    add_dispatch(d, module, @import)
    { :noreply, d }
  end

  def handle_cast({ :alias_dispatch, module }, d) do
    add_dispatch(d, module, @alias)
    { :noreply, d }
  end

  def handle_cast({ :add_import, module, line, warn }, d) do
    add_directive(d, module, line, warn, @import)
    { :noreply, d }
  end

  def handle_cast({ :add_alias, module, line, warn }, d) do
    add_directive(d, module, line, warn, @alias)
    { :noreply, d }
  end

  def handle_cast(:stop, d) do
    { :stop, :normal, d }
  end

  def handle_cast(_msg, d) do
    { :noreply, d }
  end

  def handle_info(_msg, d) do
    { :noreply, d }
  end

  def terminate(_reason, _d) do
    :ok
  end

  def code_change(_old, d, _extra) do
    { :ok, d }
  end

  # Callbacks helpers

  # In the table we keep imports and aliases.
  # If the value is false, it was not imported/aliased
  # If the value is true, it was imported/aliased
  # If the value is a line, it was imported/aliased and has a pending warning
  defp add_module(d, module) do
    :ets.insert_new(d, { module, false, false })
  end

  defp add_dispatch(d, module, pos) do
    :ets.update_element(d, module, { pos, true })
  end

  defp add_directive(d, module, line, warn, pos) do
    add_module(d, module)
    marker = if warn, do: line, else: true
    :ets.update_element(d, module, { pos, marker })
  end
end
