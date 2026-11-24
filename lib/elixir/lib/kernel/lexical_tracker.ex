# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

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
  @warn_key 0

  @doc """
  Returns all references in this lexical scope.
  """
  def references(pid) do
    :gen_server.call(pid, :references, @timeout)
  end

  @doc """
  Invoked during module expansion to annotate a require
  that must be warned if unused.
  """
  def warn_require(pid, meta, module, alias) do
    :gen_server.cast(pid, {:warn_require, module, meta, alias})
    module
  end

  @doc """
  Invoked during module expansion to annotate an alias
  that must be warned if unused.
  """
  def warn_alias(pid, meta, alias, module) do
    :gen_server.cast(pid, {:warn_alias, alias, module, meta})
    module
  end

  @doc """
  Invoked during module expansion to annotate an import
  that must be warned if unused.
  """
  def warn_import(pid, module) do
    :gen_server.cast(pid, {:warn_import, module})
    module
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
  def add_export(pid, module) when is_atom(module) do
    :gen_server.cast(pid, {:add_export, module})
  end

  @doc false
  def add_import(pid, module, fas, meta, warn) when is_atom(module) do
    :gen_server.cast(pid, {:add_import, module, fas, meta, warn})
  end

  @doc false
  def remote_dispatch(pid, module, mode) when is_atom(module) do
    :gen_server.cast(pid, {:remote_dispatch, module, mode})
  end

  @doc false
  def module_defined(pid, module) when is_atom(module) do
    :gen_server.cast(pid, {:module_defined, module})
  end

  @doc false
  def import_dispatch(pid, module, fa, mode) when is_atom(module) do
    :gen_server.cast(pid, {:import_dispatch, module, fa, mode})
  end

  @doc false
  def alias_dispatch(pid, alias, module) when is_atom(alias) and is_atom(module) do
    :gen_server.cast(pid, {:alias_dispatch, alias, module})
  end

  @doc false
  def import_quoted(pid, module, function, arities) when is_atom(module) do
    :gen_server.cast(pid, {:import_quoted, module, function, arities})
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
    :gen_server.call(pid, :unused_imports, @timeout)
  end

  @doc false
  def collect_unused_aliases(pid) do
    :gen_server.call(pid, :unused_aliases, @timeout)
  end

  @doc false
  def collect_unused_requires(pid) do
    :gen_server.call(pid, :unused_requires, @timeout)
  end

  # Callbacks

  def init(:ok) do
    state = %{
      aliases: %{},
      imports: %{},
      requires: %{},
      references: %{},
      module_references: %{},
      exports: %{},
      cache: %{},
      compile_env: :ordsets.new(),
      file: nil
    }

    {:ok, state}
  end

  @doc false
  def handle_call(:unused_aliases, _from, state) do
    aliases = for {{alias, _}, meta} when is_list(meta) <- state.aliases, do: {alias, meta}
    {:reply, Enum.sort(aliases), state}
  end

  def handle_call(:unused_imports, _from, state) do
    imports =
      for {module, %{@warn_key => _} = map} <- state.imports do
        {module, Map.delete(map, @warn_key)}
      end

    {:reply, Enum.sort(imports), state}
  end

  def handle_call(:unused_requires, _from, state) do
    %{references: references, aliases: aliases} = state

    unused_requires =
      for {module, {meta, alias}} <- state.requires,
          Map.get(references, module) != :compile do
        {module, meta, alias, Map.get(aliases, {alias, module}) == :used}
      end

    {:reply, Enum.sort(unused_requires), state}
  end

  def handle_call(:references, _from, state) do
    references = compile_references(state.references, state.module_references)
    {compile, runtime} = partition(Map.to_list(references), [], [])
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

  # We store the references of the file once each module is defined.
  # If the module is invoked at compile time, all of its runtime
  # references become compile ones (see compile_references/2). This
  # may have false positives (references from earlier modules) but
  # it is cheap.
  def handle_cast({:module_defined, module}, state) do
    %{references: references, module_references: module_references} = state
    snapshot = {:erlang.unique_integer([:monotonic]), references}
    {:noreply, %{state | module_references: Map.put(module_references, module, snapshot)}}
  end

  def handle_cast({:import_dispatch, module, {function, arity}, mode}, state) do
    %{imports: imports, references: references} = state

    imports =
      case imports do
        %{^module => modules_and_fas} ->
          modules_and_fas
          |> Map.delete(module)
          |> Map.delete({function, arity})
          |> then(&Map.put(imports, module, &1))

        %{} ->
          imports
      end

    references = add_reference(references, module, mode)
    {:noreply, %{state | imports: imports, references: references}}
  end

  def handle_cast({:alias_dispatch, alias, module}, state) do
    {:noreply, put_in(state.aliases[{alias, module}], :used)}
  end

  def handle_cast({:import_quoted, module, function, arities}, state) do
    %{imports: imports} = state

    imports =
      case imports do
        %{^module => modules_and_fas} ->
          arities
          |> Enum.reduce(modules_and_fas, &Map.delete(&2, {function, &1}))
          |> Map.delete(module)
          |> then(&Map.put(imports, module, &1))

        %{} ->
          imports
      end

    {:noreply, %{state | imports: imports}}
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

  def handle_cast({:add_export, module}, state) do
    {:noreply, put_in(state.exports[module], true)}
  end

  def handle_cast({:add_import, module, fas, meta, warn}, state) do
    keys = if warn, do: [@warn_key, module | fas], else: [module | fas]
    {:noreply, put_in(state.imports[module], Map.from_keys(keys, meta))}
  end

  def handle_cast({:warn_alias, alias, module, meta}, %{aliases: aliases} = state) do
    key = {alias, module}

    case aliases do
      %{^key => :used} -> {:noreply, state}
      %{} -> {:noreply, %{state | aliases: Map.put(aliases, key, meta)}}
    end
  end

  def handle_cast({:warn_import, module}, state) do
    {:noreply, put_in(state.imports[module][@warn_key], true)}
  end

  def handle_cast({:warn_require, module, meta, alias}, state) do
    {:noreply, put_in(state.requires[module], {meta, alias})}
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

  # Modules defined in this file and invoked at compile time have
  # their runtime references, transitively, become compile ones.
  #
  # References are only added (or upgraded from runtime to compile),
  # so a snapshot contains all earlier ones. Therefore we only need the
  # runtime entries of the latest snapshot among the modules invoked at
  # compile time, moving to later snapshots reachable from it.
  # We can't use protocols here, as this runs during bootstrap.
  defp compile_references(references, module_references) do
    case latest_snapshot(references, :compile, module_references, nil) do
      nil -> references
      latest -> promote_snapshot(latest, references, module_references)
    end
  end

  defp promote_snapshot({_index, snapshot} = latest, references, module_references) do
    case latest_snapshot(snapshot, :runtime, module_references, latest) do
      ^latest ->
        :maps.fold(
          fn
            ref, :runtime, acc -> Map.put(acc, ref, :compile)
            _ref, :compile, acc -> acc
          end,
          references,
          snapshot
        )

      later ->
        promote_snapshot(later, references, module_references)
    end
  end

  defp latest_snapshot(references, mode, module_references, latest) do
    :maps.fold(
      fn
        module, ^mode, latest when is_map_key(module_references, module) ->
          max_snapshot(Map.fetch!(module_references, module), latest)

        _module, _mode, latest ->
          latest
      end,
      latest,
      references
    )
  end

  defp max_snapshot(snapshot, nil), do: snapshot
  defp max_snapshot({index, _} = snapshot, {latest, _}) when index > latest, do: snapshot
  defp max_snapshot(_snapshot, latest), do: latest

  defp add_reference(references, module, :compile) when is_atom(module),
    do: Map.put(references, module, :compile)

  defp add_reference(references, module, :runtime) when is_atom(module) do
    case references do
      %{^module => _} -> references
      %{} -> Map.put(references, module, :runtime)
    end
  end
end
