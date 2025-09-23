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
  Invoked during module expansion to annotate an alias
  must be warned if unused.
  """
  def warn_alias(pid, meta, alias, module) do
    :gen_server.cast(pid, {:warn_alias, alias, meta})
    module
  end

  @doc """
  Invoked during module expansion to annotate an import
  must be warned if unused.
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
  def add_require(pid, module, meta) when is_atom(module) do
    :gen_server.cast(pid, {:add_require, module, meta})
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
  def import_dispatch(pid, module, fa, mode) when is_atom(module) do
    :gen_server.cast(pid, {:import_dispatch, module, fa, mode})
  end

  @doc false
  def alias_dispatch(pid, module) when is_atom(module) do
    :gen_server.cast(pid, {:alias_dispatch, module})
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
      exports: %{},
      cache: %{},
      compile_env: :ordsets.new(),
      file: nil
    }

    {:ok, state}
  end

  @doc false
  def handle_call(:unused_aliases, _from, state) do
    aliases = for {alias, meta} when is_list(meta) <- state.aliases, do: {alias, meta}
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
    unused_requires =
      for {module, meta} <- state.requires,
          Map.get(state.references, module) != :compile,
          Keyword.get(meta[:opts], :warn, true) != false do
        {module, meta}
      end

    {:reply, Enum.sort(unused_requires), state}
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

  def handle_cast({:alias_dispatch, module}, state) do
    {:noreply, put_in(state.aliases[module], :used)}
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

  def handle_cast({:warn_alias, alias, meta}, %{aliases: aliases} = state) do
    case aliases do
      %{^alias => :used} -> {:noreply, state}
      %{} -> {:noreply, %{state | aliases: Map.put(aliases, alias, meta)}}
    end
  end

  def handle_cast({:warn_import, module}, state) do
    {:noreply, put_in(state.imports[module][@warn_key], true)}
  end

  def handle_cast({:add_require, module, meta}, state) do
    {:noreply, put_in(state.requires[module], meta)}
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
    case references do
      %{^module => _} -> references
      %{} -> Map.put(references, module, :runtime)
    end
  end
end
