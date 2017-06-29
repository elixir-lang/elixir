# This is an Elixir module responsible for tracking
# calls in order to extract Elixir modules' behaviour
# during compilation time.
#
# ## Implementation
#
# The implementation uses the digraph module to track
# all dependencies. The graph starts with one main vertex:
#
# * `:local` - points to local functions
#
# We can also have the following vertices:
#
# * `Module` - a module that was invoked via an import
# * `{name, arity}` - a local function/arity pair
# * `{:import, name, arity}` - an invoked function/arity import
#
# Each of those vertices can associate to other vertices
# as described below:
#
# * `Module`
#   * in neighbours:  `{:import, name, arity}`
#
# * `{name, arity}`
#   * in neighbours: `:local`, `{name, arity}`
#   * out neighbours: `{:import, name, arity}`
#
# * `{:import, name, arity}`
#   * in neighbours: `{name, arity}`
#   * out neighbours: `Module`
#
# Note that since this is required for bootstrap, we can't use
# any of the `GenServer` conveniences.
defmodule Module.LocalsTracker do
  @moduledoc false

  @timeout   30_000
  @behaviour :gen_server

  @type ref :: pid | module
  @type name :: atom
  @type name_arity :: {name, arity}

  @type local :: {name, arity}
  @type import :: {:import, name, arity}

  # Public API

  @doc """
  Returns all imported modules that had the given
  `{name, arity}` invoked.
  """
  @spec imports_with_dispatch(ref, name_arity) :: [module]
  def imports_with_dispatch(ref, {name, arity}) do
    d = :gen_server.call(to_pid(ref), :digraph, @timeout)
    :digraph.out_neighbours(d, {:import, name, arity})
  end

  @doc """
  Returns all locals that are reachable.

  By default, all public functions are reachable.
  A private function is only reachable if it has
  a public function that it invokes directly.
  """
  @spec reachable(ref) :: [local]
  def reachable(ref) do
    ref
    |> to_pid()
    |> :gen_server.call(:digraph, @timeout)
    |> reachable_from(:local)
    |> :sets.to_list()
  end

  defp reachable_from(d, starting) do
    reduce_reachable(d, starting, :sets.new)
  end

  defp reduce_reachable(d, vertex, vertices) do
    neighbours = :digraph.out_neighbours(d, vertex)
    neighbours = (for {_, _} = t <- neighbours, do: t) |> :sets.from_list
    remaining  = :sets.subtract(neighbours, vertices)
    vertices   = :sets.union(neighbours, vertices)
    :sets.fold(&reduce_reachable(d, &1, &2), vertices, remaining)
  end

  defp to_pid(pid) when is_pid(pid),  do: pid
  defp to_pid(mod) when is_atom(mod) do
    table = :elixir_module.data_table(mod)
    :ets.lookup_element(table, {:elixir, :locals_tracker}, 2)
  end

  # Internal API

  # Starts the tracker and returns its PID.
  @doc false
  def start_link do
    :gen_server.start_link(__MODULE__, [], [])
  end

  # Adds a definition into the tracker. A public
  # definition is connected with the :local node
  # while a private one is left unreachable until
  # a call is made to.
  @doc false
  def add_definition(pid, kind, tuple) when kind in [:def, :defp, :defmacro, :defmacrop] do
    :gen_server.cast(pid, {:add_definition, kind, tuple})
  end

  # Adds and tracks defaults for a definition into the tracker.
  @doc false
  def add_defaults(pid, kind, tuple, defaults) when kind in [:def, :defp, :defmacro, :defmacrop] do
    :gen_server.cast(pid, {:add_defaults, kind, tuple, defaults})
  end

  # Adds a local dispatch to the given target.
  def add_local(pid, to) when is_tuple(to) do
    :gen_server.cast(pid, {:add_local, :local, to})
  end

  # Adds a local dispatch from-to the given target.
  @doc false
  def add_local(pid, from, to) when is_tuple(from) and is_tuple(to) do
    :gen_server.cast(pid, {:add_local, from, to})
  end

  # Adds an import dispatch to the given target.
  @doc false
  def add_import(pid, function, module, target) when is_atom(module) and is_tuple(target) do
    :gen_server.cast(pid, {:add_import, function, module, target})
  end

  # Yanks a local node. Returns its in and out vertices in a tuple.
  @doc false
  def yank(pid, local) do
    :gen_server.call(to_pid(pid), {:yank, local}, @timeout)
  end

  # Reattach a previously yanked node
  @doc false
  def reattach(pid, kind, tuple, neighbours) do
    :gen_server.cast(to_pid(pid), {:reattach, kind, tuple, neighbours})
  end

  # Collecting all conflicting imports with the given functions
  @doc false
  def collect_imports_conflicts(pid, all_defined) do
    d = :gen_server.call(pid, :digraph, @timeout)

    for {{name, arity}, _, meta, _} <- all_defined,
        :digraph.in_neighbours(d, {:import, name, arity}) != [],
        n = :digraph.out_neighbours(d, {:import, name, arity}),
        n != [] do
      {meta, {n, name, arity}}
    end
  end

  # Collect all unused definitions based on the private
  # given also accounting the expected amount of default
  # clauses a private function have.
  @doc false
  def collect_unused_locals(ref, private) do
    d = :gen_server.call(to_pid(ref), :digraph, @timeout)
    reachable = reachable_from(d, :local)
    {unreachable(reachable, private), collect_warnings(reachable, private)}
  end

  defp unreachable(reachable, private) do
    for {tuple, kind, _, _} <- private,
        kind == :defmacrop or not :sets.is_element(tuple, reachable),
        do: tuple
  end

  defp collect_warnings(reachable, private) do
    :lists.foldl(&collect_warnings(&1, &2, reachable), [], private)
  end

  defp collect_warnings({_, _, false, _}, acc, _reachable) do
    acc
  end

  defp collect_warnings({tuple, kind, meta, 0}, acc, reachable) do
    if :sets.is_element(tuple, reachable) do
      acc
    else
      [{meta, {:unused_def, tuple, kind}} | acc]
    end
  end

  defp collect_warnings({tuple, kind, meta, default}, acc, reachable) when default > 0 do
    {name, arity} = tuple
    min = arity - default
    max = arity

    case min_reachable_default(max, min, :none, name, reachable) do
      :none -> [{meta, {:unused_def, tuple, kind}} | acc]
      ^min -> acc
      ^max -> [{meta, {:unused_args, tuple}} | acc]
      diff -> [{meta, {:unused_args, tuple, diff}} | acc]
    end
  end

  defp min_reachable_default(max, min, last, name, reachable) when max >= min do
    case :sets.is_element({name, max}, reachable) do
      true -> min_reachable_default(max - 1, min, max, name, reachable)
      false -> min_reachable_default(max - 1, min, last, name, reachable)
    end
  end
  defp min_reachable_default(_max, _min, last, _name, _reachable) do
    last
  end

  # Stops the gen server
  @doc false
  def stop(pid) do
    :gen_server.cast(pid, :stop)
  end

  # Callbacks

  def init([]) do
    d = :digraph.new([:protected])
    :digraph.add_vertex(d, :local)
    {:ok, d}
  end

  def handle_call({:yank, local}, _from, d) do
    out_vertices = :digraph.out_neighbours(d, local)
    :digraph.del_edges(d, :digraph.out_edges(d, local))
    {:reply, {[], out_vertices}, d}
  end

  def handle_call(:digraph, _from, d) do
    {:reply, d, d}
  end

  @doc false
  def handle_info(_msg, d) do
    {:noreply, d}
  end

  def handle_cast({:add_local, from, to}, d) do
    handle_add_local(d, from, to)
    {:noreply, d}
  end

  def handle_cast({:add_import, function, module, {name, arity}}, d) do
    handle_import(d, function, module, name, arity)
    {:noreply, d}
  end

  def handle_cast({:add_definition, kind, tuple}, d) do
    handle_add_definition(d, kind, tuple)
    {:noreply, d}
  end

  def handle_cast({:add_defaults, kind, {name, arity}, defaults}, d) do
    for i <- :lists.seq(arity - defaults, arity - 1) do
      handle_add_definition(d, kind, {name, i})
      handle_add_local(d, {name, i}, {name, arity})
    end
    {:noreply, d}
  end

  def handle_cast({:reattach, _kind, tuple, {in_neigh, out_neigh}}, d) do
    for from <- in_neigh do
      :digraph.add_vertex(d, from)
      replace_edge!(d, from, tuple)
    end

    for to <- out_neigh do
      :digraph.add_vertex(d, to)
      replace_edge!(d, tuple, to)
    end

    {:noreply, d}
  end

  def handle_cast(:stop, d) do
    {:stop, :normal, d}
  end

  @doc false
  def terminate(_reason, _state) do
    :ok
  end

  @doc false
  def code_change(_old, state, _extra) do
    {:ok, state}
  end

  defp handle_import(d, function, module, name, arity) do
    :digraph.add_vertex(d, module)

    tuple = {:import, name, arity}
    :digraph.add_vertex(d, tuple)
    replace_edge!(d, tuple, module)

    if function != nil do
      replace_edge!(d, function, tuple)
    end

    :ok
  end

  defp handle_add_local(d, from, to) do
    :digraph.add_vertex(d, to)
    replace_edge!(d, from, to)
  end

  defp handle_add_definition(d, public, tuple) when public in [:def, :defmacro] do
    :digraph.add_vertex(d, tuple)
    replace_edge!(d, :local, tuple)
  end

  defp handle_add_definition(d, private, tuple) when private in [:defp, :defmacrop] do
    :digraph.add_vertex(d, tuple)
  end

  defp replace_edge!(d, from, to) do
    _ = unless :lists.member(to, :digraph.out_neighbours(d, from)) do
      [:"$e" | _] = :digraph.add_edge(d, from, to)
    end
    :ok
  end
end
