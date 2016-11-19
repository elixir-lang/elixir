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
    reachable_from(:gen_server.call(to_pid(ref), :digraph, @timeout), :local)
  end

  defp reachable_from(d, starting) do
    :sets.to_list(reduce_reachable(d, starting, :sets.new))
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

    for {name, arity} <- all_defined,
        :digraph.in_neighbours(d, {:import, name, arity}) != [],
        n = :digraph.out_neighbours(d, {:import, name, arity}),
        n != [] do
      {n, name, arity}
    end
  end

  # Collect all unused definitions based on the private
  # given also accounting the expected amount of default
  # clauses a private function have.
  @doc false
  def collect_unused_locals(ref, private) do
    d = :gen_server.call(to_pid(ref), :digraph, @timeout)
    {unreachable(d, private), collect_warnings(d, private)}
  end

  defp unreachable(d, private) do
    unreachable = for {tuple, _, _} <- private, do: tuple

    private =
      for {tuple, :defp, _} <- private do
        neighbours = :digraph.in_neighbours(d, tuple)
        neighbours = for {_, _} = t <- neighbours, do: t
        {tuple, :sets.from_list(neighbours)}
      end

    reduce_unreachable(private, [], :sets.from_list(unreachable))
  end

  defp reduce_unreachable([{vertex, callers} | t], acc, unreachable) do
    if :sets.is_subset(callers, unreachable) do
      reduce_unreachable(t, [{vertex, callers} | acc], unreachable)
    else
      reduce_unreachable(acc ++ t, [], :sets.del_element(vertex, unreachable))
    end
  end

  defp reduce_unreachable([], _acc, unreachable) do
    :sets.to_list(unreachable)
  end

  defp collect_warnings(d, private) do
    reachable = reachable_from(d, :local)
    :lists.foldl(&collect_warnings(&1, &2, reachable), [], private)
  end

  defp collect_warnings({tuple, kind, 0}, acc, reachable) do
    if :lists.member(tuple, reachable) do
      acc
    else
      [{:unused_def, tuple, kind} | acc]
    end
  end

  defp collect_warnings({tuple, kind, default}, acc, reachable) when default > 0 do
    {name, arity} = tuple
    min = arity - default
    max = arity

    invoked = for {n, a} <- reachable, n == name, a in min..max, do: a

    if invoked == [] do
      [{:unused_def, tuple, kind} | acc]
    else
      case :lists.min(invoked) - min do
        0 -> acc
        ^default -> [{:unused_args, tuple} | acc]
        unused_args -> [{:unused_args, tuple, unused_args} | acc]
      end
    end
  end

  @doc false
  def cache_env(pid, env) do
    :gen_server.call(pid, {:cache_env, env}, @timeout)
  end

  @doc false
  def get_cached_env(pid, ref) do
    :gen_server.call(pid, {:get_cached_env, ref}, @timeout)
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
    {:ok, {d, []}}
  end

  @doc false
  def handle_call({:cache_env, env}, _from, {d, cache}) do
    case cache do
      [{i, ^env} | _] ->
        {:reply, i, {d, cache}}
      t ->
        i = length(t)
        {:reply, i, {d, [{i, env} | t]}}
    end
  end

  def handle_call({:get_cached_env, ref}, _from, {_, cache} = state) do
    {^ref, env} = :lists.keyfind(ref, 1, cache)
    {:reply, env, state}
  end

  def handle_call({:yank, local}, _from, {d, _} = state) do
    out_vertices = :digraph.out_neighbours(d, local)
    :digraph.del_edges(d, :digraph.out_edges(d, local))
    {:reply, {[], out_vertices}, state}
  end

  def handle_call(:digraph, _from, {d, _} = state) do
    {:reply, d, state}
  end

  @doc false
  def handle_info(_msg, state) do
    {:noreply, state}
  end

  def handle_cast({:add_local, from, to}, {d, _} = state) do
    handle_add_local(d, from, to)
    {:noreply, state}
  end

  def handle_cast({:add_import, function, module, {name, arity}}, {d, _} = state) do
    handle_import(d, function, module, name, arity)
    {:noreply, state}
  end

  def handle_cast({:add_definition, kind, tuple}, {d, _} = state) do
    handle_add_definition(d, kind, tuple)
    {:noreply, state}
  end

  def handle_cast({:add_defaults, kind, {name, arity}, defaults}, {d, _} = state) do
    for i <- :lists.seq(arity - defaults, arity - 1) do
      handle_add_definition(d, kind, {name, i})
      handle_add_local(d, {name, i}, {name, i + 1})
    end
    {:noreply, state}
  end

  def handle_cast({:reattach, _kind, tuple, {in_neigh, out_neigh}}, {d, _} = state) do
    for from <- in_neigh do
      :digraph.add_vertex(d, from)
      replace_edge!(d, from, tuple)
    end

    for to <- out_neigh do
      :digraph.add_vertex(d, to)
      replace_edge!(d, tuple, to)
    end

    {:noreply, state}
  end

  def handle_cast(:stop, state) do
    {:stop, :normal, state}
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
