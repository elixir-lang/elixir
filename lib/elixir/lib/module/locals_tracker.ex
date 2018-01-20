# This is an Elixir module responsible for tracking
# calls in order to extract Elixir modules' behaviour
# during compilation time.
#
# ## Implementation
#
# The implementation uses ets to track all dependencies
# resembling a graph. The graph has the following vertices:
#
#   * `Module` - a module that was invoked via an import
#   * `{name, arity}` - a local function/arity pair
#   * `{:import, name, arity}` - an invoked function/arity import
#   * `:reattach` - points to reattached functions
#
# Those vertices can associate to other vertices as described
# below:
#
#   * `{name, arity}`
#     * in neighbours: `:reattach`, `{name, arity}`
#     * out neighbours: `{:import, name, arity}`
#
#   * `{:import, name, arity}`
#     * in neighbours: `{name, arity}`
#     * out neighbours: `Module`
#
defmodule Module.LocalsTracker do
  @moduledoc false

  @doc """
  Starts the tracker table.
  """
  def init do
    :ets.new(__MODULE__, [:bag, :public])
  end

  @doc """
  Deletes the tracker table.
  """
  def delete(d) do
    :ets.delete(d)
  end

  @doc """
  Adds and tracks defaults for a definition into the tracker.
  """
  def add_defaults(d, _kind, {name, arity}, defaults) do
    for i <- :lists.seq(arity - defaults, arity - 1) do
      put_edge(d, {name, i}, {name, arity})
    end

    :ok
  end

  @doc """
  Adds a local dispatch from-to the given target.
  """
  def add_local(d, from, to) when is_tuple(from) and is_tuple(to) do
    put_edge(d, from, to)
  end

  @doc """
  Adds an import dispatch to the given target.
  """
  def add_import(d, function, module, {name, arity})
      when is_tuple(function) and is_atom(module) do
    tuple = {:import, name, arity}
    put_edge(d, tuple, module)
    put_edge(d, function, tuple)
    :ok
  end

  @doc """
  Yanks a local node. Returns its in and out vertices in a tuple.
  """
  def yank(d, local) do
    {[], take_out_neighbours(d, local)}
  end

  @doc """
  Reattach a previously yanked node.
  """
  def reattach(d, tuple, _kind, function, {in_neigh, out_neigh}) do
    # Reattach the old function
    for from <- in_neigh do
      put_edge(d, from, function)
    end

    for to <- out_neigh do
      put_edge(d, function, to)
    end

    # Make a call from the old function to the new one
    if function != tuple do
      put_edge(d, function, tuple)
    end

    # Finally marked the new one as reattached
    put_edge(d, :reattach, tuple)
    :ok
  end

  # Collecting all conflicting imports with the given functions
  @doc false
  def collect_imports_conflicts(d, all_defined) do
    for {{name, arity}, _, meta, _} <- all_defined,
        n = out_neighbours(d, {:import, name, arity}),
        n != [] do
      {meta, {n, name, arity}}
    end
  end

  @doc """
  Collect all unused definitions based on the private
  given, also accounting the expected number of default
  clauses a private function have.
  """
  def collect_unused_locals(d, all_defined, private) do
    reachable =
      Enum.reduce(all_defined, %{}, fn {pair, kind, _, _}, acc ->
        if kind in [:def, :defmacro] do
          reachable_from(d, pair, acc)
        else
          acc
        end
      end)

    reattached = out_neighbours(d, :reattach)
    {unreachable(reachable, reattached, private), collect_warnings(reachable, private)}
  end

  defp unreachable(reachable, reattached, private) do
    for {tuple, kind, _, _} <- private,
        not reachable?(tuple, kind, reachable, reattached),
        do: tuple
  end

  defp reachable?(tuple, :defmacrop, reachable, reattached) do
    # All private micros are unreachable unless they have been
    # reattached and they are reachable.
    :lists.member(tuple, reattached) and Map.has_key?(reachable, tuple)
  end

  defp reachable?(tuple, :defp, reachable, _reattached) do
    Map.has_key?(reachable, tuple)
  end

  defp collect_warnings(reachable, private) do
    :lists.foldl(&collect_warnings(&1, &2, reachable), [], private)
  end

  defp collect_warnings({_, _, false, _}, acc, _reachable) do
    acc
  end

  defp collect_warnings({tuple, kind, meta, 0}, acc, reachable) do
    if Map.has_key?(reachable, tuple) do
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
    case Map.has_key?(reachable, {name, max}) do
      true -> min_reachable_default(max - 1, min, max, name, reachable)
      false -> min_reachable_default(max - 1, min, last, name, reachable)
    end
  end

  defp min_reachable_default(_max, _min, last, _name, _reachable) do
    last
  end

  @doc """
  Returns all local nodes reachable from `vertex`.

  By default, all public functions are reachable.
  A private function is only reachable if it has
  a public function that it invokes directly.
  """
  def reachable_from(d, vertex) do
    d
    |> reachable_from(vertex, %{})
    |> Map.keys()
  end

  defp reachable_from(d, vertex, vertices) do
    vertices = Map.put(vertices, vertex, true)

    Enum.reduce(out_neighbours(d, vertex), vertices, fn
      {_, _} = local, acc ->
        case acc do
          %{^local => true} -> acc
          _ -> reachable_from(d, local, acc)
        end

      _, acc ->
        acc
    end)
  end

  ## Lightweight digraph implementation

  defp put_edge(d, from, to) do
    :ets.insert(d, {from, to})
  end

  defp out_neighbours(d, from) do
    try do
      :ets.lookup_element(d, from, 2)
    catch
      :error, :badarg -> []
    end
  end

  defp take_out_neighbours(d, from) do
    Keyword.values(:ets.take(d, from))
  end
end
