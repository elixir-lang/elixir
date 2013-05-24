# This is a module Elixir responsible for tracking
# local calls in order to emit proper warnings when
# any local private function is unused.
#
# Since this is required for bootstrap, we can't use
# any of the GenServer.Behaviour conveniences.
defmodule Kernel.LocalsTracker do
  @moduledoc false

  @timeout  30_000
  @behavior :gen_server

  def start_link do
    { :ok, pid } = :gen_server.start_link(__MODULE__, [], [])
    pid
  end

  def add_definition(pid, kind, tuple) when kind in [:def, :defp, :defmacro, :defmacrop] do
    :gen_server.cast(pid, { :vertex, kind, tuple })
  end

  def add_dispatch(pid, from, to) do
    :gen_server.cast(pid, { :edge, from, to })
  end

  def reachable(pid) do
    :gen_server.call(pid, :reachable, @timeout)
  end

  def collect_unused(pid, private) do
    # Add a vertex for each private given
    lc { tuple, kind, _defaults } inlist private do
      add_definition(pid, kind, tuple)
    end

    # Process all unused
    reachable = reachable(pid)
    :lists.foldl(collect_unused(&1, &2, reachable), [], private)
  end

  defp collect_unused({ tuple, kind, 0 }, acc, reachable) do
    if :lists.member(tuple, reachable) do
      acc
    else
      [{ :unused_def, tuple, kind }|acc]
    end
  end

  defp collect_unused({ tuple, kind, default }, acc, reachable) when default > 0 do
    { name, arity } = tuple
    min = arity - default
    max = arity

    invoked = lc { n, a } inlist reachable, n == name, a in min..max, do: a

    if invoked == [] do
      [{ :unused_def, tuple, kind }|acc]
    else
      case :lists.min(invoked) - min do
        0 -> acc
        ^default -> [{ :unused_args, tuple }|acc]
        unused_args -> [{ :unused_args, tuple, unused_args }|acc]
      end
    end
  end

  def stop(pid) do
    :gen_server.cast(pid, :stop)
  end

  # Callbacks

  def init([]) do
    d = :digraph.new
    :digraph.add_vertex(d, :root)
    { :ok, d }
  end

  def handle_call(:reachable, _from, d) do
    { :reply, reduce_reachable(d, :root, []), d }
  end

  def handle_call(_request, _from, d) do
    { :noreply, d }
  end

  def handle_info(_msg, d) do
    { :noreply, d }
  end

  def handle_cast({ :edge, from, to }, d) do
    :digraph.add_vertex(d, to)
    [:"$e"|_] = :digraph.add_edge(d, from, to)
    { :noreply, d }
  end

  def handle_cast({ :vertex, public, tuple }, d) when public in [:def, :defmacro] do
    :digraph.add_vertex(d, tuple)
    :digraph.add_edge(d, :root, tuple)
    { :noreply, d }
  end

  def handle_cast({ :vertex, private, tuple }, d) when private in [:defp, :defmacrop] do
    :digraph.add_vertex(d, tuple)
    { :noreply, d }
  end

  def handle_cast(:stop, d) do
    { :stop, :normal, d }
  end

  def handle_cast(_msg, d) do
    { :noreply, d }
  end

  def terminate(_reason, _d) do
    :ok
  end

  def code_change(_old, d, _extra) do
    { :ok, d }
  end

  # Helpers

  defp reduce_reachable(d, vertex, vertices) do
    neighbours = :digraph.out_neighbours(d, vertex)
    remaining  = neighbours -- vertices
    vertices   = neighbours ++ vertices
    :lists.foldl(reduce_reachable(d, &1, &2), vertices, remaining)
  end
end
