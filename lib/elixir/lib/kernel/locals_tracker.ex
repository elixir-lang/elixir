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

  # Public API

  @doc """
  Returns all locals that are reachable.

  By default, all public functions are reachable.
  A private function is only reachable if it has
  a public function that invokes it directly.
  """
  def reachable(pid) do
    :gen_server.call(to_pid(pid), :reachable, @timeout)
  end

  defp to_pid(pid) when is_pid(pid),  do: pid
  defp to_pid(mod) when is_atom(mod), do: Module.get(mod, :__locals_tracker)

  # Internal API

  # Starts the tracker and returns its pid.
  @doc false
  def start_link do
    { :ok, pid } = :gen_server.start_link(__MODULE__, [], [])
    pid
  end

  # Adds a definition into the tracker. A public
  # definition is connected with the :local node
  # while a private one is left unreachable until
  # a call is made to.
  @doc false
  def add_definition(pid, kind, tuple) when kind in [:def, :defp, :defmacro, :defmacrop] do
    :gen_server.cast(pid, { :add_definition, kind, tuple })
  end

  # Adds a local dispatch to the given target.
  def add_local(pid, to) when is_tuple(to) do
    :gen_server.cast(pid, { :add_local, :local, to })
  end

  # Adds a local dispatch from-to the given target.
  @doc false
  def add_local(pid, from, to) when is_tuple(from) and is_tuple(to) do
    :gen_server.cast(pid, { :add_local, from, to })
  end

  # Collect all unused definitions based on the private
  # given also accounting the expected amount of default
  # clauses a private function have.
  @doc false
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

  # Stops the gen server
  @doc false
  def stop(pid) do
    :gen_server.cast(pid, :stop)
  end

  # Callbacks

  def init([]) do
    d = :digraph.new
    :digraph.add_vertex(d, :local)
    { :ok, d }
  end

  def handle_call(:reachable, _from, d) do
    { :reply, reduce_reachable(d, :local, []), d }
  end

  def handle_call(_request, _from, d) do
    { :noreply, d }
  end

  def handle_info(_msg, d) do
    { :noreply, d }
  end

  def handle_cast({ :add_local, from, to }, d) do
    :digraph.add_vertex(d, to)
    [:"$e"|_] = :digraph.add_edge(d, from, to)
    { :noreply, d }
  end

  def handle_cast({ :add_definition, public, tuple }, d) when public in [:def, :defmacro] do
    :digraph.add_vertex(d, tuple)
    :digraph.add_edge(d, :local, tuple)
    { :noreply, d }
  end

  def handle_cast({ :add_definition, private, tuple }, d) when private in [:defp, :defmacrop] do
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
