# This is a module Elixir responsible for tracking
# calls in order to extract Elixir modules' behaviour
# during compilation time.
#
# ## Implementation
#
# The implementation uses the digraph module to track
# all dependencies. The graph starts with three main
# vertices:
#
# * `:local` - points to local functions
# * `:import` - points to imported modules
# * `:warn` - points to imported modules that should be warned
# * `:remote` - points to remote modules
#
# Besides those, we have can the following vertices:
#
# * `Module` - a module that was invoked via an import or remotely
# * `{ name, arity }` - a local function/arity pair
# * `{ :import, name, arity }` - an invoked function/arity import
# * `{ :remote, name, arity }` - an remotely invoked function/arity
#
# Each of those vertices can associate to other vertices
# as described below:
#
# * `Module`
#   * in neighbours: `:import`, `:remote`, `:warn`,
#     `{ :import, name, arity }` and `{ :remote, name arity }`
#   * out neighbours: `:warn`
#
# * `{ name, arity }`
#   * in neighbours: `:local`, `{ name, arity }`
#   * out neighbours: `{ :import, name, arity }` and `{ :remote, name arity }`
#
# * `{ :import, name, arity }`
#   * in neighbours: `{ name, arity }`
#   * out neighbours: `Module`
#
# * `{ :remote, name, arity }`
#   * in neighbours: `{ name, arity }`
#   * out neighbours: `Module`
#
# Note that since this is required for bootstrap, we can't use
# any of the `GenServer.Behaviour` conveniences.
defmodule Module.DispatchTracker do
  @moduledoc false

  @timeout  30_000
  @behavior :gen_server

  # Public API

  @doc """
  Returns all the modules which were imported and
  made a call to.
  """
  def imports(pid) do
    d = :gen_server.call(to_pid(pid), :digraph, @timeout)
    :digraph.out_neighbours(d, :import)
  end

  @doc """
  Returns all imported modules that had the given
  { name, arity } invoked.
  """
  def imports_with_dispatch(pid, { name, arity }) do
    d = :gen_server.call(to_pid(pid), :digraph, @timeout)
    :digraph.out_neighbours(d, { :import, name, arity })
  end

  @doc """
  Returns all locals that are reachable.

  By default, all public functions are reachable.
  A private function is only reachable if it has
  a public function that it invokes directly.
  """
  def reachable(pid) do
    d = :gen_server.call(to_pid(pid), :digraph, @timeout)
    reduce_reachable(d, :local, [])
  end

  defp reduce_reachable(d, vertex, vertices) do
    neighbours = :digraph.out_neighbours(d, vertex) |> :ordsets.from_list
    remaining  = :ordsets.subtract(neighbours, vertices)
    vertices   = :ordsets.union(neighbours, vertices)
    :lists.foldl(reduce_reachable(d, &1, &2), vertices, remaining)
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

  # Adds a import dispatch to the given target.
  @doc false
  def add_import(pid, module, target) when is_atom(module) and is_tuple(target) do
    :gen_server.cast(pid, { :add_import, module, target })
  end

  # Associates a module with a warn. This adds the given
  # module and associates it with the `:import` vertex
  # permanently, even if warn is false.
  @doc false
  def add_warnable(pid, module, warn, line) when is_atom(module) and is_boolean(warn) do
    :gen_server.cast(pid, { :add_warnable, module, warn, line })
  end

  # Collect all unused imports where warn has been set to true.
  def collect_unused_imports(pid) do
    d = :gen_server.call(pid, :digraph)
    warnable = :digraph.out_neighbours(d, :warn)

    lc mod inlist warnable, not has_imports?(d, mod), line = get_warn_line(d, mod) do
      { mod, line }
    end
  end

  defp get_warn_line(d, mod) do
    [edge] = :digraph.out_edges(d, mod)
    { ^edge, ^mod, :warn, line } = :digraph.edge(d, edge)
    line
  end

  defp has_imports?(d, mod) do
    Enum.any?(:digraph.in_neighbours(d, mod), match?({ :import, _, _ }, &1))
  end

  # Collecting all conflicting imports with the given functions
  @doc false
  def collect_imports_conflicts(pid, all_defined) do
    d = :gen_server.call(to_pid(pid), :digraph, @timeout)

    lc { name, arity } inlist all_defined,
       n = :digraph.out_neighbours(d, { :import, name, arity }),
       n != [] do
      { n, name, arity }
    end
  end

  # Collect all unused definitions based on the private
  # given also accounting the expected amount of default
  # clauses a private function have.
  @doc false
  def collect_unused_locals(pid, private) do
    reachable = reachable(pid)
    :lists.foldl(collect_unused_locals(&1, &2, reachable), [], private)
  end

  defp collect_unused_locals({ tuple, kind, 0 }, acc, reachable) do
    if :lists.member(tuple, reachable) do
      acc
    else
      [{ :unused_def, tuple, kind }|acc]
    end
  end

  defp collect_unused_locals({ tuple, kind, default }, acc, reachable) when default > 0 do
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
    d = :digraph.new([:protected])
    :digraph.add_vertex(d, :local)
    :digraph.add_vertex(d, :import)
    :digraph.add_vertex(d, :warn)
    { :ok, d }
  end

  def handle_call(:digraph, _from, d) do
    { :reply, d, d }
  end

  def handle_call(_request, _from, d) do
    { :noreply, d }
  end

  def handle_info(_msg, d) do
    { :noreply, d }
  end

  def handle_cast({ :add_local, from, to }, d) do
    :digraph.add_vertex(d, to)
    replace_edge(d, from, to)
    { :noreply, d }
  end

  def handle_cast({ :add_import, module, { name, arity } }, d) do
    :digraph.add_vertex(d, module)
    replace_edge(d, :import, module)

    tuple = { :import, name, arity }
    :digraph.add_vertex(d, tuple)
    replace_edge(d, tuple, module)

    { :noreply, d }
  end

  def handle_cast({ :add_warnable, module, warn, line }, d) do
    :digraph.add_vertex(d, module)
    replace_edge(d, :import, module)

    if warn do
      :digraph.add_edge(d, :warn, module, line)
      :digraph.add_edge(d, module, :warn, line)
    else
      :digraph.del_path(d, :warn, module)
    end
    { :noreply, d }
  end

  def handle_cast({ :add_definition, public, tuple }, d) when public in [:def, :defmacro] do
    :digraph.add_vertex(d, tuple)
    replace_edge(d, :local, tuple)
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

  defp replace_edge(d, from, to) do
    unless :lists.member(to, :digraph.out_neighbours(d, from)) do
      [:"$e"|_] = :digraph.add_edge(d, from, to)
    end
  end
end
