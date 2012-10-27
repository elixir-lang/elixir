defmodule Node do
  @moduledoc """
  Functions related to Erlang nodes.
  """

  @doc """
  Returns the current node. It returns the same as the built-in node().
  """
  def self do
    :erlang.node()
  end

  @doc """
  Returns true if the local node is alive; that is, if the node can be
  part of a distributed system. Otherwise, it returns false.
  """
  def alive? do
    :erlang.is_alive()
  end

  @doc """
  Returns a list of all visible nodes in the system, excluding
  the local node. Same as list(visible).
  """
  def list do
    :erlang.nodes()
  end

  @doc """
  Returns a list of nodes according to argument given. The result
  returned when the argument is a list, is the list of nodes
  satisfying the disjunction(s) of the list elements.
  
  See http://www.erlang.org/doc/man/erlang.html#nodes-1 for more info.
  """
  def list(args) do
    :erlang.nodes(args)
  end

  @doc """
  Monitors the status of the node. If flag is true, monitoring is
  turned on. If flag is false, monitoring is turned off.

  See http://www.erlang.org/doc/man/erlang.html#monitor_node-2 for more info.
  """
  def monitor(node, flag) do
    :erlang.monitor_node(node, flag)
  end

  @doc """
  Behaves as monitor_node/2 except that it allows an extra
  option to be given, namely :allow_passive_connect.

  See http://www.erlang.org/doc/man/erlang.html#monitor_node-3 for more info.
  """
  def monitor(node, flag, options) do
    :erlang.monitor_node(node, flag, options)
  end

  @doc """
  Forces the disconnection of a node. This will appear to the `node` as if
  the local node has crashed. This BIF is mainly used in the Erlang network
  authentication protocols. Returns true if disconnection succeeds, otherwise
  false. If the local node is not alive, the function returns ignored.
  
  See http://www.erlang.org/doc/man/erlang.html#disconnect_node-1 for more info.
  """
  def disconnect(node) do
    :erlang.disconnect_node(node)
  end

  @doc """
  Returns the pid of a new process started by the application of `fun`
  on `node`. If `node` does not exist, a useless pid is returned.

  Check http://www.erlang.org/doc/man/erlang.html#spawn-2 for
  the list of available options.
  """
  def spawn(node, fun) do
    :erlang.spawn(node, fun)
  end

  @doc """
  Returns the pid of a new process started by the application of `fun`
  on `node`. If `node` does not exist, a useless pid is returned.

  Check http://www.erlang.org/doc/man/erlang.html#spawn_opt-3 for
  the list of available options.
  """
  def spawn(node, fun, opts) do
    :erlang.spawn_opt(node, fun, opts)
  end

  @doc """
  Returns the pid of a new process started by the application of
  `module.function(args)` on `node`. If `node` does not exists,
  a useless pid is returned.

  Check http://www.erlang.org/doc/man/erlang.html#spawn-4 for
  the list of available options.
  """
  def spawn(node, module, fun, args) do
    :erlang.spawn(node, module, fun, args)
  end

  @doc """
  Returns the pid of a new process started by the application of
  `module.function(args)` on `node`. If `node` does not exists,
  a useless pid is returned.

  Check http://www.erlang.org/doc/man/erlang.html#spawn_opt-5 for
  the list of available options.
  """
  def spawn(node, module, fun, args, opts) do
    :erlang.spawn_opt(node, module, fun, args, opts)
  end

  @doc """
  Returns the pid of a new process started by the application of `fun`
  on `node`. A link is created between the calling process and the
  new process, atomically. If `node` does not exist, a useless pid is returned
  (and due to the link, an exit signal with exit reason :noconnection will be
  received).
  """
  def spawn_link(node, fun) do
    :erlang.spawn_link(node, fun)
  end

  @doc """
  Returns the pid of a new process started by the application of
  `module.function(args)` on `node`. A link is created between the calling
  process and the new process, atomically. If `node` does not exist, a useless
  pid is returned (and due to the link, an exit signal with exit reason
  :noconnection will be received).
  """
  def spawn_link(node, module, fun, args) do
    :erlang.spawn_link(node, module, fun, args)
  end
end