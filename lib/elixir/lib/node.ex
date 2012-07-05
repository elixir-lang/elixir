defmodule Node do
  @moduledoc """
  Functions related to Erlang nodes.
  """

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
end