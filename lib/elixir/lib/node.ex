defmodule Node do
  @moduledoc """
  Functions related to VM nodes.

  Some of the functions in this module are inlined by the compiler,
  similar to functions in the `Kernel` module and they are explicitly
  marked in their docs as "inlined by the compiler". For more information
  about inlined functions, check out the `Kernel` module.
  """

  @type t :: node

  @doc """
  Turns a non-distributed node into a distributed node.

  This functionality starts the `:net_kernel` and other
  related processes.
  """
  @spec start(node, :longnames | :shortnames, non_neg_integer) ::
             {:ok, pid} | {:error, term}
  def start(name, type \\ :longnames, tick_time \\ 15000) do
    :net_kernel.start([name, type, tick_time])
  end

  @doc """
  Turns a distributed node into a non-distributed node.

  For other nodes in the network, this is the same as the node going down.
  Only possible when the node was started with `Node.start/3`, otherwise
  returns `{:error, :not_allowed}`. Returns `{:error, :not_found}` if the
  local node is not alive.
  """
  @spec stop() :: :ok | {:error, :not_allowed | :not_found}
  def stop() do
    :net_kernel.stop()
  end

  @doc """
  Returns the current node.

  It returns the same as the built-in `node()`.
  """
  @spec self :: t
  def self do
    :erlang.node()
  end

  @doc """
  Returns `true` if the local node is alive.

  That is, if the node can be part of a distributed system.
  """
  @spec alive? :: boolean
  def alive? do
    :erlang.is_alive()
  end

  @doc """
  Returns a list of all visible nodes in the system, excluding
  the local node.

  Same as `list(:visible)`.
  """
  @spec list :: [t]
  def list do
    :erlang.nodes()
  end

  @doc """
  Returns a list of nodes according to argument given.

  The result returned when the argument is a list, is the list of nodes
  satisfying the disjunction(s) of the list elements.

  For more information, see
  [`:erlang.nodes/1`](http://www.erlang.org/doc/man/erlang.html#nodes-1).
  """
  @typep state :: :visible | :hidden | :connected | :this | :known
  @spec list(state | [state]) :: [t]
  def list(args) do
    :erlang.nodes(args)
  end

  @doc """
  Monitors the status of the node.

  If `flag` is `true`, monitoring is turned on.
  If `flag` is `false`, monitoring is turned off.

  For more information, see
  [`:erlang.monitor_node/2`](http://www.erlang.org/doc/man/erlang.html#monitor_node-2).
  """
  @spec monitor(t, boolean) :: true
  def monitor(node, flag) do
    :erlang.monitor_node(node, flag)
  end

  @doc """
  Behaves as `monitor/2` except that it allows an extra
  option to be given, namely `:allow_passive_connect`.

  For more information, see
  [`:erlang.monitor_node/3`](http://www.erlang.org/doc/man/erlang.html#monitor_node-3).
  """
  @spec monitor(t, boolean, [:allow_passive_connect]) :: true
  def monitor(node, flag, options) do
    :erlang.monitor_node(node, flag, options)
  end

  @doc """
  Tries to set up a connection to node.

  Returns `:pang` if it fails, or `:pong` if it is successful.

  ## Examples

      iex> Node.ping(:unknown_node)
      :pang

  """
  @spec ping(t) :: :pong | :pang
  def ping(node) do
    :net_adm.ping(node)
  end

  @doc """
  Forces the disconnection of a node.

  This will appear to the `node` as if the local node has crashed.
  This function is mainly used in the Erlang network authentication
  protocols. Returns `true` if disconnection succeeds, otherwise `false`.
  If the local node is not alive, the function returns `:ignored`.

  For more information, see
  [`:erlang.disconnect_node/1`](http://www.erlang.org/doc/man/erlang.html#disconnect_node-1).
  """
  @spec disconnect(t) :: boolean | :ignored
  def disconnect(node) do
    :erlang.disconnect_node(node)
  end

  @doc """
  Establishes a connection to `node`.

  Returns `true` if successful, `false` if not, and the atom
  `:ignored` if the local node is not alive.

  For more information, see
  [`:net_kernel.connect_node/1`](http://www.erlang.org/doc/man/net_kernel.html#connect_node-1).
  """
  @spec connect(t) :: boolean | :ignored
  def connect(node) do
    :net_kernel.connect_node(node)
  end

  @doc """
  Returns the PID of a new process started by the application of `fun`
  on `node`. If `node` does not exist, a useless PID is returned.

  For the list of available options, see
  [`:erlang.spawn/2`](http://www.erlang.org/doc/man/erlang.html#spawn-2).

  Inlined by the compiler.
  """
  @spec spawn(t, (() -> any)) :: pid
  def spawn(node, fun) do
    :erlang.spawn(node, fun)
  end

  @doc """
  Returns the PID of a new process started by the application of `fun`
  on `node`.

  If `node` does not exist, a useless PID is returned.

  For the list of available options, see
  [`:erlang.spawn_opt/3`](http://www.erlang.org/doc/man/erlang.html#spawn_opt-3).

  Inlined by the compiler.
  """
  @spec spawn(t, (() -> any), Process.spawn_opts) :: pid | {pid, reference}
  def spawn(node, fun, opts) do
    :erlang.spawn_opt(node, fun, opts)
  end

  @doc """
  Returns the PID of a new process started by the application of
  `module.function(args)` on `node`.

  If `node` does not exist, a useless PID is returned.

  For the list of available options, see
  [`:erlang.spawn/4`](http://www.erlang.org/doc/man/erlang.html#spawn-4).

  Inlined by the compiler.
  """
  @spec spawn(t, module, atom, [any]) :: pid
  def spawn(node, module, fun, args) do
    :erlang.spawn(node, module, fun, args)
  end

  @doc """
  Returns the PID of a new process started by the application of
  `module.function(args)` on `node`.

  If `node` does not exist, a useless PID is returned.

  For the list of available options, see
  [`:erlang.spawn/5`](http://www.erlang.org/doc/man/erlang.html#spawn_opt-5).

  Inlined by the compiler.
  """
  @spec spawn(t, module, atom, [any], Process.spawn_opts) :: pid | {pid, reference}
  def spawn(node, module, fun, args, opts) do
    :erlang.spawn_opt(node, module, fun, args, opts)
  end

  @doc """
  Returns the PID of a new linked process started by the application of `fun` on `node`.

  A link is created between the calling process and the new process, atomically.
  If `node` does not exist, a useless PID is returned (and due to the link, an exit
  signal with exit reason `:noconnection` will be received).

  Inlined by the compiler.
  """
  @spec spawn_link(t, (() -> any)) :: pid
  def spawn_link(node, fun) do
    :erlang.spawn_link(node, fun)
  end

  @doc """
  Returns the PID of a new linked process started by the application of
  `module.function(args)` on `node`.

  A link is created between the calling process and the new process, atomically.
  If `node` does not exist, a useless PID is returned (and due to the link, an exit
  signal with exit reason `:noconnection` will be received).

  Inlined by the compiler.
  """
  @spec spawn_link(t, module, atom, [any]) :: pid
  def spawn_link(node, module, fun, args) do
    :erlang.spawn_link(node, module, fun, args)
  end

  @doc """
  Sets the magic cookie of `node` to the atom `cookie`.

  The default node is `Node.self/0`, the local node. If `node` is the local node,
  the function also sets the cookie of all other unknown nodes to `cookie`.

  This function will raise `FunctionClauseError` if the given `node` is not alive.
  """
  def set_cookie(node \\ Node.self, cookie) when is_atom(cookie) do
    :erlang.set_cookie(node, cookie)
  end

  @doc """
  Returns the magic cookie of the local node.

  Returns the cookie if the node is alive, otherwise `:nocookie`.
  """
  def get_cookie() do
    :erlang.get_cookie()
  end
end
