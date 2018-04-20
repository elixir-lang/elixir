defmodule Global do
  @moduledoc """
  A global name registration facility.

  This module consists of the following services:

  Registration of global names
  Global locks
  Maintenance of the fully connected network
  These services are controlled through the process global_name_server that exists on every node.
  The global name server starts automatically when a node is started.
  With the term global is meant over a system consisting of many Erlang nodes.

  The ability to globally register names is a central concept in the programming of distributed Erlang systems.
  In this module, the equivalent of the register/2 and whereis/1 BIFs (for local name registration) are provided,
  but for a network of Erlang nodes. A registered name is an alias for a process identifier (pid).
  The global name server monitors globally registered pids. If a process terminates, the name is also globally unregistered.

  The registered names are stored in replica global name tables on every node.
  There is no central storage point. Thus, the translation of a name to a pid is fast, as it is always done locally.
  For any action resulting in a change to the global name table, all tables on other nodes are automatically updated.

  Global locks have lock identities and are set on a specific resource.
  For example, the specified resource can be a pid. When a global lock is set,
  access to the locked resource is denied for all resources other than the lock requester.

  Both the registration and lock services are atomic. All nodes involved in these actions have the same view of the information.

  The global name server also performs the critical task of continuously monitoring changes in node configuration.
  If a node that runs a globally registered process goes down, the name is globally unregistered.
  To this end, the global name server subscribes to nodeup and nodedown messages sent from module net_kernel.
  Relevant Kernel application variables in this context are net_setuptime, net_ticktime, and dist_auto_connect.
  See also kernel(6).

  The name server also maintains a fully connected network.
  For example, if node N1 connects to node N2 (which is already connected to N3),
  the global name servers on the nodes N1 and N3 ensure that also N1 and N3 are connected.
  If this is not desired, command-line flag -connect_all false can be used (see also erl(1)).
  In this case, the name registration service cannot be used, but the lock mechanism still works.

  If the global name server fails to connect nodes (N1 and N3 in the example), a warning event is sent to the error logger.
  The presence of such an event does not exclude the nodes to connect later (you can, for example, try command rpc:call(N1, net_adm, ping, [N2]) in the Erlang shell), but it indicates a network problem.
  """

  @type id :: {resource_id :: term, lock_requester_id :: term}

  @doc """
  Deletes the lock Id synchronously.
  """
  @spec del_lock(id) :: true
  defdelegate del_lock(id), to: :global

  @spec del_lock(id, [node]) :: true
  defdelegate del_lock(id, nodes), to: :global

  @doc """
  Can be used as a name resolving function for register_name/3 and re_register_name/3.

  The function unregisters both pids and sends the message `{:global_name_conflict, name, other_pid}` to both processes.
  """
  @spec notify_all_name(atom, pid, pid) :: nil
  defdelegate notify_all_name(id, pid1, pid2), to: :global

  @doc """
  Can be used as a name resolving function for register_name/3 and re_register_name/3.

  The function randomly selects one of the pids for registration and kills the other one.
  """
  @spec random_exit_name(atom, pid, pid) :: pid
  defdelegate random_exit_name(id, pid1, pid2), to: :global

  @doc """
  Can be used as a name resolving function for register_name/3 and re_register_name/3.

  The function randomly selects one of the pids for registration, and sends the message {:global_name_conflict, name} to the other pid.
  """
  @spec random_notify_name(atom, pid, pid) :: pid
  defdelegate random_notify_name(id, pid1, pid2), to: :global

  @doc """
  Atomically changes the registered name Name on all nodes to refer to Pid.

  Function Resolve has the same behavior as in register_name/2,3.
  """
  @spec re_register_name(atom, pid) :: pid
  defdelegate re_register_name(id, pid), to: :global

  @spec re_register_name(atom, pid, (atom, pid, pid -> pid)) :: pid
  defdelegate re_register_name(id, pid, resolve), to: :global

  @doc """
  Globally associates name Name with a pid, that is, globally notifies all nodes of a new global name in a network of Erlang nodes.

  When new nodes are added to the network, they are informed of the globally registered names that already exist.
  The network is also informed of any global names in newly connected nodes.
  If any name clashes are discovered, function Resolve is called. Its purpose is to decide which pid is correct.
  If the function crashes, or returns anything other than one of the pids, the name is unregistered.
  This function is called once for each name clash.

  WARNING: If you plan to change code without restarting your system,
  you must use an external fun (fun Module:Function/Arity) as function Resolve.
  If you use a local fun, you can never replace the code for the module that the fun belongs to.

  Three predefined resolve functions exist: random_exit_name/3, random_notify_name/3, and notify_all_name/3.
  If no Resolve function is defined, random_exit_name is used.
  This means that one of the two registered processes is selected as correct while the other is killed.

  This function is completely synchronous, that is, when this function returns, the name is either registered on all nodes or none.

  The function returns yes if successful, no if it fails.
  For example, no is returned if an attempt is made to register an already registered process or to register a process with a name that is already in use.

  Note:
  Releases up to and including Erlang/OTP R10 did not check if the process was already registered.
  The global name table could therefore become inconsistent.
  The old (buggy) behavior can be chosen by giving the Kernel application variable global_multi_name_action the value allow.

  If a process with a registered name dies, or the node goes down, the name is unregistered on all nodes.
  """
  @spec register_name(atom, pid) :: :yes | :no
  defdelegate register_name(name, pid), to: :global

  @spec register_name(atom, pid, (atom, pid, pid -> pid)) :: :yes | :no
  defdelegate register_name(name, pid, resolve), to: :global

  @doc """
  Returns a list of all globally registered names.
  """
  @spec registered_names() :: [atom]
  defdelegate registered_names(), to: :global

  @doc """
  Sends message Msg to the pid globally registered as Name.

  If Name is not a globally registered name, the calling function exits with reason {badarg, {Name, Msg}}.
  """
  @spec send(atom, term) :: pid | {:badarg, {atom, term}}
  defdelegate send(name, msg), to: :global

  @doc """
  Sets a lock on the specified nodes (or on all nodes if none are specified) on ResourceId for LockRequesterId.
  If a lock already exists on ResourceId for another requester than LockRequesterId, and Retries is not equal to 0,
  the process sleeps for a while and tries to execute the action later.
  When Retries attempts have been made, false is returned, otherwise true.
  If Retries is infinity, true is eventually returned (unless the lock is never released).

  If no value for Retries is specified, infinity is used.

  This function is completely synchronous.

  If a process that holds a lock dies, or the node goes down, the locks held by the process are deleted.

  The global name server keeps track of all processes sharing the same lock, that is,
  if two processes set the same lock, both processes must delete the lock.

  This function does not address the problem of a deadlock.
  A deadlock can never occur as long as processes only lock one resource at a time.
  A deadlock can occur if some processes try to lock two or more resources.
  It is up to the application to detect and rectify a deadlock.

  NOTE:
  Avoid the following values of ResourceId, otherwise Erlang/OTP does not work properly:

  * dist_ac
  * global
  * mnesia_adjust_log_writes
  * mnesia_table_lock
  * pg2
  """
  @spec set_lock(id) :: boolean
  defdelegate set_lock(id), to: :global

  @spec set_lock(id, [node]) :: boolean
  defdelegate set_lock(id, nodes), to: :global

  @spec set_lock(id, [node], term) :: boolean
  defdelegate set_lock(id, nodes, retries), to: :global

  @doc """
  Synchronizes the global name server with all nodes known to this node. These are the nodes that are returned from erlang:nodes(). When this function returns, the global name server receives global information from all nodes. This function can be called when new nodes are added to the network.

  The only possible error reason Reason is {:"global_groups definition error", term}.
  """
  @spec sync() :: :ok | {:error, reason :: term}
  defdelegate sync(), to: :global

  @doc """
  Sets a lock on Id (using set_lock/3). If this succeeds, Fun() is evaluated and the result Res is returned. Returns aborted if the lock attempt fails. If Retries is set to infinity, the transaction does not abort.

  :infinity is the default setting and is used if no value is specified for Retries.
  """
  @spec trans(id, (() -> any)) :: any | :aborted
  defdelegate trans(id, fun), to: :global

  @spec trans(id, (() -> any), [node]) :: any | :aborted
  defdelegate trans(id, fun, nodes), to: :global

  @spec trans(id, (() -> any), [node], term) :: any | :aborted
  defdelegate trans(id, fun, nodes, retries), to: :global

  @doc """
  Removes the globally registered name Name from the network of Erlang nodes.
  """
  @spec unregister_name(atom) :: term
  defdelegate unregister_name(name), to: :global

  @doc """
  Returns the pid with the globally registered name Name. Returns undefined if the name is not globally registered.
  """
  @spec whereis_name(atom) :: pid | nil
  def whereis_name(name) do
    nillify(:global.whereis_name(name))
  end

  @compile {:inline, nillify: 1}
  defp nillify(:undefined), do: nil
  defp nillify(other), do: other
end
