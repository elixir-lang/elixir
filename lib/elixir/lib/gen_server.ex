defmodule GenServer do
  @moduledoc """
  A behaviour module for implementing the server of a client-server relation.

  A GenServer is a process like any other Elixir process and it can be used
  to keep state, execute code asynchronously and so on. The advantage of using
  a generic server process (GenServer) implemented using this module is that it
  will have a standard set of interface functions and include functionality for
  tracing and error reporting. It will also fit into a supervision tree.

  ## Example

  The GenServer behaviour abstracts the common client-server interaction.
  Developers are only required to implement the callbacks and functionality they are
  interested in.

  Let's start with a code example and then explore the available callbacks.
  Imagine we want a GenServer that works like a stack, allowing us to push
  and pop items:

      defmodule Stack do
        use GenServer

        # Callbacks

        def handle_call(:pop, _from, [h|t]) do
          {:reply, h, t}
        end

        def handle_cast({:push, item}, state) do
          {:noreply, [item|state]}
        end
      end

      # Start the server
      {:ok, pid} = GenServer.start_link(Stack, [:hello])

      # This is the client
      GenServer.call(pid, :pop)
      #=> :hello

      GenServer.cast(pid, {:push, :world})
      #=> :ok

      GenServer.call(pid, :pop)
      #=> :world

  We start our `Stack` by calling `start_link/3`, passing the module
  with the server implementation and its initial argument (a list
  representing the stack containing the item `:hello`). We can primarily
  interact with the server by sending two types of messages. **call**
  messages expect a reply from the server (and are therefore synchronous)
  while **cast** messages do not.

  Every time you do a `GenServer.call/3`, the client will send a message
  that must be handled by the `handle_call/3` callback in the GenServer.
  A `cast/2` message must be handled by `handle_cast/2`.

  ## Callbacks

  There are 6 callbacks required to be implemented in a `GenServer`. By
  adding `use GenServer` to your module, Elixir will automatically define
  all 6 callbacks for you, leaving it up to you to implement the ones
  you want to customize.

  ## Name Registration

  Both `start_link/3` and `start/3` support the `GenServer` to register
  a name on start via the `:name` option. Registered names are also
  automatically cleaned up on termination. The supported values are:

    * an atom - the GenServer is registered locally with the given name
      using `Process.register/2`.

    * `{:global, term}`- the GenServer is registered globally with the given
      term using the functions in the `:global` module.

    * `{:via, module, term}` - the GenServer is registered with the given
      mechanism and name. The `:via` option expects a module that exports
      `register_name/2`, `unregister_name/1`, `whereis_name/1` and `send/2`.
      One such example is the `:global` module which uses these functions
      for keeping the list of names of processes and their  associated pid's
      that are available globally for a network of Erlang nodes.

  For example, we could start and register our Stack server locally as follows:

      # Start the server and register it locally with name MyStack
      {:ok, _} = GenServer.start_link(Stack, [:hello], name: MyStack)

      # Now messages can be sent directly to MyStack
      GenServer.call(MyStack, :pop) #=> :hello

  Once the server is started, the remaining functions in this module (`call/3`,
  `cast/2`, and friends) will also accept an atom, or any `:global` or `:via`
  tuples. In general, the following formats are supported:

    * a `pid`
    * an `atom` if the server is locally registered
    * `{atom, node}` if the server is locally registered at another node
    * `{:global, term}` if the server is globally registered
    * `{:via, module, name}` if the server is registered through an alternative
      registry

  ## Client / Server APIs

  Although in the example above we have used `GenServer.start_link/3` and
  friends to directly start and communicate with the server, most of the
  time we don't call the `GenServer` functions directly. Instead, we wrap
  the calls in new functions representing the public API of the server.

  Here is a better implementation of our Stack module:

      defmodule Stack do
        use GenServer

        # Client

        def start_link(default) do
          GenServer.start_link(__MODULE__, default)
        end

        def push(pid, item) do
          GenServer.cast(pid, {:push, item})
        end

        def pop(pid) do
          GenServer.call(pid, :pop)
        end

        # Server (callbacks)

        def handle_call(:pop, _from, [h|t]) do
          {:reply, h, t}
        end

        def handle_call(request, from, state) do
          # Call the default implementation from GenServer
          super(request, from, state)
        end

        def handle_cast({:push, item}, state) do
          {:noreply, [item|state]}
        end

        def handle_cast(request, state) do
          super(request, state)
        end
      end

  In practice, it is common to have both server and client functions in
  the same module. If the server and/or client implementations are growing
  complex, you may want to have them in different modules.

  ## Receiving custom messages

  The goal of a `GenServer` is to abstract the "receive" loop for developers,
  automatically handling system messages, support code change, synchronous
  calls and more. Therefore, you should never call your own "receive" inside
  the GenServer callbacks as doing so will cause the GenServer to misbehave.
  If you want to receive custom messages, always receive them in `handle_info/2`.

  ## Learn more

  If you wish to find out more about gen servers, the Elixir Getting Started
  guide provides a tutorial-like introduction. The documentation and links
  in Erlang can also provide extra insight.

    * [GenServer – Elixir's Getting Started Guide](http://elixir-lang.org/getting-started/mix-otp/genserver.html)
    * [`:gen_server` module documentation](http://www.erlang.org/doc/man/gen_server.html)
    * [gen_server Behaviour – OTP Design Principles](http://www.erlang.org/doc/design_principles/gen_server_concepts.html)
    * [Clients and Servers – Learn You Some Erlang for Great Good!](http://learnyousomeerlang.com/clients-and-servers)
  """

  @doc """
  Invoked when the server is started. `start_link/3` (or `start/3`) will
  block until it returns.

  `args` is the argument term (second argument) passed to `start_link/3`.

  Returning `{:ok, state}` will cause `start_link/3` to return
  `{:ok, pid}` and the process to enter its loop.

  Returning `{:ok, state, timeout}` is similar to `{:ok, state}`
  except `handle_info(:timeout, state)` will be called after `timeout`
  milliseconds if no messages are received within the timeout.

  Returning `{:ok, state, :hibernate}` is similar to
  `{:ok, state}` except the process is hibernated before entering the loop. See
  `handle_call/3` for more information on hibernation.

  Returning `:ignore` will cause `start_link/3` to return `:ignore` and the
  process will exit normally without entering the loop or calling `terminate/2`.
  If used when part of a supervision tree the parent supervisor will not fail
  to start nor immediately try to restart the `GenServer`. The remainder of the
  supervision tree will be (re)started and so the `GenServer` should not be
  required by other processes. It can be started later with
  `Supervisor.restart_child/2` as the child specification is saved in the parent
  supervisor. The main use cases for this are:

  - The `GenServer` is disabled by configuration but might be enabled later.
  - An error occured and it will be handled by a different mechanism than the
  `Supervisor`. Likely this approach involves calling `Supervisor.restart_child/2`
  after a delay to attempt a restart.

  Returning `{:stop, reason}` will cause `start_link/3` to return
  `{:error, reason}` and the process to exit with reason `reason` without
  entering the loop or calling `terminate/2`.
  """
  @callback init(args :: term) ::
    {:ok, state} |
    {:ok, state, timeout | :hibernate} |
    :ignore |
    {:stop, reason :: any} when state: any

  @doc """
  Invoked to handle synchronous `call/3` messages. `call/3` will block until a
  reply is received (unless the call times out or nodes are disconnected).

  `request` is the request message sent by a `call/3`, `from` is a 2-tuple
  containing the caller's pid and a term that uniquely identifies the call, and
  `state` is the current state of the `GenServer`.

  Returning `{:reply, reply, new_state}` sends the response `reply` to the
  caller and continues the loop with new state `new_state`.

  Returning `{:reply, reply, new_state, timeout}` is similar to
  `{:reply, reply, new_state}` except `handle_info(:timeout, new_state)` will be
  called after `timeout` milliseconds if no messages are received.

  Returning `{:reply, reply, new_state, :hibernate}` is similar to
  `{:reply, reply, new_state}` except the process is hibernated and will
  continue the loop once a message is its message queue. If a message is already
  in the message queue this will be immediately. Hibernating a `GenServer`
  causes garbage collection and leaves a continuous heap that minimises the
  memory used by the process.

  Hibernating should not be used aggressively as too much time could be spent
  garbage collecting. Normally it should only be used when a message is not
  expected soon and minimising the memory of the process is shown to be
  beneficial.

  Returning `{:noreply, new_state}` does not send a response to the caller and
  continues the loop with new state `new_state`. The response must be sent with
  `reply/2`.

  There are three main use cases for not replying using the return value:

  - To reply before returning from the callback because the response is known
  before calling a slow function.
  - To reply after returning from the callback because the response is not yet
  available.
  - To reply from another process, such as a task.

  When replying from another process the `GenServer` should exit if the other
  process exits without replying as the caller will be blocking awaiting a
  reply.

  Returning `{:noreply, new_state, timeout | :hibernate}` is similar to
  `{:noreply, new_state}` except a timeout or hibernation occurs as with a
  `:reply` tuple.

  Returning `{:stop, reason, reply, new_state}` stops the loop and `terminate/2`
  is called with reason `reason` and state `new_state`. Then the `reply` is sent
  as the response to call and the process exits with reason `reason`.

  Returning `{:stop, reason, new_state}` is similar to
  `{:stop, reason, reply, new_state}` except a reply is not sent.
  """
  @callback handle_call(request :: term, from, state :: term) ::
    {:reply, reply, new_state} |
    {:reply, reply, new_state, timeout | :hibernate} |
    {:noreply, new_state} |
    {:noreply, new_state, timeout | :hibernate} |
    {:stop, reason, reply, new_state} |
    {:stop, reason, new_state} when reply: term, new_state: term, reason: term

  @doc """
  Invoked to handle asynchronous `cast/2` messages.

  `request` is the request message sent by a `cast/2` and `state` is the current
  state of the `GenServer`.

  Returning `{:noreply, new_state}` continues the loop with new state `new_state`.

  Returning `{:noreply, new_state, timeout}` is similar to
  `{:noreply, reply, new_state}` except `handle_info(:timeout, new_state)` will
  be called after `timeout` milliseconds if no messages are received.

  Returning `{:noreply, new_state, :hibernate}` is similar to
  `{:noreply, new_state}` except the process is hibernated before continuing the
  loop. See `handle_call/3` for more information.

  Returning `{:stop, reason, new_state}` stops the loop and `terminate/2` is
  called with the reason `reason` and state `new_state`. The process exits with
  reason `reason`.
  """
  @callback handle_cast(request :: term, state :: term) ::
    {:noreply, new_state} |
    {:noreply, new_state, timeout | :hibernate} |
    {:stop, reason :: term, new_state} when new_state: term

  @doc """
  Invoked to handle all other messages.

  `msg` is the message and `state` is the current state of the `GenServer`. When
  a timeout occurs the message is `:timeout`.

  Return values are the same as `handle_cast/2`.
  """
  @callback handle_info(msg :: :timeout | term, state :: term) ::
    {:noreply, new_state} |
    {:noreply, new_state, timeout | :hibernate} |
    {:stop, reason :: term, new_state} when new_state: term

  @doc """
  Invoked when the server is about to exit. It should do any cleanup required.

  `reason` is exit reason and `state` is the current state of the `GenServer`.
  The return value is ignored.

  `terminate/2` is called if a callback (except `init/1`) returns a `:stop`
  tuple, raises, calls `Kernel.exit/1` or returns an invalid value. It may also
  be called if the `GenServer` traps exits using `Process.flag/2` *and* the
  parent process sends an exit signal.

  If part of a supervision tree a `GenServer`'s `Supervisor` will send an exit
  signal when shutting it down. The exit signal is based on the shutdown
  strategy in the child's specification. If it is `:brutal_kill` the `GenServer`
  is killed and so `terminate/2` is not called. However if it is a timeout the
  `Supervisor` will send the exit signal `:shutdown` and the `GenServer` will
  have the duration of the timeout to call `terminate/2` - if the process is
  still alive after the timeout it is killed.

  If the `GenServer` receives an exit signal (that is not `:normal`) from any
  process when it is not trapping exits it will exit abruptly with the same
  reason and so not call `terminate/2`. Note that a process does *NOT* trap
  exits by default and an exit signal is sent when a linked process exits or its
  node is disconnected.

  Therefore it is not guaranteed that `terminate/2` is called when a `GenServer`
  exits. For such reasons, we usually recommend important clean-up rules to
  happen in separated processes either by use of monitoring or by links
  themselves. For example if the `GenServer` controls a `port` (e.g.
  `:gen_tcp.socket`) or `File.io_device`, they will be closed on receiving a
  `GenServer`'s exit signal and do not need to be closed in `terminate/2`.

  If `reason` is not `:normal`, `:shutdown` nor `{:shutdown, term}` an error is
  logged.
  """
  @callback terminate(reason, state :: term) ::
    term when reason: :normal | :shutdown | {:shutdown, term} | term

  @doc """
  Invoked to change the state of the `GenServer` when a different version of a
  module is loaded (hot code swapping) and the state's term structure should be
  changed.

  `old_vsn` is the previous version of the module (defined by the `@vsn`
  attribute) when upgrading. When downgrading the previous version is wrapped in
  a 2-tuple with first element `:down`. `state` is the current state of the
  `GenServer` and `extra` is any extra data required to change the state.

  Returning `{:ok, new_state}` changes the state to `new_state` and the code
  change is successful.

  Returning `{:error, reason}` fails the code change with reason `reason` and
  the state remains as the previous state.

  If `code_change/3` raises the code change fails and the loop will continue
  with its previous state. Therefore this callback does not usually contain side effects.
  """
  @callback code_change(old_vsn, state :: term, extra :: term) ::
    {:ok, new_state :: term} |
    {:error, reason :: term} when old_vsn: term | {:down, term}

  @typedoc "Return values of `start*` functions"
  @type on_start :: {:ok, pid} | :ignore | {:error, {:already_started, pid} | term}

  @typedoc "The GenServer name"
  @type name :: atom | {:global, term} | {:via, module, term}

  @typedoc "Options used by the `start*` functions"
  @type options :: [option]

  @typedoc "Option values used by the `start*` functions"
  @type option :: {:debug, debug} |
                  {:name, name} |
                  {:timeout, timeout} |
                  {:spawn_opt, Process.spawn_opt}

  @typedoc "Debug options supported by the `start*` functions"
  @type debug :: [:trace | :log | :statistics | {:log_to_file, Path.t}]

  @typedoc "The server reference"
  @type server :: pid | name | {atom, node}

  @typedoc """
  Tuple describing the client of a call request.

  `pid` is the pid of the caller and `tag` is a unique term used to identify the
  call.
  """
  @type from :: {pid, tag :: term}

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @behaviour :gen_server

      @doc false
      def init(args) do
        {:ok, args}
      end

      @doc false
      def handle_call(msg, _from, state) do
        # We do this to trick Dialyzer to not complain about non-local returns.
        reason = {:bad_call, msg}
        case :erlang.phash2(1, 1) do
          0 -> exit(reason)
          1 -> {:stop, reason, state}
        end
      end

      @doc false
      def handle_info(_msg, state) do
        {:noreply, state}
      end

      @doc false
      def handle_cast(msg, state) do
        # We do this to trick Dialyzer to not complain about non-local returns.
        reason = {:bad_cast, msg}
        case :erlang.phash2(1, 1) do
          0 -> exit(reason)
          1 -> {:stop, reason, state}
        end
      end

      @doc false
      def terminate(_reason, _state) do
        :ok
      end

      @doc false
      def code_change(_old, state, _extra) do
        {:ok, state}
      end

      defoverridable [init: 1, handle_call: 3, handle_info: 2,
                      handle_cast: 2, terminate: 2, code_change: 3]
    end
  end

  @doc """
  Starts a `GenServer` process linked to the current process.

  This is often used to start the `GenServer` as part of a supervision tree.

  Once the server is started, it calls the `init/1` function in the given `module`
  passing the given `args` to initialize it. To ensure a synchronized start-up
  procedure, this function does not return until `init/1` has returned.

  Note that a `GenServer` started with `start_link/3` is linked to the
  parent process and will exit in case of crashes. The GenServer will also
  exit due to the `:normal` reasons in case it is configured to trap exits
  in the `init/1` callback.

  ## Options

  The `:name` option is used for name registration as described in the module
  documentation. If the option `:timeout` option is present, the server is
  allowed to spend the given milliseconds initializing or it will be
  terminated and the start function will return `{:error, :timeout}`.

  If the `:debug` option is present, the corresponding function in the
  [`:sys` module](http://www.erlang.org/doc/man/sys.html) will be invoked.

  If the `:spawn_opt` option is present, its value will be passed as options
  to the underlying process as in `Process.spawn/4`.

  ## Return values

  If the server is successfully created and initialized, the function returns
  `{:ok, pid}`, where pid is the pid of the server. If a process with the
  specified server name already exists, the function returns
  `{:error, {:already_started, pid}}` with the pid of that process.

  If the `init/1` callback fails with `reason`, the function returns
  `{:error, reason}`. Otherwise, if it returns `{:stop, reason}`
  or `:ignore`, the process is terminated and the function returns
  `{:error, reason}` or `:ignore`, respectively.
  """
  @spec start_link(module, any, options) :: on_start
  def start_link(module, args, options \\ []) when is_atom(module) and is_list(options) do
    do_start(:link, module, args, options)
  end

  @doc """
  Starts a `GenServer` process without links (outside of a supervision tree).

  See `start_link/3` for more information.
  """
  @spec start(module, any, options) :: on_start
  def start(module, args, options \\ []) when is_atom(module) and is_list(options) do
    do_start(:nolink, module, args, options)
  end

  defp do_start(link, module, args, options) do
    case Keyword.pop(options, :name) do
      {nil, opts} ->
        :gen.start(:gen_server, link, module, args, opts)
      {atom, opts} when is_atom(atom) ->
        :gen.start(:gen_server, link, {:local, atom}, module, args, opts)
      {other, opts} when is_tuple(other) ->
        :gen.start(:gen_server, link, other, module, args, opts)
    end
  end

  @doc """
  Stops the server with the given `reason`.

  The `terminate/2` callback will be invoked before exiting.
  It returns `:ok` if the server terminates with the given
  reason, if it terminates with another reason, the call will
  exit.

  This function keeps OTP semantics regarding error reporting.
  If the reason is any other than `:normal`, `:shutdown` or
  `{:shutdown, _}`, an error report will be logged.
  """
  @spec stop(server, reason :: term, timeout) :: :ok
  def stop(server, reason \\ :normal, timeout \\ :infinity) do
    :gen.stop(server, reason, timeout)
  end

  @doc """
  Makes a synchronous call to the `server` and waits for its reply.

  The client sends the given `request` to the server and waits until a reply
  arrives or a timeout occurs. `handle_call/3` will be called on the server
  to handle the request.

  The server can be any of the values described in the `Name Registration`
  section of the module documentation.

  ## Timeouts

  The `timeout` is an integer greater than zero which specifies how many
  milliseconds to wait for a reply, or the atom `:infinity` to wait
  indefinitely. The default value is 5000. If no reply is received within
  the specified time, the function call fails. If the caller catches the
  failure and continues running, and the server is just late with the reply,
  it may arrive at any time later into the caller's message queue. The caller
  must in this case be prepared for this and discard any such garbage messages
  that are two-element tuples with a reference as the first element.
  """
  @spec call(server, term, timeout) :: term
  def call(server, request, timeout \\ 5000) do
    try do
      :gen.call(server, :"$gen_call", request, timeout)
    catch
      :exit, reason ->
        exit({reason, {__MODULE__, :call, [server, request, timeout]}})
    else
      {:ok, res} -> res
    end
  end

  @doc """
  Sends an asynchronous request to the `server`.

  This function always returns `:ok` regardless of whether
  the destination `server` (or node) exists. Therefore it
  is unknown whether the destination `server` successfully
  handled the message.

  `handle_cast/2` will be called on the server to handle
  the request. In case the `server` is on a node which is
  not yet connected to the caller one, the call is going to
  block until a connection happens. This is different than
  the behaviour in OTP's `:gen_server` where the message
  is sent by another process in this case, which could cause
  messages to other nodes to arrive out of order.
  """
  @spec cast(server, term) :: :ok
  def cast(server, request)

  def cast({:global, name}, request) do
    try do
      :global.send(name, cast_msg(request))
      :ok
    catch
      _, _ -> :ok
    end
  end

  def cast({:via, mod, name}, request) do
    try do
      mod.send(name, cast_msg(request))
      :ok
    catch
      _, _ -> :ok
    end
  end

  def cast({name, node}, request) when is_atom(name) and is_atom(node),
    do: do_send({name, node}, cast_msg(request))

  def cast(dest, request) when is_atom(dest) or is_pid(dest),
    do: do_send(dest, cast_msg(request))

  @doc """
  Casts all servers locally registered as `name` at the specified nodes.

  The function returns immediately and ignores nodes that do not exist, or where the
  server name does not exist.

  See `multi_call/4` for more information.
  """
  @spec abcast([node], name :: atom, term) :: :abcast
  def abcast(nodes \\ nodes(), name, request) when is_list(nodes) and is_atom(name) do
    msg = cast_msg(request)
    _   = for node <- nodes, do: do_send({name, node}, msg)
    :abcast
  end

  defp cast_msg(req) do
    {:"$gen_cast", req}
  end

  defp do_send(dest, msg) do
    try do
      send(dest, msg)
      :ok
    catch
      _, _ -> :ok
    end
  end

  @doc """
  Calls all servers locally registered as `name` at the specified `nodes`.

  The `request` is first sent to every node and then we wait for the
  replies. This function returns a tuple containing the node and its reply
  as first element and all bad nodes as second element. The bad nodes is a
  list of nodes that either did not exist, or where a server with the given
  `name` did not exist or did not reply.

  Nodes is a list of node names to which the request is sent. The default
  value is the list of all known nodes.

  To avoid that late answers (after the timeout) pollute the caller's message
  queue, a middleman process is used to do the actual calls. Late answers will
  then be discarded when they arrive to a terminated process.
  """
  @spec multi_call([node], name :: atom, term, timeout) ::
                  {replies :: [{node, term}], bad_nodes :: [node]}
  def multi_call(nodes \\ nodes(), name, request, timeout \\ :infinity) do
    :gen_server.multi_call(nodes, name, request, timeout)
  end

  @doc """
  Replies to a client.

  This function can be used by a server to explicitly send a reply to a
  client that called `call/3` or `multi_call/4`. When the reply cannot be
  defined in the return value of `handle_call/3`.

  The `client` must be the `from` argument (the second argument) received
  in `handle_call/3` callbacks. Reply is an arbitrary term which will be
  given back to the client as the return value of the call.

  This function always returns `:ok`.
  """
  @spec reply(from, term) :: :ok
  def reply(client, reply)

  def reply({to, tag}, reply) do
    try do
      send(to, {tag, reply})
      :ok
    catch
      _, _ -> :ok
    end
  end

  @doc """
  Returns the `pid` or `{name, node}` of a GenServer process.
  Returns `nil` if no process is associated with the given name.

  For example, to lookup a server process, monitor it and send a cast:

      process = GenServer.whereis(server)
      monitor = Process.monitor(process)
      GenServer.cast(process, :hello)

  """
  @spec whereis(server) :: pid | {atom, node} | nil
  def whereis(pid) when is_pid(pid), do: pid
  def whereis(name) when is_atom(name) do
    Process.whereis(name)
  end
  def whereis({:global, name}) do
    case :global.whereis_name(name) do
      pid when is_pid(pid) -> pid
      :undefined           -> nil
    end
  end
  def whereis({:via, mod, name}) do
    case apply(mod, :whereis_name, [name]) do
      pid when is_pid(pid) -> pid
      :undefined           -> nil
    end
  end
  def whereis({name, local}) when is_atom(name) and local == node() do
    Process.whereis(name)
  end
  def whereis({name, node} = server) when is_atom(name) and is_atom(node) do
    server
  end

  @compile {:inline, [nodes: 0]}

  defp nodes do
    [node()|:erlang.nodes()]
  end
end
