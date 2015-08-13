defmodule GenServer do
  @moduledoc """
  A behaviour module for implementing the server of a client-server relation.

  A GenServer is a process as any other Elixir process and it can be used
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
  you want to customize. The callbacks are:

    * `init(args)` - invoked when the server is started.

      It must return:

      -  `{:ok, state}`
      -  `{:ok, state, timeout}`
      -  `{:ok, state, :hibernate}`
      -  `:ignore`
      -  `{:stop, reason}`

    * `handle_call(msg, {from, ref}, state)` - invoked to handle call (sync)
      messages.

      It must return:

      -  `{:reply, reply, new_state}`
      -  `{:reply, reply, new_state, timeout}`
      -  `{:reply, reply, new_state, :hibernate}`
      -  `{:noreply, new_state}`
      -  `{:noreply, new_state, timeout}`
      -  `{:noreply, new_state, :hibernate}`
      -  `{:stop, reason, new_state}`
      -  `{:stop, reason, reply, new_state}`

    * `handle_cast(msg, state)` - invoked to handle cast (async) messages.

      It must return:

      -  `{:noreply, new_state}`
      -  `{:noreply, new_state, timeout}`
      -  `{:noreply, new_state, :hibernate}`
      -  `{:stop, reason, new_state}`

    * `handle_info(msg, state)` - invoked to handle all other messages which
      are received by the process.

      It must return:

      -  `{:noreply, state}`
      -  `{:noreply, state, timeout}`
      -  `{:stop, reason, state}`

    * `terminate(reason, state)` - called when the server is about to
      terminate, useful for cleaning up. It must return `:ok`.
      If part of a supervision tree, terminate only gets called if the
      GenServer is set to trap exits using `Process.flag/2` *and*
      the shutdown strategy of the Supervisor is a timeout value,
      not `:brutal_kill`.

    * `code_change(old_vsn, state, extra)` - called when the application
      code is being upgraded live (hot code swapping).

      It must return:

      -  `{:ok, new_state}`
      -  `{:error, reason}`

  ## Name Registration

  Both `start_link/3` and `start/3` support the `GenServer` to register
  a name on start via the `:name` option. Registered names are also
  automatically cleaned up on termination. The supported values are:

    * an atom - the GenServer is registered locally with the given name
      using `Process.register/2`.

    * `{:global, term}`- the GenServer is registered globally with the given
      term using the functions in the `:global` module.

    * `{:via, module, term}` - the GenServer is registered with the given
      mechanism and name. The `:via` option expects a module name to control
      the registration mechanism alongside a name which can be any term.

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

  ## Learn more

  If you wish to find out more about gen servers, the Elixir Getting Started
  guide provides a tutorial-like introduction. The documentation and links
  in Erlang can also provide extra insight.

    * http://elixir-lang.org/getting-started/mix-otp/genserver.html
    * http://www.erlang.org/doc/man/gen_server.html
    * http://www.erlang.org/doc/design_principles/gen_server_concepts.html
    * http://learnyousomeerlang.com/clients-and-servers
  """

  @typedoc "Return values of `start*` functions"
  @type on_start :: {:ok, pid} | :ignore | {:error, {:already_started, pid} | term}

  @typedoc "The GenServer name"
  @type name :: atom | {:global, term} | {:via, module, term}

  @typedoc "Options used by the `start*` functions"
  @type options :: [debug: debug,
                    name: name,
                    timeout: timeout,
                    spawn_opt: Process.spawn_opt]

  @typedoc "debug options supported by the `start*` functions"
  @type debug :: [:trace | :log | :statistics | {:log_to_file, Path.t}]

  @typedoc "The server reference"
  @type server :: pid | name | {atom, node}

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
        # We do this to trick dialyzer to not complain about non-local returns.
        case :random.uniform(1) do
          1 -> exit({:bad_call, msg})
          2 -> {:noreply, state}
        end
      end

      @doc false
      def handle_info(_msg, state) do
        {:noreply, state}
      end

      @doc false
      def handle_cast(msg, state) do
        # We do this to trick dialyzer to not complain about non-local returns.
        case :random.uniform(1) do
          1 -> exit({:bad_cast, msg})
          2 -> {:noreply, state}
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
  that are two element tuples with a reference as the first element.
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

  This function returns `:ok` without waiting for the
  destination `server` to handle the message. Therefore it
  is unknown whether the destination `server` successfully
  handled the message. If the `server` is an atom without
  an associated process an `ArgumentError` is raised. In
  all other cases the function returns `:ok` regardless of
  whether the destination `server` (or node) exists. Note
  that `{name, node()}` can be used when an exception is
  not desired if no process is locally associated with the
  atom `name`.

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
    send(dest, msg)
    :ok
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
  @spec reply({pid, reference}, term) :: :ok
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
