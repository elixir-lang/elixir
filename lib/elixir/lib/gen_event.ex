defmodule GenEvent do
  # TODO: Deprecate by 1.5

  @moduledoc """
  A behaviour module for implementing event handling functionality.

  The event handling model consists of a generic event manager
  process with an arbitrary number of event handlers which are
  added and deleted dynamically.

  An event manager implemented using this module will have a standard
  set of interface functions and include functionality for tracing and
  error reporting. It will also fit into a supervision tree.

  ## Example

  There are many use cases for event handlers. For example, a logging
  system can be built using event handlers where each log message is
  an event and different event handlers can be attached to handle the
  log messages. One handler may print error messages on the terminal,
  another can write it to a file, while a third one can keep the
  messages in memory (like a buffer) until they are read.

  As an example, let's have a GenEvent that accumulates messages until
  they are collected by an explicit call.

      # Define an Event Handler
      defmodule LoggerHandler do
        use GenEvent

        # Callbacks

        def handle_event({:log, x}, messages) do
          {:ok, [x | messages]}
        end

        def handle_call(:messages, messages) do
          {:ok, Enum.reverse(messages), []}
        end
      end

      # Start a new event manager.
      {:ok, pid} = GenEvent.start_link([])

      # Attach an event handler to the event manager.
      GenEvent.add_handler(pid, LoggerHandler, [])
      #=> :ok

      # Send some events to the event manager.
      GenEvent.notify(pid, {:log, 1})
      #=> :ok

      GenEvent.notify(pid, {:log, 2})
      #=> :ok

      # Call functions on specific handlers in the manager.
      GenEvent.call(pid, LoggerHandler, :messages)
      #=> [1, 2]

      GenEvent.call(pid, LoggerHandler, :messages)
      #=> []

  We start a new event manager by calling `GenEvent.start_link/1`.
  Notifications can be sent to the event manager which will then
  invoke `c:handle_event/2` for each registered handler.

  We can add new handlers with `add_handler/3` and `add_mon_handler/3`.
  Calls can also be made to specific handlers by using `call/3`.

  ## Callbacks

  There are 6 callbacks required to be implemented in a `GenEvent`. By
  adding `use GenEvent` to your module, Elixir will automatically define
  all 6 callbacks for you, leaving it up to you to implement the ones
  you want to customize.

  ## Name Registration

  A GenEvent is bound to the same name registration rules as a `GenServer`.
  Read more about it in the `GenServer` docs.

  ## Modes

  GenEvent supports three different notifications.

  On `GenEvent.ack_notify/2`, the manager acknowledges each event,
  providing backpressure, but processing of the message happens
  asynchronously.

  On `GenEvent.sync_notify/2`, the manager acknowledges an event
  just after it is processed by all event handlers.

  On `GenEvent.notify/2`, all events are processed asynchronously and
  there is no ack (which means there is no backpressure).

  ## Streaming

  `GenEvent` messages can be streamed with the help of `stream/2`.
  You will need to start another process to consume the stream:

      Task.start_link fn ->
        stream = GenEvent.stream(pid)

        # Discard the next 3 events
        _ = Enum.take(stream, 3)

        # Print all remaining events
        for event <- stream do
          IO.inspect event
        end
      end

  Now call `GenEvent.notify/2` multiple times. You will see the
  first three events will be skipped while the rest will be
  continuously printed.

  ## Learn more and compatibility

  If you wish to find out more about GenEvent, the documentation and links
  in Erlang can provide extra insight.

    * [`:gen_event` module documentation](http://www.erlang.org/doc/man/gen_event.html)
    * [Event Handlers – Learn You Some Erlang for Great Good!](http://learnyousomeerlang.com/event-handlers)

  Keep in mind though Elixir and Erlang gen events are not 100% compatible.
  The `:gen_event.add_sup_handler/3` is not supported by Elixir's GenEvent,
  which in turn supports `GenEvent.add_mon_handler/3`.

  The benefits of the monitoring approach are described in the "Don't drink
  too much kool aid" section of the "Learn you some Erlang" link above. Due
  to those changes, Elixir's GenEvent does not trap exits by default.

  Furthermore, Elixir also normalizes the `{:error, _}` tuples returned
  by many functions, in order to be more consistent with themselves and
  the `GenServer` module.
  """

  @doc """
  Invoked when the handler is added to the `GenEvent` process. `add_handler/3`
  (and `add_mon_handler/3`) will block until it returns.

  `args` is the argument term (third argument) passed to `add_handler/3`.

  Returning `{:ok, state}` will cause `add_handler/3` to return `:ok` and the
  handler to become part of the `GenEvent` loop with state `state`.

  Returning `{:ok, state, :hibernate}` is similar to
  `{:ok, state}` except the `GenEvent` process is hibernated before continuing
  its loop. See `c:handle_event/2` for more information on hibernation.

  Returning `{:error, reason}` will cause `add_handler/3` to return
  `{:error, reason}` and the handler is not added to `GenEvent` loop.
  """
  @callback init(args :: term) ::
    {:ok, state} |
    {:ok, state, :hibernate} |
    {:error, reason :: any} when state: any

  @doc """
  Invoked to handle `notify/2`, `ack_notify/2` or `sync_notify/2` messages.

  `event` is the event message and `state` is the current state of the handler.

  Returning `{:ok, new_state}` sets the handler's state to `new_state` and the
  `GenEvent` loop continues.

  Returning `{:ok, new_state, :hibernate}` is similar to
  `{:ok, new_state}` except the process is hibernated once all handlers have
  handled the events. The `GenEvent` process will continue the loop once a
  message is its message queue. If a message is already in the message queue
  this will be immediately. Hibernating a `GenEvent` causes garbage collection
  and leaves a continuous heap that minimises the memory used by the process.

  Hibernating should not be used aggressively as too much time could be spent
  garbage collecting. Normally it should only be used when a message is not
  expected soon and minimising the memory of the process is shown to be
  beneficial.

  Returning `:remove_handler` removes the handler from the `GenEvent` loop and
  calls `c:terminate/2` with reason `:remove_handler` and state `state`.
  """
  @callback handle_event(event :: term, state :: term) ::
    {:ok, new_state} |
    {:ok, new_state, :hibernate} |
    :remove_handler when new_state: term

  @doc """
  Invoked to handle synchronous `call/4` messages to a specific handler.

  `request` is the request message sent by a `call/4` and `state` is the current
  state of the handler.

  Returning `{:ok, reply, new_state}` sends `reply` as a response to the call
  and sets the handler's state to `new_state`.

  Returning `{:ok, reply, new_state, :hibernate}` is similar to
  `{:ok, reply, new_state}` except the process is hibernated. See
  `c:handle_event/2` for more information on hibernation.

  Returning `{:remove_handler, reply}` sends `reply` as a response to the call,
  removes the handler from the `GenEvent` loop and calls `c:terminate/2` with
  reason `:remove_handler` and state `state`.
  """

  @callback handle_call(request :: term, state :: term) ::
    {:ok, reply, new_state} |
    {:ok, reply, new_state, :hibernate} |
    {:remove_handler, reply} when reply: term, new_state: term

  @doc """
  Invoked to handle all other messages. All handlers are run in the `GenEvent`
  process so messages intended for other handlers should be ignored with a catch
  all clause.

  `msg` is the message and `state` is the current state of the handler.

  Return values are the same as `c:handle_event/2`.
  """
  @callback handle_info(msg :: term, state :: term) ::
    {:ok, new_state} |
    {:ok, new_state, :hibernate} |
    :remove_handler when new_state: term

  @doc """
  Invoked when the server is about to exit. It should do any cleanup required.

  `reason` is removal reason and `state` is the current state of the handler.
  The return value is returned to `GenEvent.remove_handler/3` or ignored if
  removing for another reason.

  `reason` is one of:

  -  `:stop` - manager is terminating
  -  `{:stop, term}` - monitored process terminated (for monitored handlers)
  -  `:remove_handler` - handler is being removed
  -  `{:error, term}` - handler crashed or returned a bad value and an error is
  logged
  -  `term` - any term passed to functions like `GenEvent.remove_handler/3`

  If part of a supervision tree, a `GenEvent`'s `Supervisor` will send an exit
  signal when shutting it down. The exit signal is based on the shutdown
  strategy in the child's specification. If it is `:brutal_kill` the `GenEvent`
  is killed and so `c:terminate/2` is not called for its handlers. However if it is
  a timeout the `Supervisor` will send the exit signal `:shutdown` and the
  `GenEvent` will have the duration of the timeout to call `c:terminate/2` on all
  of its handlers - if the process is still alive after the timeout it is
  killed.

  If the `GenEvent` receives an exit signal (that is not `:normal`) from any
  process when it is not trapping exits it will exit abruptly with the same
  reason and so not call the handlers' `c:terminate/2`. Note that a process does
  *NOT* trap exits by default and an exit signal is sent when a linked process
  exits or its node is disconnected. Therefore it is not guaranteed that
  `c:terminate/2` is called when a `GenEvent` exits.

  Care should be taken to cleanup because the `GenEvent` can continue to loop
  after removing the handler. This is different to most other OTP behaviours.
  For example if the handler controls a `port` (e.g. `:gen_tcp.socket`) or
  `t:File.io_device/0`, it will be need to be closed in `c:terminate/2` as the
  process is not exiting so will not be automatically cleaned up.
  """
  @callback terminate(reason, state :: term) ::
    term when reason: :stop | {:stop, term} | :remove_handler | {:error, term} | term

  @doc """
  Invoked to change the state of the handler when a different version of the
  handler's module is loaded (hot code swapping) and the state's term
  structure should be changed.

  `old_vsn` is the previous version of the module (defined by the `@vsn`
  attribute) when upgrading. When downgrading the previous version is wrapped in
  a 2-tuple with first element `:down`. `state` is the current state of the
  handler and `extra` is any extra data required to change the state.

  Returning `{:ok, new_state}` changes the state to `new_state` and the code
  change is successful.

  If `c:code_change/3` raises, the code change fails and the handler will continue
  with its previous state. Therefore this callback does not usually contain side
  effects.
  """
  @callback code_change(old_vsn, state :: term, extra :: term) ::
    {:ok, new_state :: term} when old_vsn: term | {:down, term}

  @typedoc "Return values of `start*` functions"
  @type on_start :: {:ok, pid} | {:error, {:already_started, pid}}

  @typedoc "The GenEvent manager name"
  @type name :: atom | {:global, term} | {:via, module, term}

  @typedoc "Options used by the `start*` functions"
  @type options :: [name: name]

  @typedoc "The event manager reference"
  @type manager :: pid | name | {atom, node}

  @typedoc "Supported values for new handlers"
  @type handler :: atom | {atom, term}

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @behaviour :gen_event

      @doc false
      def init(args) do
        {:ok, args}
      end

      @doc false
      def handle_event(_event, state) do
        {:ok, state}
      end

      @doc false
      def handle_call(msg, state) do
        proc =
          case Process.info(self(), :registered_name) do
            {_, []}   -> self()
            {_, name} -> name
          end

        # We do this to trick Dialyzer to not complain about non-local returns.
        case :erlang.phash2(1, 1) do
          0 -> raise "attempted to call GenEvent #{inspect proc} but no handle_call/2 clause was provided"
          1 -> {:remove_handler, {:bad_call, msg}}
        end
      end

      @doc false
      def handle_info(_msg, state) do
        {:ok, state}
      end

      @doc false
      def terminate(_reason, _state) do
        :ok
      end

      @doc false
      def code_change(_old, state, _extra) do
        {:ok, state}
      end

      defoverridable [init: 1, handle_event: 2, handle_call: 2,
                      handle_info: 2, terminate: 2, code_change: 3]
    end
  end

  @doc """
  Starts an event manager linked to the current process.

  This is often used to start the `GenEvent` as part of a supervision tree.

  It accepts the `:name` option which is described under the `Name Registration`
  section in the `GenServer` module docs.

  If the event manager is successfully created and initialized, the function
  returns `{:ok, pid}`, where `pid` is the PID of the server. If a process with
  the specified server name already exists, the function returns
  `{:error, {:already_started, pid}}` with the PID of that process.

  Note that a `GenEvent` started with `start_link/1` is linked to the
  parent process and will exit not only on crashes but also if the parent
  process exits with `:normal` reason.
  """
  @spec start_link(options) :: on_start
  def start_link(options \\ []) when is_list(options) do
    do_start(:link, options)
  end

  @doc """
  Starts an event manager process without links (outside of a supervision tree).

  See `start_link/1` for more information.
  """
  @spec start(options) :: on_start
  def start(options \\ []) when is_list(options) do
    do_start(:nolink, options)
  end

  @no_callback :"no callback module"

  defp do_start(mode, options) do
    case Keyword.get(options, :name) do
      nil ->
        :gen.start(GenEvent, mode, @no_callback, [], [])
      atom when is_atom(atom) ->
        :gen.start(GenEvent, mode, {:local, atom}, @no_callback, [], [])
      {:global, _term} = tuple ->
        :gen.start(GenEvent, mode, tuple, @no_callback, [], [])
      {:via, via_module, _term} = tuple when is_atom(via_module) ->
        :gen.start(GenEvent, mode, tuple, @no_callback, [], [])
      other ->
        raise ArgumentError, """
        expected :name option to be one of:

          * nil
          * atom
          * {:global, term}
          * {:via, module, term}

        Got: #{inspect(other)}
        """
    end
  end

  @doc """
  Returns a stream that consumes events from the `manager`.

  The stream is a `GenEvent` struct that implements the `Enumerable`
  protocol. Consumption of events only begins when enumeration starts.

  Note streaming is specific to Elixir's GenEvent and does not work
  with Erlang ones.

  ## Options

    * `:timeout` - raises if no event arrives in X milliseconds
      (defaults to `:infinity`)

  """
  @spec stream(manager, Keyword.t) :: GenEvent.Stream.t
  def stream(manager, options \\ []) do
    %GenEvent.Stream{
      manager: manager,
      timeout: Keyword.get(options, :timeout, :infinity)}
  end

  @doc """
  Adds a new event handler to the event `manager`.

  The event manager will call the `c:init/1` callback with `args` to
  initiate the event handler and its internal state.

  If `c:init/1` returns a correct value indicating successful completion,
  the event manager adds the event handler and this function returns
  `:ok`. If the callback fails with `reason` or returns `{:error, reason}`,
  the event handler is ignored and this function returns `{:error, reason}`.

  If the given handler was previously installed at the manager, this
  function returns `{:error, :already_present}`.

  For installing multiple instances of the same handler, `{Module, id}` instead
  of `Module` must be used. The handler could be then referenced with
  `{Module, id}` instead of just `Module`.
  """
  @spec add_handler(manager, handler, term) :: :ok | {:error, term}
  def add_handler(manager, handler, args) do
    rpc(manager, {:add_handler, handler, args})
  end

  @doc """
  Adds a monitored event handler to the event `manager`.

  Expects the same input and returns the same values as `add_handler/3`.

  ## Monitored handlers

  A monitored handler implies the calling process will now be monitored
  by the GenEvent manager.

  If the calling process later terminates with `reason`, the event manager
  will delete the event handler by calling the `c:terminate/2` callback with
  `{:stop, reason}` as argument. If the event handler later is deleted,
  the event manager sends a message `{:gen_event_EXIT, handler, reason}`
  to the calling process. Reason is one of the following:

    * `:normal` - if the event handler has been removed due to a call to
      `remove_handler/3`, or `:remove_handler` has been returned by a callback
      function

    * `:shutdown` - if the event handler has been removed because the event
      manager is terminating

    * `{:swapped, new_handler, pid}` - if the process PID has replaced the
      event handler by another

    * `term` - if the event handler is removed due to an error. Which term
      depends on the error

  Keep in mind that the `{:gen_event_EXIT, handler, reason}` message is not
  guaranteed to be delivered in case the manager crashes. If you want to
  guarantee the message is delivered, you have two options:

    * monitor the event manager
    * link to the event manager and then set `Process.flag(:trap_exit, true)`
      in your handler callback

  Finally, this functionality only works with GenEvent started via this
  module (it is not backwards compatible with Erlang's `:gen_event`).
  """
  @spec add_mon_handler(manager, handler, term) :: :ok | {:error, term}
  def add_mon_handler(manager, handler, args) do
    rpc(manager, {:add_mon_handler, handler, args, self()})
  end

  @doc """
  Sends an event notification to the event `manager`.

  The event manager will call `c:handle_event/2` for each
  installed event handler.

  `notify` is asynchronous and will return immediately after the
  notification is sent. `notify` will not fail even if the specified
  event manager does not exist, unless it is specified as an atom.
  """
  @spec notify(manager, term) :: :ok
  def notify(manager, event)

  def notify({:global, name}, msg) do
    try do
      :global.send(name, {:notify, msg})
      :ok
    catch
      _, _ -> :ok
    end
  end

  def notify({:via, mod, name}, msg) when is_atom(mod) do
    try do
      mod.send(name, {:notify, msg})
      :ok
    catch
      _, _ -> :ok
    end
  end

  def notify(manager, msg)
      when is_pid(manager)
      when is_atom(manager)
      when tuple_size(manager) == 2 and
        is_atom(elem(manager, 0)) and is_atom(elem(manager, 1)) do
    send(manager, {:notify, msg})
    :ok
  end

  @doc """
  Sends a sync event notification to the event `manager`.

  In other words, this function only returns `:ok` after the event manager
  invokes the `c:handle_event/2` callback on each installed event handler.

  See `notify/2` for more info.
  """
  @spec sync_notify(manager, term) :: :ok
  def sync_notify(manager, event) do
    rpc(manager, {:sync_notify, event})
  end

  @doc """
  Sends an ack event notification to the event `manager`.

  In other words, this function only returns `:ok` as soon as the
  event manager starts processing this event, but it does not wait
  for event handlers to process the sent event.

  See `notify/2` for more info. Note this function is specific
  to Elixir's GenEvent and does not work with Erlang ones.
  """
  @spec ack_notify(manager, term) :: :ok
  def ack_notify(manager, event) do
    rpc(manager, {:ack_notify, event})
  end

  @doc """
  Makes a synchronous call to the event `handler` installed in `manager`.

  The given `request` is sent and the caller waits until a reply arrives or
  a timeout occurs. The event manager will call `c:handle_call/2` to handle
  the request.

  The return value `reply` is defined in the return value of `c:handle_call/2`.
  If the specified event handler is not installed, the function returns
  `{:error, :not_found}`.
  """
  @spec call(manager, handler, term, timeout) ::  term | {:error, term}
  def call(manager, handler, request, timeout \\ 5000) do
    try do
      :gen.call(manager, self(), {:call, handler, request}, timeout)
    catch
      :exit, reason ->
        exit({reason, {__MODULE__, :call, [manager, handler, request, timeout]}})
    else
      {:ok, res} -> res
    end
  end

  @doc """
  Removes an event handler from the event `manager`.

  The event manager will call `c:terminate/2` to terminate the event handler
  and return the callback value. If the specified event handler is not
  installed, the function returns `{:error, :not_found}`.
  """
  @spec remove_handler(manager, handler, term) :: term | {:error, term}
  def remove_handler(manager, handler, args) do
    rpc(manager, {:delete_handler, handler, args})
  end

  @doc """
  Replaces an old event handler with a new one in the event `manager`.

  First, the old event handler is deleted by calling `c:terminate/2` with
  the given `args1` and collects the return value. Then the new event handler
  is added and initiated by calling `init({args2, term})`, where `term` is the
  return value of calling `c:terminate/2` in the old handler. This makes it
  possible to transfer information from one handler to another.

  The new handler will be added even if the specified old event handler
  is not installed or if the handler fails to terminate with a given reason
  in which case `state = {:error, term}`.

  If `c:init/1` in the second handler returns a correct value, this
  function returns `:ok`.
  """
  @spec swap_handler(manager, handler, term, handler, term) :: :ok | {:error, term}
  def swap_handler(manager, handler1, args1, handler2, args2) do
    rpc(manager, {:swap_handler, handler1, args1, handler2, args2})
  end

  @doc """
  Replaces an old event handler with a new monitored one in the event `manager`.

  Read the docs for `add_mon_handler/3` and `swap_handler/5` for more information.
  """
  @spec swap_mon_handler(manager, handler, term, handler, term) :: :ok | {:error, term}
  def swap_mon_handler(manager, handler1, args1, handler2, args2) do
    rpc(manager, {:swap_mon_handler, handler1, args1, handler2, args2, self()})
  end

  @doc """
  Returns a list of all event handlers installed in the `manager`.
  """
  @spec which_handlers(manager) :: [handler]
  def which_handlers(manager) do
    rpc(manager, :which_handlers)
  end

  @doc """
  Stops the manager with the given `reason`.

  Before terminating, the event manager will call
  `terminate(:stop, ...)` for each installed event handler.
  It returns `:ok` if the manager terminates with the given
  reason, if it terminates with another reason, the call will
  exit.

  This function keeps OTP semantics regarding error reporting.
  If the reason is any other than `:normal`, `:shutdown` or
  `{:shutdown, _}`, an error report will be logged.
  """
  @spec stop(manager, reason :: term, timeout) :: :ok
  def stop(manager, reason \\ :normal, timeout \\ :infinity) do
    :gen.stop(manager, reason, timeout)
  end

  defp rpc(module, cmd) do
    {:ok, reply} = :gen.call(module, self(), cmd, :infinity)
    reply
  end

  ## Init callbacks

  require Record
  Record.defrecordp :handler, [:module, :id, :state, :pid, :ref]

  @doc false
  def init_it(starter, :self, name, mod, args, options) do
    init_it(starter, self(), name, mod, args, options)
  end

  def init_it(starter, parent, name, _mod, _args, options) do
    Process.put(:"$initial_call", {__MODULE__, :init_it, 6})
    debug =
      if function_exported?(:gen, :debug_options, 2) do
        :gen.debug_options(name, options)
      else
        :gen.debug_options(options)
      end
    :proc_lib.init_ack(starter, {:ok, self()})
    loop(parent, name(name), [], debug, false)
  end

  @doc false
  def init_hib(parent, name, handlers, debug) do
    fetch_msg(parent, name, handlers, debug, true)
  end

  defp name({:local, name}),  do: name
  defp name({:global, name}), do: name
  defp name({:via, _, name}), do: name
  defp name(pid) when is_pid(pid), do: pid

  ## Loop

  defp loop(parent, name, handlers, debug, true) do
    :proc_lib.hibernate(__MODULE__, :init_hib, [parent, name, handlers, debug])
  end

  defp loop(parent, name, handlers, debug, false) do
    fetch_msg(parent, name, handlers, debug, false)
  end

  defp fetch_msg(parent, name, handlers, debug, hib) do
    receive do
      {:system, from, req} ->
        :sys.handle_system_msg(req, from, parent, __MODULE__,
          debug, [name, handlers, hib], hib)
      {:EXIT, ^parent, reason} ->
        server_terminate(reason, parent, handlers, name)
      msg when debug == [] ->
        handle_msg(msg, parent, name, handlers, [])
      msg ->
        debug = :sys.handle_debug(debug, &print_event/3, name, {:in, msg})
        handle_msg(msg, parent, name, handlers, debug)
    end
  end

  defp handle_msg(msg, parent, name, handlers, debug) do
    case msg do
      {:notify, event} ->
        {hib, handlers} = server_event(:async, event, handlers, name)
        loop(parent, name, handlers, debug, hib)
      {_from, _tag, {:notify, event}} ->
        {hib, handlers} = server_event(:async, event, handlers, name)
        loop(parent, name, handlers, debug, hib)
      {_from, tag, {:ack_notify, event}} ->
        reply(tag, :ok)
        {hib, handlers} = server_event(:ack, event, handlers, name)
        loop(parent, name, handlers, debug, hib)
      {_from, tag, {:sync_notify, event}} ->
        {hib, handlers} = server_event(:sync, event, handlers, name)
        reply(tag, :ok)
        loop(parent, name, handlers, debug, hib)
      {:DOWN, ref, :process, _pid, reason} = other ->
        case handle_down(ref, reason, handlers, name) do
          {:ok, handlers} ->
            loop(parent, name, handlers, debug, false)
          :error ->
            {hib, handlers} = server_info(other, handlers, name)
            loop(parent, name, handlers, debug, hib)
        end
      {_from, tag, {:call, handler, query}} ->
        {hib, reply, handlers} = server_call(handler, query, handlers, name)
        reply(tag, reply)
        loop(parent, name, handlers, debug, hib)
      {_from, tag, {:add_handler, handler, args}} ->
        {hib, reply, handlers} = server_add_handler(handler, args, handlers)
        reply(tag, reply)
        loop(parent, name, handlers, debug, hib)
      {_from, tag, {:add_mon_handler, handler, args, notify}} ->
        {hib, reply, handlers} = server_add_mon_handler(handler, args, handlers, notify)
        reply(tag, reply)
        loop(parent, name, handlers, debug, hib)
      {_from, tag, {:add_process_handler, pid, notify}} ->
        {hib, reply, handlers} = server_add_process_handler(pid, handlers, notify)
        reply(tag, reply)
        loop(parent, name, handlers, debug, hib)
      {_from, tag, {:delete_handler, handler, args}} ->
        {reply, handlers} = server_remove_handler(handler, args, handlers, name)
        reply(tag, reply)
        loop(parent, name, handlers, debug, false)
      {_from, tag, {:swap_handler, handler1, args1, handler2, args2}} ->
        {hib, reply, handlers} = server_swap_handler(handler1, args1, handler2, args2, handlers, nil, name)
        reply(tag, reply)
        loop(parent, name, handlers, debug, hib)
      {_from, tag, {:swap_mon_handler, handler1, args1, handler2, args2, mon}} ->
        {hib, reply, handlers} = server_swap_handler(handler1, args1, handler2, args2, handlers, mon, name)
        reply(tag, reply)
        loop(parent, name, handlers, debug, hib)
      {_from, tag, :which_handlers} ->
        reply(tag, server_which_handlers(handlers))
        loop(parent, name, handlers, debug, false)
      {_from, tag, :get_modules} ->
        reply(tag, server_get_modules(handlers))
        loop(parent, name, handlers, debug, false)
      other ->
        {hib, handlers} = server_info(other, handlers, name)
        loop(parent, name, handlers, debug, hib)
    end
  end

  ## System callbacks

  @doc false
  def system_continue(parent, debug, [name, handlers, hib]) do
    loop(parent, name, handlers, debug, hib)
  end

  @doc false
  def system_terminate(reason, parent, _debug, [name, handlers, _hib]) do
    server_terminate(reason, parent, handlers, name)
  end

  @doc false
  def system_code_change([name, handlers, hib], module, old_vsn, extra) do
    handlers =
      for handler <- handlers do
        if handler(handler, :module) == module do
          {:ok, state} = module.code_change(old_vsn, handler(handler, :state), extra)
          handler(handler, state: state)
        else
          handler
        end
      end
    {:ok, [name, handlers, hib]}
  end

  @doc false
  def system_get_state([_name, handlers, _hib]) do
    tuples = for handler(module: mod, id: id, state: state) <- handlers do
      {mod, id, state}
    end
    {:ok, tuples}
  end

  @doc false
  def system_replace_state(fun, [name, handlers, hib]) do
    {handlers, states} =
      :lists.unzip(for handler <- handlers do
        handler(module: mod, id: id, state: state) = handler
        cur = {mod, id, state}
        try do
          new = {^mod, ^id, new_state} = fun.(cur)
          {handler(handler, state: new_state), new}
        catch
          _, _ ->
            {handler, cur}
        end
      end)
    {:ok, states, [name, handlers, hib]}
  end

  @doc false
  def format_status(opt, status_data) do
    [pdict, sys_state, parent, _debug, [name, handlers, _hib]] = status_data
    header = :gen.format_status_header('Status for event handler', name)

    formatted = for handler <- handlers do
      handler(module: module, state: state) = handler
      if function_exported?(module, :format_status, 2) do
        try do
          state = module.format_status(opt, [pdict, state])
          handler(handler, state: state)
        catch
          _, _ -> handler
        end
      else
        handler
      end
    end

    [header: header,
     data: [{'Status', sys_state}, {'Parent', parent}],
     items: {'Installed handlers', formatted}]
  end

  ## Loop helpers

  defp print_event(dev, {:in, msg}, name) do
    case msg do
      {:notify, event} ->
        IO.puts dev, "*DBG* #{inspect name} got event #{inspect event}"
      {_, _, {:call, handler, query}} ->
        IO.puts dev, "*DBG* #{inspect name} (handler #{inspect handler}) got call #{inspect query}"
      _ ->
        IO.puts dev, "*DBG* #{inspect name} got #{inspect msg}"
    end
  end

  defp print_event(dev, dbg, name) do
    IO.puts dev, "*DBG* #{inspect name}: #{inspect dbg}"
  end

  defp server_add_handler({module, id}, args, handlers) do
    handler = handler(module: module, id: {module, id})
    do_add_handler(module, handler, args, handlers, :ok)
  end

  defp server_add_handler(module, args, handlers) do
    handler = handler(module: module, id: module)
    do_add_handler(module, handler, args, handlers, :ok)
  end

  defp server_add_mon_handler({module, id}, args, handlers, notify) do
    ref = Process.monitor(notify)
    handler = handler(module: module, id: {module, id}, pid: notify, ref: ref)
    do_add_handler(module, handler, args, handlers, :ok)
  end

  defp server_add_mon_handler(module, args, handlers, notify) do
    ref = Process.monitor(notify)
    handler = handler(module: module, id: module, pid: notify, ref: ref)
    do_add_handler(module, handler, args, handlers, :ok)
  end

  defp server_add_process_handler(pid, handlers, notify) do
    ref = Process.monitor(pid)
    handler = handler(module: GenEvent.Stream, id: {self(), ref},
                      pid: notify, ref: ref)
    do_add_handler(GenEvent.Stream, handler, {pid, ref}, handlers, {self(), ref})
  end

  defp server_remove_handler(module, args, handlers, name) do
    do_take_handler(module, args, handlers, name, :remove, :normal)
  end

  defp server_swap_handler(module1, args1, module2, args2, handlers, sup, name) do
    {state, handlers} =
      do_take_handler(module1, args1, handlers, name, :swapped, {:swapped, module2, sup})

    if sup do
      server_add_mon_handler(module2, {args2, state}, handlers, sup)
    else
      server_add_handler(module2, {args2, state}, handlers)
    end
  end

  defp server_info(event, handlers, name) do
    handlers = :lists.reverse(handlers)
    server_notify(event, :handle_info, handlers, name, handlers, [], false)
  end

  defp server_event(mode, event, handlers, name) do
    {handlers, streams} = server_split_process_handlers(mode, event, handlers, [], [])
    {hib, handlers} = server_notify(event, :handle_event, handlers, name, handlers, [], false)
    {hib, server_collect_process_handlers(mode, event, streams, handlers, name)}
  end

  defp server_split_process_handlers(mode, event, [handler | t], handlers, streams) do
    case handler(handler, :id) do
      {pid, _ref} when is_pid(pid) ->
        server_process_notify(mode, event, handler)
        server_split_process_handlers(mode, event, t, handlers, [handler | streams])
      _ ->
        server_split_process_handlers(mode, event, t, [handler | handlers], streams)
    end
  end

  defp server_split_process_handlers(_mode, _event, [], handlers, streams) do
    {handlers, streams}
  end

  defp server_process_notify(mode, event, handler(state: {pid, ref})) do
    send pid, {self(), {self(), ref}, {mode_to_tag(mode), event}}
  end

  defp mode_to_tag(:ack),   do: :ack_notify
  defp mode_to_tag(:sync),  do: :sync_notify
  defp mode_to_tag(:async), do: :notify

  defp server_notify(event, fun, [handler | t], name, handlers, acc, hib) do
    case server_update(handler, fun, event, name, handlers) do
      {new_hib, handler} ->
        server_notify(event, fun, t, name, handlers, [handler | acc], hib or new_hib)
      :error ->
        server_notify(event, fun, t, name, handlers, acc, hib)
    end
  end

  defp server_notify(_, _, [], _, _, acc, hib) do
    {hib, acc}
  end

  defp server_update(handler, fun, event, name, _handlers) do
    handler(module: module, state: state) = handler

    case do_handler(module, fun, [event, state]) do
      {:ok, res} ->
        case res do
          {:ok, state} ->
            {false, handler(handler, state: state)}
          {:ok, state, :hibernate} ->
            {true, handler(handler, state: state)}
          :remove_handler ->
            do_terminate(handler, :remove_handler, event, name, :normal)
            :error
          other ->
            reason = {:bad_return_value, other}
            do_terminate(handler, {:error, reason}, event, name, reason)
            :error
        end
      {:error, reason} ->
        do_terminate(handler, {:error, reason}, event, name, reason)
        :error
    end
  end

  defp server_collect_process_handlers(:async, event, [handler | t], handlers, name) do
    server_collect_process_handlers(:async, event, t, [handler | handlers], name)
  end

  defp server_collect_process_handlers(mode, event, [handler | t], handlers, name) when mode in [:sync, :ack] do
    handler(ref: ref, id: id) = handler

    receive do
      {^ref, :ok} ->
        server_collect_process_handlers(mode, event, t, [handler | handlers], name)
      {_from, tag, {:delete_handler, ^id, args}} ->
        do_terminate(handler, args, :remove, name, :normal)
        reply(tag, :ok)
        server_collect_process_handlers(mode, event, t, handlers, name)
      {:DOWN, ^ref, _, _, reason} ->
        do_terminate(handler, {:stop, reason}, :DOWN, name, :shutdown)
        server_collect_process_handlers(mode, event, t, handlers, name)
    end
  end

  defp server_collect_process_handlers(_mode, _event, [], handlers, _name) do
    handlers
  end

  defp server_call(module, query, handlers, name) do
    case :lists.keyfind(module, handler(:id) + 1, handlers) do
      false ->
        {false, {:error, :not_found}, handlers}
      handler ->
        case server_call_update(handler, query, name, handlers) do
          {{hib, handler}, reply} ->
            {hib, reply, :lists.keyreplace(module, handler(:id) + 1, handlers, handler)}
          {:error, reply} ->
            {false, reply, :lists.keydelete(module, handler(:id) + 1, handlers)}
        end
    end
  end

  defp server_call_update(handler, query, name, _handlers) do
    handler(module: module, state: state) = handler
    case do_handler(module, :handle_call, [query, state]) do
      {:ok, res} ->
        case res do
          {:ok, reply, state} ->
            {{false, handler(handler, state: state)}, reply}
          {:ok, reply, state, :hibernate} ->
            {{true, handler(handler, state: state)}, reply}
          {:remove_handler, reply} ->
            do_terminate(handler, :remove_handler, query, name, :normal)
            {:error, reply}
          other ->
            reason = {:bad_return_value, other}
            do_terminate(handler, {:error, reason}, query, name, reason)
            {:error, {:error, reason}}
        end
      {:error, reason} ->
        do_terminate(handler, {:error, reason}, query, name, reason)
        {:error, {:error, reason}}
    end
  end

  defp server_get_modules(handlers) do
    (for handler(module: module) <- handlers, do: module)
    |> :ordsets.from_list
    |> :ordsets.to_list
  end

  defp server_which_handlers(handlers) do
    for handler(id: id) <- handlers, do: id
  end

  defp server_terminate(reason, _parent, handlers, name) do
    _ =
      for handler <- handlers do
        do_terminate(handler, :stop, :stop, name, :shutdown)
      end
    exit(reason)
  end

  defp reply({from, ref}, msg) do
    send from, {ref, msg}
  end

  defp handle_down(ref, reason, handlers, name) do
    case :lists.keyfind(ref, handler(:ref) + 1, handlers) do
      false -> :error
      handler ->
        do_terminate(handler, {:stop, reason}, :DOWN, name, :shutdown)
        {:ok, :lists.keydelete(ref, handler(:ref) + 1, handlers)}
    end
  end

  defp do_add_handler(module, handler, arg, handlers, succ) do
    case :lists.keyfind(handler(handler, :id), handler(:id) + 1, handlers) do
      false ->
        case do_handler(module, :init, [arg]) do
          {:ok, res} ->
            case res do
              {:ok, state} ->
                {false, succ, [handler(handler, state: state) | handlers]}
              {:ok, state, :hibernate} ->
                {true, succ, [handler(handler, state: state) | handlers]}
              {:error, _} = error ->
                {false, error, handlers}
              other ->
                {false, {:error, {:bad_return_value, other}}, handlers}
            end
          {:error, _} = error ->
            {false, error, handlers}
        end
      _ ->
        {false, {:error, :already_present}, handlers}
    end
  end

  defp do_take_handler(module, args, handlers, name, last_in, reason) do
    case :lists.keytake(module, handler(:id) + 1, handlers) do
      {:value, handler, handlers} ->
        {do_terminate(handler, args, last_in, name, reason), handlers}
      false ->
        {{:error, :not_found}, handlers}
    end
  end

  defp do_terminate(handler, arg, last_in, name, reason) do
    handler(module: module, state: state) = handler

    res =
      case do_handler(module, :terminate, [arg, state]) do
        {:ok, res} -> res
        {:error, _} = error -> error
      end
    report_terminate(handler, reason, state, last_in, name)
    res
  end

  defp do_handler(mod, fun, args) do
    try do
      apply(mod, fun, args)
    catch
      :throw, val -> {:ok, val}
      :error, val -> {:error, {val, System.stacktrace}}
      :exit, val  -> {:error, val}
    else
      res -> {:ok, res}
    end
  end

  defp report_terminate(handler, reason, state, last_in, name) do
    report_error(handler, reason, state, last_in, name)
    if ref = handler(handler, :ref) do
      Process.demonitor(ref, [:flush])
    end
    if pid = handler(handler, :pid) do
      send pid, {:gen_event_EXIT, handler(handler, :id), reason}
    end
  end

  defp report_error(_handler, :normal, _, _, _), do: :ok
  defp report_error(_handler, :shutdown, _, _, _), do: :ok
  defp report_error(_handler, {:swapped, _, _}, _, _, _), do: :ok
  defp report_error(handler, reason, state, last_in, name) do
    reason =
      case reason do
        {:undef, [{m, f, a, _} | _]=mfas} ->
          cond do
            :code.is_loaded(m) === false ->
              {:"module could not be loaded", mfas}
            function_exported?(m, f, length(a)) ->
              reason
            true ->
              {:"function not exported", mfas}
          end
        _ ->
          reason
      end

    formatted = report_status(handler, state)

    :error_logger.error_msg(
      '** gen_event handler ~p crashed.~n' ++
      '** Was installed in ~p~n' ++
      '** Last event was: ~p~n' ++
      '** When handler state == ~p~n' ++
      '** Reason == ~p~n', [handler(handler, :id), name, last_in, formatted, reason])
  end

  defp report_status(handler(module: module), state) do
    if function_exported?(module, :format_status, 2) do
      try do
        module.format_status(:terminate, [Process.get(), state])
      catch
        _, _ -> state
      end
    else
      state
    end
  end
end
