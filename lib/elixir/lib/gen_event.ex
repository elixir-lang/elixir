defmodule GenEvent do
  @moduledoc """
  A behaviour module for implementing event handling functionality.

  The event handling model consists of a generic event manager
  process with an arbitrary number of event handlers which are
  added and deleted dynamically.

  An event manager implemented using this module will have a standard
  set of interface functions and include functionality for tracing and
  error reporting. It will also fit into an supervision tree.

  ## Example

  There are many use cases for event handlers. For example, a logging
  system can be built using event handlers where which log message is
  an event and different event handlers can be plugged to handle the
  log messages. One handler may print error messages on the terminal,
  another can write it to a file, while a third one can keep the
  messages in memory (like a buffer) until they are read.

  As an example, let's have a GenEvent that accumulates messages until
  they are collected by an explicit call.

      defmodule LoggerHandler do
        use GenEvent

        # Callbacks

        def handle_event({:log, x}, messages) do
          {:ok, [x|messages]}
        end

        def handle_call(:messages, messages) do
          {:ok, Enum.reverse(messages), []}
        end
      end

      {:ok, pid} = GenEvent.start_link()

      GenEvent.add_handler(pid, LoggerHandler, [])
      #=> :ok

      GenEvent.notify(pid, {:log, 1})
      #=> :ok

      GenEvent.notify(pid, {:log, 2})
      #=> :ok

      GenEvent.call(pid, LoggerHandler, :messages)
      #=> [1, 2]

      GenEvent.call(pid, LoggerHandler, :messages)
      #=> []

  We start a new event manager by calling `GenEvent.start_link/0`.
  Notifications can be sent to the event manager which will then
  invoke `handle_event/0` for each registered handler.

  We can add new handlers with `add_handler/4`. Calls can also
  be made to specific handlers by using `call/3`.

  ## Callbacks

  There are 6 callbacks required to be implemented in a `GenEvent`. By
  adding `use GenEvent` to your module, Elixir will automatically define
  all 6 callbacks for you, leaving it up to you to implement the ones
  you want to customize. The callbacks are:

  * `init(args)` - invoked when the event handler is added

    It must return:

    *  `{:ok, state}`
    *  `{:ok, state, :hibernate}`
    *  `{:error, reason}`

  * `handle_event(msg, state)` - invoked whenever an event is sent via
    `notify/2` or `sync_notify/2`.

    It must return:

    *  `{:ok, new_state}`
    *  `{:ok, new_state, :hibernate}`
    *  `{:swap_handler, args1, new_state, handler2, args2}`
    *  `:remove_handler`

  * `handle_call(msg, state)` - invoked when a `call/3` is done to a specific handler.

    It must return:

    *  `{:ok, reply, new_state}`
    *  `{:ok, reply, new_state, :hibernate}`
    *  `{:swap_handler, reply, args1, new_state, handler2, args2}`
    *  `{:remove_handler, reply}`

  * `handle_info(msg, state)` - invoked to handle all other messages which
    are received by the process. Must return the same values as `handle_event/2`;

    It must return:

    *  `{:noreply, state}`
    *  `{:noreply, state, timeout}`
    *  `{:stop, reason, state}`

  * `terminate(reason, state)` - called when the event handler is removed or the
    event manager is terminating. It can return any term.

  * `code_change(old_vsn, state, extra)` - called when the application
    code is being upgraded live (hot code swapping).

    It must return:

    *  `{:ok, new_state}`

  ## Name Registration

  A GenEvent is bound to the same name registration rules as a `GenServer`.
  Read more about it in the `GenServer` docs.

  ## Streaming

  `GenEvent`s can be streamed from and streamed with the help of `stream/2`.
  Here are some examples:

      stream = GenEvent.stream(pid)

      # Take the next 10 events
      Enum.take(stream, 10)

      # Print all remaining events
      for event <- stream do
        IO.inspect event
      end

  A stream may also be given an id, which allows all streams with the given
  id to be cancelled at any moment via `cancel_streams/1`.

  ## Learn more

  If you wish to find out more about gen events, Elixir getting started
  guides provide a tutorial-like introduction. The documentation and links
  in Erlang can also provide extra insight.

  * http://elixir-lang.org/getting_started/mix/1.html
  * http://www.erlang.org/doc/man/gen_event.html
  * http://learnyousomeerlang.com/event-handlers
  """

  @typedoc "Return values of `start*` functions"
  @type on_start :: {:ok, pid} | {:error, {:already_started, pid}}

  @typedoc "The GenEvent manager name"
  @type name :: atom | {:global, term} | {:via, module, term}

  @typedoc "Options used by the `start*` functions"
  @type options :: [name: name]

  @typedoc "The event manager reference"
  @type manager :: pid | name | {atom, node}

  @typedoc "Supported values for new handlers"
  @type handler :: module | {module, term}

  @doc """
  Defines a `GenEvent` stream.

  This is a struct returned by `stream/2`. The struct is public and
  contains the following fields:

  * `:manager` - the manager reference given to `GenEvent.stream/2`
  * `:id` - the event stream id for cancellation
  * `:timeout` - the timeout in between events, defaults to `:infinity`
  * `:duration` - the duration of the subscription, defaults to `:infinity`
  """
  defstruct manager: nil, id: nil, timeout: :infinity, duration: :infinity

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
      def handle_call(_request, state) do
        {:ok, {:error, :bad_call}, state}
      end

      @doc false
      def handle_info(_msg, state) do
        {:ok, state}
      end

      @doc false
      def terminate(reason, state) do
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
  returns `{:ok, pid}`, where pid is the pid of the server. If there already
  exists a process with the specified server name, the function returns
  `{:error, {:already_started, pid}}` with the pid of that process.
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

  defp do_start(mode, options) do
    case Keyword.get(options, :name) do
      nil ->
        :gen.start(:gen_event, mode, :"no callback module", [], [])
      atom when is_atom(atom) ->
        :gen.start(:gen_event, mode, {:local, atom}, :"no callback module", [], [])
      other when is_tuple(other) ->
        :gen.start(:gen_event, mode, other, :"no callback module", [], [])
    end
  end

  @doc """
  Returns a stream that consumes and notifies events to the `manager`.

  The stream is a `GenEvent` struct that implements the `Enumerable`
  protocol. The supported options are:

  * `:id` - an id to identify all live stream instances. When an `:id` is given,
    existing streams can be called with via `cancel_streams`;
  * `:timeout` (Enumerable) - raises if no event arrives in X milliseconds;
  * `:duration` (Enumerable) - only consume events during the X milliseconds
    from the streaming start;
  """
  def stream(manager, options \\ []) do
    %GenEvent{manager: manager,
              id: Keyword.get(options, :id),
              timeout: Keyword.get(options, :timeout, :infinity),
              duration: Keyword.get(options, :duration, :infinity)}
  end

  @doc """
  Adds a new event handler to the event `manager`.

  The event manager will call the `init/1` callback with `args` to
  initiate the event handler and its internal state.

  If `init/1` returns a correct value indicating successful completion,
  the event manager adds the event handler and this function returns
  `:ok`. If the callback fails with `reason` or returns `{:error, reason}`,
  the event handler is ignored and this function returns `{:EXIT, reason}`
  or `{:error, reason}`, respectively.

  ## Linked handlers

  When adding a handler, a `:link` option with value `true` can be given.
  This means the event handler and the calling process are now linked.

  If the calling process later terminates with `reason`, the event manager
  will delete the event handler by calling the `terminate/2` callback with
  `{:stop, reason}` as argument. If the event handler later is deleted,
  the event manager sends a message `{:gen_event_EXIT, handler, reason}`
  to the calling process. Reason is one of the following:

  * `:normal` - if the event handler has been removed due to a call to
    `remove_handler/3`, or `:remove_handler` has been returned by a callback
    function;

  * `:shutdown` - if the event handler has been removed because the event
    manager is terminating;

  * `{:swapped, new_handler, pid}` - if the process pid has replaced the
    event handler by another;

  * a term - if the event handler is removed due to an error. Which term
    depends on the error;

  """
  @spec add_handler(manager, handler, term, [link: boolean]) :: :ok | {:EXIT, term} | {:error, term}
  def add_handler(manager, handler, args, options \\ []) do
    case Keyword.get(options, :link, false) do
      true  -> :gen_event.add_sup_handler(manager, handler, args)
      false -> :gen_event.add_handler(manager, handler, args)
    end
  end

  @doc """
  Sends an event notification to the event `manager`.

  The event manager will call `handle_event/2` for each installed event handler.

  `notify` is asynchronous and will return immediately after the notification is
  sent. `notify` will not fail even if the specified event manager does not exist,
  unless it is specified as `name` (atom).
  """
  @spec notify(manager, term) :: :ok
  defdelegate notify(manager, event), to: :gen_event

  @doc """
  Sends a sync event notification to the event `manager`.

  In other words, this function only returns `:ok` after the event manager
  invokes the `handle_event/2` on each installed event handler.

  See `notify/2` for more info.
  """
  @spec sync_notify(manager, term) :: :ok
  defdelegate sync_notify(manager, event), to: :gen_event

  @doc """
  Makes a synchronous call to the event `handler` installed in `manager`.

  The given `request` is sent and the caller waits until a reply arrives or
  a timeout occurs. The event manager will call `handle_call/2` to handle
  the request.

  The return value `reply` is defined in the return value of `handle_call/2`.
  If the specified event handler is not installed, the function returns
  `{:error, :bad_module}`.
  """
  @spec call(manager, handler, term, timeout) ::  term | {:error, term}
  def call(manager, handler, request, timeout \\ 5000) do
    :gen_event.call(manager, handler, request, timeout)
  end

  @doc """
  Cancels all streams currently running with the given `:id`.

  In order for a stream to be cancelled, an `:id` must be passed
  when the stream is created via `stream/2`. Passing a stream without
  an id leads to an argument error.
  """
  @spec cancel_streams(t) :: :ok
  def cancel_streams(%GenEvent{id: nil}) do
    raise ArgumentError, "cannot cancel streams without an id"
  end

  def cancel_streams(%GenEvent{manager: manager, id: id}) do
    handlers = :gen_event.which_handlers(manager)

    for {Enumerable.GenEvent, {handler_id, _}} = ref <- handlers,
        handler_id === id do
      :gen_event.delete_handler(manager, ref, :remove_handler)
    end

    :ok
  end

  @doc """
  Removes an event handler from the event `manager`.

  The event manager will call `terminate/2` to terminate the event handler
  and return the callback value. If the specified event handler is not
  installed, the function returns `{:error, :module_not_found}`.
  """
  @spec remove_handler(manager, handler, term) :: term | {:error, term}
  def remove_handler(manager, handler, args) do
    :gen_event.delete_handler(manager, handler, args)
  end

  @doc """
  Replaces an old event handler with a new one in the event `manager`.

  First, the old event handler is deleted by calling `terminate/2` with
  the given `args1` and collects the return value. Then the new event handler
  is added and initiated by calling `init({args2, term}), where term is the
  return value of calling `terminate/2` in the old handler. This makes it
  possible to transfer information from one handler to another.

  The new handler will be added even if the specified old event handler
  is not installed in which case `term = :error` or if the handler fails to
  terminate with a given reason.

  If there was a linked connection between handler1 and a process pid, there
  will be a link connection between handler2 and pid instead. A new link in
  between the caller process and the new handler can also be set with by
  giving `link: true` as option. See `add_handler/4` for more information.

  If `init/1` in the second handler returns a correct value, this function
  returns `:ok`.
  """
  @spec swap_handler(manager, handler, term, handler, term, [link: boolean]) :: :ok | {:error, term}
  def swap_handler(manager, handler1, args1, handler2, args2, options \\ []) do
    case Keyword.get(options, :link, false) do
      true  -> :gen_event.swap_sup_handler(manager, {handler1, args1}, {handler2, args2})
      false -> :gen_event.swap_handler(manager, {handler1, args1}, {handler2, args2})
    end
  end

  @doc """
  Returns a list of all event handlers installed in the `manager`.
  """
  @spec which_handlers(manager) :: [handler]
  defdelegate which_handlers(manager), to: :gen_event

  @doc """
  Terminates the event `manager`.

  Before terminating, the event manager will call `terminate(:stop, ...)`
  for each installed event handler.
  """
  @spec stop(manager) :: :ok
  defdelegate stop(manager), to: :gen_event
end

defimpl Enumerable, for: GenEvent do
  use GenEvent

  @doc false
  def init({mon_pid, pid, ref}) do
    # Tell the mon_pid we are good to go, and send self() so that this handler
    # can be removed later without using the managers name.
    send(mon_pid, {:UP, ref, self()})
    {:ok, {pid, ref}}
  end

  @doc false
  def handle_event(event, {pid, ref} = state) do
    send pid, {ref, event}
    {:ok, state}
  end

  def reduce(stream, acc, fun) do
    start_fun = fn() -> start(stream) end
    next_fun = &next(stream, &1)
    stop_fun = &stop(stream, &1)
    Stream.resource(start_fun, next_fun, stop_fun).(acc, fun)
  end

  def count(_stream) do
    {:error, __MODULE__}
  end

  def member?(_stream, _item) do
    {:error, __MODULE__}
  end

  defp start(%{manager: manager, id: id, duration: duration} = stream) do
    {mon_pid, mon_ref} = add_handler(manager, id, duration)
    send mon_pid, {:UP, mon_ref, self()}

    receive do
      # The subscription process gave us a go.
      {:UP, ^mon_ref, manager_pid} ->
        {mon_ref, manager_pid}
        # The subscription process died due to an abnormal reason.
      {:DOWN, ^mon_ref, _, _, reason} ->
        exit({reason, {__MODULE__, :start, [stream]}})
    end
  end

  defp next(%{timeout: timeout} = stream, {mon_ref, _manager_pid} = acc) do
    receive do
      {^mon_ref, event} -> {event, acc}
      {:DOWN, ^mon_ref, _, _, :normal} -> nil
      {:DOWN, ^mon_ref, _, _, reason} ->
        exit({reason, {__MODULE__, :next, [stream, acc]}})
    after
      timeout ->
        exit({:timeout, {__MODULE__, :next, [stream, acc]}})
    end
  end

  defp stop(%{id: id}, {mon_ref, manager_pid}) do
    remove_handler(mon_ref, manager_pid, id)
    flush_events(mon_ref)
  end

  defp add_handler(manager, id, duration) do
    parent = self()

    # The subscription is managed by another process, that dies if
    # the handler dies, and is killed when there is a need to remove
    # the subscription.
    spawn_monitor(fn ->
      # It is possible that the handler could be removed, and then the GenEvent
      # could exit before this process has exited normally. Because the removal
      # does not cause an unlinking this process would exit with the same
      # reason. Trapping exits ensures that no errors is raised in this case.
      Process.flag(:trap_exit, true)
      parent_ref = Process.monitor(parent)

      # Receive the notification from the parent, unless it died.
      mon_ref = receive do
        {:UP, ref, ^parent} -> ref
        {:DOWN, ^parent_ref, _, _, _} -> exit(:normal)
      end

      cancel = cancel_ref(id, mon_ref)
      :ok = :gen_event.add_sup_handler(manager, {__MODULE__, cancel},
                                       {self(), parent, mon_ref})

      receive do
        # This message is already in the mailbox if we got this far.
        {:UP, ^mon_ref, manager_pid} ->
          send(parent, {:UP, mon_ref, manager_pid})
          receive do
            # If the parent died, we can exit normally.
            {:DOWN, ^parent_ref, _, _, _} ->
              exit(:normal)

            # reason should be normal unless the handler is swapped.
            {:gen_event_EXIT, {__MODULE__, ^cancel}, reason} ->
              exit(reason)

            # Exit if the manager dies, so the streamer is notified.
            {:EXIT, ^manager_pid, :noconnection} ->
              exit({:nodedown, node(manager_pid)})

            {:EXIT, ^manager_pid, reason} ->
              exit(reason)
          after
            # Our time is over, notify the parent.
            duration -> exit(:normal)
          end
      end
    end)
  end

  defp cancel_ref(nil, mon_ref), do: mon_ref
  defp cancel_ref(id, mon_ref),  do: {id, mon_ref}

  defp remove_handler(mon_ref, manager_pid, id) do
    Process.demonitor(mon_ref, [:flush])
    handler = {__MODULE__, cancel_ref(id, mon_ref)}
    # handler may nolonger be there, if it is the removal will cause the monitor
    # process to exit. If this returns successfuly then no more events will be
    # forwarded.
    _ = :gen_event.delete_handler(manager_pid, handler, :remove_handler)
  catch
    # Do not want to overide the exit reason of the mon_pid so catch errors.
    # However if the exit is due to a disconnection, exit because messages could
    # leak if the nodes are reconnected before the manager on the other node
    # removes the handler. In this case it is very likely that the mon_pid
    # exited with the same reason.
    :exit, reason when reason !== {:nodedown, node(manager_pid)} ->
      :ok
  end

  defp flush_events(mon_ref) do
    receive do
      {^mon_ref, _} -> flush_events(mon_ref)
    after
      0 -> :ok
    end
  end
end
