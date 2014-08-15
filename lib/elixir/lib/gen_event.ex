defmodule GenEvent.Stream do
  @moduledoc """
  Defines a `GenEvent` stream.

  This is a struct returned by `stream/2`. The struct is public and
  contains the following fields:

    * `:manager`  - the manager reference given to `GenEvent.stream/2`
    * `:id`       - the event stream id for cancellation
    * `:timeout`  - the timeout in between events, defaults to `:infinity`
    * `:duration` - the duration of the subscription, defaults to `:infinity`
    * `:mode`     - if the subscription mode is ack, sync or async, defaults to `:ack`
  """
  defstruct manager: nil, id: nil, timeout: :infinity, duration: :infinity, mode: :ack

  @typedoc "The stream mode"
  @type mode :: :ack | :sync | :async

  @type t :: %__MODULE__{
               manager: GenEvent.manager,
               id: term,
               timeout: timeout,
               duration: timeout,
               mode: mode}
end

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
  system can be built using event handlers where each log message is
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
  invoke `handle_event/2` for each registered handler.

  We can add new handlers with `add_handler/4`. Calls can also
  be made to specific handlers by using `call/3`.

  ## Callbacks

  There are 6 callbacks required to be implemented in a `GenEvent`. By
  adding `use GenEvent` to your module, Elixir will automatically define
  all 6 callbacks for you, leaving it up to you to implement the ones
  you want to customize. The callbacks are:

    * `init(args)` - invoked when the event handler is added.

      It must return:

      -  `{:ok, state}`
      -  `{:ok, state, :hibernate}`
      -  `{:error, reason}`

    * `handle_event(msg, state)` - invoked whenever an event is sent via
      `notify/2` or `sync_notify/2`.

      It must return:

      -  `{:ok, new_state}`
      -  `{:ok, new_state, :hibernate}`
      -  `{:swap_handler, args1, new_state, handler2, args2}`
      -  `:remove_handler`

    * `handle_call(msg, state)` - invoked when a `call/3` is done to a specific
      handler.

      It must return:

      -  `{:ok, reply, new_state}`
      -  `{:ok, reply, new_state, :hibernate}`
      -  `{:swap_handler, reply, args1, new_state, handler2, args2}`
      -  `{:remove_handler, reply}`

    * `handle_info(msg, state)` - invoked to handle all other messages which
      are received by the process. Must return the same values as
      `handle_event/2`.

    * `terminate(reason, state)` - called when the event handler is removed or
      the event manager is terminating. It can return any term.

      The reason is one of:

      -  `:stop` - manager is terminating
      -  `{:stop, reason}` - linked process terminated
      -  `:remove_handler` - handler is being removed
      -  `{:error, term}` - handler crashed or returned a bad value
      -  `term` - any term passed to functions like `GenEvent.remove_handler/2`

    * `code_change(old_vsn, state, extra)` - called when the application
      code is being upgraded live (hot code swapping).

      It must return:

      -  `{:ok, new_state}`

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
        # We do this to trick dialyzer to not complain about non-local returns.
        case :random.uniform(1) do
          1 -> exit({:bad_call, msg})
          2 -> {:remove_handler, :ok}
        end
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
      other when is_tuple(other) ->
        :gen.start(GenEvent, mode, other, @no_callback, [], [])
    end
  end

  @doc """
  Returns a stream that consumes events from the `manager`.

  The stream is a `GenEvent` struct that implements the `Enumerable`
  protocol. Consumption of events only begins when enumeration starts.

  ## Options

    * `:id` - an id to identify all live stream instances; when an `:id` is
      given, existing streams can be called with via `cancel_streams`.

    * `:timeout` (Enumerable) - raises if no event arrives in X milliseconds.

    * `:duration` (Enumerable) - only consume events during the X milliseconds
      from the streaming start.

    * `:mode` - the mode to consume events, can be `:ack` (default), `:sync`
      or `:async`. See modes below.

  ## Modes

  GenEvent stream supports three different modes.

  On `:ack`, the stream acknowledges each event, providing back pressure,
  but processing of the message happens asynchronously, allowing the event
  manager to move on to the next handler as soon as the event is
  acknowledged.

  On `:sync`, the event manager waits for the event to be consumed
  before moving on to the next event handler.

  On `:async`, all events are processed asynchronously but there is no
  ack (which means there is no backpressure).

  """
  @spec stream(manager, Keyword.t) :: GenEvent.Stream.t
  def stream(manager, options \\ []) do
    %GenEvent.Stream{
      manager: manager,
      id: Keyword.get(options, :id),
      timeout: Keyword.get(options, :timeout, :infinity),
      duration: Keyword.get(options, :duration, :infinity),
      mode: Keyword.get(options, :mode, :ack)}
  end

  @doc """
  Adds a new event handler to the event `manager`.

  The event manager will call the `init/1` callback with `args` to
  initiate the event handler and its internal state.

  If `init/1` returns a correct value indicating successful completion,
  the event manager adds the event handler and this function returns
  `:ok`. If the callback fails with `reason` or returns `{:error, reason}`,
  the event handler is ignored and this function returns `{:error, reason}`.

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
      function

    * `:shutdown` - if the event handler has been removed because the event
      manager is terminating

    * `{:swapped, new_handler, pid}` - if the process pid has replaced the
      event handler by another

    * a term - if the event handler is removed due to an error. Which term
      depends on the error

  """
  @spec add_handler(manager, handler, term, [link: boolean]) :: :ok | {:error, term}
  def add_handler(manager, handler, args, options \\ []) do
    case Keyword.get(options, :link, false) do
      true  -> rpc(manager, {:add_sup_handler, handler, args, self()})
      false -> rpc(manager, {:add_handler, handler, args})
    end
  end

  @doc """
  Sends an event notification to the event `manager`.

  The event manager will call `handle_event/2` for each
  installed event handler.

  `notify` is asynchronous and will return immediately after the notification is
  sent. `notify` will not fail even if the specified event manager does not exist,
  unless it is specified as an atom.
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

  def notify({:via, mod, name}, msg) do
    try do
      mod.send(name, {:notify, msg})
      :ok
    catch
      _, _ -> :ok
    end
  end

  def notify(other, msg) do
    send(other, {:notify, msg})
    :ok
  end

  @doc """
  Sends a sync event notification to the event `manager`.

  In other words, this function only returns `:ok` after the event manager
  invokes the `handle_event/2` on each installed event handler.

  See `notify/2` for more info.
  """
  @spec sync_notify(manager, term) :: :ok
  def sync_notify(manager, event) do
    rpc(manager, {:sync_notify, event})
  end

  @doc """
  Makes a synchronous call to the event `handler` installed in `manager`.

  The given `request` is sent and the caller waits until a reply arrives or
  a timeout occurs. The event manager will call `handle_call/2` to handle
  the request.

  The return value `reply` is defined in the return value of `handle_call/2`.
  If the specified event handler is not installed, the function returns
  `{:error, :module_not_found}`.
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
  Cancels all streams currently running with the given `:id`.

  In order for a stream to be cancelled, an `:id` must be passed
  when the stream is created via `stream/2`. Passing a stream without
  an id leads to an argument error.
  """
  @spec cancel_streams(GenEvent.Stream.t) :: :ok
  def cancel_streams(%GenEvent.Stream{id: nil}) do
    raise ArgumentError, "cannot cancel streams without an id"
  end

  def cancel_streams(%GenEvent.Stream{manager: manager, id: id}) do
    IO.write :stderr, "warning: GenEvent.cancel_streams/1 is deprecated\n#{Exception.format_stacktrace}"
    handlers = which_handlers(manager)

    _ = for {Enumerable.GenEvent.Stream, {handler_id, _}} = ref <- handlers,
        handler_id === id do
      remove_handler(manager, ref, :remove_handler)
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
    rpc(manager, {:delete_handler, handler, args})
  end

  @doc """
  Replaces an old event handler with a new one in the event `manager`.

  First, the old event handler is deleted by calling `terminate/2` with
  the given `args1` and collects the return value. Then the new event handler
  is added and initiated by calling `init({args2, state}), where term is the
  return value of calling `terminate/2` in the old handler. This makes it
  possible to transfer information from one handler to another.

  The new handler will be added even if the specified old event handler
  is not installed or if the handler fails to terminate with a given reason
  in which case `state = {:error, term}`.

  A `:link` option can also be set to specify if the new handler should
  be linked or not. See `add_handler/4` for more information.

  If `init/1` in the second handler returns a correct value, this
  function returns `:ok`.
  """
  @spec swap_handler(manager, handler, term, handler, term, [link: boolean]) :: :ok | {:error, term}
  def swap_handler(manager, handler1, args1, handler2, args2, options \\ []) do
    case Keyword.get(options, :link, false) do
      true  -> rpc(manager, {:swap_sup_handler, handler1, args1, handler2, args2, self()})
      false -> rpc(manager, {:swap_handler, handler1, args1, handler2, args2})
    end
  end

  @doc """
  Returns a list of all event handlers installed in the `manager`.
  """
  @spec which_handlers(manager) :: [handler]
  def which_handlers(manager) do
    rpc(manager, :which_handlers)
  end

  @doc """
  Terminates the event `manager`.

  Before terminating, the event manager will call `terminate(:stop, ...)`
  for each installed event handler.
  """
  @spec stop(manager) :: :ok
  def stop(manager) do
    rpc(manager, :stop)
  end

  defp rpc(module, cmd) do
    # TODO: Change the tag once patch is accepted by OTP
    {:ok, reply} = :gen.call(module, self(), cmd, :infinity)
    reply
  end

  ## Init callbacks

  require Record
  Record.defrecordp :handler, [:module, :id, :state, :supervised]

  @doc false
  def init_it(starter, :self, name, mod, args, options) do
    init_it(starter, self(), name, mod, args, options)
  end

  def init_it(starter, parent, name, _, _, options) do
    # TODO: Remove this once we removed linked handlers
    Process.flag(:trap_exit, true)
    debug = :gen.debug_options(options)
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
        terminate_server(reason, parent, handlers, name)
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
        {hib, handlers} = server_notify(event, :handle_event, handlers, name)
        loop(parent, name, handlers, debug, hib)
      {_from, tag, {:sync_notify, event}} ->
        {hib, handlers} = server_notify(event, :handle_event, handlers, name)
        reply(tag, :ok)
        loop(parent, name, handlers, debug, hib)
      {:EXIT, from, reason} ->
        # TODO: Rework this once we remove linked handlers
        # TODO: There seems to be no apparent reason for ignoring hibernate
        handlers = handle_exit(from, reason, handlers, name)
        loop(parent, name, handlers, debug, false)
      {_from, tag, {:call, handler, query}} ->
        {hib, reply, handlers} = server_call(handler, query, handlers, name)
        reply(tag, reply)
        loop(parent, name, handlers, debug, hib)
      {_from, tag, {:add_handler, handler, args}} ->
        {hib, reply, handlers} = server_add_handler(handler, args, handlers)
        reply(tag, reply)
        loop(parent, name, handlers, debug, hib)
      {_from, tag, {:add_sup_handler, handler, args, sup}} ->
        {hib, reply, handlers} = server_add_sup_handler(handler, args, handlers, sup)
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
      {_from, tag, {:swap_sup_handler, handler1, args1, handler2, args2, sup}} ->
        {hib, reply, handlers} = server_swap_handler(handler1, args1, handler2, args2, handlers, sup, name)
        reply(tag, reply)
        loop(parent, name, handlers, debug, hib)
      {_from, tag, :stop} ->
        try do
          terminate_server(:normal, parent, handlers, name)
        catch
          _, _ -> :ok
        end
        reply(tag, :ok)
      {_from, tag, :which_handlers} ->
        reply(tag, server_which_handlers(handlers))
        loop(parent, name, handlers, debug, false)
      {_from, tag, :get_modules} ->
        reply(tag, server_get_modules(handlers))
        loop(parent, name, handlers, debug, false)
      other ->
        {hib, handlers} = server_notify(other, :handle_info, handlers, name)
        loop(parent, name, handlers, debug, hib)
    end
  end

  defp terminate_server(reason, parent, handlers, name) do
    _ =
      for handler <- handlers do
        # TODO: Consider making it {:stop, :shutdown} for consistency
        do_terminate(handler, :stop, :stop, name, :shutdown)
      end
    do_unlink(parent, handlers) # TODO: Rework this once we remove linked handlers
    exit(reason)
  end

  defp reply({from, ref}, msg) do
    send from, {ref, msg}
  end

  defp do_unlink(parent, handlers) do
    _ =
      for handler <- handlers do
        case handler(handler, :supervised) do
          ^parent -> true
          nil     -> true
          sup     -> Process.unlink(sup)
        end
      end

    :ok
  end

  defp handle_exit(from, reason, handlers, name) do
    # TODO: Respect hibernate here and convert this function to monitors
    handlers = terminate_supervised(from, reason, handlers, name)
    {_, handlers} = server_notify({:EXIT, from, reason}, :handle_info, handlers, name)
    handlers
  end

  defp terminate_supervised(pid, reason, handlers, name) do
    Enum.reject handlers, fn handler ->
      if handler(handler, :supervised) == pid do
        do_terminate(handler, {:stop, reason}, :remove, name, :shutdown)
        true
      end
    end
  end

  ## System callbacks

  @doc false
  def system_continue(parent, debug, [name, handlers, hib]) do
    loop(parent, name, handlers, debug, hib)
  end

  @doc false
  def system_terminate(reason, parent, _debug, [name, handlers, _hib]) do
    terminate_server(reason, parent, handlers, name)
  end

  @doc false
  def system_code_change([name, handlers, hib], module , old_vsn, extra) do
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
      List.unzip(for handler <- handlers do
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
     data: [{"Status", sys_state}, {"Parent", parent}],
     items: {"Installed handlers", formatted}]
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
    server_add_handler(module, handler, args, handlers)
  end

  defp server_add_handler(module, args, handlers) do
    handler = handler(module: module, id: module)
    server_add_handler(module, handler, args, handlers)
  end

  defp server_add_handler(module, handler, arg, handlers) do
    # TODO: Do not allow duplicated handlers
    # TODO: Test ignore and bad return value logic
    case do_handler(module, :init, [arg]) do
      {:ok, res} ->
        case res do
          {:ok, state} ->
            {false, :ok, [handler(handler, state: state)|handlers]}
          {:ok, state, :hibernate} ->
            {true, :ok, [handler(handler, state: state)|handlers]}
          {:error, _} = error ->
            {false, error, handlers}
          other ->
            {false, {:error, {:bad_return_value, other}}, handlers}
        end
      {:error, _} = error ->
        {false, error, handlers}
    end
  end

  defp server_add_sup_handler({module, id}, args, handlers, parent) do
    Process.link(parent)
    handler = handler(module: module, id: {module, id}, supervised: parent)
    server_add_handler(module, handler, args, handlers)
  end

  defp server_add_sup_handler(module, args, handlers, parent) do
    Process.link(parent)
    handler = handler(module: module, id: module, supervised: parent)
    server_add_handler(module, handler, args, handlers)
  end

  defp server_remove_handler(module, args, handlers, name) do
    do_take_handler(module, args, handlers, name, :remove, :normal)
  end

  defp server_swap_handler(module1, args1, module2, args2, handlers, sup, name) do
    {state, handlers} =
      do_take_handler(module1, args1, handlers, name, :swapped, {:swapped, module2, sup})

    if sup do
      server_add_sup_handler(module2, {args2, state}, handlers, sup)
    else
      server_add_handler(module2, {args2, state}, handlers)
    end
  end

  defp server_notify(event, fun, [handler|t], name) do
    case server_update(handler, fun, event, name) do
      {hib1, handler} ->
        {hib2, t} = server_notify(event, fun, t, name)
        {hib1 or hib2, [handler|t]}
      :error ->
        server_notify(event, fun, t, name)
    end
  end

  defp server_notify(_, _, [], _) do
    {false, []}
  end

  defp server_update(handler, fun, event, name) do
    handler(module: module, state: state) = handler

    case do_handler(module, fun, [event, state]) do
      {:ok, res} ->
        case res do
          {:ok, state} ->
            {false, handler(handler, state: state)}
          {:ok, state, :hibernate} ->
            {true, handler(handler, state: state)}
          {:swap_handler, args1, state, handler2, args2} ->
            do_swap(handler(handler, state: state), args1, handler2, args2, name)
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

  defp server_call(module, query, handlers, name) do
    case :lists.keyfind(module, handler(:id) + 1, handlers) do
      false ->
        {false, {:error, :module_not_found}, handlers}
      handler ->
        case server_call_update(handler, query, name) do
          {{hib, handler}, reply} ->
            {hib, reply, :lists.keyreplace(module, handler(:id) + 1, handlers, handler)}
          {:error, reply} ->
            {false, reply, :lists.keydelete(module, handler(:id) + 1, handlers)}
        end
    end
  end

  defp server_call_update(handler, query, name) do
    handler(module: module, state: state) = handler
    case do_handler(module, :handle_call, [query, state]) do
      {:ok, res} ->
        case res do
          {:ok, reply, state} ->
            {{false, handler(handler, state: state)}, reply}
          {:ok, reply, state, :hibernate} ->
            {{true, handler(handler, state: state)}, reply}
          {:swap_handler, reply, args1, state, handler2, args2} ->
            {do_swap(handler(handler, state: state), args1, handler2, args2, name), reply}
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

  defp do_swap(handler, args1, module2, args2, name) do
    sup   = handler(handler, :supervised)
    state = do_terminate(handler, args1, :swapped, name, {:swapped, module2, sup})

    {hib, res, handlers} =
      if sup do
        server_add_sup_handler(module2, {args2, state}, [], sup)
      else
        server_add_handler(module2, {args2, state}, [])
      end

    case res do
      :ok ->
        {hib, hd(handlers)}
      {:error, reason} ->
        report_terminate(handler, reason, state, :swapped, name)
    end
  end

  defp do_take_handler(module, args, handlers, name, last_in, reason) do
    case :lists.keytake(module, handler(:id) + 1, handlers) do
      {:value, handler, handlers} ->
        {do_terminate(handler, args, last_in, name, reason), handlers}
      false ->
        {{:error, :module_not_found}, handlers}
    end
  end

  # The value of this function is returned by remove_handler.
  # TODO: arg can be a wide range set of values. It is worth
  # discussing if we want to limit those down. We just need
  # to look for all do_terminate calls.
  # TODO: Update documentation for arg to say it is never
  # {:error, {:EXIT, _}}.
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
      :throw, val ->
        {:error, {{:nocatch, val}, System.stacktrace}}
      :error, val ->
        {:error, {val, System.stacktrace}}
      :exit, val ->
        {:error, val}
    else
      res -> {:ok, res}
    end
  end

  defp report_terminate(handler, reason, state, last_in, name) do
    report_error(handler, reason, state, last_in, name)
    if sup = handler(handler, :supervised) do
      send sup, {:gen_event_EXIT, handler(handler, :id), reason}
    end
  end

  defp report_error(_handler, :normal, _, _, _), do: :ok
  defp report_error(_handler, :shutdown, _, _, _), do: :ok
  defp report_error(_handler, {:swapped, _, _}, _, _, _), do: :ok
  defp report_error(handler, reason, state, last_in, name) do
    reason =
      case reason do
        {:undef, [{m,f,a,_}|_]=mfas} ->
          cond do
            :code.is_loaded(m) ->
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

defimpl Enumerable, for: GenEvent.Stream do
  use GenEvent

  @doc false
  def init({_mode, mon_pid, _pid, ref} = state) do
    # Tell the mon_pid we are good to go, and send self() so that this handler
    # can be removed later without using the managers name.
    send(mon_pid, {:UP, ref, self()})
    {:ok, state}
  end

  @doc false
  def handle_event(event, {mode, mon_pid, pid, ref} = state) when mode in [:sync, :ack] do
    sync = Process.monitor(mon_pid)
    send pid, {ref, sync, event}
    receive do
      {^sync, :done} ->
        Process.demonitor(sync, [:flush])
        :remove_handler
      {^sync, :next} ->
        Process.demonitor(sync, [:flush])
        {:ok, state}
      {:DOWN, ^sync, _, _, _} ->
        {:ok, state}
    end
  end

  def handle_event(event, {:async, _mon_pid, pid, ref} = state) do
    send pid, {ref, nil, event}
    {:ok, state}
  end

  def reduce(stream, acc, fun) do
    start_fun = fn() -> start(stream) end
    next_fun = &next(stream, &1)
    stop_fun = &stop(stream, &1)
    Stream.resource(start_fun, next_fun, stop_fun).(acc, wrap_reducer(fun))
  end

  def count(_stream) do
    {:error, __MODULE__}
  end

  def member?(_stream, _item) do
    {:error, __MODULE__}
  end

  defp wrap_reducer(fun) do
    fn
      {:ack, ref, manager, event}, acc ->
        send manager, {ref, :next}
        fun.(event, acc)
      {:async, _, _manager, event}, acc ->
        fun.(event, acc)
      {:sync, ref, manager, event}, acc ->
        try do
          fun.(event, acc)
        after
          send manager, {ref, :next}
        end
    end
  end

  defp start(%{manager: manager, id: id, duration: duration, mode: mode} = stream) do
    {mon_pid, mon_ref} = add_handler(mode, manager, id, duration)
    send mon_pid, {:UP, mon_ref, self()}

    receive do
      # The subscription process gave us a go.
      {:UP, ^mon_ref, manager_pid} ->
        {mon_ref, mon_pid, manager_pid}
      # The subscription process died due to an abnormal reason.
      {:DOWN, ^mon_ref, _, _, reason} ->
        exit({reason, {__MODULE__, :start, [stream]}})
    end
  end

  defp next(%{timeout: timeout} = stream, {mon_ref, mon_pid, manager_pid} = acc) do
    # If :DOWN is received must resend it to self so that stop/2 can receive it
    # and know that the handler has been removed.
    receive do
      {:DOWN, ^mon_ref, _, _, :normal} ->
        send(self(), {:DOWN, mon_ref, :process, mon_pid, :normal})
        nil
      {:DOWN, ^mon_ref, _, _, reason} ->
        send(self(), {:DOWN, mon_ref, :process, mon_pid, :normal})
        exit({reason, {__MODULE__, :next, [stream, acc]}})
      {^mon_ref, sync_ref, event} ->
        {{stream.mode, sync_ref, manager_pid, event}, acc}
    after
      timeout ->
        exit({:timeout, {__MODULE__, :next, [stream, acc]}})
    end
  end

  defp stop(%{mode: mode} = stream, {mon_ref, mon_pid, manager_pid} = acc) do
    case remove_handler(mon_ref, mon_pid, manager_pid) do
      :ok when mode == :async ->
        flush_events(mon_ref)
      :ok ->
        :ok
      {:error, reason} ->
        exit({reason, {__MODULE__, :stop, [stream, acc]}})
    end
  end

  defp add_handler(mode, manager, id, duration) do
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
      :ok = GenEvent.add_handler(manager, {__MODULE__, cancel},
                                {mode, self(), parent, mon_ref}, link: true)

      receive do
        # This message is already in the mailbox if we got this far.
        {:UP, ^mon_ref, manager_pid} ->
          send(parent, {:UP, mon_ref, manager_pid})
          receive do
            # The stream has finished, remove the handler.
            {:DONE, ^mon_ref} ->
              exit_handler(manager_pid, parent_ref, cancel)

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

  defp exit_handler(manager_pid, parent_ref, cancel) do
    # Send exit signal so manager removes handler.
    Process.exit(manager_pid, :shutdown)
    receive do
      # If the parent died, we can exit normally.
      {:DOWN, ^parent_ref, _, _, _} ->
        exit(:normal)

      # Probably the reason is :shutdown, which occurs when the manager receives
      # an exit signal from a handler supervising process. However whatever the
      # reason the handler has been removed so it is ok.
      {:gen_event_EXIT, {__MODULE__, ^cancel}, _} ->
        exit(:normal)

      # The connection broke, perhaps the handler might try to forward events
      # before it removes the handler, so must exit abnormally.
      {:EXIT, ^manager_pid, :noconnection} ->
        exit({:nodedown, node(manager_pid)})

      # The manager has exited but don't exit abnormally as the handler has died
      # with the manager and all expected events have been handled. This is ok.
      {:EXIT, ^manager_pid, _} ->
        exit(:normal)
    end
  end

  defp remove_handler(mon_ref, mon_pid, manager_pid) do
    send(mon_pid, {:DONE, mon_ref})
    receive do
      {^mon_ref, sync, _} when sync != nil ->
        send(manager_pid, {sync, :done})
        Process.demonitor(mon_ref, [:flush])
        :ok
      {:DOWN, ^mon_ref, _, _, :normal} ->
        :ok
      {:DOWN, ^mon_ref, _, _, reason} ->
        {:error, reason}
    end
  end

  defp flush_events(mon_ref) do
    receive do
      {^mon_ref, _, _} ->
        flush_events(mon_ref)
    after
      0 -> :ok
    end
  end
end
