defmodule GenEvent.Stream do
  @moduledoc """
  Defines a `GenEvent` stream.

  This is a struct returned by `stream/2`. The struct is public and
  contains the following fields:

    * `:manager`  - the manager reference given to `GenEvent.stream/2`
    * `:timeout`  - the timeout in between events, defaults to `:infinity`
    * `:duration` - the duration of the subscription, defaults to `:infinity`
    * `:mode`     - if the subscription mode is ack, sync or async, defaults to `:ack`
  """
  defstruct manager: nil, timeout: :infinity, duration: :infinity, mode: :ack

  @typedoc "The stream mode"
  @type mode :: :ack | :sync | :async

  @type t :: %__MODULE__{
               manager: GenEvent.manager,
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
      -  `{:stop, reason}` - monitored process terminated (for monitored handlers)
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

  ## Learn more and compatibility

  If you wish to find out more about gen events, Elixir getting started
  guides provide a tutorial-like introduction. The documentation and links
  in Erlang can also provide extra insight.

    * http://elixir-lang.org/getting_started/mix/1.html
    * http://www.erlang.org/doc/man/gen_event.html
    * http://learnyousomeerlang.com/event-handlers

  Keep in mind though Elixir and Erlang gen events are not 100% compatible.
  The `:gen_event.add_sup_handler/3` is not supported by Elixir's GenEvent,
  which in turn supports `monitor: true` in `GenEvent.add_handler/4`.

  The benefits of the monitoring approach are described in the "Don't drink
  too much kool aid" section of the "Learn you some Erlang" link above.

  Futhermore, Elixir's also normalizes the `{:error, _}` tuples returned
  by many functions, in order to be more consistent with themselves and
  the `GenServer` module.
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

  ## Monitored handlers

  When adding a handler, a `:monitor` option with value `true` can be given.
  This means the calling process will now be monitored by the GenEvent handler.

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

  Notice this functionality only works with GenEvent started via this
  module (it is not backwards compatible with Erlang's `:gen_event`).
  """
  @spec add_handler(manager, handler, term, [monitor: boolean]) :: :ok | {:error, term}
  def add_handler(manager, handler, args, options \\ []) do
    cond do
      Keyword.get(options, :link, false) ->
        raise ArgumentError, message: "GenEvent.add_handler/4 with link is deprecated and no longer works"
      Keyword.get(options, :monitor, false) ->
        rpc(manager, {:add_mon_handler, handler, args, self()})
      true ->
        rpc(manager, {:add_handler, handler, args})
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

  A `:monitor` option can also be set to specify if the new handler
  should be monitored by the manager. See `add_handler/4` for more
  information.

  If `init/1` in the second handler returns a correct value, this
  function returns `:ok`.
  """
  @spec swap_handler(manager, handler, term, handler, term, [monitor: boolean]) :: :ok | {:error, term}
  def swap_handler(manager, handler1, args1, handler2, args2, options \\ []) do
    cond do
      Keyword.get(options, :link, false) ->
        raise ArgumentError, message: "GenEvent.swap_handler/6 with link is deprecated and no longer works"
      Keyword.get(options, :monitor, false) ->
        rpc(manager, {:swap_mon_handler, handler1, args1, handler2, args2, self()})
      true ->
        rpc(manager, {:swap_handler, handler1, args1, handler2, args2})
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
  Record.defrecordp :handler, [:module, :id, :state, :pid, :ref]

  @doc false
  def init_it(starter, :self, name, mod, args, options) do
    init_it(starter, self(), name, mod, args, options)
  end

  def init_it(starter, parent, name, _, _, options) do
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
        {hib, handlers} = server_notify(event, :handle_event, handlers, name)
        loop(parent, name, handlers, debug, hib)
      {_from, tag, {:sync_notify, event}} ->
        {hib, handlers} = server_notify(event, :handle_event, handlers, name)
        reply(tag, :ok)
        loop(parent, name, handlers, debug, hib)
      {:DOWN, ref, :process, _pid, reason} = other ->
        case handle_down(ref, reason, handlers, name) do
          {:ok, handlers} ->
            loop(parent, name, handlers, debug, false)
          :error ->
            {hib, handlers} = server_notify(other, :handle_info, handlers, name)
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
      {_from, tag, {:add_mon_handler, handler, args, mon}} ->
        {hib, reply, handlers} = server_add_mon_handler(handler, args, handlers, mon)
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
      {_from, tag, :stop} ->
        try do
          server_terminate(:normal, parent, handlers, name)
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

  defp server_add_mon_handler({module, id}, args, handlers, pid) do
    ref = Process.monitor(pid)
    handler = handler(module: module, id: {module, id}, pid: pid, ref: ref)
    server_add_handler(module, handler, args, handlers)
  end

  defp server_add_mon_handler(module, args, handlers, pid) do
    ref = Process.monitor(pid)
    handler = handler(module: module, id: module, pid: pid, ref: ref)
    server_add_handler(module, handler, args, handlers)
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

  defp server_terminate(reason, _parent, handlers, name) do
    _ =
      for handler <- handlers do
        # TODO: Consider making it {:stop, :shutdown} or
        # {:stop, reason} for consistency?
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
        do_terminate(handler, {:stop, reason}, :remove, name, :shutdown)
        {:ok, :lists.keydelete(ref, handler(:ref) + 1, handlers)}
    end
  end

  defp do_swap(handler, args1, module2, args2, name) do
    pid   = handler(handler, :pid)
    ref   = handler(handler, :ref)
    state = do_terminate(handler, args1, :swapped, name, {:swapped, module2, pid})

    {hib, res, handlers} =
      server_add_handler(module2, {args2, state}, [])

    case res do
      :ok ->
        {hib, handler(hd(handlers), pid: pid, ref: ref)}
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
      :throw, val -> {:error, {{:nocatch, val}, System.stacktrace}}
      :error, val -> {:error, {val, System.stacktrace}}
      :exit, val  -> {:error, val}
    else
      res -> {:ok, res}
    end
  end

  defp report_terminate(handler, reason, state, last_in, name) do
    report_error(handler, reason, state, last_in, name)
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
  def init({_mode, _pid, _ref} = state) do
    {:ok, state}
  end

  @doc false
  def handle_event(event, {mode, pid, ref} = state) when mode in [:sync, :ack] do
    # TODO: The process is already monitored by the GenEvent
    # but we don't have access to the reference in here.
    sync = Process.monitor(pid)
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

  def handle_event(event, {:async, pid, ref} = state) do
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

  defp start(%{manager: manager, duration: duration, mode: mode} = stream) do
    pid = whereis(manager)
    ref = Process.monitor(pid)

    try do
      :ok = GenEvent.add_handler(pid, {__MODULE__, ref},
                                 {mode, self(), ref}, monitor: true)
    catch
      :exit, :noproc -> exit({:noproc, {__MODULE__, :start, [stream]}})
    end

    timer = cond do
      duration == :infinity -> nil
      is_integer(duration)  -> :erlang.send_after(duration, self(), {ref, :timedout})
    end

    {pid, ref, timer}
  end

  defp whereis(pid) when is_pid(pid), do: pid
  defp whereis(atom) when is_atom(atom), do: :erlang.whereis(atom)
  defp whereis({:global, name}), do: :global.whereis_name(name)
  defp whereis({:via, module, name}), do: module.whereis_name(name)
  defp whereis({atom, node}), do: :rpc.call(node, :erlang, :whereis, [atom])

  defp next(%{timeout: timeout} = stream, {pid, ref, _timer} = acc) do
    receive do
      # The handler was removed. Stop iteration, resolve the event later.
      # We need to demonitor now, otherwise it appears with
      # higher priority in the shutdown process.
      {:gen_event_EXIT, {__MODULE__, ^ref}, _reason} = event ->
        Process.demonitor(ref, [:flush])
        send(self(), event)
        {:halt, acc}

      # The manager died. Stop iteration, resolve the event later.
      {:DOWN, ^ref, _, _, _} = event ->
        send(self(), event)
        {:halt, acc}

      # Duration timeout.
      {^ref, :timedout} ->
        {:halt, acc}

      # Got an event!
      {^ref, sync_ref, event} ->
        {[{stream.mode, sync_ref, pid, event}], acc}
    after
      timeout ->
        exit({:timeout, {__MODULE__, :next, [stream, acc]}})
    end
  end

  defp stop(%{mode: mode} = stream, {pid, ref, timer} = acc) do
    remove_timer(timer)

    # TODO: If we got an gen_event_EXIT or a DOWN, the handler was
    # already removed but we can't pass this information based on
    # resource current implementation. So right now, we may send a
    # remove_handler even after it was already removed. Removing
    # this extra call would help with removing overhead.
    spawn(fn ->
      GenEvent.remove_handler(pid, {__MODULE__, ref}, :shutdown)
    end)

    case wait_for_handler_removal(pid, ref) do
      :ok when mode == :async ->
        flush_events(ref)
      :ok ->
        :ok
      {:error, reason} ->
        exit({reason, {__MODULE__, :stop, [stream, acc]}})
    end
  end

  defp remove_timer(nil), do: :ok
  defp remove_timer(ref) do
    unless :erlang.cancel_timer(ref) do
      receive do
        {^ref, :timedout} -> :ok
      after
        0 -> :ok
      end
    end
  end

  defp wait_for_handler_removal(pid, ref) do
    receive do
      {^ref, sync, _} when sync != nil ->
        send(pid, {sync, :done})
        wait_for_handler_removal(pid, ref)
      {:gen_event_EXIT, {__MODULE__, ^ref}, reason} when reason in [:normal, :shutdown] ->
        Process.demonitor(ref, [:flush])
        :ok
      {:gen_event_EXIT, {__MODULE__, ^ref}, reason} ->
        Process.demonitor(ref, [:flush])
        {:error, reason}
      {:DOWN, ^ref, _, _, reason} ->
        {:error, reason}
    end
  end

  defp flush_events(ref) do
    receive do
      {^ref, _, _} ->
        flush_events(ref)
    after
      0 -> :ok
    end
  end
end
