defmodule GenEvent do
  # Functions from this module are deprecated in elixir_dispatch.

  @moduledoc """
  A event manager with event handlers behaviour.

  If you are interested in implementing an event manager, please read the
  "Alternatives" section below. If you have to implement an event handler to
  integrate with an existing system, such as Elixir's Logger, please use
  `:gen_event` instead.

  ## Alternatives

  There are a few suitable alternatives to replace GenEvent. Each of them can be
  the most beneficial based on the use case.

  ### Supervisor and GenServers

  One alternative to GenEvent is a very minimal solution consisting of using a
  supervisor and multiple GenServers started under it. The supervisor acts as
  the "event manager" and the children GenServers act as the "event handlers".
  This approach has some shortcomings (it provides no backpressure for example)
  but can still replace GenEvent for low-profile usages of it. [This blog post
  by José
  Valim](http://blog.plataformatec.com.br/2016/11/replacing-genevent-by-a-supervisor-genserver/)
  has more detailed information on this approach.

  ### GenStage

  If the use case where you were using GenEvent requires more complex logic,
  [GenStage](https://github.com/elixir-lang/gen_stage) provides a great
  alternative. GenStage is an external Elixir library maintained by the Elixir
  team; it provides a tool to implement systems that exchange events in a
  demand-driven way with built-in support for backpressure. See the [GenStage
  documentation](https://hexdocs.pm/gen_stage) for more information.

  ### `:gen_event`

  If your use case requires exactly what GenEvent provided, or you have to
  integrate with an existing `:gen_event`-based system, you can still use the
  [`:gen_event`](http://erlang.org/doc/man/gen_event.html) Erlang module.
  """

  @moduledoc deprecated: "Use Erlang/OTP's :gen_event module instead"

  @callback init(args :: term) ::
              {:ok, state}
              | {:ok, state, :hibernate}
              | {:error, reason :: any}
            when state: any

  @callback handle_event(event :: term, state :: term) ::
              {:ok, new_state}
              | {:ok, new_state, :hibernate}
              | :remove_handler
            when new_state: term

  @callback handle_call(request :: term, state :: term) ::
              {:ok, reply, new_state}
              | {:ok, reply, new_state, :hibernate}
              | {:remove_handler, reply}
            when reply: term, new_state: term

  @callback handle_info(msg :: term, state :: term) ::
              {:ok, new_state}
              | {:ok, new_state, :hibernate}
              | :remove_handler
            when new_state: term

  @callback terminate(reason, state :: term) :: term
            when reason: :stop | {:stop, term} | :remove_handler | {:error, term} | term

  @callback code_change(old_vsn, state :: term, extra :: term) :: {:ok, new_state :: term}
            when old_vsn: term | {:down, term}

  @type on_start :: {:ok, pid} | {:error, {:already_started, pid}}

  @type name :: atom | {:global, term} | {:via, module, term}

  @type options :: [name: name]

  @type manager :: pid | name | {atom, node}

  @type handler :: atom | {atom, term}

  message = "Use one of the alternatives described in the documentation for the GenEvent module"

  @deprecated message
  @doc false
  defmacro __using__(_) do
    %{file: file, line: line} = __CALLER__

    deprecation_message =
      "the GenEvent module is deprecated, see its documentation for alternatives"

    :elixir_errors.warn(line, file, deprecation_message)

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
            {_, []} -> self()
            {_, name} -> name
          end

        # We do this to trick Dialyzer to not complain about non-local returns.
        case :erlang.phash2(1, 1) do
          0 ->
            raise "attempted to call GenEvent #{inspect(proc)} but no handle_call/2 clause was provided"

          1 ->
            {:remove_handler, {:bad_call, msg}}
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

      defoverridable init: 1,
                     handle_event: 2,
                     handle_call: 2,
                     handle_info: 2,
                     terminate: 2,
                     code_change: 3
    end
  end

  @doc false
  @deprecated message
  @spec start_link(options) :: on_start
  def start_link(options \\ []) when is_list(options) do
    do_start(:link, options)
  end

  @doc false
  @deprecated message
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
        expected :name option to be one of the following:

          * nil
          * atom
          * {:global, term}
          * {:via, module, term}

        Got: #{inspect(other)}
        """
    end
  end

  @doc false
  @deprecated message
  @spec stream(manager, keyword) :: GenEvent.Stream.t()
  def stream(manager, options \\ []) do
    %GenEvent.Stream{manager: manager, timeout: Keyword.get(options, :timeout, :infinity)}
  end

  @doc false
  @deprecated message
  @spec add_handler(manager, handler, term) :: :ok | {:error, term}
  def add_handler(manager, handler, args) do
    rpc(manager, {:add_handler, handler, args})
  end

  @doc false
  @deprecated message
  @spec add_mon_handler(manager, handler, term) :: :ok | {:error, term}
  def add_mon_handler(manager, handler, args) do
    rpc(manager, {:add_mon_handler, handler, args, self()})
  end

  @doc false
  @deprecated message
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
      when tuple_size(manager) == 2 and is_atom(elem(manager, 0)) and is_atom(elem(manager, 1)) do
    send(manager, {:notify, msg})
    :ok
  end

  @doc false
  @deprecated message
  @spec sync_notify(manager, term) :: :ok
  def sync_notify(manager, event) do
    rpc(manager, {:sync_notify, event})
  end

  @doc false
  @deprecated message
  @spec ack_notify(manager, term) :: :ok
  def ack_notify(manager, event) do
    rpc(manager, {:ack_notify, event})
  end

  @doc false
  @deprecated message
  @spec call(manager, handler, term, timeout) :: term | {:error, term}
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

  @doc false
  @deprecated message
  @spec remove_handler(manager, handler, term) :: term | {:error, term}
  def remove_handler(manager, handler, args) do
    rpc(manager, {:delete_handler, handler, args})
  end

  @doc false
  @deprecated message
  @spec swap_handler(manager, handler, term, handler, term) :: :ok | {:error, term}
  def swap_handler(manager, handler1, args1, handler2, args2) do
    rpc(manager, {:swap_handler, handler1, args1, handler2, args2})
  end

  @doc false
  @deprecated message
  @spec swap_mon_handler(manager, handler, term, handler, term) :: :ok | {:error, term}
  def swap_mon_handler(manager, handler1, args1, handler2, args2) do
    rpc(manager, {:swap_mon_handler, handler1, args1, handler2, args2, self()})
  end

  @doc false
  @deprecated message
  @spec which_handlers(manager) :: [handler]
  def which_handlers(manager) do
    rpc(manager, :which_handlers)
  end

  @doc false
  @deprecated message
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
  Record.defrecordp(:handler, [:module, :id, :state, :pid, :ref])

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

  defp name({:local, name}), do: name
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
        :sys.handle_system_msg(req, from, parent, __MODULE__, debug, [name, handlers, hib], hib)

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
        {hib, reply, handlers} =
          server_swap_handler(handler1, args1, handler2, args2, handlers, nil, name)

        reply(tag, reply)
        loop(parent, name, handlers, debug, hib)

      {_from, tag, {:swap_mon_handler, handler1, args1, handler2, args2, mon}} ->
        {hib, reply, handlers} =
          server_swap_handler(handler1, args1, handler2, args2, handlers, mon, name)

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
    tuples =
      for handler(module: mod, id: id, state: state) <- handlers do
        {mod, id, state}
      end

    {:ok, tuples}
  end

  @doc false
  def system_replace_state(fun, [name, handlers, hib]) do
    {handlers, states} =
      :lists.unzip(
        for handler <- handlers do
          handler(module: mod, id: id, state: state) = handler
          cur = {mod, id, state}

          try do
            new = {^mod, ^id, new_state} = fun.(cur)
            {handler(handler, state: new_state), new}
          catch
            _, _ ->
              {handler, cur}
          end
        end
      )

    {:ok, states, [name, handlers, hib]}
  end

  @doc false
  def format_status(opt, status_data) do
    [pdict, sys_state, parent, _debug, [name, handlers, _hib]] = status_data
    header = :gen.format_status_header('Status for event handler', name)

    formatted =
      for handler <- handlers do
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

    [
      header: header,
      data: [{'Status', sys_state}, {'Parent', parent}],
      items: {'Installed handlers', formatted}
    ]
  end

  ## Loop helpers

  defp print_event(dev, {:in, msg}, name) do
    case msg do
      {:notify, event} ->
        IO.puts(dev, "*DBG* #{inspect(name)} got event #{inspect(event)}")

      {_, _, {:call, handler, query}} ->
        IO.puts(
          dev,
          "*DBG* #{inspect(name)} (handler #{inspect(handler)}) got call #{inspect(query)}"
        )

      _ ->
        IO.puts(dev, "*DBG* #{inspect(name)} got #{inspect(msg)}")
    end
  end

  defp print_event(dev, dbg, name) do
    IO.puts(dev, "*DBG* #{inspect(name)}: #{inspect(dbg)}")
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
    handler = handler(module: GenEvent.Stream, id: {self(), ref}, pid: notify, ref: ref)
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
    send(pid, {self(), {self(), ref}, {mode_to_tag(mode), event}})
  end

  defp mode_to_tag(:ack), do: :ack_notify
  defp mode_to_tag(:sync), do: :sync_notify
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

  defp server_collect_process_handlers(mode, event, [handler | t], handlers, name)
       when mode in [:sync, :ack] do
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
    for(handler(module: module) <- handlers, do: module)
    |> :ordsets.from_list()
    |> :ordsets.to_list()
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
    send(from, {ref, msg})
  end

  defp handle_down(ref, reason, handlers, name) do
    case :lists.keyfind(ref, handler(:ref) + 1, handlers) do
      false ->
        :error

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
      :error, val -> {:error, {val, __STACKTRACE__}}
      :exit, val -> {:error, val}
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
      send(pid, {:gen_event_EXIT, handler(handler, :id), reason})
    end
  end

  defp report_error(_handler, :normal, _, _, _), do: :ok
  defp report_error(_handler, :shutdown, _, _, _), do: :ok
  defp report_error(_handler, {:swapped, _, _}, _, _, _), do: :ok

  defp report_error(handler, reason, state, last_in, name) do
    reason =
      case reason do
        {:undef, [{m, f, a, _} | _] = mfas} ->
          cond do
            :code.is_loaded(m) == false ->
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
        '** Last event was: ~p~n' ++ '** When handler state == ~p~n' ++ '** Reason == ~p~n',
      [handler(handler, :id), name, last_in, formatted, reason]
    )
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
