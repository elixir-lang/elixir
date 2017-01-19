defmodule GenServer.Loop do
  @moduledoc false

  @typep gen_name() ::
    pid() | {:local, atom()} | {:global, term()} | {:via, module(), term}

  ## :gen callbacks

  @spec init_it(pid(), :self | pid(), gen_name(), module(), term(), Keyword.t()) ::
    no_return()
  def init_it(starter, gen_parent, gen_name, mod, args, opts) do
    parent = server_parent(gen_parent)
    name = server_name(gen_name)
    dbg = server_debug(name, opts)
    try do
      apply(mod, :init, [args])
    catch
      kind, reason ->
        reason = exit_reason(kind, reason, Systen.stacktrace())
        init_stop(starter, reason, gen_name)
    else
      {:ok, state} ->
        init_ok(starter, parent, dbg, name, mod, state, :infinity)
      {:ok, state, await} ->
        init_ok(starter, parent, dbg, name, mod, state, await)
      {:stop, reason} ->
        init_stop(starter, reason, gen_name)
      :ignore ->
        init_stop(starter, :normal, gen_name, :ignore)
      other ->
        init_stop(starter, {:bad_return_value, other}, gen_name)
    end
  end

  ## :proc_lib callbacks

  @spec continue(pid(), [:sys.dbg_opt], term(), module(), term()) :: no_return()
  def continue(parent, dbg, name, mod, state) do
    # must be message in queue as woke up from hibernation
    receive do
      msg ->
        handle(msg, parent, dbg, name, mod, state, :hibernate)
    end
  end

  ## :sys callbacks

  def system_continue(parent, dbg, {name, mod, state, await}) do
    loop(parent, dbg, name, mod, state, await)
  end

  def system_get_state({_, _, state, _}) do
    {:ok, state}
  end

  def system_replace_state(replace, {name, mod, state, await}) do
    state = replace.(state)
    {:ok, state, {name, mod, state, await}}
  end

  def system_code_change({name, mod, state, await}, _, oldvsn, extra) do
    try do
      apply(mod, :code_change, [oldvsn, state, extra])
    catch
      :throw, value ->
        :erlang.raise(:error, {:nocatch, value}, System.stacktrace())
    else
      {:ok, state} ->
        {:ok, {name, mod, state, await}}
    end
  end

  # TODO: add format_status/2 for :sys.get_status

  def system_terminate(reason, _, dbg, {name, mod, state, _}) do
    try do
      throw(:terminate)
    catch
      :throw, :terminate ->
        stack = System.stacktrace()
        handle_exception(:exit, reason, stack, nil, dbg, name, mod, state)
    end
  end

  ## Helpers

  defp server_parent(:self),  do: self()
  defp server_parent(parent), do: parent

  defp server_name(name) when is_pid(name), do: name
  defp server_name({:local, name}),         do: name
  defp server_name({:global, name}),        do: name
  defp server_name({:via, _, name}),        do: name

  defp server_debug(name, opts) do
    dbg_opts = Keyword.get(opts, :debug, [])
    try do
      :sys.debug_options(opts)
    catch
      _, _ ->
        format = '** Generic server ~p started with invalid debug options: ~p~n'
        :error_logger.format(format, [name, dbg_opts])
        []
    end
  end

  defp debug([], _, _, _, _), do: []
  defp debug(dbg, event, name, mod, state) do
    :sys.handle_debug(dbg, &debug_inspect/3, {name, mod, state}, event)
  end

  defp debug_inspect(device, event, {name, mod, state}) do
    msg = ["** (DEBUG from ", inspect(name), ?), ?\s, inspect(mod),
           " with state ", inspect(state) | debug_format(event)]
    IO.puts(device, msg)
  end

  defp debug_format({:in, {:call, req}, {from, _}}) do
    [" received call (from ", inspect(from), ?:, ?\s | inspect(req)]
  end
  defp debug_format({:in, {type, req}}) do
    [" received ", Atom.to_string(type), ?:, ?\s | inspect(req)]
  end
  defp debug_format({:out, {:reply, resp}, {to, _}}) do
    [" replied (to ", inspect(to), ?:, ?\s | inspect(resp)]
  end
  defp debug_format(:noreply) do
    " did not reply"
  end
  defp debug_format(:timeout) do
    " timed out"
  end
  defp debug_format(:enter_loop) do
    " entered loop"
  end
  defp debug_format({:stop, reason}) do
    [" stopped: " | Exception.format_exit(reason)]
  end

  defp init_stop(starter, reason, gen_name) do
    init_stop(starter, reason, gen_name, {:error, reason})
  end

  defp init_stop(starter, reason, gen_name, return) do
    # Unregister before init_ack/2 so that if supervisor restarts process the
    # name is not taken
    _ = gen_unregister(gen_name)
    :proc_lib.init_ack(starter, return)
    exit(reason)
  end

  defp gen_unregister(name) when is_pid(name), do: :ok
  defp gen_unregister({:local, name}),         do: Process.unregister(name)
  defp gen_unregister({:global, name}),        do: :gobal.unregister_name(name)
  defp gen_unregister({:via, mod, name}),      do: mod.unregister_name(name)

  defp exit_reason(:exit, reason, _),      do: reason
  defp exit_reason(:error, reason, stack), do: {reason, stack}
  defp exit_reason(:throw, value, stack),  do: {{:nocatch, value}, stack}

  defp init_ok(starter, parent, dbg, name, mod, state, await) do
    :proc_lib.init_ack(starter, {:ok, self()})
    dbg = debug(dbg, :enter_loop, name, mod, state)
    loop(parent, dbg, name, mod, state, await)
  end

  defp loop(parent, dbg, name, mod, state, :hibernate) do
    args = [parent, dbg, name, mod, state]
    :proc_lib.hibernate(__MODULE__, :continue, args)
  end

  defp loop(parent, dbg, name, mod, state, timeout) do
    receive do
      msg ->
        handle(msg, parent, dbg, name, mod, state, timeout)
    after
      timeout ->
        dbg = debug(dbg, :timeout, name, mod, state)
        async(:handle_info, :timeout, parent, dbg, name, mod, state)
    end
  end

  defp handle({:"$gen_call", from, req}, parent, dbg, name, mod, state, _) do
    dbg = debug(dbg, {:in, {:call, req}, from}, name, mod, state)
    sync(req, from, parent, dbg, name, mod, state)
  end
  defp handle({:"$gen_cast", req}, parent, dbg, name, mod, state, _) do
    dbg = debug(dbg, {:in, {:cast, req}}, name, mod, state)
    async(:handle_cast, req, parent, dbg, name, mod, state)
  end
  defp handle({:system, from, req}, parent, dbg, name, mod, state, await) do
    misc = {name, mod, state, await}
    :sys.handle_system_msg(req, from, parent, __MODULE__, dbg, misc)
  end
  defp handle({:EXIT, parent, reason}, parent, dbg, name, mod, state, _) do
    try do
      throw(:EXIT)
    catch
      :throw, :EXIT ->
        stack = System.stacktrace()
        handle_exception(:exit, reason, stack, nil, dbg, name, mod, state)
    end
  end
  defp handle(msg, parent, dbg, name, mod, state, _) do
    dbg = debug(dbg, {:in, {:info, msg}}, name, mod, state)
    async(:handle_info, msg, parent, dbg, name, mod, state)
  end

  defp sync(req, from, parent, dbg, name, mod, state) do
    try do
      apply(mod, :handle_call, [req, from, state])
    catch
      kind, reason ->
        stack = System.stacktrace()
        handle_exception(kind, reason, stack, req, dbg, name, mod, state)
    else
      {:reply, resp, state} ->
        :gen.reply(from, resp)
        dbg = debug(dbg, {:out, {:reply, resp}, from}, name, mod, state)
        loop(parent, dbg, name, mod, state, :infinity)
      {:reply, resp, state, await} ->
        :gen.reply(from, resp)
        dbg = debug(dbg, {:out, {:reply, resp}, from}, name, mod, state)
        loop(parent, dbg, name, mod, state, await)
      {:stop, reason, resp, state} ->
        :gen.reply(from, resp)
        dbg = debug(dbg, {:out, {:reply, resp}, from}, name, mod, state)
        handle_stop(reason, req, dbg, name, mod, state)
      other ->
        handle_async(other, req, parent, dbg, name, mod, state)
    end
  end

  defp async(fun, msg, parent, dbg, name, mod, state) do
    try do
      apply(mod, fun, [msg, state])
    catch
      kind, reason ->
        stack = System.stacktrace()
        handle_exception(kind, reason, stack, msg, dbg, name, mod, state)
    else
      return ->
        handle_async(return, msg, parent, dbg, name, mod, state)
    end
  end

  defp handle_async({:noreply, state}, _, parent, dbg, name, mod, _) do
    dbg = debug(dbg, :noreply, name, mod, state)
    loop(parent, dbg, name, mod, state, :infinity)
  end
  defp handle_async({:noreply, state, await}, _, parent, dbg, name, mod, _) do
    dbg = debug(dbg, :noreply, name, mod, state)
    loop(parent, dbg, name, mod, state, await)
  end
  defp handle_async({:stop, reason, state}, msg, _, dbg, name, mod, _) do
    handle_stop(reason, msg, dbg, name, mod, state)
  end
  defp handle_async(other, msg, _, dbg, name, mod, state) do
    try do
      throw(:bad_return_value)
    catch
      :bad_return_value ->
        reason = {:bad_return_value, other}
        stack = System.stacktrace()
        handle_exception(:exit, reason, stack, msg, dbg, name, mod, state)
    end
  end

  defp handle_exception(kind, reason, stack, msg, dbg, name, mod, state) do
    exit_reason = exit_reason(kind, reason, stack)
    log_reason = log_reason(kind, reason, stack)
    terminate(exit_reason, log_reason, msg, dbg, name, mod, state)
  end

  defp handle_stop(reason, msg, dbg, name, mod, state) do
    terminate(reason, reason, msg, dbg, name, mod, state)
  end

  defp log_reason(:exit, reason, stack),  do: {reason, stack}
  defp log_reason(:error, reason, stack), do: {reason, stack}
  defp log_reason(:throw, value, stack),  do: {{:nocatch, value}, stack}

  defp terminate(exit_reason, log_reason, msg, dbg, name, mod, state) do
    dbg = debug(dbg, {:stop, exit_reason}, name, mod, state)
    try do
      apply(mod, :terminate, [exit_reason, state])
    catch
      kind, reason ->
        stack = System.stacktrace()
        exit_reason = exit_reason(kind, reason, stack)
        log_reason = log_reason(kind, reason, stack)
        log_exit(exit_reason, log_reason, msg, dbg, name, state)
    else
      _ ->
        handle_exit(exit_reason, log_reason, msg, dbg, name, state)
    end
  end

  defp handle_exit(exit_reason, log_reason, msg, dbg, name, state) do
    case exit_reason do
      :normal        -> exit(:normal)
      :shutdown      -> exit(:shutdown)
      {:shutdown, _} -> exit(exit_reason)
      _              -> log_exit(exit_reason, log_reason, msg, dbg, name, state)
    end
  end

  defp log_exit(exit_reason, log_reason, msg, dbg, name, state) do
    format = '** Generic server ~p terminating~n' ++
             '** Last message in was ~p~n' ++
             '** When Server state == ~p~n' ++
             '** Reason for termination == ~n** ~p~n'
    # TODO: format status on state
    :error_logger.format(format, [name, msg, state, log_reason])
    :sys.print_log(dbg)
    exit(exit_reason)
  end
end
