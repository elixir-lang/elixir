defmodule GenServer.Loop do
  @moduledoc false

  @typep gen_name() ::
    pid() | {:local, atom()} | {:global, term()} | {:via, module(), term}

  ## :gen callbacks

  @spec init_it(pid(), :self | pid(), gen_name(), module(), term(), Keyword.t()) ::
    no_return()
  def init_it(starter, parent, gen_name, mod, args, opts) do
    parent = gen_parent(parent)
    name = :gen.name(gen_name)
    dbg  = :gen.debug_options(name, opts)
    try do
      apply(mod, :init, [args])
    catch
      kind, reason ->
        reason = exit_reason(kind, reason, Systen.stacktrace())
        init_stop(starter, reason, gen_name)
    else
      {:ok, state} ->
        init_ok(starter, parent, dbg, mod, state, :infinity)
      {:ok, state, await} ->
        init_ok(starter, parent, dbg, mod, state, await)
      {:stop, reason} ->
        init_stop(starter, reason, gen_name)
      :ignore ->
        init_stop(starter, :normal, gen_name, :ignore)
      other ->
        init_stop(starter, {:bad_return_value, other}, gen_name)
    end
  end

  ## :proc_lib callbacks

  @spec continue(pid(), [:sys.dbg_opt], module(), term()) :: no_return()
  def continue(parent, dbg, mod, state) do
    # must be message in queue as woke up from hibernation
    receive do
      msg ->
        handle(msg, parent, dbg, mod, state, :hibernate)
    end
  end

  ## :sys callbacks

  def system_continue(parent, dbg, {mod, state, await}) do
    loop(parent, dbg, mod, state, await)
  end

  def system_terminate(reason, _, _, {mod, state, _}) do
    try do
      throw(:terminate)
    catch
      :throw, :terminate ->
        handle_exception(:exit, reason, System.stacktrace(), nil, mod, state)
    end
  end

  ## Helpers

  defp gen_parent(:self),  do: self()
  defp gen_parent(parent), do: parent

  defp init_stop(starter, reason, gen_name) do
    init_stop(starter, reason, gen_name, {:error, reason})
  end

  defp init_stop(starter, reason, gen_name, return) do
    # Unregister before init_ack/2 so that if supervisor restarts process the
    # name is not taken
    :gen.unregister_name(gen_name)
    :proc_lib.init_ack(starter, return)
    exit(reason)
  end

  defp exit_reason(:exit, reason, _),      do: reason
  defp exit_reason(:error, reason, stack), do: {reason, stack}
  defp exit_reason(:throw, value, stack),  do: {{:nocatch, value}, stack}

  defp init_ok(starter, parent, dbg, mod, state, await) do
    :proc_lib.init_ack(starter, {:ok, self()})
    loop(parent, dbg, mod, state, await)
  end

  defp loop(parent, dbg, mod, state, :hibernate) do
    args = [parent, dbg, mod, state]
    :proc_lib.hibernate(__MODULE__, :continue, args)
  end

  defp loop(parent, dbg, mod, state, timeout) do
    receive do
      msg ->
        handle(msg, parent, dbg, mod, state, timeout)
    after
      timeout ->
        handle(:timeout, parent, dbg, mod, state, timeout)
    end
  end

  defp handle({:"$gen_call", from, req}, parent, dbg, mod, state, _) do
    sync(req, from, parent, dbg, mod, state)
  end
  defp handle({:"$gen_cast", req}, parent, dbg, mod, state, _) do
    async(:handle_cast, req, parent, dbg, mod, state)
  end
  defp handle({:system, from, req}, parent, dbg, mod, state, await) do
    misc = {mod, state, await}
    :sys.handle_system_msg(req, from, parent, __MODULE__, dbg, misc)
  end
  defp handle({:EXIT, parent, reason}, parent, _, mod, state, _) do
    try do
      throw(:EXIT)
    catch
      :throw, :EXIT ->
        handle_exception(:exit, reason, System.stacktrace, nil, mod, state)
    end
  end
  defp handle(msg, parent, dbg, mod, state, _) do
    async(:handle_info, msg, parent, dbg, mod, state)
  end

  defp sync(req, from, parent, dbg, mod, state) do
    try do
      apply(mod, :handle_call, [req, from, state])
    catch
      kind, reason ->
        handle_exception(kind, reason, System.stacktrace(), req, mod, state)
    else
      {:reply, resp, state} ->
        :gen.reply(from, resp)
        loop(parent, dbg, mod, state, :infinity)
      {:reply, resp, state, await} ->
        :gen.reply(from, resp)
        loop(parent, dbg, mod, state, await)
      {:stop, reason, resp, state} ->
        :gen.reply(from, resp)
        handle_stop(reason, req, mod, state)
      other ->
        handle_async(other, req, parent, dbg, mod, state)
    end
  end

  defp async(fun, msg, parent, dbg, mod, state) do
    try do
      apply(mod, fun, [msg, state])
    catch
      kind, reason ->
        handle_exception(kind, reason, System.stacktrace(), msg, mod, state)
    else
      return ->
        handle_async(return, msg, parent, dbg, mod, state)
    end
  end

  defp handle_async({:noreply, state}, _, parent, dbg, mod, _) do
    loop(parent, dbg, mod, state, :infinity)
  end
  defp handle_async({:noreply, state, await}, _, parent, dbg, mod, _) do
    loop(parent, dbg, mod, state, await)
  end
  defp handle_async({:stop, reason, state}, msg, _, _, mod, _) do
    handle_stop(reason, msg, mod, state)
  end
  defp handle_async(other, msg, _, _, mod, state) do
    try do
      throw(:bad_return_value)
    catch
      :bad_return_value ->
        reason = {:bad_return_value, other}
        handle_exception(:exit, reason, System.stacktrace(), msg, mod, state)
    end
  end

  defp handle_exception(kind, reason, stack, msg, mod, state) do
    exit_reason = exit_reason(kind, reason, stack)
    log_reason = log_reason(kind, reason, stack)
    terminate(exit_reason, log_reason, msg, mod, state)
  end

  defp handle_stop(reason, msg, mod, state) do
    terminate(reason, reason, msg, mod, state)
  end

  defp log_reason(:exit, reason, stack),  do: {reason, stack}
  defp log_reason(:error, reason, stack), do: {reason, stack}
  defp log_reason(:throw, value, stack),  do: {{:nocatch, value}, stack}

  defp terminate(exit_reason, log_reason, msg, mod, state) do
    try do
      apply(mod, :terminate, [exit_reason, state])
    catch
      kind, reason ->
        stack = System.stacktrace()
        exit_reason = exit_reason(kind, reason, stack)
        log_exit(exit_reason, log_reason(kind, reason, stack), msg, state)
    else
      _ ->
        handle_exit(exit_reason, log_reason, msg, state)
    end
  end

  defp handle_exit(:normal, _, _, _),                   do: exit(:normal)
  defp handle_exit(:shutdown, _, _, _),                 do: exit(:shutdown)
  defp handle_exit(shutdown = {:shutdown, _}, _, _, _), do: exit(shutdown)
  defp handle_exit(exit_reason, log_reason, msg, state) do
    log_exit(exit_reason, log_reason, msg, state)
  end

  defp log_exit(exit_reason, log_reason, msg, state) do
    format = '** Generic server ~p terminating~n' ++
             '** Last message in was ~p~n' ++
             '** When Server state == ~p~n' ++
             '** Reason for termination == ~n** ~p~n'
    # TODO: log name
    # TODO: format status on state
    :error_logger.format(format, [self(), msg, state, log_reason])
    exit(exit_reason)
  end
end
