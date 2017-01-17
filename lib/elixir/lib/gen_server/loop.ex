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
    handle_stop(reason, mod, state)
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
    handle_sync(req, from, parent, dbg, mod, state)
  end
  defp handle({:"$gen_cast", req}, parent, dbg, mod, state, _) do
    handle_async(:handle_cast, req, parent, dbg, mod, state)
  end
  defp handle({:system, from, req}, parent, dbg, mod, state, await) do
    misc = {mod, state, await}
    :sys.handle_system_msg(req, from, parent, __MODULE__, dbg, misc)
  end
  defp handle(msg, parent, dbg, mod, state, _) do
    handle_async(:handle_info, msg, parent, dbg, mod, state)
  end

  defp handle_sync(req, from, parent, dbg, mod, state) do
    try do
      apply(mod, :handle_call, [req, from, state])
    catch
      kind, reason ->
        reason = exit_reason(kind, reason, System.stacktrace())
        handle_stop(reason, mod, state)
    else
      {:reply, resp, state} ->
        :gen.reply(from, resp)
        loop(parent, dbg, mod, state, :infinity)
      {:reply, resp, state, await} ->
        :gen.reply(from, resp)
        loop(parent, dbg, mod, state, await)
      {:stop, reason, resp, state} ->
        :gen.reply(from, resp)
        handle_stop(reason, mod, state)
      other ->
        handle_async(other, parent, dbg, mod, state)
    end
  end

  defp handle_async(fun, msg, parent, dbg, mod, state) do
    try do
      apply(mod, fun, [msg, state])
    catch
      kind, reason ->
        reason = exit_reason(kind, reason, System.stacktrace())
        handle_stop(reason, mod, state)
    else
      return ->
        handle_async(return, parent, dbg, mod, state)
    end
  end

  defp handle_async({:noreply, state}, parent, dbg, mod, _) do
    loop(parent, dbg, mod, state, :infinity)
  end
  defp handle_async({:noreply, state, await}, parent, dbg, mod, _) do
    loop(parent, dbg, mod, state, await)
  end
  defp handle_async({:stop, reason, state}, _, _, mod, _) do
    handle_stop(reason, mod, state)
  end
  defp handle_async(other, _, _, mod, state) do
    handle_stop({:bad_return_value, other}, mod, state)
  end

  defp handle_stop(reason, mod, state) do
    try do
      apply(mod, :terminate, [reason, state])
    catch
      kind, reason ->
        reason = exit_reason(kind, reason, System.stacktrace())
        exit(reason)
    else
      _ ->
        exit(reason)
    end
  end
end
