defmodule Task.Supervised do
  @moduledoc false

  @ref_timeout 5_000

  def start(info, fun) do
    {:ok, :proc_lib.spawn(__MODULE__, :noreply, [info, fun])}
  end

  def start_link(info, fun) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :noreply, [info, fun])}
  end

  def start_link(caller, link, info, fun) do
    {:ok, spawn_link(caller, link, info, fun)}
  end

  def spawn_link(caller, link \\ :nolink, info, fun) do
    :proc_lib.spawn_link(__MODULE__, :reply, [caller, link, info, fun])
  end

  def reply(caller, link, info, mfa) do
    initial_call(mfa)
    case link do
      :link ->
        Process.link(caller)
        reply(caller, nil, @ref_timeout, info, mfa)
      :monitor ->
        mref = Process.monitor(caller)
        reply(caller, mref, @ref_timeout, info, mfa)
      :nolink ->
        reply(caller, nil, :infinity, info, mfa)
    end
  end

  defp reply(caller, mref, timeout, info, mfa) do
    receive do
      {^caller, ref} ->
        _ = if mref, do: Process.demonitor(mref, [:flush])
        send caller, {ref, do_apply(info, mfa)}
      {:DOWN, ^mref, _, _, reason} when is_reference(mref) ->
        exit(reason)
    after
      # There is a race condition on this operation when working across
      # node that manifests if a "Task.Supervisor.async/2" call is made
      # while the supervisor is busy spawning previous tasks.
      #
      # Imagine the following workflow:
      #
      # 1. The nodes disconnect
      # 2. The async call fails and is caught, the calling process does not exit
      # 3. The task is spawned and links to the calling process, causing the nodes to reconnect
      # 4. The calling process has not exited and so does not send its monitor reference
      # 5. The spawned task waits forever for the monitor reference so it can begin
      #
      # We have solved this by specifying a timeout of 5000 seconds.
      # Given no work is done in the client between the task start and
      # sending the reference, 5000 should be enough to not raise false
      # negatives unless the nodes are indeed not available.
      #
      # The same situation could occur with "Task.Supervisor.async_nolink/2",
      # except a monitor is used instead of a link.
      timeout ->
        exit(:timeout)
    end
  end

  def noreply(info, mfa) do
    initial_call(mfa)
    do_apply(info, mfa)
  end

  defp initial_call(mfa) do
    Process.put(:"$initial_call", get_initial_call(mfa))
  end

  defp get_initial_call({:erlang, :apply, [fun, []]}) when is_function(fun, 0) do
    {:module, module} = :erlang.fun_info(fun, :module)
    {:name, name} = :erlang.fun_info(fun, :name)
    {module, name, 0}
  end

  defp get_initial_call({mod, fun, args}) do
    {mod, fun, length(args)}
  end

  defp do_apply(info, {module, fun, args} = mfa) do
    try do
      apply(module, fun, args)
    catch
      :error, value ->
        reason = {value, System.stacktrace()}
        exit(info, mfa, reason, reason)
      :throw, value ->
        reason = {{:nocatch, value}, System.stacktrace()}
        exit(info, mfa, reason, reason)
      :exit, value ->
        exit(info, mfa, {value, System.stacktrace()}, value)
    end
  end

  defp exit(_info, _mfa, _log_reason, reason)
      when reason == :normal
      when reason == :shutdown
      when tuple_size(reason) == 2 and elem(reason, 0) == :shutdown do
    exit(reason)
  end

  defp exit(info, mfa, log_reason, reason) do
    {fun, args} = get_running(mfa)

    :error_logger.format(
      '** Task ~p terminating~n' ++
      '** Started from ~p~n' ++
      '** When function  == ~p~n' ++
      '**      arguments == ~p~n' ++
      '** Reason for termination == ~n' ++
      '** ~p~n', [self, get_from(info), fun, args, get_reason(log_reason)])

    exit(reason)
  end

  defp get_from({node, pid_or_name}) when node == node(), do: pid_or_name
  defp get_from(other), do: other

  defp get_running({:erlang, :apply, [fun, []]}) when is_function(fun, 0), do: {fun, []}
  defp get_running({mod, fun, args}), do: {:erlang.make_fun(mod, fun, length(args)), args}

  defp get_reason({:undef, [{mod, fun, args, _info} | _] = stacktrace} = reason)
  when is_atom(mod) and is_atom(fun) do
    cond do
      :code.is_loaded(mod) === false ->
        {:"module could not be loaded", stacktrace}
      is_list(args) and not function_exported?(mod, fun, length(args)) ->
        {:"function not exported", stacktrace}
      is_integer(args) and not function_exported?(mod, fun, args) ->
        {:"function not exported", stacktrace}
      true ->
        reason
    end
  end

  defp get_reason(reason) do
    reason
  end
end
