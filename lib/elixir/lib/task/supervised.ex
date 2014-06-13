defmodule Task.Supervised do
  @moduledoc false

  def start_link(info, fun) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :noreply, [info, fun])}
  end

  def start_link(caller, info, fun) do
    :proc_lib.start_link(__MODULE__, :reply, [caller, info, fun])
  end

  def async(caller, info, mfa) do
    ref = receive do: ({^caller, ref} -> ref)
    send caller, {ref, do_apply(info, mfa)}
  end

  def reply(caller, info, mfa) do
    :erlang.link(caller)
    :proc_lib.init_ack({:ok, self()})

    ref =
      # There is a race condition on this operation when working accross
      # node that manifests if a `Task.Supervisor.async/1` call is made
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
      # Given no work is done in the client in between the task start and
      # sending the reference, 5000 should be enough to not raise false
      # negatives unless the nodes are indeed not available.
      receive do
        {^caller, ref} -> ref
      after
        5000 -> exit(:timeout)
      end

    send caller, {ref, do_apply(info, mfa)}
  end

  def noreply(info, mfa) do
    do_apply(info, mfa)
  end

  defp do_apply(info, {module, fun, args} = mfa) do
    try do
      apply(module, fun, args)
    catch
      :error, value ->
        exit(info, mfa, {value, System.stacktrace()})
      :throw, value ->
        exit(info, mfa, {{:nocatch, value}, System.stacktrace()})
      :exit, value ->
        exit(info, mfa, value)
    end
  end

  defp exit(_info, _mfa, reason)
      when reason == :normal
      when reason == :shutdown
      when tuple_size(reason) == 2 and elem(reason, 0) == :shutdown do
    exit(reason)
  end

  defp exit(info, mfa, reason) do
    :error_logger.format(
      "** Task ~p terminating~n" <>
      "** Started from ~p~n" <>
      "** Running ~p~n" <>
      "** Reason for termination == ~n" <>
      "** ~p~n", [self, get_from(info), get_running(mfa), reason])

    exit(reason)
  end

  defp get_from({node, pid_or_name}) when node == node(), do: pid_or_name
  defp get_from(other), do: other

  defp get_running({:erlang, :apply, [fun, []]}), do: fun
  defp get_running(other), do: other
end
