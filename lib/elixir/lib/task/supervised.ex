defmodule Task.Supervised do
  @moduledoc false

  def start_link(fun) do
    :proc_lib.start_link(__MODULE__, :noreply, [fun])
  end

  def start_link(caller, fun) do
    :proc_lib.start_link(__MODULE__, :reply, [caller, fun])
  end

  def async(caller, mfa) do
    ref = receive do: ({^caller, ref} -> ref)
    send caller, {ref, apply(mfa)}
  end

  def reply(caller, mfa) do
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

    send caller, {ref, apply(mfa)}
  end

  def noreply(mfa) do
    :proc_lib.init_ack({:ok, self()})
    apply(mfa)
  end

  def apply({module, fun, args}) do
    try do
      apply(module, fun, args)
    catch
      :error, reason ->
        exit({reason, System.stacktrace()})
      :throw, value ->
        exit({{:nocatch, value}, System.stacktrace()})
    end
  end
end
