defmodule Task do
  @moduledoc """
  Conveniences for spawning and awaiting for tasks.

  Tasks are processes meant to execute one particular
  action throughout their life-cycle, often with little or no
  communication with other processes. The most common use case
  for tasks is to compute a value asynchronously:

      task = Task.async(fn -> do_some_work() end)
      res  = do_some_other_work()
      res + Task.await(task)

  Tasks spawned with async can be awaited on by its caller
  process (and only its caller) as shown in the example above.
  They are implemented by spawning a process that sends a message
  to the caller once the given computation is performed.

  Besides `async/1` and `await/1`, tasks can also be used be
  started as part of supervision trees and dynamically spawned
  in remote nodes. We will explore all three scenarios next.

  ## async and await

  The most common way to spawn a task is with `Task.async/1`. A new
  process will be created and linked to the caller. Once the task
  action finishes, a message will be sent to the caller with its
  result.

  `Task.await/1` is used to read the message sent by the task. On
  await, Elixir will also setup a monitor to verify if the process
  exited with any abnormal reason (or in case exits are being
  trapped by the caller).

  ## Supervised tasks

  It is also possible to spawn a task inside a supervision tree
  with `start_link/1` and `start_link/3`:

      Task.start_link(fn -> IO.puts "ok" end)

  Such can be mounted in your supervision tree as:

      import Supervisor.Spec

      children = [
        worker(Task, [fn -> IO.puts "ok" end])
      ]

  Since such tasks are supervised and not directly linked to
  the caller, they cannot be awaited on. Note `start_link/1`,
  differently from `async/1`, returns `{:ok, pid}` (which is
  the result expected by supervision trees).

  ## Supervision trees

  The `Task.Supervisor` module allows developers to start supervisors
  that dynamically supervise tasks:

      {:ok, pid} = Task.Supervisor.start_link()
      Task.Supervisor.async(pid, fn -> do_work() end)

  `Task.Supervisor` also makes it possible to spawn tasks in remote nodes as
  long as the supervisor is registered locally or globally:

      # In the remote node
      Task.Supervisor.start_link(name: :tasks_sup)

      # In the client
      Task.Supervisor.async({:tasks_sup, :remote@local}, fn -> do_work() end)

  `Task.Supervisor` is more often started in your supervision tree as:

      import Supervisor.Spec

      children = [
        supervisor(Task.Supervisor, [[name: :tasks_sup]])
      ]

  Check `Task.Supervisor` for other operations supported by the Task supervisor.
  """

  @doc """
  The Task struct.

  It contains two fields:

  * `:pid` - the proces reference of the task process. It may be a pid
    or a tuple containing the process and node names;

  * `:ref` - the task monitor reference;

  """
  defstruct pid: nil, ref: nil

  @doc """
  Starts a task as part of a supervision tree.
  """
  @spec start_link(fun) :: {:ok, pid}
  def start_link(fun) do
    start_link(:erlang, :apply, [fun, []])
  end

  @doc """
  Starts a task as part of a supervision tree.
  """
  @spec start_link(module, atom, [term]) :: {:ok, pid}
  def start_link(mod, fun, args) do
    Task.Supervised.start_link({mod, fun, args})
  end

  @doc """
  Starts a task that can be awaited on.

  This function spawns a process that is linked and monitored
  to the caller process. A `Task` struct is returned containing
  the relevant information.

  ## Task's message format

  The reply sent by the task will be in the format `{ref, msg}`,
  where `ref` is the monitoring reference hold by the task.
  """
  @spec async(fun) :: t
  def async(fun) do
    async(:erlang, :apply, [fun, []])
  end

  @doc """
  Starts a task that can be awaited on.

  Similar to `async/1`, but the task is specified by the given
  module, function and arguments.
  """
  @spec async(module, atom, [term]) :: t
  def async(mod, fun, args) do
    mfa = {mod, fun, args}
    ref = make_ref
    pid = :proc_lib.spawn_link(Task.Supervised, :async, [mfa, self(), ref])
    %Task{pid: pid, ref: ref}
  end

  @doc """
  Awaits for a task reply.

  A timeout, in miliseconds, can be given with default value
  of `5000`. In case the task process dies, this function will
  exit with the same reason as the task.
  """
  @spec await(t, timeout) :: term | no_return
  def await(%Task{pid: pid, ref: ref}=task, timeout \\ 5000) do
    mon_ref = Process.monitor(pid)

    receive do
      {^ref, reply} ->
        Process.demonitor(mon_ref, [:flush])
        reply
      {:DOWN, ^mon_ref, _, _, :noconnection} ->
        exit({{:nodedown, get_node(task.pid)}, {__MODULE__, :await, [task, timeout]}})
      {:DOWN, ^mon_ref, _, _, reason} ->
        exit({reason, {__MODULE__, :await, [task, timeout]}})
    after
      timeout ->
        Process.demonitor(mon_ref, [:flush])
        exit({:timeout, {__MODULE__, :await, [task, timeout]}})
    end
  end

  defp get_node({_, n}) when is_atom(n), do: n
  defp get_node(pid) when is_pid(pid),   do: pid
end
