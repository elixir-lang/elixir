defmodule Task do
  @moduledoc """
  Conveniences for spawning and awaiting for tasks.

  Tasks are processes that meant to execute one particular
  action throughout their life-cycle, often with little
  explicit communication with other processes. The most common
  use case for tasks is to compute a value asynchronously:

      task = Task.async(fn -> do_some_work() end)
      res  = do_some_other_work()
      res + Task.await(task)

  Tasks spawned with async can be awaited on by its caller
  process (and only its caller) as shown in the example above.
  They are implemented by spawning a process that sends a message
  to the caller once the given computation is performed.

  Besides `async/1` and `await/1`, tasks can also be used as part
  of supervision trees and dynamically spawned in remote nodes.
  We will explore all three scenarios next.

  ## async and await

  The most common way to spawn a task is with `Task.async/1`. A new
  process will be created and this process is linked and monitored
  by the caller. However, the processes are unlinked right before
  the task finishes, allowing the proper error to be triggered only
  on `await/1`.

  This implies three things:

  1) In case the caller crashes, the task will be killed and its
     computation will abort;

  2) In case the task crashes due to an error, the parent will
     crash only on `await/1`;

  3) In case the task crashes because a linked process caused
     it to crash, the parent will crash immediately;

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
  the caller, they cannot be awaited on. For such reason,
  differently from `async/1`, `start_link/1` returns `{:ok, pid}`
  (which is the result expected by supervision trees).

  Such tasks are useful as workers that run during your application
  life-cycle and rarely communicate with other workers. For example,
  a worker that pushes data to another server or a worker that consumes
  events from an event manager and writes it to a log file.

  ## Supervision trees

  The `Task.Supervisor` module allows developers to start supervisors
  that dynamically supervise tasks:

      {:ok, pid} = Task.Supervisor.start_link()
      Task.Supervisor.async(pid, fn -> do_work() end)

  `Task.Supervisor` also makes it possible to spawn tasks in remote nodes as
  long as the supervisor is registered locally or globally:

      # In the remote node
      Task.Supervisor.start_link(name: :tasks_sup)

      # On the client
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
    Task.Supervised.start_link(:undefined, {mod, fun, args})
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
    pid = :proc_lib.spawn_link(Task.Supervised, :async, [self(), mfa])
    ref = Process.monitor(pid)
    send(pid, {self(), ref})
    %Task{pid: pid, ref: ref}
  end

  @doc """
  Awaits for a task reply.

  A timeout, in miliseconds, can be given with default value
  of `5000`. In case the task process dies, this function will
  exit with the same reason as the task.
  """
  @spec await(t, timeout) :: term | no_return
  def await(%Task{ref: ref}=task, timeout \\ 5000) do
    receive do
      {^ref, reply} ->
        Process.demonitor(ref, [:flush])
        reply
      {:DOWN, ^ref, _, _, :noconnection} ->
        mfa = {__MODULE__, :await, [task, timeout]}
        exit({{:nodedown, get_node(task.pid)}, mfa})
      {:DOWN, ^ref, _, _, reason} ->
        exit({reason, {__MODULE__, :await, [task, timeout]}})
    after
      timeout ->
        Process.demonitor(ref, [:flush])
        exit({:timeout, {__MODULE__, :await, [task, timeout]}})
    end
  end

  @doc """
  Receives a group of tasks and a message and finds
  a task that matches the given message.

  This function returns a tuple with the task and the
  returned value in case the message matches a task that
  exited with success, it raises in case the found task
  failed or nil if no task was found.

  This function is useful in situations where multiple
  tasks are spawned and their results are collected just
  later on. For example, a GenServer can spawn tasks,
  store the tasks in a list and later use `Task.find/2`
  to see if upcoming messages are from any of the tasks.
  """
  @spec find([t], any) :: {term, t} | nil | no_return
  def find(tasks, msg)

  def find(tasks, {ref, reply}) when is_reference(ref) do
    Enum.find_value tasks, fn
      %Task{ref: task_ref} = t when ref == task_ref ->
        Process.demonitor(ref, [:flush])
        {reply, t}
      %Task{} ->
        nil
    end
  end

  def find(tasks, {:DOWN, ref, _, _, reason} = msg) when is_reference(ref) do
    find = fn(%Task{ref: task_ref}) -> task_ref == ref end
    case Enum.find(tasks, find) do
      %Task{pid: pid} when reason == :noconnection ->
        exit({{:nodedown, get_node(pid)}, {__MODULE__, :find, [tasks, msg]}})
      %Task{} ->
        exit({reason, {__MODULE__, :find, [tasks, msg]}})
      nil ->
        nil
    end
  end

  def find(_tasks, _msg) do
    nil
  end

  defp get_node({_, n}) when is_atom(n), do: n
  defp get_node(pid) when is_pid(pid),     do: pid
end
