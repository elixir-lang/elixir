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

  Tasks spawned with `async` can be awaited on by its caller
  process (and only its caller) as shown in the example above.
  They are implemented by spawning a process that sends a message
  to the caller once the given computation is performed.

  Besides `async/1` and `await/2`, tasks can also be
  started as part of supervision trees and dynamically spawned
  in remote nodes. We will explore all three scenarios next.

  ## async and await

  The most common way to spawn a task is with `Task.async/1`. A new
  process will be created, linked and monitored by the caller. Once
  the task action finishes, a message will be sent to the caller
  with the result.

  `Task.await/2` is used to read the message sent by the task. On
  `await`, Elixir will also setup a monitor to verify if the process
  exited for any abnormal reason (or in case exits are being
  trapped by the caller).

  ## Supervised tasks

  It is also possible to spawn a task inside a supervision tree
  with `start_link/1` and `start_link/3`:

      Task.start_link(fn -> IO.puts "ok" end)

  Such tasks can be mounted in your supervision tree as:

      import Supervisor.Spec

      children = [
        worker(Task, [fn -> IO.puts "ok" end])
      ]

  Since these tasks are supervised and not directly linked to
  the caller, they cannot be awaited on. Note `start_link/1`,
  unlike `async/1`, returns `{:ok, pid}` (which is
  the result expected by supervision trees).

  ## Dynamically supervised tasks

  The `Task.Supervisor` module allows developers to dynamically
  create multiple supervised tasks.

  A short example is:

      {:ok, pid} = Task.Supervisor.start_link()
      task = Task.Supervisor.async(pid, fn ->
        # Do something
      end)
      Task.await(task)

  However, in the majority of cases, you want to add the task supervisor
  to your supervision tree:

      import Supervisor.Spec

      children = [
        supervisor(Task.Supervisor, [[name: MyApp.TaskSupervisor]])
      ]

  Now you can dynamically start supervised tasks:

      Task.Supervisor.start_child(MyApp.TaskSupervisor, fn ->
        # Do something
      end)

  Or even use the async/await pattern:

      Task.Supervisor.async(MyApp.TaskSupervisor, fn ->
        # Do something
      end) |> Task.await()

  Finally, check `Task.Supervisor` for other operations supported by the
  Task supervisor.

  ## Distributed tasks

  Since Elixir provides a Task supervisor, it is easy to use a task
  supervisor to dynamically spawn tasks across nodes:

      # In the remote node
      Task.Supervisor.start_link(name: MyApp.DistSupervisor)

      # In the client
      Task.Supervisor.async({:MyApp.DistSupervisor, :remote@local},
                            MyMod, :my_fun, [arg1, arg2, arg3])

  Note that, when working with distributed tasks, one should use the `async/4` function
  that expects explicit module, function and arguments, instead of `async/2` that
  works with anonymous functions. That's because anonymous functions expect
  the same module version to exist on all involved nodes. Check the `Agent` module
  documentation for more information on distributed processes as the limitations
  described in the agents documentation apply to the whole ecosystem.
  """

  @doc """
  The Task struct.

  It contains two fields:

    * `:pid` - the process reference of the task process; it may be a pid
      or a tuple containing the process and node names

    * `:ref` - the task monitor reference

  """
  defstruct pid: nil, ref: nil

  @type t :: %__MODULE__{}

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
    Task.Supervised.start_link(get_info(self), {mod, fun, args})
  end

  @doc """
  Starts a task.

  This is only used when the task is used for side-effects
  (i.e. no interest in its return result) and it should not
  be linked to the current process.
  """
  @spec start(fun) :: {:ok, pid}
  def start(fun) do
    start(:erlang, :apply, [fun, []])
  end

  @doc """
  Starts a task.

  This is only used when the task is used for side-effects
  (i.e. no interest in its return result) and it should not
  be linked to the current process.
  """
  @spec start(module, atom, [term]) :: {:ok, pid}
  def start(mod, fun, args) do
    Task.Supervised.start(get_info(self), {mod, fun, args})
  end

  @doc """
  Starts a task that can be awaited on.

  This function spawns a process that is linked to and monitored
  by the caller process. A `Task` struct is returned containing
  the relevant information.

  ## Task's message format

  The reply sent by the task will be in the format `{ref, msg}`,
  where `ref` is the monitoring reference held by the task.
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
    pid = :proc_lib.spawn_link(Task.Supervised, :async, [self, get_info(self), mfa])
    ref = Process.monitor(pid)
    send(pid, {self(), ref})
    %Task{pid: pid, ref: ref}
  end

  defp get_info(self) do
    {node(),
     case Process.info(self, :registered_name) do
       {:registered_name, []} -> self()
       {:registered_name, name} -> name
     end}
  end

  @doc """
  Awaits a task reply.

  A timeout, in milliseconds, can be given with default value
  of `5000`. In case the task process dies, this function will
  exit with the same reason as the task.

  If the timeout is exceeded, `await` will exit, however,
  the task will continue to run.  Use `Process.kill/2` to
  terminate it.
  """
  @spec await(t, timeout) :: term | no_return
  def await(%Task{ref: ref}=task, timeout \\ 5000) do
    receive do
      {^ref, reply} ->
        Process.demonitor(ref, [:flush])
        reply
      {:DOWN, ^ref, _, _, :noconnection} ->
        mfa = {__MODULE__, :await, [task, timeout]}
        exit({{:nodedown, node(task.pid)}, mfa})
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
  failed or `nil` if no task was found.

  This function is useful in situations where multiple
  tasks are spawned and their results are collected
  later on. For example, a `GenServer` can spawn tasks,
  store the tasks in a list and later use `Task.find/2`
  to see if incoming messages are from any of the tasks.
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
        exit({{:nodedown, node(pid)}, {__MODULE__, :find, [tasks, msg]}})
      %Task{} ->
        exit({reason, {__MODULE__, :find, [tasks, msg]}})
      nil ->
        nil
    end
  end

  def find(_tasks, _msg) do
    nil
  end
end
