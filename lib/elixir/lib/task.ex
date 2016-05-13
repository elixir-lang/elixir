defmodule Task do
  @moduledoc """
  Conveniences for spawning and awaiting tasks.

  Tasks are processes meant to execute one particular
  action throughout their lifetime, often with little or no
  communication with other processes. The most common use case
  for tasks is to convert sequential code into concurrent code
  by computing a value asynchronously:

      task = Task.async(fn -> do_some_work() end)
      res  = do_some_other_work()
      res + Task.await(task)

  Tasks spawned with `async` can be awaited on by their caller
  process (and only their caller) as shown in the example above.
  They are implemented by spawning a process that sends a message
  to the caller once the given computation is performed.

  Besides `async/1` and `await/2`, tasks can also be
  started as part of a supervision tree and dynamically spawned
  on remote nodes. We will explore all three scenarios next.

  ## async and await

  One of the common uses of tasks is to convert sequential code
  into concurrent code with `Task.async/1` while keeping its semantics.
  When invoked, a new process will be created, linked and monitored
  by the caller. Once the task action finishes, a message will be sent
  to the caller with the result.

  `Task.await/2` is used to read the message sent by the task.

  There are two important things to consider when using `async`:

    1. If you are using async tasks, you must await a reply
       as they are *always* sent. If you are not expecting a reply,
       consider using `Task.start_link/1` detailed below.

    2. async tasks link the caller and the spawned process. This
       means that, if the caller crashes, the task will crash
       too and vice-versa. This is on purpose: if the process
       meant to receive the result no longer exists, there is
       no purpose in completing the computation.

       If this is not desired, use `Task.start/1` or consider starting
       the task under a `Task.Supervisor` using `async_nolink` or
       `start_child`.

  `Task.yield/2` is an alternative to `await/2` where the caller will
  temporarily block, waiting until the task replies or crashes. If the
  result does not arrive within the timeout, it can be called again at a
  later moment. This allows checking for the result of a task multiple
  times. If a reply does not arrive within the desired time,
  `Task.shutdown/2` can be used to stop the task.

  ## Supervised tasks

  It is also possible to spawn a task under a supervisor
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

  By default, most supervision strategies will try to restart
  a worker after it exits regardless of the reason. If you design the
  task to terminate normally (as in the example with `IO.puts/2` above),
  consider passing `restart: :transient` in the options to `worker/3`.

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

  Finally, check `Task.Supervisor` for other supported operations.

  ## Distributed tasks

  Since Elixir provides a Task supervisor, it is easy to use one
  to dynamically spawn tasks across nodes:

      # On the remote node
      Task.Supervisor.start_link(name: MyApp.DistSupervisor)

      # On the client
      Task.Supervisor.async({MyApp.DistSupervisor, :remote@local},
                            MyMod, :my_fun, [arg1, arg2, arg3])

  Note that, when working with distributed tasks, one should use the `async/4` function
  that expects explicit module, function and arguments, instead of `async/2` that
  works with anonymous functions. That's because anonymous functions expect
  the same module version to exist on all involved nodes. Check the `Agent` module
  documentation for more information on distributed processes as the limitations
  described there apply to the whole ecosystem.
  """

  @doc """
  The Task struct.

  It contains these fields:

    * `:pid` - the PID of the task process; `nil` if the task does
      not use a task process

    * `:ref` - the task monitor reference

    * `:owner` - the PID of the process that started the task

  """
  defstruct pid: nil, ref: nil, owner: nil

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
  (i.e. no interest in the returned result) and it should not
  be linked to the current process.
  """
  @spec start(fun) :: {:ok, pid}
  def start(fun) do
    start(:erlang, :apply, [fun, []])
  end

  @doc """
  Starts a task.

  This is only used when the task is used for side-effects
  (i.e. no interest in the returned result) and it should not
  be linked to the current process.
  """
  @spec start(module, atom, [term]) :: {:ok, pid}
  def start(mod, fun, args) do
    Task.Supervised.start(get_info(self), {mod, fun, args})
  end

  @doc """
  Starts a task that must be awaited on.

  This function spawns a process that is linked to and monitored
  by the caller process. A `Task` struct is returned containing
  the relevant information.

  Read the `Task` module documentation for more info on general
  usage of `async/1` and `async/3`.

  See also `async/3`.
  """
  @spec async(fun) :: t
  def async(fun) do
    async(:erlang, :apply, [fun, []])
  end

  @doc """
  Starts a task that must be awaited on.

  A `Task` struct is returned containing the relevant information.
  Developers must eventually call `Task.await/2` or `Task.yield/2`
  followed by `Task.shutdown/2` on the returned task.

  Read the `Task` module documentation for more info on general
  usage of `async/1` and `async/3`.

  ## Linking

  This function spawns a process that is linked to and monitored
  by the caller process. The linking part is important because it
  aborts the task if the parent process dies. It also guarantees
  the code before async/await has the same properties after you
  add the async call. For example, imagine you have this:

      x = heavy_fun()
      y = some_fun()
      x + y

  Now you want to make the `heavy_fun()` async:

      x = Task.async(&heavy_fun/0)
      y = some_fun()
      Task.await(x) + y

  As before, if `heavy_fun/0` fails, the whole computation will
  fail, including the parent process. If you don't want the task
  to fail then you must change the `heavy_fun/0` code in the
  same way you would achieve it if you didn't have the async call.
  For example, to either return `{:ok, val} | :error` results or,
  in more extreme cases, by using `try/rescue`. In other words,
  an asynchronous task should be thought of as an extension of a
  process rather than a mechanism to isolate it from all errors.

  If you don't want to link the caller to the task, then you
  must use a supervised task with `Task.Supervisor` and call
  `Task.Supervisor.async_nolink/2`.

  In any case, avoid any of the following:

    * Setting `:trap_exit` to `true` - trapping exits should be
      used only in special circumstances as it would make your
      process immune to not only exits from the task but from
      any other processes.

      Moreover, even when trapping exists, calling `await` will
      still exit if the task has terminated without sending its
      result back.

    * Unlinking the task process started with `async`/`await`.
      If you unlink the processes and the task does not belong
      to any supervisor, you may leave dangling tasks in case
      the parent dies.

  ## Message format

  The reply sent by the task will be in the format `{ref, result}`,
  where `ref` is the monitor reference held by the task struct
  and `result` is the return value of the task function.
  """
  @spec async(module, atom, [term]) :: t
  def async(mod, fun, args) do
    mfa = {mod, fun, args}
    owner = self()
    pid = Task.Supervised.spawn_link(owner, get_info(owner), mfa)
    ref = Process.monitor(pid)
    send(pid, {owner, ref})
    %Task{pid: pid, ref: ref, owner: owner}
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

  If the timeout is exceeded, `await` will exit; however,
  the task will continue to run. When the calling process exits, its
  exit signal will terminate the task if it is not trapping exits.

  This function assumes the task's monitor is still active or the monitor's
  `:DOWN` message is in the message queue. If it has been demonitored, or the
  message already received, this function will wait for the duration of the
  timeout awaiting the message.

  This function can only be called once for any given task. If you want
  to be able to check multiple times if a long-running task has finished
  its computation, use `yield/2` instead.

  ## Compatibility with OTP behaviours

  It is not recommended to `await` a long-running task inside an OTP
  behaviour such as `GenServer`. Instead, you should match on the message
  coming from a task inside your `handle_info` callback.
  """
  @spec await(t, timeout) :: term | no_return
  def await(task, timeout \\ 5000)

  def await(%Task{owner: owner} = task, _) when owner != self() do
    raise ArgumentError, invalid_owner_error(task)
  end

  def await(%Task{ref: ref} = task, timeout) do
    receive do
      {^ref, reply} ->
        Process.demonitor(ref, [:flush])
        reply
      {:DOWN, ^ref, _, proc, reason} ->
        exit({reason(reason, proc), {__MODULE__, :await, [task, timeout]}})
    after
      timeout ->
        Process.demonitor(ref, [:flush])
        exit({:timeout, {__MODULE__, :await, [task, timeout]}})
    end
  end

  @doc false
  def find(tasks, msg) do
    IO.warn "Task.find/2 is deprecated, please match on the message directly"
    do_find(tasks, msg)
  end

  defp do_find(tasks, {ref, reply}) when is_reference(ref) do
    Enum.find_value tasks, fn
      %Task{ref: ^ref} = task ->
        Process.demonitor(ref, [:flush])
        {reply, task}
      %Task{} ->
        nil
    end
  end

  defp do_find(tasks, {:DOWN, ref, _, proc, reason} = msg) when is_reference(ref) do
    find = fn %Task{ref: task_ref} -> task_ref == ref end
    if Enum.find(tasks, find) do
      exit({reason(reason, proc), {__MODULE__, :find, [tasks, msg]}})
    end
  end

  defp do_find(_tasks, _msg) do
    nil
  end

  @doc """
  Temporarily blocks the current process waiting for a task reply.

  Returns `{:ok, reply}` if the reply is received, `nil` if
  no reply has arrived, or `{:exit, reason}` if the task has already
  exited. Keep in mind that normally a task failure also causes
  the process owning the task to exit. Therefore this function can
  return `{:exit, reason}` only if

    * the task process exited with the reason `:normal`
    * it isn't linked to the caller
    * the caller is trapping exits

  A timeout, in milliseconds, can be given with default value
  of `5000`. If the time runs out before a message from
  the task is received, this function will return `nil`
  and the monitor will remain active. Therefore `yield/2` can be
  called multiple times on the same task.

  This function assumes the task's monitor is still active or the
  monitor's `:DOWN` message is in the message queue. If it has been
  demonitored or the message already received, this function will wait
  for the duration of the timeout awaiting the message.
  """
  @spec yield(t, timeout) :: {:ok, term} | {:exit, term} | nil
  def yield(task, timeout \\ 5_000)

  def yield(%Task{owner: owner} = task, _) when owner != self() do
    raise ArgumentError, invalid_owner_error(task)
  end

  def yield(%Task{ref: ref} = task, timeout) do
    receive do
      {^ref, reply} ->
        Process.demonitor(ref, [:flush])
        {:ok, reply}
      {:DOWN, ^ref, _, proc, :noconnection} ->
        exit({reason(:noconnection, proc), {__MODULE__, :yield, [task, timeout]}})
      {:DOWN, ^ref, _, _, reason} ->
        {:exit, reason}
    after
      timeout ->
        nil
    end
  end

  @doc """
  Yields to multiple tasks in the given time interval.

  This function receives a list of tasks and waits for their
  replies in the given time interval. It returns a list
  of tuples of two elements, with the task as the first element
  and the yielded result as the second.

  Similarly to `yield/2`, each task's result will be

    * `{:ok, term}` if the task has successfully reported its
      result back in the given time interval
    * `{:exit, reason}` if the task has died
    * `nil` if the task keeps running past the timeout

  Check `yield/2` for more information.

  ## Example

  `Task.yield_many/2` allows developers to spawn multiple tasks
  and retrieve the results received in a given timeframe.
  If we combine it with `Task.shutdown/2`, it allows us to gather
  those results and cancel the tasks that have not replied in time.

  Let's see an example.

      tasks =
        for i <- 1..10 do
          Task.async(fn ->
            :timer.sleep(i * 1000)
            i
          end)
        end

      tasks_with_results = Task.yield_many(tasks, 5000)

      results = Enum.map(tasks_with_results, fn {task, res} ->
        # Shutdown the tasks that did not reply nor exit
        res || Task.shutdown(task, :brutal_kill)
      end)

      # Here we are matching only on {:ok, value} and
      # ignoring {:exit, _} (crashed tasks) and `nil` (no replies)
      for {:ok, value} <- results do
        IO.inspect value
      end

  In the example above, we create tasks that sleep from 1
  up to 10 seconds and return the amount of seconds they slept.
  If you execute the code all at once, you should see 1 up to 5
  printed, as those were the tasks that have replied in the
  given time. All other tasks will have been shut down using
  the `Task.shutdown/2` call.
  """
  @spec yield_many([t], timeout) :: [{t, {:ok, term} | {:exit, term} | nil}]
  def yield_many(tasks, timeout \\ 5000) do
    timeout_ref = make_ref()
    timer_ref = Process.send_after(self(), timeout_ref, timeout)
    try do
      yield_many(tasks, timeout_ref, :infinity)
    catch
      {:noconnection, reason} ->
        exit({reason, {__MODULE__, :yield_many, [tasks, timeout]}})
    after
      Process.cancel_timer(timer_ref)
      receive do: (^timeout_ref -> :ok), after: (0 -> :ok)
    end
  end

  defp yield_many([%Task{ref: ref, owner: owner}=task | rest], timeout_ref, timeout) do
    if owner != self() do
      raise ArgumentError, invalid_owner_error(task)
    end

    receive do
      {^ref, reply} ->
        Process.demonitor(ref, [:flush])
        [{task, {:ok, reply}} | yield_many(rest, timeout_ref, timeout)]

      {:DOWN, ^ref, _, proc, :noconnection} ->
        throw({:noconnection, reason(:noconnection, proc)})

      {:DOWN, ^ref, _, _, reason} ->
        [{task, {:exit, reason}} | yield_many(rest, timeout_ref, timeout)]

      ^timeout_ref ->
        [{task, nil} | yield_many(rest, timeout_ref, 0)]

    after
      timeout ->
        [{task, nil} | yield_many(rest, timeout_ref, 0)]

    end
  end

  defp yield_many([], _timeout_ref, _timeout) do
    []
  end

  @doc """
  Unlinks and shuts down the task, and then checks for a reply.

  Returns `{:ok, reply}` if the reply is received while shutting down the task,
  `{:exit, reason}` if the task died, otherwise `nil`.

  The shutdown method is either a timeout or `:brutal_kill`. In case
  of a `timeout`, a `:shutdown` exit signal is sent to the task process
  and if it does not exit within the timeout, it is killed. With `:brutal_kill`
  the task is killed straight away. In case the task terminates abnormally
  (possibly killed by another process), this function will exit with the same reason.

  It is not required to call this function when terminating the caller, unless
  exiting with reason `:normal` or if the task is trapping exits. If the caller is
  exiting with a reason other than `:normal` and the task is not trapping exits, the
  caller's exit signal will stop the task. The caller can exit with reason
  `:shutdown` to shutdown all of its linked processes, including tasks, that
  are not trapping exits without generating any log messages.

  This function assumes the task's monitor is still active or the monitor's
  `:DOWN` message is in the message queue. If it has been demonitored, or the
  message already received, this function will block forever awaiting the message.
  """
  @spec shutdown(t, timeout | :brutal_kill) :: {:ok, term} | {:exit, term} | nil
  def shutdown(task, shutdown \\ 5_000)

  def shutdown(%Task{pid: nil} = task, _) do
    raise ArgumentError, "task #{inspect task} does not have an associated task process"
  end

  def shutdown(%Task{owner: owner} = task, _) when owner != self() do
    raise ArgumentError, invalid_owner_error(task)
  end

  def shutdown(%Task{pid: pid} = task, :brutal_kill) do
    exit(pid, :kill)

    case shutdown_receive(task, :brutal_kill, :infinity) do
      {:down, proc, :noconnection} ->
        exit({reason(:noconnection, proc), {__MODULE__, :shutdown, [task, :brutal_kill]}})
      {:down, _, reason} ->
        {:exit, reason}
      result ->
        result
    end
  end

  def shutdown(%Task{pid: pid} = task, timeout) do
    exit(pid, :shutdown)
    case shutdown_receive(task, :shutdown, timeout) do
      {:down, proc, :noconnection} ->
        exit({reason(:noconnection, proc), {__MODULE__, :shutdown, [task, timeout]}})
      {:down, _, reason} ->
        {:exit, reason}
      result ->
        result
    end
  end

  ## Helpers

  defp reason(:noconnection, proc), do: {:nodedown, monitor_node(proc)}
  defp reason(reason, _),           do: reason

  defp monitor_node(pid) when is_pid(pid), do: node(pid)
  defp monitor_node({_, node}),            do: node

  # spawn a process to ensure task gets exit signal if process dies from exit signal
  # between unlink and exit.
  defp exit(task, reason) do
    caller = self()
    ref = make_ref()
    enforcer = spawn(fn() -> enforce_exit(task, reason, caller, ref) end)
    Process.unlink(task)
    Process.exit(task, reason)
    send(enforcer, {:done, ref})
    :ok
  end

  defp enforce_exit(pid, reason, caller, ref) do
    mon = Process.monitor(caller)
    receive do
      {:done, ^ref}          -> :ok
      {:DOWN, ^mon, _, _, _} -> Process.exit(pid, reason)
    end
  end

  defp shutdown_receive(%{ref: ref} = task, type, timeout) do
    receive do
      {:DOWN, ^ref, _, _, :shutdown} when type in [:shutdown, :timeout_kill] ->
        flush_reply(ref)
      {:DOWN, ^ref, _, _, :killed} when type == :brutal_kill ->
        flush_reply(ref)
      {:DOWN, ^ref, _, proc, reason} ->
        flush_reply(ref) || {:down, proc, reason}
    after
      timeout ->
        Process.exit(task.pid, :kill)
        shutdown_receive(task, :timeout_kill, :infinity)
    end
  end

  defp flush_reply(ref) do
    receive do
      {^ref, reply} -> {:ok, reply}
    after
      0 -> nil
    end
  end

  defp invalid_owner_error(task) do
    "task #{inspect task} must be queried from the owner but was queried from #{inspect self()}"
  end
end
