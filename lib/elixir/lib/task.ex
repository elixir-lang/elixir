defmodule Task do
  @moduledoc """
  Conveniences for spawning and awaiting tasks.

  Tasks are processes meant to execute one particular
  action throughout their lifetime, often with little or no
  communication with other processes. The most common use case
  for tasks is to convert sequential code into concurrent code
  by computing a value asynchronously:

      task = Task.async(fn -> do_some_work() end)
      res = do_some_other_work()
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

    1. If you are using async tasks, you **must await** a reply
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

  It is also possible to spawn a task under a supervisor. The `Task`
  module implements the `child_spec/1` function, which allows it to
  be started directly under a supervisor by passing a tuple with
  a function to run:

      Supervisor.start_link([
        {Task, fn -> :some_work end}
      ], strategy: :one_for_one)

  However, if you want to invoke a specific module, function and
  arguments, or give the task process a name, you need to define
  the task in its own module:

      defmodule MyTask do
        use Task

        def start_link(arg) do
          Task.start_link(__MODULE__, :run, [arg])
        end

        def run(arg) do
          # ...
        end
      end

  And then passing it to the supervisor:

      Supervisor.start_link([
        {MyTask, arg}
      ], strategy: :one_for_one)

  Since these tasks are supervised and not directly linked to
  the caller, they cannot be awaited on. `start_link/1`, unlike
  `async/1`, returns `{:ok, pid}` (which is the result expected
  by supervisors).

  `use Task` defines a `child_spec/1` function, allowing the
  defined module to be put under a supervision tree. The generated
  `child_spec/1` can be customized with the following options:

    * `:id` - the child specification identifier, defaults to the current module
    * `:start` - how to start the child process (defaults to calling `__MODULE__.start_link/1`)
    * `:restart` - when the child should be restarted, defaults to `:temporary`
    * `:shutdown` - how to shut down the child, either immediately or by giving it time to shut down

  Opposite to `GenServer`, `Agent` and `Supervisor`, a Task has
  a default `:restart` of `:temporary`. This means the task will
  not be restarted even if it crashes. If you desire the task to
  be restarted for non-successful exits, do:

      use Task, restart: :transient

  If you want the task to always be restarted:

      use Task, restart: :permanent

  See the "Child specification" section in the `Supervisor` module
  for more detailed information. The `@doc` annotation immediately
  preceding `use Task` will be attached to the generated `child_spec/1`
  function.

  ## Dynamically supervised tasks

  The `Task.Supervisor` module allows developers to dynamically
  create multiple supervised tasks.

  A short example is:

      {:ok, pid} = Task.Supervisor.start_link()

      task =
        Task.Supervisor.async(pid, fn ->
          # Do something
        end)

      Task.await(task)

  However, in the majority of cases, you want to add the task supervisor
  to your supervision tree:

      Supervisor.start_link([
        {Task.Supervisor, name: MyApp.TaskSupervisor}
      ], strategy: :one_for_one)

  Now you can dynamically start supervised tasks:

      Task.Supervisor.start_child(MyApp.TaskSupervisor, fn ->
        # Do something
      end)

  Or even use the async/await pattern:

      Task.Supervisor.async(MyApp.TaskSupervisor, fn ->
        # Do something
      end)
      |> Task.await()

  Finally, check `Task.Supervisor` for other supported operations.

  ## Distributed tasks

  Since Elixir provides a `Task.Supervisor`, it is easy to use one
  to dynamically start tasks across nodes:

      # On the remote node
      Task.Supervisor.start_link(name: MyApp.DistSupervisor)

      # On the client
      supervisor = {MyApp.DistSupervisor, :remote@local}
      Task.Supervisor.async(supervisor, MyMod, :my_fun, [arg1, arg2, arg3])

  Note that, when working with distributed tasks, one should use the `Task.Supervisor.async/4` function
  that expects explicit module, function and arguments, instead of `Task.Supervisor.async/2` that
  works with anonymous functions. That's because anonymous functions expect
  the same module version to exist on all involved nodes. Check the `Agent` module
  documentation for more information on distributed processes as the limitations
  described there apply to the whole ecosystem.

  ## Ancestor and Caller Tracking

  Whenever you start a new process, Elixir annotates the parent of that process
  through the `$ancestors` key in the process dictionary. This is often used to
  track the hierarchy inside a supervision tree.

  For example, we recommend developers to always start tasks under a supervisor.
  This provides more visibility and allows you to control how those tasks are
  terminated when a node shuts down. That might look something like
  `Task.Supervisor.start_child(MySupervisor, task_specification)`. This means
  that, although your code is the one who invokes the task, the actual ancestor of
  the task is the supervisor, as the supervisor is the one effectively starting it.

  To track the relationship between your code and the task, we use the `$callers`
  key in the process dictionary. Therefore, assuming the `Task.Supervisor` call
  above, we have:

      [your code] -- calls --> [supervisor] ---- spawns --> [task]

  Which means we store the following relationships:

      [your code]              [supervisor] <-- ancestor -- [task]
          ^                                                  |
          |--------------------- caller ---------------------|

  The list of callers of the current process can be retrieved from the Process
  dictionary with `Process.get(:"$callers")`. This will return either `nil` or
  a list `[pid_n, ..., pid2, pid1]` with at least one entry Where `pid_n` is
  the PID that called the current process, `pid2` called `pid_n`, and `pid2` was
  called by `pid1`.
  """

  @doc """
  The Task struct.

  It contains these fields:

    * `:pid` - the PID of the task process; `nil` if the task does
      not use a task process

    * `:ref` - the task monitor reference

    * `:owner` - the PID of the process that started the task

  """
  @enforce_keys [:pid, :ref, :owner]
  defstruct pid: nil, ref: nil, owner: nil

  @typedoc """
  The Task type.

  See `%Task{}` for information about each field of the structure.
  """
  @type t :: %__MODULE__{
          pid: pid() | nil,
          ref: reference() | nil,
          owner: pid() | nil
        }

  defguardp is_timeout(timeout)
            when timeout == :infinity or (is_integer(timeout) and timeout >= 0)

  @doc """
  Returns a specification to start a task under a supervisor.

  `arg` is passed as the argument to `Task.start_link/1` in the `:start` field
  of the spec.

  For more information, see the `Supervisor` module,
  the `Supervisor.child_spec/2` function and the `t:Supervisor.child_spec/0` type.
  """
  @doc since: "1.5.0"
  @spec child_spec(term) :: Supervisor.child_spec()
  def child_spec(arg) do
    %{
      id: Task,
      start: {Task, :start_link, [arg]},
      restart: :temporary
    }
  end

  @doc false
  defmacro __using__(opts) do
    quote location: :keep, bind_quoted: [opts: opts] do
      if Module.get_attribute(__MODULE__, :doc) == nil do
        @doc """
        Returns a specification to start this module under a supervisor.

        `arg` is passed as the argument to `Task.start_link/1` in the `:start` field
        of the spec.

        For more information, see the `Supervisor` module,
        the `Supervisor.child_spec/2` function and the `t:Supervisor.child_spec/0` type.
        """
      end

      def child_spec(arg) do
        default = %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [arg]},
          restart: :temporary
        }

        Supervisor.child_spec(default, unquote(Macro.escape(opts)))
      end

      defoverridable child_spec: 1
    end
  end

  @doc """
  Starts a process linked to the current process.

  `fun` must be a zero-arity anonymous function.

  This is often used to start the process as part of a supervision tree.
  """
  @spec start_link((() -> any)) :: {:ok, pid}
  def start_link(fun) when is_function(fun, 0) do
    start_link(:erlang, :apply, [fun, []])
  end

  @doc """
  Starts a task as part of a supervision tree.
  """
  @spec start_link(module, atom, [term]) :: {:ok, pid}
  def start_link(module, function_name, args)
      when is_atom(module) and is_atom(function_name) and is_list(args) do
    mfa = {module, function_name, args}
    Task.Supervised.start_link(get_owner(self()), get_callers(self()), mfa)
  end

  @doc """
  Starts a task.

  `fun` must be a zero-arity anonymous function.

  This is only used when the task is used for side-effects
  (i.e. no interest in the returned result) and it should not
  be linked to the current process.
  """
  @spec start((() -> any)) :: {:ok, pid}
  def start(fun) when is_function(fun, 0) do
    start(:erlang, :apply, [fun, []])
  end

  @doc """
  Starts a task.

  This is only used when the task is used for side-effects
  (i.e. no interest in the returned result) and it should not
  be linked to the current process.
  """
  @spec start(module, atom, [term]) :: {:ok, pid}
  def start(module, function_name, args)
      when is_atom(module) and is_atom(function_name) and is_list(args) do
    mfa = {module, function_name, args}
    Task.Supervised.start(get_owner(self()), get_callers(self()), mfa)
  end

  @doc """
  Starts a task that must be awaited on.

  `fun` must be a zero-arity anonymous function.
  This function spawns a process that is linked to and monitored
  by the caller process. A `Task` struct is returned containing
  the relevant information.

  Read the `Task` module documentation for more information about the
  general usage of `async/1` and `async/3`.

  See also `async/3`.
  """
  @spec async((() -> any)) :: t
  def async(fun) when is_function(fun, 0) do
    async(:erlang, :apply, [fun, []])
  end

  @doc """
  Starts a task that must be awaited on.

  A `Task` struct is returned containing the relevant information.
  Developers must eventually call `Task.await/2` or `Task.yield/2`
  followed by `Task.shutdown/2` on the returned task.

  Read the `Task` module documentation for more information about
  the general usage of `async/1` and `async/3`.

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

      Moreover, even when trapping exits, calling `await` will
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
  def async(module, function_name, args)
      when is_atom(module) and is_atom(function_name) and is_list(args) do
    mfa = {module, function_name, args}
    owner = self()
    {:ok, pid} = Task.Supervised.start_link(get_owner(owner), get_callers(owner), :nomonitor, mfa)
    ref = Process.monitor(pid)
    send(pid, {owner, ref})
    %Task{pid: pid, ref: ref, owner: owner}
  end

  @doc """
  Returns a stream where the given function (`module` and `function_name`)
  is mapped concurrently on each item in `enumerable`.

  Each item of `enumerable` will be prepended to the given `args` and
  processed by its own task. The tasks will be linked to an intermediate
  process that is then linked to the current process. This means a failure
  in a task terminates the current process and a failure in the current process
  terminates all tasks.

  When streamed, each task will emit `{:ok, value}` upon successful
  completion or `{:exit, reason}` if the caller is trapping exits.
  The order of results depends on the value of the `:ordered` option.

  The level of concurrency and the time tasks are allowed to run can
  be controlled via options (see the "Options" section below).

  Consider using `Task.Supervisor.async_stream/6` to start tasks
  under a supervisor. If you find yourself trapping exits to handle exits
  inside the async stream, consider using `Task.Supervisor.async_stream_nolink/6`
  to start tasks that are not linked to the calling process.

  ## Options

    * `:max_concurrency` - sets the maximum number of tasks to run
      at the same time. Defaults to `System.schedulers_online/0`.

    * `:ordered` - whether the results should be returned in the same order
      as the input stream. This option is useful when you have large
      streams and don't want to buffer results before they are delivered.
      This is also useful when you're using the tasks for side effects.
      Defaults to `true`.

    * `:timeout` - the maximum amount of time (in milliseconds) each
      task is allowed to execute for. Defaults to `5000`.

    * `:on_timeout` - what to do when a task times out. The possible
      values are:
      * `:exit` (default) - the process that spawned the tasks exits.
      * `:kill_task` - the task that timed out is killed. The value
        emitted for that task is `{:exit, :timeout}`.

  ## Example

  Let's build a stream and then enumerate it:

      stream = Task.async_stream(collection, Mod, :expensive_fun, [])
      Enum.to_list(stream)

  The concurrency can be increased or decreased using the `:max_concurrency`
  option. For example, if the tasks are IO heavy, the value can be increased:

      max_concurrency = System.schedulers_online() * 2
      stream = Task.async_stream(collection, Mod, :expensive_fun, [], max_concurrency: max_concurrency)
      Enum.to_list(stream)

  If you do not care about the results of the computation, you can run
  the stream with `Stream.run/1`. Also set `ordered: false`, as you don't
  care about the order of the results either:

      stream = Task.async_stream(collection, Mod, :expensive_fun, [], ordered: false)
      Stream.run(stream)

  """
  @doc since: "1.4.0"
  @spec async_stream(Enumerable.t(), module, atom, [term], keyword) :: Enumerable.t()
  def async_stream(enumerable, module, function_name, args, options \\ [])
      when is_atom(module) and is_atom(function_name) and is_list(args) do
    build_stream(enumerable, {module, function_name, args}, options)
  end

  @doc """
  Returns a stream that runs the given function `fun` concurrently
  on each item in `enumerable`.

  Works the same as `async_stream/5` but with an anonymous function instead of a
  module-function-arguments tuple. `fun` must be a one-arity anonymous function.

  Each `enumerable` item is passed as argument to the given function `fun` and
  processed by its own task. The tasks will be linked to the current process,
  similarly to `async/1`.

  ## Example

  Count the code points in each string asynchronously, then add the counts together using reduce.

      iex> strings = ["long string", "longer string", "there are many of these"]
      iex> stream = Task.async_stream(strings, fn text -> text |> String.codepoints() |> Enum.count() end)
      iex> Enum.reduce(stream, 0, fn {:ok, num}, acc -> num + acc end)
      47

  See `async_stream/5` for discussion, options, and more examples.
  """
  @doc since: "1.4.0"
  @spec async_stream(Enumerable.t(), (term -> term), keyword) :: Enumerable.t()
  def async_stream(enumerable, fun, options \\ [])
      when is_function(fun, 1) and is_list(options) do
    build_stream(enumerable, fun, options)
  end

  defp build_stream(enumerable, fun, options) do
    &Task.Supervised.stream(enumerable, &1, &2, fun, options, fn [owner | _] = callers, mfa ->
      {:ok, pid} = Task.Supervised.start_link(get_owner(owner), callers, :nomonitor, mfa)
      {:ok, :link, pid}
    end)
  end

  # Returns a tuple with the node where this is executed and either the
  # registered name of the given PID or the PID of where this is executed. Used
  # when exiting from tasks to print out from where the task was started.
  defp get_owner(pid) do
    self_or_name =
      case Process.info(pid, :registered_name) do
        {:registered_name, name} when is_atom(name) -> name
        _ -> pid
      end

    {node(), self_or_name, pid}
  end

  defp get_callers(owner) do
    case :erlang.get(:"$callers") do
      [_ | _] = list -> [owner | list]
      _ -> [owner]
    end
  end

  @doc """
  Awaits a task reply and returns it.

  In case the task process dies, the current process will exit with the same
  reason as the task.

  A timeout in milliseconds or `:infinity`, can be given with a default value of `5000`. If the
  timeout is exceeded, then the current process will exit. If the task process
  is linked to the current process which is the case when a task is started with
  `async`, then the task process will also exit. If the task process is trapping
  exits or not linked to the current process, then it will continue to run.

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
  coming from a task inside your `c:GenServer.handle_info/2` callback. For
  more information on the format of the message, see the documentation for
  `async/1`.

  ## Examples

      iex> task = Task.async(fn -> 1 + 1 end)
      iex> Task.await(task)
      2

  """
  @spec await(t, timeout) :: term
  def await(%Task{ref: ref, owner: owner} = task, timeout \\ 5000) when is_timeout(timeout) do
    if owner != self() do
      raise ArgumentError, invalid_owner_error(task)
    end

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
  @deprecated "Pattern match directly on the message instead"
  def find(tasks, {ref, reply}) when is_reference(ref) do
    Enum.find_value(tasks, fn
      %Task{ref: ^ref} = task ->
        Process.demonitor(ref, [:flush])
        {reply, task}

      %Task{} ->
        nil
    end)
  end

  def find(tasks, {:DOWN, ref, _, proc, reason} = msg) when is_reference(ref) do
    find = fn %Task{ref: task_ref} -> task_ref == ref end

    if Enum.find(tasks, find) do
      exit({reason(reason, proc), {__MODULE__, :find, [tasks, msg]}})
    end
  end

  def find(_tasks, _msg) do
    nil
  end

  @doc ~S"""
  Temporarily blocks the current process waiting for a task reply.

  Returns `{:ok, reply}` if the reply is received, `nil` if
  no reply has arrived, or `{:exit, reason}` if the task has already
  exited. Keep in mind that normally a task failure also causes
  the process owning the task to exit. Therefore this function can
  return `{:exit, reason}` only if

    * the task process exited with the reason `:normal`
    * it isn't linked to the caller
    * the caller is trapping exits

  A timeout, in milliseconds or `:infinity`, can be given with a default value
  of `5000`. If the time runs out before a message from
  the task is received, this function will return `nil`
  and the monitor will remain active. Therefore `yield/2` can be
  called multiple times on the same task.

  This function assumes the task's monitor is still active or the
  monitor's `:DOWN` message is in the message queue. If it has been
  demonitored or the message already received, this function will wait
  for the duration of the timeout awaiting the message.

  If you intend to shut the task down if it has not responded within `timeout`
  milliseconds, you should chain this together with `shutdown/1`, like so:

      case Task.yield(task, timeout) || Task.shutdown(task) do
        {:ok, result} ->
          result

        nil ->
          Logger.warn("Failed to get a result in #{timeout}ms")
          nil
      end

  That ensures that if the task completes after the `timeout` but before `shutdown/1`
  has been called, you will still get the result, since `shutdown/1` is designed to
  handle this case and return the result.
  """
  @spec yield(t, timeout) :: {:ok, term} | {:exit, term} | nil
  def yield(%Task{ref: ref, owner: owner} = task, timeout \\ 5000) when is_timeout(timeout) do
    if owner != self() do
      raise ArgumentError, invalid_owner_error(task)
    end

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
  of two-element tuples, with the task as the first element
  and the yielded result as the second. The tasks in the returned
  list will be in the same order as the tasks supplied in the `tasks`
  input argument.

  Similarly to `yield/2`, each task's result will be

    * `{:ok, term}` if the task has successfully reported its
      result back in the given time interval
    * `{:exit, reason}` if the task has died
    * `nil` if the task keeps running past the timeout

  A timeout, in milliseconds or `:infinity`, can be given with a default value
  of `5000`.

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
            Process.sleep(i * 1000)
            i
          end)
        end

      tasks_with_results = Task.yield_many(tasks, 5000)

      results =
        Enum.map(tasks_with_results, fn {task, res} ->
          # Shut down the tasks that did not reply nor exit
          res || Task.shutdown(task, :brutal_kill)
        end)

      # Here we are matching only on {:ok, value} and
      # ignoring {:exit, _} (crashed tasks) and `nil` (no replies)
      for {:ok, value} <- results do
        IO.inspect(value)
      end

  In the example above, we create tasks that sleep from 1
  up to 10 seconds and return the number of seconds they slept for.
  If you execute the code all at once, you should see 1 up to 5
  printed, as those were the tasks that have replied in the
  given time. All other tasks will have been shut down using
  the `Task.shutdown/2` call.
  """
  @spec yield_many([t], timeout) :: [{t, {:ok, term} | {:exit, term} | nil}]
  def yield_many(tasks, timeout \\ 5000) when is_timeout(timeout) do
    timeout_ref = make_ref()

    timer_ref =
      if timeout != :infinity do
        Process.send_after(self(), timeout_ref, timeout)
      end

    try do
      yield_many(tasks, timeout_ref, :infinity)
    catch
      {:noconnection, reason} ->
        exit({reason, {__MODULE__, :yield_many, [tasks, timeout]}})
    after
      timer_ref && Process.cancel_timer(timer_ref)
      receive do: (^timeout_ref -> :ok), after: (0 -> :ok)
    end
  end

  defp yield_many([%Task{ref: ref, owner: owner} = task | rest], timeout_ref, timeout) do
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

  The second argument is either a timeout or `:brutal_kill`. In case
  of a timeout, a `:shutdown` exit signal is sent to the task process
  and if it does not exit within the timeout, it is killed. With `:brutal_kill`
  the task is killed straight away. In case the task terminates abnormally
  (possibly killed by another process), this function will exit with the same reason.

  It is not required to call this function when terminating the caller, unless
  exiting with reason `:normal` or if the task is trapping exits. If the caller is
  exiting with a reason other than `:normal` and the task is not trapping exits, the
  caller's exit signal will stop the task. The caller can exit with reason
  `:shutdown` to shut down all of its linked processes, including tasks, that
  are not trapping exits without generating any log messages.

  If a task's monitor has already been demonitored or received and there is not
  a response waiting in the message queue this function will return
  `{:exit, :noproc}` as the result or exit reason can not be determined.
  """
  @spec shutdown(t, timeout | :brutal_kill) :: {:ok, term} | {:exit, term} | nil
  def shutdown(task, shutdown \\ 5000)

  def shutdown(%Task{pid: nil} = task, _) do
    raise ArgumentError, "task #{inspect(task)} does not have an associated task process"
  end

  def shutdown(%Task{owner: owner} = task, _) when owner != self() do
    raise ArgumentError, invalid_owner_error(task)
  end

  def shutdown(%Task{pid: pid} = task, :brutal_kill) do
    mon = Process.monitor(pid)
    exit(pid, :kill)

    case shutdown_receive(task, mon, :brutal_kill, :infinity) do
      {:down, proc, :noconnection} ->
        exit({reason(:noconnection, proc), {__MODULE__, :shutdown, [task, :brutal_kill]}})

      {:down, _, reason} ->
        {:exit, reason}

      result ->
        result
    end
  end

  def shutdown(%Task{pid: pid} = task, timeout) when is_timeout(timeout) do
    mon = Process.monitor(pid)
    exit(pid, :shutdown)

    case shutdown_receive(task, mon, :shutdown, timeout) do
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
  defp reason(reason, _), do: reason

  defp monitor_node(pid) when is_pid(pid), do: node(pid)
  defp monitor_node({_, node}), do: node

  # spawn a process to ensure task gets exit signal if process dies from exit signal
  # between unlink and exit.
  defp exit(task, reason) do
    caller = self()
    ref = make_ref()
    enforcer = spawn(fn -> enforce_exit(task, reason, caller, ref) end)
    Process.unlink(task)
    Process.exit(task, reason)
    send(enforcer, {:done, ref})
    :ok
  end

  defp enforce_exit(pid, reason, caller, ref) do
    mon = Process.monitor(caller)

    receive do
      {:done, ^ref} -> :ok
      {:DOWN, ^mon, _, _, _} -> Process.exit(pid, reason)
    end
  end

  defp shutdown_receive(%{ref: ref} = task, mon, type, timeout) do
    receive do
      {:DOWN, ^mon, _, _, :shutdown} when type in [:shutdown, :timeout_kill] ->
        Process.demonitor(ref, [:flush])
        flush_reply(ref)

      {:DOWN, ^mon, _, _, :killed} when type == :brutal_kill ->
        Process.demonitor(ref, [:flush])
        flush_reply(ref)

      {:DOWN, ^mon, _, proc, :noproc} ->
        reason = flush_noproc(ref, proc, type)
        flush_reply(ref) || reason

      {:DOWN, ^mon, _, proc, reason} ->
        Process.demonitor(ref, [:flush])
        flush_reply(ref) || {:down, proc, reason}
    after
      timeout ->
        Process.exit(task.pid, :kill)
        shutdown_receive(task, mon, :timeout_kill, :infinity)
    end
  end

  defp flush_reply(ref) do
    receive do
      {^ref, reply} -> {:ok, reply}
    after
      0 -> nil
    end
  end

  defp flush_noproc(ref, proc, type) do
    receive do
      {:DOWN, ^ref, _, _, :shutdown} when type in [:shutdown, :timeout_kill] ->
        nil

      {:DOWN, ^ref, _, _, :killed} when type == :brutal_kill ->
        nil

      {:DOWN, ^ref, _, _, reason} ->
        {:down, proc, reason}
    after
      0 ->
        Process.demonitor(ref, [:flush])
        {:down, proc, :noproc}
    end
  end

  defp invalid_owner_error(task) do
    "task #{inspect(task)} must be queried from the owner but was queried from #{inspect(self())}"
  end
end
