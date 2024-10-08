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

  Compared to plain processes, started with `spawn/1`, tasks
  include monitoring metadata and logging in case of errors.

  Besides `async/1` and `await/2`, tasks can also be
  started as part of a supervision tree and dynamically spawned
  on remote nodes. We will explore these scenarios next.

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
       consider using `Task.start_link/1` as detailed below.

    2. Async tasks link the caller and the spawned process. This
       means that, if the caller crashes, the task will crash
       too and vice-versa. This is on purpose: if the process
       meant to receive the result no longer exists, there is
       no purpose in completing the computation. If this is not
       desired, you will want to use supervised tasks, described
       in a subsequent section.

  ## Tasks are processes

  Tasks are processes and so data will need to be completely copied
  to them. Take the following code as an example:

      large_data = fetch_large_data()
      task = Task.async(fn -> do_some_work(large_data) end)
      res = do_some_other_work()
      res + Task.await(task)

  The code above copies over all of `large_data`, which can be
  resource intensive depending on the size of the data.
  There are two ways to address this.

  First, if you need to access only part of `large_data`,
  consider extracting it before the task:

      large_data = fetch_large_data()
      subset_data = large_data.some_field
      task = Task.async(fn -> do_some_work(subset_data) end)

  Alternatively, if you can move the data loading altogether
  to the task, it may be even better:

      task = Task.async(fn ->
        large_data = fetch_large_data()
        do_some_work(large_data)
      end)

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

  And now you can use async/await by passing the name of
  the supervisor instead of the pid:

      Task.Supervisor.async(MyApp.TaskSupervisor, fn ->
        # Do something
      end)
      |> Task.await()

  We encourage developers to rely on supervised tasks as much as possible.
  Supervised tasks improve the visibility of how many tasks are running
  at a given moment and enable a variety of patterns that give you
  explicit control on how to handle the results, errors, and timeouts.
  Here is a summary:

    * Using `Task.Supervisor.start_child/2` allows you to start a fire-and-forget
      task when you don't care about its results or if it completes successfully or not.

    * Using `Task.Supervisor.async/2` + `Task.await/2` allows you to execute
      tasks concurrently and retrieve its result. If the task fails,
      the caller will also fail.

    * Using `Task.Supervisor.async_nolink/2` + `Task.yield/2` + `Task.shutdown/2`
      allows you to execute tasks concurrently and retrieve their results
      or the reason they failed within a given time frame. If the task fails,
      the caller won't fail. You will receive the error reason either on
      `yield` or `shutdown`.

  Furthermore, the supervisor guarantees all tasks terminate within a
  configurable shutdown period when your application shuts down. See the
  `Task.Supervisor` module for details on the supported operations.

  ### Distributed tasks

  With `Task.Supervisor`, it is easy to dynamically start tasks across nodes:

      # First on the remote node named :remote@local
      Task.Supervisor.start_link(name: MyApp.DistSupervisor)

      # Then on the local client node
      supervisor = {MyApp.DistSupervisor, :remote@local}
      Task.Supervisor.async(supervisor, MyMod, :my_fun, [arg1, arg2, arg3])

  Note that, as above, when working with distributed tasks, one should use the
  `Task.Supervisor.async/5` function that expects explicit module, function,
  and arguments, instead of `Task.Supervisor.async/3` that works with anonymous
  functions. That's because anonymous functions expect the same module version
  to exist on all involved nodes. Check the `Agent` module documentation for
  more information on distributed processes as the limitations described there
  apply to the whole ecosystem.

  ## Statically supervised tasks

  The `Task` module implements the `child_spec/1` function, which
  allows it to be started directly under a regular `Supervisor` -
  instead of a `Task.Supervisor` - by passing a tuple with a function
  to run:

      Supervisor.start_link([
        {Task, fn -> :some_work end}
      ], strategy: :one_for_one)

  This is often useful when you need to execute some steps while
  setting up your supervision tree. For example: to warm up caches,
  log the initialization status, and such.

  If you don't want to put the Task code directly under the `Supervisor`,
  you can wrap the `Task` in its own module, similar to how you would
  do with a `GenServer` or an `Agent`:

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

  Since these tasks are supervised and not directly linked to the caller,
  they cannot be awaited on. By default, the functions `Task.start/1`
  and `Task.start_link/1` are for fire-and-forget tasks, where you don't
  care about the results or if it completes successfully or not.

  > #### `use Task` {: .info}
  >
  > When you `use Task`, the `Task` module will define a
  > `child_spec/1` function, so your module can be used
  > as a child in a supervision tree.

  `use Task` defines a `child_spec/1` function, allowing the
  defined module to be put under a supervision tree. The generated
  `child_spec/1` can be customized with the following options:

    * `:id` - the child specification identifier, defaults to the current module
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

  ## Ancestor and Caller Tracking

  Whenever you start a new process, Elixir annotates the parent of that process
  through the `$ancestors` key in the process dictionary. This is often used to
  track the hierarchy inside a supervision tree.

  For example, we recommend developers to always start tasks under a supervisor.
  This provides more visibility and allows you to control how those tasks are
  terminated when a node shuts down. That might look something like
  `Task.Supervisor.start_child(MySupervisor, task_function)`. This means
  that, although your code is the one invoking the task, the actual ancestor of
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
  a list `[pid_n, ..., pid2, pid1]` with at least one entry where `pid_n` is
  the PID that called the current process, `pid2` called `pid_n`, and `pid2` was
  called by `pid1`.

  If a task crashes, the callers field is included as part of the log message
  metadata under the `:callers` key.
  """

  @doc """
  The Task struct.

  It contains these fields:

    * `:mfa` - a three-element tuple containing the module, function name,
      and arity invoked to start the task in `async/1` and `async/3`

    * `:owner` - the PID of the process that started the task

    * `:pid` - the PID of the task process; `nil` if there is no process
      specifically assigned for the task

    * `:ref` - an opaque term used as the task monitor reference

  """
  @enforce_keys [:mfa, :owner, :pid, :ref]
  defstruct @enforce_keys

  @typedoc """
  The Task type.

  See [`%Task{}`](`__struct__/0`) for information about each field of the structure.
  """
  @type t :: %__MODULE__{
          mfa: mfa(),
          owner: pid(),
          pid: pid() | nil,
          ref: ref()
        }

  @typedoc """
  The task opaque reference.
  """
  @opaque ref :: reference()

  @typedoc """
  Options given to `async_stream` functions.
  """
  @typedoc since: "1.17.0"
  @type async_stream_option ::
          {:max_concurrency, pos_integer()}
          | {:ordered, boolean()}
          | {:timeout, timeout()}
          | {:on_timeout, :exit | :kill_task}
          | {:zip_input_on_exit, boolean()}

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
      if not Module.has_attribute?(__MODULE__, :doc) do
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
  Starts a task as part of a supervision tree with the given `fun`.

  `fun` must be a zero-arity anonymous function.

  This is used to start a statically supervised task under a supervision tree.
  """
  @spec start_link((-> any)) :: {:ok, pid}
  def start_link(fun) when is_function(fun, 0) do
    start_link(:erlang, :apply, [fun, []])
  end

  @doc """
  Starts a task as part of a supervision tree with the given
  `module`, `function`, and `args`.

  This is used to start a statically supervised task under a supervision tree.
  """
  @spec start_link(module, atom, [term]) :: {:ok, pid}
  def start_link(module, function, args)
      when is_atom(module) and is_atom(function) and is_list(args) do
    mfa = {module, function, args}
    Task.Supervised.start_link(get_owner(self()), get_callers(self()), mfa)
  end

  @doc """
  Starts a task.

  `fun` must be a zero-arity anonymous function.

  This should only used when the task is used for side-effects
  (like I/O) and you have no interest on its results nor if it
  completes successfully.

  If the current node is shutdown, the node will terminate even
  if the task was not completed. For this reason, we recommend
  to use `Task.Supervisor.start_child/2` instead, which allows
  you to control the shutdown time via the `:shutdown` option.
  """
  @spec start((-> any)) :: {:ok, pid}
  def start(fun) when is_function(fun, 0) do
    start(:erlang, :apply, [fun, []])
  end

  @doc """
  Starts a task.

  This should only used when the task is used for side-effects
  (like I/O) and you have no interest on its results nor if it
  completes successfully.

  If the current node is shutdown, the node will terminate even
  if the task was not completed. For this reason, we recommend
  to use `Task.Supervisor.start_child/2` instead, which allows
  you to control the shutdown time via the `:shutdown` option.
  """
  @spec start(module, atom, [term]) :: {:ok, pid}
  def start(module, function_name, args)
      when is_atom(module) and is_atom(function_name) and is_list(args) do
    mfa = {module, function_name, args}
    Task.Supervised.start(get_owner(self()), get_callers(self()), mfa)
  end

  @doc """
  Starts a task that must be awaited on.

  `fun` must be a zero-arity anonymous function. This function
  spawns a process that is linked to and monitored by the caller
  process. A `Task` struct is returned containing the relevant
  information.

  If you start an `async`, you **must await**. This is either done
  by calling `Task.await/2` or `Task.yield/2` followed by
  `Task.shutdown/2` on the returned task. Alternatively, if you
  spawn a task inside a `GenServer`, then the `GenServer` will
  automatically await for you and call `c:GenServer.handle_info/2`
  with the task response and associated `:DOWN` message.

  Read the `Task` module documentation for more information about
  the general usage of async tasks.

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
  fail, including the caller process. If you don't want the task
  to fail then you must change the `heavy_fun/0` code in the
  same way you would achieve it if you didn't have the async call.
  For example, to either return `{:ok, val} | :error` results or,
  in more extreme cases, by using `try/rescue`. In other words,
  an asynchronous task should be thought of as an extension of the
  caller process rather than a mechanism to isolate it from all errors.

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
      the caller process dies.

  ## Metadata

  The task created with this function stores `:erlang.apply/2` in
  its `:mfa` metadata field, which is used internally to apply
  the anonymous function. Use `async/3` if you want another function
  to be used as metadata.
  """
  @spec async((-> any)) :: t
  def async(fun) when is_function(fun, 0) do
    async(:erlang, :apply, [fun, []])
  end

  @doc """
  Starts a task that must be awaited on.

  Similar to `async/1` except the function to be started is
  specified by the given `module`, `function_name`, and `args`.
  The `module`, `function_name`, and its arity are stored as
  a tuple in the `:mfa` field for reflection purposes.
  """
  @spec async(module, atom, [term]) :: t
  def async(module, function_name, args)
      when is_atom(module) and is_atom(function_name) and is_list(args) do
    mfargs = {module, function_name, args}
    owner = self()
    # No need to monitor because the processes are linked
    {:ok, pid} = Task.Supervised.start_link(get_owner(owner), :nomonitor)

    alias = build_alias(pid)
    send(pid, {owner, alias, alias, get_callers(owner), mfargs})
    %Task{pid: pid, ref: alias, owner: owner, mfa: {module, function_name, length(args)}}
  end

  @doc """
  Starts a task that immediately completes with the given `result`.

  Unlike `async/1`, this task does not spawn a linked process. It can
  be awaited or yielded like any other task.

  ## Usage

  In some cases, it is useful to create a "completed" task that represents
  a task that has already run and generated a result. For example, when
  processing data you may be able to determine that certain inputs are
  invalid before dispatching them for further processing:

      def process(data) do
        tasks =
          for entry <- data do
            if invalid_input?(entry) do
              Task.completed({:error, :invalid_input})
            else
              Task.async(fn -> further_process(entry) end)
            end
          end

        Task.await_many(tasks)
      end

  In many cases, `Task.completed/1` may be avoided in favor of returning the
  result directly.  You should generally only require this variant when working
  with mixed asynchrony, when a group of inputs will be handled partially
  synchronously and partially asynchronously.
  """
  @doc since: "1.13.0"
  @spec completed(any) :: t
  def completed(result) do
    ref = make_ref()
    owner = self()

    # "complete" the task immediately
    send(owner, {ref, result})

    %Task{pid: nil, ref: ref, owner: owner, mfa: {Task, :completed, 1}}
  end

  @doc """
  Returns a stream where the given function (`module` and `function_name`)
  is mapped concurrently on each element in `enumerable`.

  Each element of `enumerable` will be prepended to the given `args` and
  processed by its own task. Those tasks will be linked to an intermediate
  process that is then linked to the caller process. This means a failure
  in a task terminates the caller process and a failure in the caller
  process terminates all tasks.

  When streamed, each task will emit `{:ok, value}` upon successful
  completion or `{:exit, reason}` if the caller is trapping exits.
  It's possible to have `{:exit, {element, reason}}` for exits
  using the `:zip_input_on_exit` option. The order of results depends
  on the value of the `:ordered` option.

  The level of concurrency and the time tasks are allowed to run can
  be controlled via options (see the "Options" section below).

  Consider using `Task.Supervisor.async_stream/6` to start tasks
  under a supervisor. If you find yourself trapping exits to ensure
  errors in the tasks do not terminate the caller process, consider
  using `Task.Supervisor.async_stream_nolink/6` to start tasks that
  are not linked to the caller process.

  ## Options

    * `:max_concurrency` - sets the maximum number of tasks to run
      at the same time. Defaults to `System.schedulers_online/0`.

    * `:ordered` - whether the results should be returned in the same order
      as the input stream. When the output is ordered, Elixir may need to
      buffer results to emit them in the original order. Setting this option
      to false disables the need to buffer at the cost of removing ordering.
      This is also useful when you're using the tasks only for the side effects.
      Note that regardless of what `:ordered` is set to, the tasks will
      process asynchronously. If you need to process elements in order,
      consider using `Enum.map/2` or `Enum.each/2` instead. Defaults to `true`.

    * `:timeout` - the maximum amount of time (in milliseconds or `:infinity`)
      each task is allowed to execute for. Defaults to `5000`.

    * `:on_timeout` - what to do when a task times out. The possible
      values are:
      * `:exit` (default) - the caller (the process that spawned the tasks) exits.
      * `:kill_task` - the task that timed out is killed. The value
        emitted for that task is `{:exit, :timeout}`.

    * `:zip_input_on_exit` - (since v1.14.0) adds the original
      input to `:exit` tuples. The value emitted for that task is
      `{:exit, {input, reason}}`, where `input` is the collection element
      that caused an exit during processing. Defaults to `false`.

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

  ## First async tasks to complete

  You can also use `async_stream/3` to execute M tasks and find the N tasks
  to complete. For example:

      [
        &heavy_call_1/0,
        &heavy_call_2/0,
        &heavy_call_3/0
      ]
      |> Task.async_stream(fn fun -> fun.() end, ordered: false, max_concurrency: 3)
      |> Stream.filter(&match?({:ok, _}, &1))
      |> Enum.take(2)

  In the example above, we are executing three tasks and waiting for the
  first 2 to complete. We use `Stream.filter/2` to restrict ourselves only
  to successfully completed tasks, and then use `Enum.take/2` to retrieve
  N items. Note it is important to set both `ordered: false` and
  `max_concurrency: M`, where M is the number of tasks, to make sure all
  calls execute concurrently.

  ### Attention: unbound async + take

  If you want to potentially process a high number of items and keep only
  part of the results, you may end-up processing more items than desired.
  Let's see an example:

      1..100
      |> Task.async_stream(fn i ->
        Process.sleep(100)
        IO.puts(to_string(i))
      end)
      |> Enum.take(10)

  Running the example above in a machine with 8 cores will process 16 items,
  even though you want only 10 elements, since `async_stream/3` process items
  concurrently. That's because it will process 8 elements at once. Then all 8
  elements complete at roughly the same time, causing 8 elements to be kicked
  off for processing. Out of these extra 8, only 2 will be used, and the rest
  will be terminated.

  Depending on the problem, you can filter or limit the number of elements
  upfront:

      1..100
      |> Stream.take(10)
      |> Task.async_stream(fn i ->
        Process.sleep(100)
        IO.puts(to_string(i))
      end)
      |> Enum.to_list()

  In other cases, you likely want to tweak `:max_concurrency` to limit how
  many elements may be over processed at the cost of reducing concurrency.
  You can also set the number of elements to take to be a multiple of
  `:max_concurrency`. For instance, setting `max_concurrency: 5` in the
  example above.
  """
  @doc since: "1.4.0"
  @spec async_stream(Enumerable.t(), module, atom, [term], [async_stream_option]) ::
          Enumerable.t()
  def async_stream(enumerable, module, function_name, args, options \\ [])
      when is_atom(module) and is_atom(function_name) and is_list(args) do
    build_stream(enumerable, {module, function_name, args}, options)
  end

  @doc """
  Returns a stream that runs the given function `fun` concurrently
  on each element in `enumerable`.

  Works the same as `async_stream/5` but with an anonymous function instead of a
  module-function-arguments tuple. `fun` must be a one-arity anonymous function.

  Each `enumerable` element is passed as argument to the given function `fun` and
  processed by its own task. The tasks will be linked to the caller process, similarly
  to `async/1`.

  ## Example

  Count the code points in each string asynchronously, then add the counts together using reduce.

      iex> strings = ["long string", "longer string", "there are many of these"]
      iex> stream = Task.async_stream(strings, fn text -> text |> String.codepoints() |> Enum.count() end)
      iex> Enum.sum_by(stream, fn {:ok, num} -> num end)
      47

  See `async_stream/5` for discussion, options, and more examples.
  """
  @doc since: "1.4.0"
  @spec async_stream(Enumerable.t(), (term -> term), [async_stream_option]) :: Enumerable.t()
  def async_stream(enumerable, fun, options \\ [])
      when is_function(fun, 1) and is_list(options) do
    build_stream(enumerable, fun, options)
  end

  defp build_stream(enumerable, fun, options) do
    options = Task.Supervised.validate_stream_options(options)

    fn acc, acc_fun ->
      owner = get_owner(self())

      Task.Supervised.stream(enumerable, acc, acc_fun, get_callers(self()), fun, options, fn ->
        # No need to monitor because the processes are linked
        {:ok, pid} = Task.Supervised.start_link(owner, :nomonitor)
        {:ok, :link, pid}
      end)
    end
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

  @doc ~S"""
  Awaits a task reply and returns it.

  In case the task process dies, the caller process will exit with the same
  reason as the task.

  A timeout, in milliseconds or `:infinity`, can be given with a default value
  of `5000`. If the timeout is exceeded, then the caller process will exit.
  If the task process is linked to the caller process which is the case when
  a task is started with `async`, then the task process will also exit. If the
  task process is trapping exits or not linked to the caller process, then it
  will continue to run.

  This function assumes the task's monitor is still active or the monitor's
  `:DOWN` message is in the message queue. If it has been demonitored, or the
  message already received, this function will wait for the duration of the
  timeout awaiting the message.

  This function can only be called once for any given task. If you want
  to be able to check multiple times if a long-running task has finished
  its computation, use `yield/2` instead.

  ## Examples

      iex> task = Task.async(fn -> 1 + 1 end)
      iex> Task.await(task)
      2

  ## Compatibility with OTP behaviours

  It is not recommended to `await` a long-running task inside an OTP
  behaviour such as `GenServer`. Instead, you should match on the message
  coming from a task inside your `c:GenServer.handle_info/2` callback.

  A GenServer will receive two messages on `handle_info/2`:

    * `{ref, result}` - the reply message where `ref` is the monitor
      reference returned by the `task.ref` and `result` is the task
      result

    * `{:DOWN, ref, :process, pid, reason}` - since all tasks are also
      monitored, you will also receive the `:DOWN` message delivered by
      `Process.monitor/1`. If you receive the `:DOWN` message without a
      a reply, it means the task crashed

  Another consideration to have in mind is that tasks started by `Task.async/1`
  are always linked to their callers and you may not want the GenServer to
  crash if the task crashes. Therefore, it is preferable to instead use
  `Task.Supervisor.async_nolink/3` inside OTP behaviours. For completeness, here
  is an example of a GenServer that start tasks and handles their results:

      defmodule GenServerTaskExample do
        use GenServer

        def start_link(opts) do
          GenServer.start_link(__MODULE__, :ok, opts)
        end

        def init(_opts) do
          # We will keep all running tasks in a map
          {:ok, %{tasks: %{}}}
        end

        # Imagine we invoke a task from the GenServer to access a URL...
        def handle_call(:some_message, _from, state) do
          url = ...
          task = Task.Supervisor.async_nolink(MyApp.TaskSupervisor, fn -> fetch_url(url) end)

          # After we start the task, we store its reference and the url it is fetching
          state = put_in(state.tasks[task.ref], url)

          {:reply, :ok, state}
        end

        # If the task succeeds...
        def handle_info({ref, result}, state) do
          # The task succeed so we can demonitor its reference
          Process.demonitor(ref, [:flush])

          {url, state} = pop_in(state.tasks[ref])
          IO.puts("Got #{inspect(result)} for URL #{inspect url}")
          {:noreply, state}
        end

        # If the task fails...
        def handle_info({:DOWN, ref, _, _, reason}, state) do
          {url, state} = pop_in(state.tasks[ref])
          IO.puts("URL #{inspect url} failed with reason #{inspect(reason)}")
          {:noreply, state}
        end
      end

  With the server defined, you will want to start the task supervisor
  above and the GenServer in your supervision tree:

      children = [
        {Task.Supervisor, name: MyApp.TaskSupervisor},
        {GenServerTaskExample, name: MyApp.GenServerTaskExample}
      ]

      Supervisor.start_link(children, strategy: :one_for_one)

  """
  @spec await(t, timeout) :: term
  def await(%Task{ref: ref, owner: owner} = task, timeout \\ 5000) when is_timeout(timeout) do
    if owner != self() do
      raise ArgumentError, invalid_owner_error(task)
    end

    await_receive(ref, task, timeout)
  end

  defp await_receive(ref, task, timeout) do
    receive do
      {^ref, reply} ->
        demonitor(ref)
        reply

      {:DOWN, ^ref, _, proc, reason} ->
        exit({reason(reason, proc), {__MODULE__, :await, [task, timeout]}})
    after
      timeout ->
        demonitor(ref)
        exit({:timeout, {__MODULE__, :await, [task, timeout]}})
    end
  end

  @doc """
  Ignores an existing task.

  This means the task will continue running, but it will be unlinked
  and you can no longer yield, await or shut it down.

  Returns `{:ok, reply}` if the reply is received before ignoring the task,
  `{:exit, reason}` if the task died before ignoring it, otherwise `nil`.

  Important: avoid using [`Task.async/1,3`](`async/1`) and then immediately ignoring
  the task. If you want to start tasks you don't care about their
  results, use `Task.Supervisor.start_child/2` instead.
  """
  @doc since: "1.13.0"
  @spec ignore(t) :: {:ok, term} | {:exit, term} | nil
  def ignore(%Task{ref: ref, pid: pid, owner: owner} = task) do
    if owner != self() do
      raise ArgumentError, invalid_owner_error(task)
    end

    ignore_receive(ref, pid, task)
  end

  defp ignore_receive(ref, pid, task) do
    receive do
      {^ref, reply} ->
        pid && Process.unlink(pid)
        demonitor(ref)
        {:ok, reply}

      {:DOWN, ^ref, _, proc, :noconnection} ->
        exit({reason(:noconnection, proc), {__MODULE__, :ignore, [task]}})

      {:DOWN, ^ref, _, _, reason} ->
        {:exit, reason}
    after
      0 ->
        pid && Process.unlink(pid)
        demonitor(ref)
        nil
    end
  end

  @doc """
  Awaits replies from multiple tasks and returns them.

  This function receives a list of tasks and waits for their replies in the
  given time interval. It returns a list of the results, in the same order as
  the tasks supplied in the `tasks` input argument.

  If any of the task processes dies, the caller process will exit with the same
  reason as that task.

  A timeout, in milliseconds or `:infinity`, can be given with a default value
  of `5000`. If the timeout is exceeded, then the caller process will exit.
  Any task processes that are linked to the caller process (which is the case
  when a task is started with `async`) will also exit. Any task processes that
  are trapping exits or not linked to the caller process will continue to run.

  This function assumes the tasks' monitors are still active or the monitor's
  `:DOWN` message is in the message queue. If any tasks have been demonitored,
  or the message already received, this function will wait for the duration of
  the timeout.

  This function can only be called once for any given task. If you want to be
  able to check multiple times if a long-running task has finished its
  computation, use `yield_many/2` instead.

  ## Compatibility with OTP behaviours

  It is not recommended to `await` long-running tasks inside an OTP behaviour
  such as `GenServer`. See `await/2` for more information.

  ## Examples

      iex> tasks = [
      ...>   Task.async(fn -> 1 + 1 end),
      ...>   Task.async(fn -> 2 + 3 end)
      ...> ]
      iex> Task.await_many(tasks)
      [2, 5]

  """
  @doc since: "1.11.0"
  @spec await_many([t], timeout) :: [term]
  def await_many(tasks, timeout \\ 5000) when is_timeout(timeout) do
    awaiting =
      Map.new(tasks, fn %Task{ref: ref, owner: owner} = task ->
        if owner != self() do
          raise ArgumentError, invalid_owner_error(task)
        end

        {ref, true}
      end)

    timeout_ref = make_ref()

    timer_ref =
      if timeout != :infinity do
        Process.send_after(self(), timeout_ref, timeout)
      end

    try do
      await_many(tasks, timeout, awaiting, %{}, timeout_ref)
    after
      timer_ref && Process.cancel_timer(timer_ref)
      receive do: (^timeout_ref -> :ok), after: (0 -> :ok)
    end
  end

  defp await_many(tasks, _timeout, awaiting, replies, _timeout_ref)
       when map_size(awaiting) == 0 do
    for %{ref: ref} <- tasks, do: Map.fetch!(replies, ref)
  end

  defp await_many(tasks, timeout, awaiting, replies, timeout_ref) do
    receive do
      ^timeout_ref ->
        demonitor_pending_tasks(awaiting)
        exit({:timeout, {__MODULE__, :await_many, [tasks, timeout]}})

      {:DOWN, ref, _, proc, reason} when is_map_key(awaiting, ref) ->
        demonitor_pending_tasks(awaiting)
        exit({reason(reason, proc), {__MODULE__, :await_many, [tasks, timeout]}})

      {ref, reply} when is_map_key(awaiting, ref) ->
        demonitor(ref)

        await_many(
          tasks,
          timeout,
          Map.delete(awaiting, ref),
          Map.put(replies, ref, reply),
          timeout_ref
        )
    end
  end

  defp demonitor_pending_tasks(awaiting) do
    Enum.each(awaiting, fn {ref, _} ->
      demonitor(ref)
    end)
  end

  @doc false
  @deprecated "Pattern match directly on the message instead"
  def find(tasks, {ref, reply}) when is_reference(ref) do
    Enum.find_value(tasks, fn
      %Task{ref: ^ref} = task ->
        demonitor(ref)
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
  Temporarily blocks the caller process waiting for a task reply.

  Returns `{:ok, reply}` if the reply is received, `nil` if
  no reply has arrived, or `{:exit, reason}` if the task has already
  exited. Keep in mind that normally a task failure also causes
  the process owning the task to exit. Therefore this function can
  return `{:exit, reason}` if at least one of the conditions below apply:

    * the task process exited with the reason `:normal`
    * the task isn't linked to the caller (the task was started
      with `Task.Supervisor.async_nolink/2` or `Task.Supervisor.async_nolink/4`)
    * the caller is trapping exits

  A timeout, in milliseconds or `:infinity`, can be given with a default value
  of `5000`. If the time runs out before a message from the task is received,
  this function will return `nil` and the monitor will remain active. Therefore
  `yield/2` can be called multiple times on the same task.

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
          Logger.warning("Failed to get a result in #{timeout}ms")
          nil
      end

  If you intend to check on the task but leave it running after the timeout,
  you can chain this together with `ignore/1`, like so:

      case Task.yield(task, timeout) || Task.ignore(task) do
        {:ok, result} ->
          result

        nil ->
          Logger.warning("Failed to get a result in #{timeout}ms")
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

    yield_receive(ref, task, timeout)
  end

  defp yield_receive(ref, task, timeout) do
    receive do
      {^ref, reply} ->
        demonitor(ref)
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
    * `nil` if the task keeps running, either because a limit
      has been reached or past the timeout

  Check `yield/2` for more information.

  ## Example

  `Task.yield_many/2` allows developers to spawn multiple tasks
  and retrieve the results received in a given time frame.
  If we combine it with `Task.shutdown/2` (or `Task.ignore/1`),
  it allows us to gather those results and cancel (or ignore)
  the tasks that have not replied in time.

  Let's see an example.

      tasks =
        for i <- 1..10 do
          Task.async(fn ->
            Process.sleep(i * 1000)
            i
          end)
        end

      tasks_with_results = Task.yield_many(tasks, timeout: 5000)

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

  As a convenience, you can achieve a similar behavior to above
  by specifying the `:on_timeout` option to be `:kill_task` (or
  `:ignore`). See `Task.await_many/2` if you would rather exit
  the caller process on timeout.

  ## Options

  The second argument is either a timeout or options, which defaults
  to this:

    * `:limit` - the maximum amount of tasks to wait for.
      If the limit is reached before the timeout, this function
      returns immediately without triggering the `:on_timeout` behaviour

    * `:timeout` - the maximum amount of time (in milliseconds or `:infinity`)
      each task is allowed to execute for. Defaults to `5000`.

    * `:on_timeout` - what to do when a task times out. The possible
      values are:
      * `:nothing` - do nothing (default). The tasks can still be
        awaited on, yielded on, ignored, or shut down later.
      * `:ignore` - the results of the task will be ignored.
      * `:kill_task` - the task that timed out is killed.
  """
  @spec yield_many([t], timeout) :: [{t, {:ok, term} | {:exit, term} | nil}]
  @spec yield_many([t],
          limit: pos_integer(),
          timeout: timeout,
          on_timeout: :nothing | :ignore | :kill_task
        ) ::
          [{t, {:ok, term} | {:exit, term} | nil}]
  def yield_many(tasks, opts \\ [])

  def yield_many(tasks, timeout) when is_timeout(timeout) do
    yield_many(tasks, timeout: timeout)
  end

  def yield_many(tasks, opts) when is_list(opts) do
    refs =
      Map.new(tasks, fn %Task{ref: ref, owner: owner} = task ->
        if owner != self() do
          raise ArgumentError, invalid_owner_error(task)
        end

        {ref, nil}
      end)

    on_timeout = Keyword.get(opts, :on_timeout, :nothing)
    timeout = Keyword.get(opts, :timeout, 5_000)
    limit = Keyword.get(opts, :limit, map_size(refs))
    timeout_ref = make_ref()

    timer_ref =
      if timeout != :infinity do
        Process.send_after(self(), timeout_ref, timeout)
      end

    try do
      yield_many(limit, refs, timeout_ref, timer_ref)
    catch
      {:noconnection, reason} ->
        exit({reason, {__MODULE__, :yield_many, [tasks, timeout]}})
    else
      {timed_out?, refs} ->
        for task <- tasks do
          value =
            with nil <- Map.fetch!(refs, task.ref) do
              case on_timeout do
                _ when not timed_out? -> nil
                :nothing -> nil
                :kill_task -> shutdown(task, :brutal_kill)
                :ignore -> ignore(task)
              end
            end

          {task, value}
        end
    end
  end

  defp yield_many(0, refs, timeout_ref, timer_ref) do
    timer_ref && Process.cancel_timer(timer_ref)
    receive do: (^timeout_ref -> :ok), after: (0 -> :ok)
    {false, refs}
  end

  defp yield_many(limit, refs, timeout_ref, timer_ref) do
    receive do
      {ref, reply} when is_map_key(refs, ref) ->
        demonitor(ref)
        yield_many(limit - 1, %{refs | ref => {:ok, reply}}, timeout_ref, timer_ref)

      {:DOWN, ref, _, proc, reason} when is_map_key(refs, ref) ->
        if reason == :noconnection do
          throw({:noconnection, reason(:noconnection, proc)})
        else
          yield_many(limit - 1, %{refs | ref => {:exit, reason}}, timeout_ref, timer_ref)
        end

      ^timeout_ref ->
        {true, refs}
    end
  end

  @doc """
  Unlinks and shuts down the task, and then checks for a reply.

  Returns `{:ok, reply}` if the reply is received while shutting down the task,
  `{:exit, reason}` if the task died, otherwise `nil`. Once shut down,
  you can no longer await or yield it.

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

  If there is no process linked to the task, such as tasks started by
  `Task.completed/1`, we check for a response or error accordingly, but without
  shutting a process down.

  If a task's monitor has already been demonitored or received and there is not
  a response waiting in the message queue this function will return
  `{:exit, :noproc}` as the result or exit reason can not be determined.
  """
  @spec shutdown(t, timeout | :brutal_kill) :: {:ok, term} | {:exit, term} | nil
  def shutdown(task, shutdown \\ 5000)

  def shutdown(%Task{pid: nil} = task, _) do
    ignore(task)
  end

  def shutdown(%Task{owner: owner} = task, _) when owner != self() do
    raise ArgumentError, invalid_owner_error(task)
  end

  def shutdown(%Task{pid: pid, ref: ref} = task, :brutal_kill) do
    mon = build_monitor(pid)
    shutdown_send(pid, :kill)

    case shutdown_receive(ref, mon, task, :brutal_kill, :infinity) do
      {:down, proc, :noconnection} ->
        exit({reason(:noconnection, proc), {__MODULE__, :shutdown, [task, :brutal_kill]}})

      {:down, _, reason} ->
        {:exit, reason}

      result ->
        result
    end
  end

  def shutdown(%Task{pid: pid, ref: ref} = task, timeout) when is_timeout(timeout) do
    mon = build_monitor(pid)
    shutdown_send(pid, :shutdown)

    case shutdown_receive(ref, mon, task, :shutdown, timeout) do
      {:down, proc, :noconnection} ->
        exit({reason(:noconnection, proc), {__MODULE__, :shutdown, [task, timeout]}})

      {:down, _, reason} ->
        {:exit, reason}

      result ->
        result
    end
  end

  # Spawn a process to ensure task gets exit signal
  # if process dies from exit signal between unlink and exit.
  defp shutdown_send(pid, reason) do
    caller = self()
    ref = make_ref()
    enforcer = spawn(fn -> shutdown_send(pid, reason, caller, ref) end)
    Process.unlink(pid)
    Process.exit(pid, reason)
    send(enforcer, {:done, ref})
    :ok
  end

  defp shutdown_send(pid, reason, caller, ref) do
    mon = Process.monitor(caller)

    receive do
      {:done, ^ref} -> :ok
      {:DOWN, ^mon, _, _, _} -> Process.exit(pid, reason)
    end
  end

  defp shutdown_receive(ref, mon, task, type, timeout) do
    receive do
      {:DOWN, ^mon, _, _, :shutdown} when type in [:shutdown, :timeout_kill] ->
        demonitor(ref)
        flush_reply(ref)

      {:DOWN, ^mon, _, _, :killed} when type == :brutal_kill ->
        demonitor(ref)
        flush_reply(ref)

      {:DOWN, ^mon, _, proc, :noproc} ->
        reason = flush_noproc(ref, proc, type)
        flush_reply(ref) || reason

      {:DOWN, ^mon, _, proc, reason} ->
        demonitor(ref)
        flush_reply(ref) || {:down, proc, reason}
    after
      timeout ->
        Process.exit(task.pid, :kill)
        shutdown_receive(ref, mon, task, :timeout_kill, :infinity)
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
        demonitor(ref)
        {:down, proc, :noproc}
    end
  end

  ## Optimizations

  defp build_monitor(pid) do
    :erlang.monitor(:process, pid)
  end

  defp build_alias(pid) do
    :erlang.monitor(:process, pid, alias: :demonitor)
  end

  @doc false
  # This instructs the Erlang compiler to apply selective
  # receive optimizations to several functions in this module.
  # This function is never invoked directly, it is only here
  # for compiler optimization purposes.
  #
  # To verify which functions have been optimized, run the
  # following command after Elixir is compiled from the project
  # root:
  #
  #     ERL_COMPILER_OPTIONS=recv_opt_info elixir lib/elixir/lib/task.ex
  #
  def __recv_opt_info__(pid, task) do
    await_receive(build_alias(pid), task, :infinity)
    shutdown_receive(build_alias(pid), build_monitor(pid), task, :shutdown, :infinity)
    yield_receive(build_alias(pid), task, :infinity)
    ignore_receive(build_alias(pid), pid, task)
  end

  ## Helpers

  defp demonitor(ref) when is_reference(ref) do
    Process.demonitor(ref, [:flush])
    :ok
  end

  defp reason(:noconnection, proc), do: {:nodedown, monitor_node(proc)}
  defp reason(reason, _), do: reason

  defp monitor_node(pid) when is_pid(pid), do: node(pid)
  defp monitor_node({_, node}), do: node

  defp invalid_owner_error(task) do
    "task #{inspect(task)} must be queried from the owner but was queried from #{inspect(self())}"
  end
end
