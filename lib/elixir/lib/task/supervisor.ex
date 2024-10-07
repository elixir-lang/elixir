defmodule Task.Supervisor do
  @moduledoc """
  A task supervisor.

  This module defines a supervisor which can be used to dynamically
  supervise tasks.

  A task supervisor is started with no children, often under a
  supervisor and a name:

      children = [
        {Task.Supervisor, name: MyApp.TaskSupervisor}
      ]

      Supervisor.start_link(children, strategy: :one_for_one)

  The options given in the child specification are documented in `start_link/1`.

  Once started, you can start tasks directly under the supervisor, for example:

      task = Task.Supervisor.async(MyApp.TaskSupervisor, fn ->
        :do_some_work
      end)

  See the `Task` module for more examples.

  ## Scalability and partitioning

  The `Task.Supervisor` is a single process responsible for starting
  other processes. In some applications, the `Task.Supervisor` may
  become a bottleneck. To address this, you can start multiple instances
  of the `Task.Supervisor` and then pick a random instance to start
  the task on.

  Instead of:

      children = [
        {Task.Supervisor, name: MyApp.TaskSupervisor}
      ]

  and:

      Task.Supervisor.async(MyApp.TaskSupervisor, fn -> :do_some_work end)

  You can do this:

      children = [
        {PartitionSupervisor,
         child_spec: Task.Supervisor,
         name: MyApp.TaskSupervisors}
      ]

  and then:

      Task.Supervisor.async(
        {:via, PartitionSupervisor, {MyApp.TaskSupervisors, self()}},
        fn -> :do_some_work end
      )

  In the code above, we start a partition supervisor that will by default
  start a dynamic supervisor for each core in your machine. Then, instead
  of calling the `Task.Supervisor` by name, you call it through the
  partition supervisor using the `{:via, PartitionSupervisor, {name, key}}`
  format, where `name` is the name of the partition supervisor and `key`
  is the routing key. We picked `self()` as the routing key, which means
  each process will be assigned one of the existing task supervisors.
  Read the `PartitionSupervisor` docs for more information.

  ## Name registration

  A `Task.Supervisor` is bound to the same name registration rules as a
  `GenServer`. Read more about them in the `GenServer` docs.
  """

  @typedoc "Option values used by `start_link`"
  @type option ::
          GenServer.option()
          | DynamicSupervisor.init_option()

  @typedoc """
  Options given to `async_stream` and `async_stream_nolink` functions.
  """
  @typedoc since: "1.17.0"
  @type async_stream_option :: Task.async_stream_option() | {:shutdown, Supervisor.shutdown()}

  @doc false
  def child_spec(opts) when is_list(opts) do
    id =
      case Keyword.get(opts, :name, Task.Supervisor) do
        name when is_atom(name) -> name
        {:global, name} -> name
        {:via, _module, name} -> name
      end

    %{
      id: id,
      start: {Task.Supervisor, :start_link, [opts]},
      type: :supervisor
    }
  end

  @doc """
  Starts a new supervisor.

  ## Examples

  A task supervisor is typically started under a supervision tree using
  the tuple format:

      {Task.Supervisor, name: MyApp.TaskSupervisor}

  You can also start it by calling `start_link/1` directly:

      Task.Supervisor.start_link(name: MyApp.TaskSupervisor)

  But this is recommended only for scripting and should be avoided in
  production code. Generally speaking, processes should always be started
  inside supervision trees.

  ## Options

    * `:name` - used to register a supervisor name, the supported values are
      described under the `Name Registration` section in the `GenServer` module
      docs;

    * `:max_restarts`, `:max_seconds`, and `:max_children` - as specified in
      `DynamicSupervisor`;

  This function could also receive `:restart` and `:shutdown` as options
  but those two options have been deprecated and it is now preferred to
  give them directly to `start_child`.
  """
  @spec start_link([option]) :: Supervisor.on_start()
  def start_link(options \\ []) do
    {restart, options} = Keyword.pop(options, :restart)
    {shutdown, options} = Keyword.pop(options, :shutdown)

    if restart || shutdown do
      IO.warn(
        ":restart and :shutdown options in Task.Supervisor.start_link/1 " <>
          "are deprecated. Please pass those options on start_child/3 instead"
      )
    end

    keys = [:max_children, :max_seconds, :max_restarts]
    {sup_opts, start_opts} = Keyword.split(options, keys)
    restart_and_shutdown = {restart || :temporary, shutdown || 5000}
    DynamicSupervisor.start_link(__MODULE__, {restart_and_shutdown, sup_opts}, start_opts)
  end

  @doc false
  def init({{_restart, _shutdown} = arg, options}) do
    Process.put(__MODULE__, arg)
    DynamicSupervisor.init([strategy: :one_for_one] ++ options)
  end

  @doc """
  Starts a task that can be awaited on.

  The `supervisor` must be a reference as defined in `Supervisor`.
  The task will still be linked to the caller, see `Task.async/1` for
  more information and `async_nolink/3` for a non-linked variant.

  Raises an error if `supervisor` has reached the maximum number of
  children.

  ## Options

    * `:shutdown` - `:brutal_kill` if the tasks must be killed directly on shutdown
      or an integer indicating the timeout value, defaults to 5000 milliseconds.
      The tasks must trap exits for the timeout to have an effect.

  """
  @spec async(Supervisor.supervisor(), (-> any), Keyword.t()) :: Task.t()
  def async(supervisor, fun, options \\ []) do
    async(supervisor, :erlang, :apply, [fun, []], options)
  end

  @doc """
  Starts a task that can be awaited on.

  The `supervisor` must be a reference as defined in `Supervisor`.
  The task will still be linked to the caller, see `Task.async/1` for
  more information and `async_nolink/3` for a non-linked variant.

  Raises an error if `supervisor` has reached the maximum number of
  children.

  ## Options

    * `:shutdown` - `:brutal_kill` if the tasks must be killed directly on shutdown
      or an integer indicating the timeout value, defaults to 5000 milliseconds.
      The tasks must trap exits for the timeout to have an effect.

  """
  @spec async(Supervisor.supervisor(), module, atom, [term], Keyword.t()) :: Task.t()
  def async(supervisor, module, fun, args, options \\ []) do
    async(supervisor, :link, module, fun, args, options)
  end

  @doc """
  Starts a task that can be awaited on.

  The `supervisor` must be a reference as defined in `Supervisor`.
  The task won't be linked to the caller, see `Task.async/1` for
  more information.

  Raises an error if `supervisor` has reached the maximum number of
  children.

  Note this function requires the task supervisor to have `:temporary`
  as the `:restart` option (the default), as `async_nolink/3` keeps a
  direct reference to the task which is lost if the task is restarted.

  ## Options

    * `:shutdown` - `:brutal_kill` if the tasks must be killed directly on shutdown
      or an integer indicating the timeout value, defaults to 5000 milliseconds.
      The tasks must trap exits for the timeout to have an effect.

  ## Compatibility with OTP behaviours

  If you create a task using `async_nolink` inside an OTP behaviour
  like `GenServer`, you should match on the message coming from the
  task inside your `c:GenServer.handle_info/2` callback.

  The reply sent by the task will be in the format `{ref, result}`,
  where `ref` is the monitor reference held by the task struct
  and `result` is the return value of the task function.

  Keep in mind that, regardless of how the task created with `async_nolink`
  terminates, the caller's process will always receive a `:DOWN` message
  with the same `ref` value that is held by the task struct. If the task
  terminates normally, the reason in the `:DOWN` message will be `:normal`.

  ## Examples

  Typically, you use `async_nolink/3` when there is a reasonable expectation that
  the task may fail, and you don't want it to take down the caller. Let's see an
  example where a `GenServer` is meant to run a single task and track its status:

      defmodule MyApp.Server do
        use GenServer

        # ...

        def start_task do
          GenServer.call(__MODULE__, :start_task)
        end

        # In this case the task is already running, so we just return :ok.
        def handle_call(:start_task, _from, %{ref: ref} = state) when is_reference(ref) do
          {:reply, :ok, state}
        end

        # The task is not running yet, so let's start it.
        def handle_call(:start_task, _from, %{ref: nil} = state) do
          task =
            Task.Supervisor.async_nolink(MyApp.TaskSupervisor, fn ->
              ...
            end)

          # We return :ok and the server will continue running
          {:reply, :ok, %{state | ref: task.ref}}
        end

        # The task completed successfully
        def handle_info({ref, answer}, %{ref: ref} = state) do
          # We don't care about the DOWN message now, so let's demonitor and flush it
          Process.demonitor(ref, [:flush])
          # Do something with the result and then return
          {:noreply, %{state | ref: nil}}
        end

        # The task failed
        def handle_info({:DOWN, ref, :process, _pid, _reason}, %{ref: ref} = state) do
          # Log and possibly restart the task...
          {:noreply, %{state | ref: nil}}
        end
      end

  """
  @spec async_nolink(Supervisor.supervisor(), (-> any), Keyword.t()) :: Task.t()
  def async_nolink(supervisor, fun, options \\ []) do
    async_nolink(supervisor, :erlang, :apply, [fun, []], options)
  end

  @doc """
  Starts a task that can be awaited on.

  The `supervisor` must be a reference as defined in `Supervisor`.
  The task won't be linked to the caller, see `Task.async/1` for
  more information.

  Raises an error if `supervisor` has reached the maximum number of
  children.

  Note this function requires the task supervisor to have `:temporary`
  as the `:restart` option (the default), as `async_nolink/5` keeps a
  direct reference to the task which is lost if the task is restarted.
  """
  @spec async_nolink(Supervisor.supervisor(), module, atom, [term], Keyword.t()) :: Task.t()
  def async_nolink(supervisor, module, fun, args, options \\ []) do
    async(supervisor, :nolink, module, fun, args, options)
  end

  @doc """
  Returns a stream where the given function (`module` and `function`)
  is mapped concurrently on each element in `enumerable`.

  Each element will be prepended to the given `args` and processed by its
  own task. The tasks will be spawned under the given `supervisor` and
  linked to the caller process, similarly to `async/5`.

  When streamed, each task will emit `{:ok, value}` upon successful
  completion or `{:exit, reason}` if the caller is trapping exits.
  The order of results depends on the value of the `:ordered` option.

  The level of concurrency and the time tasks are allowed to run can
  be controlled via options (see the "Options" section below).

  If you find yourself trapping exits to handle exits inside
  the async stream, consider using `async_stream_nolink/6` to start tasks
  that are not linked to the calling process.

  ## Options

    * `:max_concurrency` - sets the maximum number of tasks to run
      at the same time. Defaults to `System.schedulers_online/0`.

    * `:ordered` - whether the results should be returned in the same order
      as the input stream. This option is useful when you have large
      streams and don't want to buffer results before they are delivered.
      This is also useful when you're using the tasks for side effects.
      Defaults to `true`.

    * `:timeout` - the maximum amount of time to wait (in milliseconds)
      without receiving a task reply (across all running tasks).
      Defaults to `5000`.

    * `:on_timeout` - what do to when a task times out. The possible
      values are:
      * `:exit` (default) - the process that spawned the tasks exits.
      * `:kill_task` - the task that timed out is killed. The value
        emitted for that task is `{:exit, :timeout}`.

    * `:zip_input_on_exit` - (since v1.14.0) adds the original
      input to `:exit` tuples. The value emitted for that task is
      `{:exit, {input, reason}}`, where `input` is the collection element
      that caused an exited during processing. Defaults to `false`.

    * `:shutdown` - `:brutal_kill` if the tasks must be killed directly on shutdown
      or an integer indicating the timeout value. Defaults to `5000` milliseconds.
      The tasks must trap exits for the timeout to have an effect.

  ## Examples

  Let's build a stream and then enumerate it:

      stream = Task.Supervisor.async_stream(MySupervisor, collection, Mod, :expensive_fun, [])
      Enum.to_list(stream)

  """
  @doc since: "1.4.0"
  @spec async_stream(
          Supervisor.supervisor(),
          Enumerable.t(),
          module,
          atom,
          [term],
          [async_stream_option]
        ) :: Enumerable.t()
  def async_stream(supervisor, enumerable, module, function, args, options \\ [])
      when is_atom(module) and is_atom(function) and is_list(args) do
    build_stream(supervisor, :link, enumerable, {module, function, args}, options)
  end

  @doc """
  Returns a stream that runs the given function `fun` concurrently
  on each element in `enumerable`.

  Each element in `enumerable` is passed as argument to the given function `fun`
  and processed by its own task. The tasks will be spawned under the given
  `supervisor` and linked to the caller process, similarly to `async/3`.

  See `async_stream/6` for discussion, options, and examples.
  """
  @doc since: "1.4.0"
  @spec async_stream(
          Supervisor.supervisor(),
          Enumerable.t(),
          (term -> term),
          [async_stream_option]
        ) :: Enumerable.t()
  def async_stream(supervisor, enumerable, fun, options \\ []) when is_function(fun, 1) do
    build_stream(supervisor, :link, enumerable, fun, options)
  end

  @doc """
  Returns a stream where the given function (`module` and `function`)
  is mapped concurrently on each element in `enumerable`.

  Each element in `enumerable` will be prepended to the given `args` and processed
  by its own task. The tasks will be spawned under the given `supervisor` and
  will not be linked to the caller process, similarly to `async_nolink/5`.

  See `async_stream/6` for discussion, options, and examples.
  """
  @doc since: "1.4.0"
  @spec async_stream_nolink(
          Supervisor.supervisor(),
          Enumerable.t(),
          module,
          atom,
          [term],
          [async_stream_option]
        ) :: Enumerable.t()
  def async_stream_nolink(supervisor, enumerable, module, function, args, options \\ [])
      when is_atom(module) and is_atom(function) and is_list(args) do
    build_stream(supervisor, :nolink, enumerable, {module, function, args}, options)
  end

  @doc ~S"""
  Returns a stream that runs the given `function` concurrently on each
  element in `enumerable`.

  Each element in `enumerable` is passed as argument to the given function `fun`
  and processed by its own task. The tasks will be spawned under the given
  `supervisor` and will not be linked to the caller process, similarly
  to `async_nolink/3`.

  See `async_stream/6` for discussion and examples.

  ## Error handling and cleanup

  Even if tasks are not linked to the caller, there is no risk of leaving dangling tasks
  running after the stream halts.

  Consider the following example:

      Task.Supervisor.async_stream_nolink(MySupervisor, collection, fun, on_timeout: :kill_task, ordered: false)
      |> Enum.each(fn
        {:ok, _} -> :ok
        {:exit, reason} -> raise "Task exited: #{Exception.format_exit(reason)}"
      end)

  If one task raises or times out:

    1. the second clause gets called
    2. an exception is raised
    3. the stream halts
    4. all ongoing tasks will be shut down

  Here is another example:

      Task.Supervisor.async_stream_nolink(MySupervisor, collection, fun, on_timeout: :kill_task, ordered: false)
      |> Stream.filter(&match?({:ok, _}, &1))
      |> Enum.take(3)

  This will return the three first tasks to succeed, ignoring timeouts and errors, and shut down
  every ongoing task.

  Just running the stream with `Stream.run/1` on the other hand would ignore errors and process the whole stream.

  """
  @doc since: "1.4.0"
  @spec async_stream_nolink(
          Supervisor.supervisor(),
          Enumerable.t(),
          (term -> term),
          [async_stream_option]
        ) :: Enumerable.t()
  def async_stream_nolink(supervisor, enumerable, fun, options \\ []) when is_function(fun, 1) do
    build_stream(supervisor, :nolink, enumerable, fun, options)
  end

  @doc """
  Terminates the child with the given `pid`.
  """
  @spec terminate_child(Supervisor.supervisor(), pid) :: :ok | {:error, :not_found}
  def terminate_child(supervisor, pid) when is_pid(pid) do
    DynamicSupervisor.terminate_child(supervisor, pid)
  end

  @doc """
  Returns all children PIDs except those that are restarting.

  Note that calling this function when supervising a large number
  of children under low memory conditions can cause an out of memory
  exception.
  """
  @spec children(Supervisor.supervisor()) :: [pid]
  def children(supervisor) do
    for {_, pid, _, _} <- DynamicSupervisor.which_children(supervisor), is_pid(pid), do: pid
  end

  @doc """
  Starts a task as a child of the given `supervisor`.

      Task.Supervisor.start_child(MyTaskSupervisor, fn ->
        IO.puts("I am running in a task")
      end)

  Note that the spawned process is not linked to the caller, but
  only to the supervisor. This command is useful in case the
  task needs to perform side-effects (like I/O) and you have no
  interest in its results nor if it completes successfully.

  ## Options

    * `:restart` - the restart strategy, may be `:temporary` (the default),
      `:transient` or `:permanent`. `:temporary` means the task is never
      restarted, `:transient` means it is restarted if the exit is not
      `:normal`, `:shutdown` or `{:shutdown, reason}`. A `:permanent` restart
      strategy means it is always restarted.

    * `:shutdown` - `:brutal_kill` if the task must be killed directly on shutdown
      or an integer indicating the timeout value, defaults to 5000 milliseconds.
      The task must trap exits for the timeout to have an effect.

  """
  @spec start_child(Supervisor.supervisor(), (-> any), keyword) ::
          DynamicSupervisor.on_start_child()
  def start_child(supervisor, fun, options \\ []) do
    restart = options[:restart]
    shutdown = options[:shutdown]
    args = [get_owner(self()), get_callers(self()), {:erlang, :apply, [fun, []]}]
    start_child_with_spec(supervisor, args, restart, shutdown)
  end

  @doc """
  Starts a task as a child of the given `supervisor`.

  Similar to `start_child/3` except the task is specified
  by the given `module`, `fun` and `args`.
  """
  @spec start_child(Supervisor.supervisor(), module, atom, [term], keyword) ::
          DynamicSupervisor.on_start_child()
  def start_child(supervisor, module, fun, args, options \\ [])
      when is_atom(fun) and is_list(args) do
    restart = options[:restart]
    shutdown = options[:shutdown]
    mfa = {module, fun, args}
    owner = get_owner(self())
    callers = get_callers(self())

    if restart == :temporary or restart == nil do
      start_child_maybe_temporary(supervisor, owner, callers, restart, shutdown, mfa)
    else
      start_child_with_spec(supervisor, [owner, callers, mfa], restart, shutdown)
    end
  end

  defp start_child_maybe_temporary(supervisor, owner, callers, restart, shutdown, mfa) do
    case start_child_with_spec(supervisor, [owner, :monitor], restart, shutdown) do
      # TODO: This only exists because we need to support reading restart/shutdown
      # from two different places. Remove this, the init function and the associated
      # clause in DynamicSupervisor on Elixir v2.0
      {:restart, restart} ->
        start_child_with_spec(supervisor, [owner, callers, mfa], restart, shutdown)

      {:ok, pid} ->
        # We mimic async but there is nothing to reply to
        alias = make_ref()
        send(pid, {self(), alias, alias, callers, mfa})
        {:ok, pid}

      {:error, _} = error ->
        error
    end
  end

  defp start_child_with_spec(supervisor, args, restart, shutdown) do
    GenServer.call(supervisor, {:start_task, args, restart, shutdown}, :infinity)
  end

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

  defp async(supervisor, link_type, module, fun, args, options) do
    owner = self()
    shutdown = options[:shutdown]

    case start_child_with_spec(supervisor, [get_owner(owner), :monitor], :temporary, shutdown) do
      {:ok, pid} ->
        if link_type == :link, do: Process.link(pid)
        alias = :erlang.monitor(:process, pid, alias: :demonitor)
        send(pid, {owner, alias, alias, get_callers(owner), {module, fun, args}})
        %Task{pid: pid, ref: alias, owner: owner, mfa: {module, fun, length(args)}}

      {:error, :max_children} ->
        raise """
        reached the maximum number of tasks for this task supervisor. The maximum number \
        of tasks that are allowed to run at the same time under this supervisor can be \
        configured with the :max_children option passed to Task.Supervisor.start_link/1\
        """
    end
  end

  defp build_stream(supervisor, link_type, enumerable, fun, options) do
    shutdown = Keyword.get(options, :shutdown, 5000)

    if not ((is_integer(shutdown) and shutdown >= 0) or shutdown == :brutal_kill) do
      raise ArgumentError, ":shutdown must be either a positive integer or :brutal_kill"
    end

    options = Task.Supervised.validate_stream_options(options)

    fn acc, acc_fun ->
      owner = get_owner(self())

      Task.Supervised.stream(enumerable, acc, acc_fun, get_callers(self()), fun, options, fn ->
        args = [owner, :monitor]

        case start_child_with_spec(supervisor, args, :temporary, shutdown) do
          {:ok, pid} ->
            if link_type == :link, do: Process.link(pid)
            {:ok, link_type, pid}

          {:error, :max_children} ->
            {:error, :max_children}
        end
      end)
    end
  end
end
