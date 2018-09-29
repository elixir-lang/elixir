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

  See the `Task` module for more examples.

  ## Name registration

  A `Task.Supervisor` is bound to the same name registration rules as a
  `GenServer`. Read more about them in the `GenServer` docs.
  """

  @typedoc "Option values used by `start_link`"
  @type option ::
          Supervisor.option()
          | {:restart, :supervisor.restart()}
          | {:shutdown, :supervisor.shutdown()}

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

    * `:max_restarts`, `:max_seconds` and `:max_children` - as specified in
      `DynamicSupervisor`;

  This function could also receive `:restart` and `:shutdown` as options
  but those two options have been deprecated and it is now preferred to
  give them directly to `start_child` and `async`.
  """
  @spec start_link([option]) :: Supervisor.on_start()
  # TODO: Deprecate passing restart and shutdown here on Elixir v1.9.
  def start_link(options \\ []) do
    {restart, options} = Keyword.pop(options, :restart, :temporary)
    {shutdown, options} = Keyword.pop(options, :shutdown, 5000)

    keys = [:max_children, :max_seconds, :max_restarts]
    {sup_opts, start_opts} = Keyword.split(options, keys)
    DynamicSupervisor.start_link(__MODULE__, {{restart, shutdown}, sup_opts}, start_opts)
  end

  @doc false
  def init({{_restart, _shutdown} = arg, options}) do
    Process.put(__MODULE__, arg)
    DynamicSupervisor.init([strategy: :one_for_one] ++ options)
  end

  @doc """
  Starts a task that can be awaited on.

  The `supervisor` must be a reference as defined in `Task.Supervisor`.
  The task will still be linked to the caller, see `Task.async/3` for
  more information and `async_nolink/2` for a non-linked variant.

  ## Options

    * `:shutdown` - `:brutal_kill` if the tasks must be killed directly on shutdown
      or an integer indicating the timeout value, defaults to 5000 milliseconds.

  """
  @spec async(Supervisor.supervisor(), (() -> any), Keyword.t()) :: Task.t()
  def async(supervisor, fun, options \\ []) do
    async(supervisor, :erlang, :apply, [fun, []], options)
  end

  @doc """
  Starts a task that can be awaited on.

  The `supervisor` must be a reference as defined in `Task.Supervisor`.
  The task will still be linked to the caller, see `Task.async/3` for
  more information and `async_nolink/2` for a non-linked variant.

  ## Options

    * `:shutdown` - `:brutal_kill` if the tasks must be killed directly on shutdown
      or an integer indicating the timeout value, defaults to 5000 milliseconds.

  """
  @spec async(Supervisor.supervisor(), module, atom, [term], Keyword.t()) :: Task.t()
  def async(supervisor, module, fun, args, options \\ []) do
    async(supervisor, :link, module, fun, args, options)
  end

  @doc """
  Starts a task that can be awaited on.

  The `supervisor` must be a reference as defined in `Task.Supervisor`.
  The task won't be linked to the caller, see `Task.async/3` for
  more information.

  ## Options

    * `:shutdown` - `:brutal_kill` if the tasks must be killed directly on shutdown
      or an integer indicating the timeout value, defaults to 5000 milliseconds.

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
        def handle_call(:start_task, %{ref: ref} = state) when is_reference(ref) do
          {:reply, :ok, state}
        end

        # The task is not running yet, so let's start it.
        def handle_cast(:start_task, %{ref: nil} = state) do
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
  @spec async_nolink(Supervisor.supervisor(), (() -> any), Keyword.t()) :: Task.t()
  def async_nolink(supervisor, fun, options \\ []) do
    async_nolink(supervisor, :erlang, :apply, [fun, []], options)
  end

  @doc """
  Starts a task that can be awaited on.

  The `supervisor` must be a reference as defined in `Task.Supervisor`.
  The task won't be linked to the caller, see `Task.async/3` for
  more information.

  Note this function requires the task supervisor to have `:temporary`
  as the `:restart` option (the default), as `async_nolink/4` keeps a
  direct reference to the task which is lost if the task is restarted.
  """
  @spec async_nolink(Supervisor.supervisor(), module, atom, [term], Keyword.t()) :: Task.t()
  def async_nolink(supervisor, module, fun, args, options \\ []) do
    async(supervisor, :nolink, module, fun, args, options)
  end

  @doc """
  Returns a stream that runs the given `module`, `function`, and `args`
  concurrently on each item in `enumerable`.

  Each item will be prepended to the given `args` and processed by its
  own task. The tasks will be spawned under the given `supervisor` and
  linked to the current process, similarly to `async/4`.

  When streamed, each task will emit `{:ok, value}` upon successful
  completion or `{:exit, reason}` if the caller is trapping exits.
  Results are emitted in the same order as the original `enumerable`.

  The level of concurrency can be controlled via the `:max_concurrency`
  option and defaults to `System.schedulers_online/0`. A timeout
  can also be given as an option representing the maximum amount of
  time to wait without a task reply.

  Finally, if you find yourself trapping exits to handle exits inside
  the async stream, consider using `async_stream_nolink/6` to start tasks
  that are not linked to the current process.

  ## Options

    * `:max_concurrency` - sets the maximum number of tasks to run
      at the same time. Defaults to `System.schedulers_online/0`.
    * `:ordered` - whether the results should be returned in the same order
      as the input stream. This option is useful when you have large
      streams and don't want to buffer results before they are delivered.
      Defaults to `true`.
    * `:timeout` - the maximum amount of time to wait (in milliseconds)
      without receiving a task reply (across all running tasks).
      Defaults to `5000`.
    * `:on_timeout` - what do to when a task times out. The possible
      values are:
      * `:exit` (default) - the process that spawned the tasks exits.
      * `:kill_task` - the task that timed out is killed. The value
        emitted for that task is `{:exit, :timeout}`.
    * `:shutdown` - `:brutal_kill` if the tasks must be killed directly on shutdown
      or an integer indicating the timeout value, defaults to 5000 milliseconds.

  ## Examples

  Let's build a stream and then enumerate it:

      stream = Task.Supervisor.async_stream(MySupervisor, collection, Mod, :expensive_fun, [])
      Enum.to_list(stream)

  """
  @doc since: "1.4.0"
  @spec async_stream(Supervisor.supervisor(), Enumerable.t(), module, atom, [term], keyword) ::
          Enumerable.t()
  def async_stream(supervisor, enumerable, module, function, args, options \\ [])
      when is_atom(module) and is_atom(function) and is_list(args) do
    build_stream(supervisor, :link, enumerable, {module, function, args}, options)
  end

  @doc """
  Returns a stream that runs the given function `fun` concurrently
  on each item in `enumerable`.

  Each item in `enumerable` is passed as argument to the given function `fun`
  and processed by its own task. The tasks will be spawned under the given
  `supervisor` and linked to the current process, similarly to `async/2`.

  See `async_stream/6` for discussion, options, and examples.
  """
  @doc since: "1.4.0"
  @spec async_stream(Supervisor.supervisor(), Enumerable.t(), (term -> term), keyword) ::
          Enumerable.t()
  def async_stream(supervisor, enumerable, fun, options \\ []) when is_function(fun, 1) do
    build_stream(supervisor, :link, enumerable, fun, options)
  end

  @doc """
  Returns a stream that runs the given `module`, `function`, and `args`
  concurrently on each item in `enumerable`.

  Each item in `enumerable` will be prepended to the given `args` and processed
  by its own task. The tasks will be spawned under the given `supervisor` and
  will not be linked to the current process, similarly to `async_nolink/4`.

  See `async_stream/6` for discussion, options, and examples.
  """
  @doc since: "1.4.0"
  @spec async_stream_nolink(
          Supervisor.supervisor(),
          Enumerable.t(),
          module,
          atom,
          [term],
          keyword
        ) :: Enumerable.t()
  def async_stream_nolink(supervisor, enumerable, module, function, args, options \\ [])
      when is_atom(module) and is_atom(function) and is_list(args) do
    build_stream(supervisor, :nolink, enumerable, {module, function, args}, options)
  end

  @doc """
  Returns a stream that runs the given `function` concurrently on each
  item in `enumerable`.

  Each item in `enumerable` is passed as argument to the given function `fun`
  and processed by its own task. The tasks will be spawned under the given
  `supervisor` and will not be linked to the current process, similarly to `async_nolink/2`.

  See `async_stream/6` for discussion and examples.
  """
  @doc since: "1.4.0"
  @spec async_stream_nolink(Supervisor.supervisor(), Enumerable.t(), (term -> term), keyword) ::
          Enumerable.t()
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
  Returns all children PIDs.
  """
  @spec children(Supervisor.supervisor()) :: [pid]
  def children(supervisor) do
    for {_, pid, _, _} <- DynamicSupervisor.which_children(supervisor), is_pid(pid), do: pid
  end

  @doc """
  Starts a task as a child of the given `supervisor`.

  Note that the spawned process is not linked to the caller, but
  only to the supervisor. This command is useful in case the
  task needs to perform side-effects (like I/O) and does not need
  to report back to the caller.

  ## Options

    * `:restart` - the restart strategy, may be `:temporary` (the default),
      `:transient` or `:permanent`. `:temporary` means the task is never
      restarted, `:transient` means it is restarted if the exit is not
      `:normal`, `:shutdown` or `{:shutdown, reason}`. A `:permanent` restart
      strategy means it is always restarted. It defaults to `:temporary`.

    * `:shutdown` - `:brutal_kill` if the tasks must be killed directly on shutdown
      or an integer indicating the timeout value, defaults to 5000 milliseconds.

  """
  @spec start_child(Supervisor.supervisor(), (() -> any), keyword) ::
          DynamicSupervisor.on_start_child()
  def start_child(supervisor, fun, options \\ []) do
    restart = options[:restart]
    shutdown = options[:shutdown]
    args = [get_info(self()), {:erlang, :apply, [fun, []]}]
    start_child_with_spec(supervisor, args, restart, shutdown)
  end

  @doc """
  Starts a task as a child of the given `supervisor`.

  Similar to `start_child/2` except the task is specified
  by the given `module`, `fun` and `args`.
  """
  @spec start_child(Supervisor.supervisor(), module, atom, [term], keyword) ::
          DynamicSupervisor.on_start_child()
  def start_child(supervisor, module, fun, args, options \\ [])
      when is_atom(fun) and is_list(args) do
    restart = options[:restart]
    shutdown = options[:shutdown]
    args = [get_info(self()), {module, fun, args}]
    start_child_with_spec(supervisor, args, restart, shutdown)
  end

  defp start_child_with_spec(supervisor, args, restart, shutdown) do
    # TODO: This only exists because we need to support reading restart/shutdown
    # from two different places. Remove this and the associated clause in DynamicSupervisor
    # on Elixir v2.0
    GenServer.call(supervisor, {:start_task, args, restart, shutdown}, :infinity)
  end

  defp get_info(self) do
    name =
      case Process.info(self, :registered_name) do
        {:registered_name, name} when is_atom(name) -> name
        _ -> self
      end

    {node(), name}
  end

  defp async(supervisor, link_type, module, fun, args, options) do
    owner = self()
    args = [owner, :monitor, get_info(owner), {module, fun, args}]
    shutdown = options[:shutdown]

    case start_child_with_spec(supervisor, args, :temporary, shutdown) do
      {:ok, pid} ->
        if link_type == :link, do: Process.link(pid)
        ref = Process.monitor(pid)
        send(pid, {owner, ref})
        %Task{pid: pid, ref: ref, owner: owner}

      {:error, :max_children} ->
        raise """
        reached the maximum number of tasks for this task supervisor. The maximum number \
        of tasks that are allowed to run at the same time under this supervisor can be \
        configured with the :max_children option passed to Task.Supervisor.start_link/1\
        """
    end
  end

  defp build_stream(supervisor, link_type, enumerable, fun, options) do
    shutdown = options[:shutdown]

    &Task.Supervised.stream(enumerable, &1, &2, fun, options, fn owner, mfa ->
      args = [owner, :monitor, get_info(owner), mfa]
      {:ok, pid} = start_child_with_spec(supervisor, args, :temporary, shutdown)
      if link_type == :link, do: Process.link(pid)
      {link_type, pid}
    end)
  end
end
