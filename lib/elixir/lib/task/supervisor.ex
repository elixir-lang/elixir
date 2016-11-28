defmodule Task.Supervisor do
  @moduledoc """
  A task supervisor.

  This module defines a supervisor which can be used to dynamically
  supervise tasks. Behind the scenes, this module is implemented as a
  `:simple_one_for_one` supervisor where the workers are temporary by
  default (that is, they are not restarted after they die; read the docs
  for `start_link/1` for more information on choosing the restart
  strategy).

  See the `Task` module for more information.

  ## Name registration

  A `Task.Supervisor` is bound to the same name registration rules as a
  `GenServer`. Read more about them in the `GenServer` docs.
  """

  @doc """
  Starts a new supervisor.

  The supported options are:

  * `:name` - used to register a supervisor name, the supported values are
    described under the `Name Registration` section in the `GenServer` module
    docs;

  * `:restart` - the restart strategy, may be `:temporary` (the default),
    `:transient` or `:permanent`. Check `Supervisor.Spec` for more info.
    Defaults to `:temporary` so tasks aren't automatically restarted when
    they complete nor in case of crashes;

  * `:shutdown` - `:brutal_kill` if the tasks must be killed directly on shutdown
    or an integer indicating the timeout value, defaults to 5000 milliseconds;

  * `:max_restarts` and `:max_seconds` - as specified in `Supervisor.Spec.supervise/2`;

  """
  @spec start_link(Supervisor.options) :: Supervisor.on_start
  def start_link(opts \\ []) do
    import Supervisor.Spec
    {restart, opts}  = Keyword.pop(opts, :restart, :temporary)
    {shutdown, opts} = Keyword.pop(opts, :shutdown, 5000)
    children = [worker(Task.Supervised, [], restart: restart, shutdown: shutdown)]
    Supervisor.start_link(children, [strategy: :simple_one_for_one] ++ opts)
  end

  @doc """
  Starts a task that can be awaited on.

  The `supervisor` must be a reference as defined in `Task.Supervisor`.
  The task will still be linked to the caller, see `Task.async/3` for
  more information and `async_nolink/2` for a non-linked variant.
  """
  @spec async(Supervisor.supervisor, fun) :: Task.t
  def async(supervisor, fun) do
    async(supervisor, :erlang, :apply, [fun, []])
  end

  @doc """
  Starts a task that can be awaited on.

  The `supervisor` must be a reference as defined in `Task.Supervisor`.
  The task will still be linked to the caller, see `Task.async/3` for
  more information and `async_nolink/2` for a non-linked variant.
  """
  @spec async(Supervisor.supervisor, module, atom, [term]) :: Task.t
  def async(supervisor, module, fun, args) do
    do_async(supervisor, :link, module, fun, args)
  end

  @doc """
  Starts a task that can be awaited on.

  The `supervisor` must be a reference as defined in `Task.Supervisor`.
  The task won't be linked to the caller, see `Task.async/3` for
  more information.

  ## Compatibility with OTP behaviours

  If you create a task using `async_nolink` inside an OTP behaviour
  like `GenServer`, you should match on the message coming from the
  task inside your `GenServer.handle_info/2` callback.

  The reply sent by the task will be in the format `{ref, result}`,
  where `ref` is the monitor reference held by the task struct
  and `result` is the return value of the task function.

  Keep in mind that, regardless of how the task created with `async_nolink`
  terminates, the caller's process will always receive a `:DOWN` message
  with the same `ref` value that is held by the task struct. If the task
  terminates normally, the reason in the `:DOWN` message will be `:normal`.
  """
  @spec async_nolink(Supervisor.supervisor, fun) :: Task.t
  def async_nolink(supervisor, fun) do
    async_nolink(supervisor, :erlang, :apply, [fun, []])
  end

  @doc """
  Starts a task that can be awaited on.

  The `supervisor` must be a reference as defined in `Task.Supervisor`.
  The task won't be linked to the caller, see `Task.async/3` for
  more information.
  """
  @spec async_nolink(Supervisor.supervisor, module, atom, [term]) :: Task.t
  def async_nolink(supervisor, module, fun, args) do
    do_async(supervisor, :nolink, module, fun, args)
  end

  @doc """
  Returns a stream that runs the given `module`, `function` and `args`
  concurrently on each item in `enumerable`.

  Each item will be appended to the given `args` and processed by its
  own task. The tasks will be spawned under the given `supervisor` and
  linked to the current process, similar to `async/4`.

  When streamed, each task will emit `{:ok, val}` upon successful
  completion or `{:exit, val}` if the caller is trapping exits. Results
  are emitted in the same order as the original `enumerable`.

  The level of concurrency can be controlled via the `:max_concurrency`
  option and defaults to `System.schedulers_online/1`. The timeout
  can also be given as option and defaults to 5000 and it defaults to
  the maximum amount of time to wait without a task reply.

  Finally, if you find yourself trapping exits to handle exits inside
  the async stream, consider using `async_stream_nolink/6` to start tasks
  that are not linked to the current process.

  ## Options

    * `:max_concurrency` - sets the maximum number of tasks to run
      at the same time. Defaults to `System.schedulers_online/1`.
    * `:timeout` - the maximum amount of time to wait without
      receiving a task reply (across all running tasks).

  ## Examples

  Let's build a stream and then enumerate it:

      stream = Task.Supervisor.async_stream(MySupervisor, collection, Mod, :expensive_fun, [])
      Enum.to_list(stream)
  """
  @spec async_stream(Supervisor.supervisor, Enumerable.t, module, atom, [term], Keyword.t) ::
        Enumerable.t
  def async_stream(supervisor, enumerable, module, function, args, options \\ [])
      when is_atom(module) and is_atom(function) and is_list(args) do
    build_stream(supervisor, :link, enumerable, {module, function, args}, options)
  end

  @doc """
  Returns a stream that runs the given `function` concurrently on each
  item in `enumerable`.

  Each item will be appended to the given `args` and processed by its
  own task. The tasks will be spawned under the given `supervisor` and
  are linked to the current process, similar to `async/2`.

  See `async_stream/6` for discussion and examples.
  """
  @spec async_stream(Supervisor.supervisor, Enumerable.t, (term -> term), Keyword.t) ::
        Enumerable.t
  def async_stream(supervisor, enumerable, fun, options \\ []) when is_function(fun, 1) do
    build_stream(supervisor, :link, enumerable, fun, options)
  end

  @doc """
  Returns a stream that runs the given `module`, `function` and `args`
  concurrently on each item in `enumerable`.

  Each item will be appended to the given `args` and processed by its
  own task. The tasks will be spawned under the given `supervisor` and
  are not linked to the current process, similar to `async_nolink/4`.

  See `async_stream/6` for discussion and examples.
  """
  @spec async_stream_nolink(Supervisor.supervisor, Enumerable.t, module, atom, [term], Keyword.t) ::
        Enumerable.t
  def async_stream_nolink(supervisor, enumerable, module, function, args, options \\ [])
      when is_atom(module) and is_atom(function) and is_list(args) do
    build_stream(supervisor, :nolink, enumerable, {module, function, args}, options)
  end

  @doc """
  Returns a stream that runs the given `function` concurrently on each
  item in `enumerable`.

  Each item will be appended to the given `args` and processed by its
  own task. The tasks will be spawned under the given `supervisor` and
  are not linked to the current process, similar to `async_nolink/2`.

  See `async_stream/6` for discussion and examples.
  """
  @spec async_stream_nolink(Supervisor.supervisor, Enumerable.t, (term -> term), Keyword.t) ::
        Enumerable.t
  def async_stream_nolink(supervisor, enumerable, fun, options \\ []) when is_function(fun, 1) do
    build_stream(supervisor, :nolink, enumerable, fun, options)
  end

  @doc """
  Terminates the child with the given `pid`.
  """
  @spec terminate_child(Supervisor.supervisor, pid) :: :ok
  def terminate_child(supervisor, pid) when is_pid(pid) do
    Supervisor.terminate_child(supervisor, pid)
  end

  @doc """
  Returns all children PIDs.
  """
  @spec children(Supervisor.supervisor) :: [pid]
  def children(supervisor) do
    for {_, pid, _, _} <- Supervisor.which_children(supervisor), is_pid(pid), do: pid
  end

  @doc """
  Starts a task as a child of the given `supervisor`.

  Note that the spawned process is not linked to the caller, but
  only to the supervisor. This command is useful in case the
  task needs to perform side-effects (like I/O) and does not need
  to report back to the caller.
  """
  @spec start_child(Supervisor.supervisor, fun) :: {:ok, pid}
  def start_child(supervisor, fun) do
    start_child(supervisor, :erlang, :apply, [fun, []])
  end

  @doc """
  Starts a task as a child of the given `supervisor`.

  Similar to `start_child/2` except the task is specified
  by the given `module`, `fun` and `args`.
  """
  @spec start_child(Supervisor.supervisor, module, atom, [term]) :: {:ok, pid}
  def start_child(supervisor, module, fun, args) do
    Supervisor.start_child(supervisor, [get_info(self()), {module, fun, args}])
  end

  defp get_info(self) do
    {node(),
     case Process.info(self, :registered_name) do
       {:registered_name, []} -> self()
       {:registered_name, name} -> name
     end}
  end

  defp do_async(supervisor, link_type, module, fun, args) do
    owner = self()
    args = [owner, :monitor, get_info(owner), {module, fun, args}]
    {:ok, pid} = Supervisor.start_child(supervisor, args)
    if link_type == :link, do: Process.link(pid)
    ref = Process.monitor(pid)
    send pid, {owner, ref}
    %Task{pid: pid, ref: ref, owner: owner}
  end

  defp build_stream(supervisor, link_type, enumerable, fun, options) do
    &Task.Supervised.stream(enumerable, &1, &2, fun, options, fn owner, mfa ->
      args = [owner, :monitor, get_info(owner), mfa]
      {:ok, pid} = Supervisor.start_child(supervisor, args)
      if link_type == :link, do: Process.link(pid)
      {link_type, pid}
    end)
  end
end
