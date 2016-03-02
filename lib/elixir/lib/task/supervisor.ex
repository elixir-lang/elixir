defmodule Task.Supervisor do
  @moduledoc """
  A task supervisor.

  This module defines a supervisor which can be used to dynamically
  supervise tasks. Behind the scenes, this module is implemented as a
  `:simple_one_for_one` supervisor where the workers are temporary
  (i.e. they are not restarted after they die).

  See the `Task` module for more information.

  ## Name Registration

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
    Defaults to `:temporary` as most tasks can't be effectively restarted after
    a crash;

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
    do_async(supervisor, module, fun, args, :link)
  end

  @doc """
  Starts a task that can be awaited on.

  The `supervisor` must be a reference as defined in `Task.Supervisor`.
  The task won't be linked to the caller, see `Task.async/3` for
  more information.

  ## Compatibility with OTP behaviours

  If you create a task using `async_nolink` inside an OTP behaviour
  like `GenServer`, you should match on the message coming from the
  task inside your `handle_info` callback.

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
    do_async(supervisor, module, fun, args, :monitor)
  end

  @doc """
  Terminates the child with the given `pid`.
  """
  @spec terminate_child(Supervisor.supervisor, pid) :: :ok
  def terminate_child(supervisor, pid) when is_pid(pid) do
    Supervisor.terminate_child(supervisor, pid)
  end

  @doc """
  Returns all children pids.
  """
  @spec children(Supervisor.supervisor) :: [pid]
  def children(supervisor) do
    Supervisor.which_children(supervisor) |> Enum.map(&elem(&1, 1))
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
    Supervisor.start_child(supervisor, [get_info(self), {module, fun, args}])
  end

  defp get_info(self) do
    {node(),
     case Process.info(self, :registered_name) do
       {:registered_name, []} -> self()
       {:registered_name, name} -> name
     end}
  end

  defp do_async(supervisor, module, fun, args, link_type) do
    owner = self()
    args = [owner, link_type, get_info(owner), {module, fun, args}]
    {:ok, pid} = Supervisor.start_child(supervisor, args)
    if link_type == :link, do: Process.link(pid)
    ref = Process.monitor(pid)
    send pid, {owner, ref}
    %Task{pid: pid, ref: ref, owner: owner}
  end
end
