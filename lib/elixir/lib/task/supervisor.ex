defmodule Task.Supervisor do
  @moduledoc """
  A tasks supervisor.

  This module defines a supervisor which can be used to dynamically
  supervise tasks. Behind the scenes, this module is implemented as a
  `:simple_one_for_one` supervisor where the workers are temporary
  (i.e. they are not restarted after they die).

  The functions in this module allow tasks can be spawned and awaited
  from a supervisor, similar to the functions defined in the `Task` module.

  ## Name registering

  A `Task.Supervisor` is bound to the same name registering rules as a
  `GenServer`. Read more about it in the `GenServer` docs.
  """

  @doc """
  Starts a new supervisor.

  The supported options are:

  * `:name` - used to register a supervisor name, the supported values are
    described under the `Name Registering` section in the `GenServer` module
    docs;

  * `:shutdown` - `:brutal_kill` if the tasks must be killed directly on shutdown
    or an integer indicating the timeout value, defaults to 5000 miliseconds;
  """
  @spec start_link(Supervisor.options) :: Supervisor.on_start
  def start_link(opts \\ []) do
    import Supervisor.Spec
    {shutdown, opts} = Keyword.pop(opts, :shutdown, 5000)
    children = [worker(Task.Supervised, [], restart: :temporary, shutdown: shutdown)]
    Supervisor.start_link(children, [strategy: :simple_one_for_one] ++ opts)
  end

  @doc """
  Starts a task that can be awaited on.

  The `supervisor` must be a reference as defined in `Task.Supervisor`.
  For more information on tasks, check the `Task` module.
  """
  @spec async(Supervisor.supervisor, fun) :: Task.t
  def async(supervisor, fun) do
    async(supervisor, :erlang, :apply, [fun, []])
  end

  @doc """
  Starts a task that can be awaited on.

  The `supervisor` must be a reference as defined in `Task.Supervisor`.
  For more information on tasks, check the `Task` module.
  """
  @spec async(Supervisor.supervisor, module, atom, [term]) :: Task.t
  def async(supervisor, module, fun, args) do
    {:ok, pid} = Supervisor.start_child(supervisor, [self(), {module, fun, args}])
    ref = Process.monitor(pid)
    send pid, {self(), ref}
    %Task{pid: pid, ref: ref}
  end

  @doc """
  Terminates the given child at pid.
  """
  @spec terminate_child(Supervisor.supervisor, pid) :: :ok
  def terminate_child(supervisor, pid) when is_pid(pid) do
    :supervisor.terminate_child(supervisor, pid)
  end

  @doc """
  Returns all children pids.
  """
  @spec children(Supervisor.supervisor) :: [pid]
  def children(supervisor) do
    :supervisor.which_children(supervisor) |> Enum.map(&elem(&1, 1))
  end

  @doc """
  Starts a task as child of the given `supervisor`.

  Note the spawned process is not linked to the caller but
  only to the supervisor. This command is useful in case the
  task needs to emit side-effects (like I/O) and does not need
  to report back to the caller.
  """
  @spec start_child(Supervisor.supervisor, fun) :: {:ok, pid}
  def start_child(supervisor, fun) do
    start_child(supervisor, :erlang, :apply, [fun, []])
  end

  @doc """
  Starts a task as child of the given `supervisor`.

  Similar to `start_child/2` except the task is specified
  by the given `module`, `fun` and `args`.
  """
  @spec start_child(Supervisor.supervisor, module, atom, [term]) :: {:ok, pid}
  def start_child(supervisor, module, fun, args) do
    Supervisor.start_child(supervisor, [:undefined, {module, fun, args}])
  end
end
