defmodule DynamicSupervisor do
  @moduledoc ~S"""
  A supervisor that starts children dynamically.

  The `Supervisor` module was designed to handle mostly static children
  that are started in the given order when the supervisor starts. A
  `DynamicSupervisor` starts with no children. Instead, children are
  started on demand via `start_child/2`. When a dynamic supervisor
  terminates, all children are shut down at the same time, with no guarantee
  of ordering.

  ## Examples

  A dynamic supervisor is started with no children, often under a
  supervisor with the supervision strategy (the only strategy currently
  supported is `:one_for_one`) and a name:

      children = [
        {DynamicSupervisor, strategy: :one_for_one, name: MyApp.DynamicSupervisor}
      ]

      Supervisor.start_link(children, strategy: :one_for_one)

  The options given in the child specification are documented in `start_link/1`.

  Once the dynamic supervisor is running, we can start children
  with `start_child/2`, which receives a child specification:

      {:ok, agent1} = DynamicSupervisor.start_child(MyApp.DynamicSupervisor, {Agent, fn -> %{} end})
      Agent.update(agent1, &Map.put(&1, :key, "value"))
      Agent.get(agent1, & &1)
      #=> %{key: "value"}

      {:ok, agent2} = DynamicSupervisor.start_child(MyApp.DynamicSupervisor, {Agent, fn -> %{} end})
      Agent.get(agent2, & &1)
      #=> %{}

      DynamicSupervisor.count_children(MyApp.DynamicSupervisor)
      #=> %{active: 2, specs: 2, supervisors: 0, workers: 2}

  ## Module-based supervisors

  Similar to `Supervisor`, dynamic supervisors also support module-based
  supervisors.

      defmodule MyApp.DynamicSupervisor do
        # Automatically defines child_spec/1
        use DynamicSupervisor

        def start_link(arg) do
          DynamicSupervisor.start_link(__MODULE__, arg, name: __MODULE__)
        end

        @impl true
        def init(_arg) do
          DynamicSupervisor.init(strategy: :one_for_one)
        end
      end

  See the `Supervisor` docs for a discussion of when you may want to use
  module-based supervisors. The `@doc` annotation immediately preceding
  `use DymamicSupervisor` will be attached to the generated `child_spec/1`
  function.

  ## Name registration

  A supervisor is bound to the same name registration rules as a `GenServer`.
  Read more about these rules in the documentation for `GenServer`.

  ## Migrating from Supervisor's :simple_one_for_one

  In case you were using the deprecated `:simple_one_for_one` strategy from
  the `Supervisor` module, you can migrate to the `DynamicSupervisor` in
  few steps.

  Imagine the given "old" code:

      defmodule MySupervisor do
        use Supervisor

        def start_link(arg) do
          Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
        end

        def start_child(foo, bar, baz) do
          # This will start child by calling MyWorker.start_link(initial_arg, foo, bar, baz)
          Supervisor.start_child(__MODULE__, [foo, bar, baz])
        end

        @impl true
        def init(initial_arg) do
          children = [
            # Or the deprecated: worker(MyWorker, [initial_arg])
            %{id: MyWorker, start: {MyWorker, :start_link, [initial_arg]}}
          ]

          Supervisor.init(children, strategy: :simple_one_for_one)
        end
      end

  It can be upgraded to the DynamicSupervisor like this:

      defmodule MySupervisor do
        use DynamicSupervisor

        def start_link(arg) do
          DynamicSupervisor.start_link(__MODULE__, arg, name: __MODULE__)
        end

        def start_child(foo, bar, baz) do
          # If MyWorker is not using the new child specs, we need to pass a map:
          # spec = %{id: MyWorker, start: {MyWorker, :start_link, [foo, bar, baz]}}
          spec = {MyWorker, foo: foo, bar: bar, baz: baz}
          DynamicSupervisor.start_child(__MODULE__, spec)
        end

        @impl true
        def init(initial_arg) do
          DynamicSupervisor.init(
            strategy: :one_for_one,
            extra_arguments: [initial_arg]
          )
        end
      end

  The difference is that the `DynamicSupervisor` expects the child specification
  at the moment `start_child/2` is called, and no longer on the init callback.
  If there are any initial arguments given on initialization, such as `[initial_arg]`,
  it can be given in the `:extra_arguments` flag on `DynamicSupervisor.init/1`.
  """

  @behaviour GenServer

  @doc """
  Callback invoked to start the supervisor and during hot code upgrades.

  Developers typically invoke `DynamicSupervisor.init/1` at the end of
  their init callback to return the proper supervision flags.
  """
  @callback init(init_arg :: term) :: {:ok, sup_flags()} | :ignore

  @typedoc "The supervisor flags returned on init"
  @type sup_flags() :: %{
          strategy: strategy(),
          intensity: non_neg_integer(),
          period: pos_integer(),
          max_children: non_neg_integer() | :infinity,
          extra_arguments: [term()]
        }

  @typedoc "Option values used by the `start*` functions"
  @type option :: {:name, Supervisor.name()} | init_option()

  @typedoc "Options used by the `start*` functions"
  @type options :: [option, ...]

  @typedoc "Options given to `start_link/2` and `init/1`"
  @type init_option ::
          {:strategy, strategy()}
          | {:max_restarts, non_neg_integer()}
          | {:max_seconds, pos_integer()}
          | {:max_children, non_neg_integer() | :infinity}
          | {:extra_arguments, [term()]}

  @typedoc "Supported strategies"
  @type strategy :: :one_for_one

  @typedoc "Return values of `start_child` functions"
  @type on_start_child ::
          {:ok, pid}
          | {:ok, pid, info :: term}
          | :ignore
          | {:error, {:already_started, pid} | :max_children | term}

  # In this struct, `args` refers to the arguments passed to init/1 (the `init_arg`).
  defstruct [
    :args,
    :extra_arguments,
    :mod,
    :name,
    :strategy,
    :max_children,
    :max_restarts,
    :max_seconds,
    children: %{},
    restarts: []
  ]

  @doc """
  Returns a specification to start a dynamic supervisor under a supervisor.

  See `Supervisor`.
  """
  @doc since: "1.6.1"
  def child_spec(opts) when is_list(opts) do
    id =
      case Keyword.get(opts, :name, DynamicSupervisor) do
        name when is_atom(name) -> name
        {:global, name} -> name
        {:via, _module, name} -> name
      end

    %{
      id: id,
      start: {DynamicSupervisor, :start_link, [opts]},
      type: :supervisor
    }
  end

  @doc false
  defmacro __using__(opts) do
    quote location: :keep, bind_quoted: [opts: opts] do
      @behaviour DynamicSupervisor
      if Module.get_attribute(__MODULE__, :doc) == nil do
        @doc """
        Returns a specification to start this module under a supervisor.

        See `Supervisor`.
        """
      end

      def child_spec(arg) do
        default = %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [arg]},
          type: :supervisor
        }

        Supervisor.child_spec(default, unquote(Macro.escape(opts)))
      end

      defoverridable child_spec: 1
    end
  end

  @doc """
  Starts a supervisor with the given options.

  The `:strategy` is a required option and the currently supported
  value is `:one_for_one`. The remaining options can be found in the
  `init/1` docs.

  The `:name` option can also be used to register a supervisor name.
  The supported values are described under the "Name registration"
  section in the `GenServer` module docs.

  If the supervisor is successfully spawned, this function returns
  `{:ok, pid}`, where `pid` is the PID of the supervisor. If the supervisor
  is given a name and a process with the specified name already exists,
  the function returns `{:error, {:already_started, pid}}`, where `pid`
  is the PID of that process.

  Note that a supervisor started with this function is linked to the parent
  process and exits not only on crashes but also if the parent process exits
  with `:normal` reason.
  """
  @doc since: "1.6.0"
  @spec start_link(options) :: Supervisor.on_start()
  def start_link(options) when is_list(options) do
    keys = [:extra_arguments, :max_children, :max_seconds, :max_restarts, :strategy]
    {sup_opts, start_opts} = Keyword.split(options, keys)
    start_link(Supervisor.Default, init(sup_opts), start_opts)
  end

  @doc """
  Starts a module-based supervisor process with the given `module` and `arg`.

  To start the supervisor, the `c:init/1` callback will be invoked in the given
  `module`, with `arg` as its argument. The `c:init/1` callback must return a
  supervisor specification which can be created with the help of the `init/1`
  function.

  If the `c:init/1` callback returns `:ignore`, this function returns
  `:ignore` as well and the supervisor terminates with reason `:normal`.
  If it fails or returns an incorrect value, this function returns
  `{:error, term}` where `term` is a term with information about the
  error, and the supervisor terminates with reason `term`.

  The `:name` option can also be given in order to register a supervisor
  name, the supported values are described in the "Name registration"
  section in the `GenServer` module docs.
  """
  @doc since: "1.6.0"
  @spec start_link(module, term, GenServer.options()) :: Supervisor.on_start()
  def start_link(mod, init_arg, opts \\ []) do
    GenServer.start_link(__MODULE__, {mod, init_arg, opts[:name]}, opts)
  end

  @doc """
  Dynamically adds a child specification to `supervisor` and starts that child.

  `child_spec` should be a valid child specification as detailed in the
  "child_spec/1" section of the documentation for `Supervisor`. The child
  process will be started as defined in the child specification.

  If the child process start function returns `{:ok, child}` or `{:ok, child,
  info}`, then child specification and PID are added to the supervisor and
  this function returns the same value.

  If the child process start function returns `:ignore`, then no child is added
  to the supervision tree and this function returns `:ignore` too.

  If the child process start function returns an error tuple or an erroneous
  value, or if it fails, the child specification is discarded and this function
  returns `{:error, error}` where `error` is a term containing information about
  the error and child specification.

  If the supervisor already has N children in a way that N exceeds the amount
  of `:max_children` set on the supervisor initialization (see `init/1`), then
  this function returns `{:error, :max_children}`.
  """
  @doc since: "1.6.0"
  @spec start_child(Supervisor.supervisor(), Supervisor.child_spec() | {module, term} | module) ::
          on_start_child()
  def start_child(supervisor, {_, _, _, _, _, _} = child_spec) do
    validate_and_start_child(supervisor, child_spec)
  end

  def start_child(supervisor, child_spec) do
    validate_and_start_child(supervisor, Supervisor.child_spec(child_spec, []))
  end

  defp validate_and_start_child(supervisor, child_spec) do
    case validate_child(child_spec) do
      {:ok, child} -> call(supervisor, {:start_child, child})
      error -> {:error, error}
    end
  end

  defp validate_child(%{id: _, start: {mod, _, _} = start} = child) do
    restart = Map.get(child, :restart, :permanent)
    type = Map.get(child, :type, :worker)
    modules = Map.get(child, :modules, [mod])

    shutdown =
      case type do
        :worker -> Map.get(child, :shutdown, 5_000)
        :supervisor -> Map.get(child, :shutdown, :infinity)
      end

    validate_child(start, restart, shutdown, type, modules)
  end

  defp validate_child({_, start, restart, shutdown, type, modules}) do
    validate_child(start, restart, shutdown, type, modules)
  end

  defp validate_child(other) do
    {:invalid_child_spec, other}
  end

  defp validate_child(start, restart, shutdown, type, modules) do
    with :ok <- validate_start(start),
         :ok <- validate_restart(restart),
         :ok <- validate_shutdown(shutdown),
         :ok <- validate_type(type),
         :ok <- validate_modules(modules) do
      {:ok, {start, restart, shutdown, type, modules}}
    end
  end

  defp validate_start({m, f, args}) when is_atom(m) and is_atom(f) and is_list(args), do: :ok
  defp validate_start(mfa), do: {:invalid_mfa, mfa}

  defp validate_type(type) when type in [:supervisor, :worker], do: :ok
  defp validate_type(type), do: {:invalid_child_type, type}

  defp validate_restart(restart) when restart in [:permanent, :temporary, :transient], do: :ok
  defp validate_restart(restart), do: {:invalid_restart_type, restart}

  defp validate_shutdown(shutdown) when is_integer(shutdown) and shutdown > 0, do: :ok
  defp validate_shutdown(shutdown) when shutdown in [:infinity, :brutal_kill], do: :ok
  defp validate_shutdown(shutdown), do: {:invalid_shutdown, shutdown}

  defp validate_modules(:dynamic), do: :ok

  defp validate_modules(mods) do
    if is_list(mods) and Enum.all?(mods, &is_atom/1) do
      :ok
    else
      {:invalid_modules, mods}
    end
  end

  @doc """
  Terminates the given child identified by `pid`.

  If successful, this function returns `:ok`. If there is no process with
  the given PID, this function returns `{:error, :not_found}`.
  """
  @doc since: "1.6.0"
  @spec terminate_child(Supervisor.supervisor(), pid) :: :ok | {:error, :not_found}
  def terminate_child(supervisor, pid) when is_pid(pid) do
    call(supervisor, {:terminate_child, pid})
  end

  @doc """
  Returns a list with information about all children.

  Note that calling this function when supervising a large number
  of children under low memory conditions can cause an out of memory
  exception.

  This function returns a list of tuples containing:

    * `id` - it is always `:undefined` for dynamic supervisors

    * `child` - the PID of the corresponding child process or the
      atom `:restarting` if the process is about to be restarted

    * `type` - `:worker` or `:supervisor` as defined in the child
      specification

    * `modules` - as defined in the child specification

  """
  @doc since: "1.6.0"
  @spec which_children(Supervisor.supervisor()) :: [
          {:undefined, pid | :restarting, :worker | :supervisor, :supervisor.modules()}
        ]
  def which_children(supervisor) do
    call(supervisor, :which_children)
  end

  @doc """
  Returns a map containing count values for the supervisor.

  The map contains the following keys:

    * `:specs` - the number of children processes

    * `:active` - the count of all actively running child processes managed by
      this supervisor

    * `:supervisors` - the count of all supervisors whether or not the child
      process is still alive

    * `:workers` - the count of all workers, whether or not the child process
      is still alive

  """
  @doc since: "1.6.0"
  @spec count_children(Supervisor.supervisor()) :: %{
          specs: non_neg_integer,
          active: non_neg_integer,
          supervisors: non_neg_integer,
          workers: non_neg_integer
        }
  def count_children(supervisor) do
    call(supervisor, :count_children) |> :maps.from_list()
  end

  @doc """
  Synchronously stops the given supervisor with the given `reason`.

  It returns `:ok` if the supervisor terminates with the given
  reason. If it terminates with another reason, the call exits.

  This function keeps OTP semantics regarding error reporting.
  If the reason is any other than `:normal`, `:shutdown` or
  `{:shutdown, _}`, an error report is logged.
  """
  @doc since: "1.7.0"
  @spec stop(Supervisor.supervisor(), reason :: term, timeout) :: :ok
  def stop(supervisor, reason \\ :normal, timeout \\ :infinity) do
    GenServer.stop(supervisor, reason, timeout)
  end

  @doc """
  Receives a set of `options` that initializes a dynamic supervisor.

  This is typically invoked at the end of the `c:init/1` callback of
  module-based supervisors. See the "Module-based supervisors" section
  in the module documentation for more information.

  The `options` received by this function are also supported by `start_link/2`.

  This function returns a tuple containing the supervisor options.

  ## Examples

      def init(_arg) do
        DynamicSupervisor.init(max_children: 1000, strategy: :one_for_one)
      end

  ## Options

    * `:strategy` - the restart strategy option. The only supported
      value is `:one_for_one` which means that no other child is
      terminated if a child process terminates. You can learn more
      about strategies in the `Supervisor` module docs.

    * `:max_restarts` - the maximum number of restarts allowed in
      a time frame. Defaults to `3`.

    * `:max_seconds` - the time frame in which `:max_restarts` applies.
      Defaults to `5`.

    * `:max_children` - the maximum amount of children to be running
      under this supervisor at the same time. When `:max_children` is
      exceeded, `start_child/2` returns `{:error, :max_children}`. Defaults
      to `:infinity`.

    * `:extra_arguments` - arguments that are prepended to the arguments
      specified in the child spec given to `start_child/2`. Defaults to
      an empty list.

  """
  @doc since: "1.6.0"
  @spec init([init_option]) :: {:ok, sup_flags()}
  def init(options) when is_list(options) do
    unless strategy = options[:strategy] do
      raise ArgumentError, "expected :strategy option to be given"
    end

    intensity = Keyword.get(options, :max_restarts, 3)
    period = Keyword.get(options, :max_seconds, 5)
    max_children = Keyword.get(options, :max_children, :infinity)
    extra_arguments = Keyword.get(options, :extra_arguments, [])

    flags = %{
      strategy: strategy,
      intensity: intensity,
      period: period,
      max_children: max_children,
      extra_arguments: extra_arguments
    }

    {:ok, flags}
  end

  ## Callbacks

  @impl true
  def init({mod, init_arg, name}) do
    Process.put(:"$initial_call", {:supervisor, mod, 1})
    Process.flag(:trap_exit, true)

    case mod.init(init_arg) do
      {:ok, flags} when is_map(flags) ->
        name =
          cond do
            is_nil(name) -> {self(), mod}
            is_atom(name) -> {:local, name}
            is_tuple(name) -> name
          end

        state = %DynamicSupervisor{mod: mod, args: init_arg, name: name}

        case init(state, flags) do
          {:ok, state} -> {:ok, state}
          {:error, reason} -> {:stop, {:supervisor_data, reason}}
        end

      :ignore ->
        :ignore

      other ->
        {:stop, {:bad_return, {mod, :init, other}}}
    end
  end

  defp init(state, flags) do
    extra_arguments = Map.get(flags, :extra_arguments, [])
    max_children = Map.get(flags, :max_children, :infinity)
    max_restarts = Map.get(flags, :intensity, 1)
    max_seconds = Map.get(flags, :period, 5)
    strategy = Map.get(flags, :strategy, :one_for_one)

    with :ok <- validate_strategy(strategy),
         :ok <- validate_restarts(max_restarts),
         :ok <- validate_seconds(max_seconds),
         :ok <- validate_dynamic(max_children),
         :ok <- validate_extra_arguments(extra_arguments) do
      {:ok,
       %{
         state
         | extra_arguments: extra_arguments,
           max_children: max_children,
           max_restarts: max_restarts,
           max_seconds: max_seconds,
           strategy: strategy
       }}
    end
  end

  defp validate_strategy(strategy) when strategy in [:one_for_one], do: :ok
  defp validate_strategy(strategy), do: {:error, {:invalid_strategy, strategy}}

  defp validate_restarts(restart) when is_integer(restart) and restart >= 0, do: :ok
  defp validate_restarts(restart), do: {:error, {:invalid_intensity, restart}}

  defp validate_seconds(seconds) when is_integer(seconds) and seconds > 0, do: :ok
  defp validate_seconds(seconds), do: {:error, {:invalid_period, seconds}}

  defp validate_dynamic(:infinity), do: :ok
  defp validate_dynamic(dynamic) when is_integer(dynamic) and dynamic >= 0, do: :ok
  defp validate_dynamic(dynamic), do: {:error, {:invalid_max_children, dynamic}}

  defp validate_extra_arguments(list) when is_list(list), do: :ok
  defp validate_extra_arguments(extra), do: {:error, {:invalid_extra_arguments, extra}}

  @impl true
  def handle_call(:which_children, _from, state) do
    %{children: children} = state

    reply =
      for {pid, args} <- children do
        case args do
          {:restarting, {_, _, _, type, modules}} ->
            {:undefined, :restarting, type, modules}

          {_, _, _, type, modules} ->
            {:undefined, pid, type, modules}
        end
      end

    {:reply, reply, state}
  end

  def handle_call(:count_children, _from, state) do
    %{children: children} = state
    specs = map_size(children)

    {active, workers, supervisors} =
      Enum.reduce(children, {0, 0, 0}, fn
        {_pid, {:restarting, {_, _, _, :worker, _}}}, {active, worker, supervisor} ->
          {active, worker + 1, supervisor}

        {_pid, {:restarting, {_, _, _, :supervisor, _}}}, {active, worker, supervisor} ->
          {active, worker, supervisor + 1}

        {_pid, {_, _, _, :worker, _}}, {active, worker, supervisor} ->
          {active + 1, worker + 1, supervisor}

        {_pid, {_, _, _, :supervisor, _}}, {active, worker, supervisor} ->
          {active + 1, worker, supervisor + 1}
      end)

    reply = [specs: specs, active: active, supervisors: supervisors, workers: workers]
    {:reply, reply, state}
  end

  def handle_call({:terminate_child, pid}, _from, %{children: children} = state) do
    case children do
      %{^pid => info} ->
        :ok = terminate_children(%{pid => info}, state)
        {:reply, :ok, delete_child(pid, state)}

      %{} ->
        {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call({:start_task, args, restart, shutdown}, from, state) do
    {init_restart, init_shutdown} = Process.get(Task.Supervisor)
    restart = restart || init_restart
    shutdown = shutdown || init_shutdown
    child = {{Task.Supervised, :start_link, args}, restart, shutdown, :worker, [Task.Supervised]}
    handle_call({:start_child, child}, from, state)
  end

  def handle_call({:start_child, child}, _from, state) do
    %{children: children, max_children: max_children} = state

    if map_size(children) < max_children do
      handle_start_child(child, state)
    else
      {:reply, {:error, :max_children}, state}
    end
  end

  defp handle_start_child({{m, f, args} = mfa, restart, shutdown, type, modules}, state) do
    %{extra_arguments: extra} = state

    case reply = start_child(m, f, extra ++ args) do
      {:ok, pid, _} ->
        {:reply, reply, save_child(pid, mfa, restart, shutdown, type, modules, state)}

      {:ok, pid} ->
        {:reply, reply, save_child(pid, mfa, restart, shutdown, type, modules, state)}

      _ ->
        {:reply, reply, state}
    end
  end

  defp start_child(m, f, a) do
    try do
      apply(m, f, a)
    catch
      kind, reason ->
        {:error, exit_reason(kind, reason, __STACKTRACE__)}
    else
      {:ok, pid, extra} when is_pid(pid) -> {:ok, pid, extra}
      {:ok, pid} when is_pid(pid) -> {:ok, pid}
      :ignore -> :ignore
      {:error, _} = error -> error
      other -> {:error, other}
    end
  end

  defp save_child(pid, mfa, restart, shutdown, type, modules, state) do
    mfa = mfa_for_restart(mfa, restart)
    put_in(state.children[pid], {mfa, restart, shutdown, type, modules})
  end

  defp mfa_for_restart({m, f, _}, :temporary), do: {m, f, :undefined}
  defp mfa_for_restart(mfa, _), do: mfa

  defp exit_reason(:exit, reason, _), do: reason
  defp exit_reason(:error, reason, stack), do: {reason, stack}
  defp exit_reason(:throw, value, stack), do: {{:nocatch, value}, stack}

  @impl true
  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  @impl true
  def handle_info({:EXIT, pid, reason}, state) do
    case maybe_restart_child(pid, reason, state) do
      {:ok, state} -> {:noreply, state}
      {:shutdown, state} -> {:stop, :shutdown, state}
    end
  end

  def handle_info({:"$gen_restart", pid}, state) do
    %{children: children} = state

    case children do
      %{^pid => restarting_args} ->
        {:restarting, child} = restarting_args

        case restart_child(pid, child, state) do
          {:ok, state} -> {:noreply, state}
          {:shutdown, state} -> {:stop, :shutdown, state}
        end

      # We may hit clause if we send $gen_restart and then
      # someone calls terminate_child, removing the child.
      %{} ->
        {:noreply, state}
    end
  end

  def handle_info(msg, state) do
    :error_logger.error_msg('DynamicSupervisor received unexpected message: ~p~n', [msg])
    {:noreply, state}
  end

  @impl true
  def code_change(_, %{mod: mod, args: init_arg} = state, _) do
    case mod.init(init_arg) do
      {:ok, flags} when is_map(flags) ->
        case init(state, flags) do
          {:ok, state} -> {:ok, state}
          {:error, reason} -> {:error, {:supervisor_data, reason}}
        end

      :ignore ->
        {:ok, state}

      error ->
        error
    end
  end

  @impl true
  def terminate(_, %{children: children} = state) do
    :ok = terminate_children(children, state)
  end

  defp terminate_children(children, state) do
    {pids, times, stacks} = monitor_children(children)
    size = map_size(pids)

    timers =
      Enum.reduce(times, %{}, fn {time, pids}, acc ->
        Map.put(acc, :erlang.start_timer(time, self(), :kill), pids)
      end)

    stacks = wait_children(pids, size, timers, stacks)

    for {pid, {child, reason}} <- stacks do
      report_error(:shutdown_error, reason, pid, child, state)
    end

    :ok
  end

  defp monitor_children(children) do
    Enum.reduce(children, {%{}, %{}, %{}}, fn
      {_, {:restarting, _}}, acc ->
        acc

      {pid, {_, restart, _, _, _} = child}, {pids, times, stacks} ->
        case monitor_child(pid) do
          :ok ->
            times = exit_child(pid, child, times)
            {Map.put(pids, pid, child), times, stacks}

          {:error, :normal} when restart != :permanent ->
            {pids, times, stacks}

          {:error, reason} ->
            {pids, times, Map.put(stacks, pid, {child, reason})}
        end
    end)
  end

  defp monitor_child(pid) do
    ref = Process.monitor(pid)
    Process.unlink(pid)

    receive do
      {:EXIT, ^pid, reason} ->
        receive do
          {:DOWN, ^ref, :process, ^pid, _} -> {:error, reason}
        end
    after
      0 -> :ok
    end
  end

  defp exit_child(pid, {_, _, shutdown, _, _}, times) do
    case shutdown do
      :brutal_kill ->
        Process.exit(pid, :kill)
        times

      :infinity ->
        Process.exit(pid, :shutdown)
        times

      time ->
        Process.exit(pid, :shutdown)
        Map.update(times, time, [pid], &[pid | &1])
    end
  end

  defp wait_children(_pids, 0, timers, stacks) do
    for {timer, _} <- timers do
      _ = :erlang.cancel_timer(timer)

      receive do
        {:timeout, ^timer, :kill} -> :ok
      after
        0 -> :ok
      end
    end

    stacks
  end

  defp wait_children(pids, size, timers, stacks) do
    receive do
      {:DOWN, _ref, :process, pid, reason} ->
        case pids do
          %{^pid => child} ->
            stacks = wait_child(pid, child, reason, stacks)
            wait_children(pids, size - 1, timers, stacks)

          %{} ->
            wait_children(pids, size, timers, stacks)
        end

      {:timeout, timer, :kill} ->
        for pid <- Map.fetch!(timers, timer), do: Process.exit(pid, :kill)
        wait_children(pids, size, Map.delete(timers, timer), stacks)
    end
  end

  defp wait_child(pid, {_, _, :brutal_kill, _, _} = child, reason, stacks) do
    case reason do
      :killed -> stacks
      _ -> Map.put(stacks, pid, {child, reason})
    end
  end

  defp wait_child(pid, {_, restart, _, _, _} = child, reason, stacks) do
    case reason do
      {:shutdown, _} -> stacks
      :shutdown -> stacks
      :normal when restart != :permanent -> stacks
      reason -> Map.put(stacks, pid, {child, reason})
    end
  end

  defp maybe_restart_child(pid, reason, %{children: children} = state) do
    case children do
      %{^pid => {_, restart, _, _, _} = child} ->
        maybe_restart_child(restart, reason, pid, child, state)

      %{} ->
        {:ok, state}
    end
  end

  defp maybe_restart_child(:permanent, reason, pid, child, state) do
    report_error(:child_terminated, reason, pid, child, state)
    restart_child(pid, child, state)
  end

  defp maybe_restart_child(_, :normal, pid, _child, state) do
    {:ok, delete_child(pid, state)}
  end

  defp maybe_restart_child(_, :shutdown, pid, _child, state) do
    {:ok, delete_child(pid, state)}
  end

  defp maybe_restart_child(_, {:shutdown, _}, pid, _child, state) do
    {:ok, delete_child(pid, state)}
  end

  defp maybe_restart_child(:transient, reason, pid, child, state) do
    report_error(:child_terminated, reason, pid, child, state)
    restart_child(pid, child, state)
  end

  defp maybe_restart_child(:temporary, reason, pid, child, state) do
    report_error(:child_terminated, reason, pid, child, state)
    {:ok, delete_child(pid, state)}
  end

  defp delete_child(pid, %{children: children} = state) do
    %{state | children: Map.delete(children, pid)}
  end

  defp restart_child(pid, child, state) do
    case add_restart(state) do
      {:ok, %{strategy: strategy} = state} ->
        case restart_child(strategy, pid, child, state) do
          {:ok, state} ->
            {:ok, state}

          {:try_again, state} ->
            send(self(), {:"$gen_restart", pid})
            {:ok, state}
        end

      {:shutdown, state} ->
        report_error(:shutdown, :reached_max_restart_intensity, pid, child, state)
        {:shutdown, delete_child(pid, state)}
    end
  end

  defp add_restart(state) do
    %{max_seconds: max_seconds, max_restarts: max_restarts, restarts: restarts} = state

    now = :erlang.monotonic_time(1)
    restarts = add_restart([now | restarts], now, max_seconds)
    state = %{state | restarts: restarts}

    if length(restarts) <= max_restarts do
      {:ok, state}
    else
      {:shutdown, state}
    end
  end

  defp add_restart(restarts, now, period) do
    for then <- restarts, now <= then + period, do: then
  end

  defp restart_child(:one_for_one, current_pid, child, state) do
    {{m, f, args} = mfa, restart, shutdown, type, modules} = child
    %{extra_arguments: extra} = state

    case start_child(m, f, extra ++ args) do
      {:ok, pid, _} ->
        state = delete_child(current_pid, state)
        {:ok, save_child(pid, mfa, restart, shutdown, type, modules, state)}

      {:ok, pid} ->
        state = delete_child(current_pid, state)
        {:ok, save_child(pid, mfa, restart, shutdown, type, modules, state)}

      :ignore ->
        {:ok, delete_child(current_pid, state)}

      {:error, reason} ->
        report_error(:start_error, reason, {:restarting, current_pid}, child, state)
        state = put_in(state.children[current_pid], {:restarting, child})
        {:try_again, state}
    end
  end

  defp report_error(error, reason, pid, child, %{name: name, extra_arguments: extra}) do
    :error_logger.error_report(
      :supervisor_report,
      supervisor: name,
      errorContext: error,
      reason: reason,
      offender: extract_child(pid, child, extra)
    )
  end

  defp extract_child(pid, {{m, f, args}, restart, shutdown, type, _modules}, extra) do
    [
      pid: pid,
      id: :undefined,
      mfargs: {m, f, extra ++ args},
      restart_type: restart,
      shutdown: shutdown,
      child_type: type
    ]
  end

  @impl true
  def format_status(:terminate, [_pdict, state]) do
    state
  end

  def format_status(_, [_pdict, %{mod: mod} = state]) do
    [data: [{~c"State", state}], supervisor: [{~c"Callback", mod}]]
  end

  ## Helpers

  @compile {:inline, call: 2}

  defp call(supervisor, req) do
    GenServer.call(supervisor, req, :infinity)
  end
end
