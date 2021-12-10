defmodule PartitionSupervisor do
  @moduledoc """
  A supervisor that starts multiple partitions of the same child.

  Certain processes may become bottlenecks in large systems.
  If those processes can have their state trivially partitioned,
  in a way there is no dependency between them, then they can use
  the `PartitionSupervisor` to create multiple isolated and
  independent partitions.

  Once the `PartitionSupervisor` starts, you can dispatch to its
  children using `{:via, PartitionSupervisor, {name, key}}`, where
  `name` is the name of the `PartitionSupervisor` and key is used
  for routing.

  ## Example

  The `DynamicSupervisor` is a single process responsible for starting
  other processes. In some applications, the `DynamicSupervisor` may
  become a bottleneck. To address this, you can start multiple instances
  of the `DynamicSupervisor` and then pick a "random" instance to start
  the child on.

  Instead of:

      children = [
        {DynamicSupervisor, name: MyApp.DynamicSupervisor}
      ]

      Supervisor.start_link(children, strategy: :one_for_one)

  and:

      DynamicSupervisor.start_child(MyApp.DynamicSupervisor, {Agent, fn -> %{} end})

  You can do this:

      children = [
        {PartitionSupervisor,
         child_spec: DynamicSupervisor,
         name: MyApp.DynamicSupervisors}
      ]

      Supervisor.start_link(children, strategy: :one_for_one)

  and then:

      DynamicSupervisor.start_child(
        {:via, PartitionSupervisor, {MyApp.DynamicSupervisors, self()}},
        {Agent, fn -> %{} end}
      )

  In the code above, we start a partition supervisor that will by default
  start a dynamic supervisor for each core in your machine. Then, instead
  of calling the `DynamicSupervisor` by name, you call it through the
  partition supervisor using the `{:via, PartitionSupervisor, {name, key}}`
  format. We picked `self()` as the routing key, which means each process
  will be assigned one of the existing dynamic supervisors. See `start_link/1`
  to see all options supported by the `PartitionSupervisor`.

  ## Implementation notes

  The `PartitionSupervisor` requires a name as an atom to be given on start,
  as it uses an ETS table to keep all of the partitions. Under the hood,
  the `PartitionSupervisor` generates a child spec for each partition and
  then act as a regular supervisor. The id of each child spec is the
  partition number.

  For routing, two strategies are used. If `key` is an integer, it is routed
  using `rem(abs(key), partitions)`. Otherwise it uses `:erlang.phash2(key, partitions)`.
  The particular routing may change in the future, and therefore cannot
  be relied on. If you want to retrieve a particular PID for a certain key,
  you can use `GenServer.whereis({:via, PartitionSupervisor, {name, key}})`.
  """

  @behaviour Supervisor
  @type name :: atom()

  @doc false
  def child_spec(opts) when is_list(opts) do
    %{
      id: Keyword.get(opts, :name, PartitionSupervisor),
      start: {PartitionSupervisor, :start_link, [opts]},
      type: :supervisor
    }
  end

  @doc """
  Starts a partition supervisor with the given options.

  This function is typically not invoked directly, instead it is invoked
  when using a `PartitionSupervisor` as a child of another supervisor:

      children = [
        {PartitionSupervisor, child_spec: SomeChild, name: MyPartitionSupervisor}
      ]

  If the supervisor is successfully spawned, this function returns
  `{:ok, pid}`, where `pid` is the PID of the supervisor. If the given name
    for the partition supervisor is already assigned to a process,
  the function returns `{:error, {:already_started, pid}}`, where `pid`
  is the PID of that process.

  Note that a supervisor started with this function is linked to the parent
  process and exits not only on crashes but also if the parent process exits
  with `:normal` reason.

  ## Options

    * `:name` - an atom representing the name of the partition supervisor.

    * `:partitions` - a positive integer with the number of partitions.
      Defaults to `System.schedulers_online()` (typically the number of cores).

    * `:strategy` - the restart strategy option, defaults to `:one_for_one`.
      You can learn more about strategies in the `Supervisor` module docs.

    * `:max_restarts` - the maximum number of restarts allowed in
      a time frame. Defaults to `3`.

    * `:max_seconds` - the time frame in which `:max_restarts` applies.
      Defaults to `5`.

    * `:with_arguments` - a two-argument anonymous function that allows
      the partition to be given to the child starting function. See the
      `:with_arguments` section below.

  ## `:with_arguments`

  Sometimes you want each partition to know their partition assigned number.
  This can be done with the `:with_arguments` option. This function receives
  the list of arguments of the child specification with the partition and
  it must return a new list of arguments.

  For example, most processes are started by calling `start_link(opts)`,
  where `opts` is a keyword list. You could attach the partition to the
  keyword list like this:

      with_arguments: fn [opts], partition ->
        [Keyword.put(opts, :partition, partition)]
      end
  """
  def start_link(opts) do
    name = opts[:name]

    unless name && is_atom(name) do
      raise ArgumentError,
            "the :name option must be given to PartitionSupervisor as an atom, got: #{inspect(name)}"
    end

    {child_spec, opts} = Keyword.pop(opts, :child_spec)

    unless child_spec do
      raise ArgumentError, "the :child_spec option must be given to PartitionSupervisor"
    end

    {partitions, opts} = Keyword.pop(opts, :partitions, System.schedulers_online())

    unless is_integer(partitions) and partitions >= 1 do
      raise ArgumentError,
            "the :partitions option must be a positive integer, got: #{inspect(partitions)}"
    end

    {with_arguments, opts} = Keyword.pop(opts, :with_arguments, fn args, _partition -> args end)

    unless is_function(with_arguments, 2) do
      raise ArgumentError,
            "the :with_arguments option must be a function that receives two arguments, " <>
              "the current call arguments and the partition, got: #{inspect(with_arguments)}"
    end

    %{start: {mod, fun, args}} = map = Supervisor.child_spec(child_spec, [])
    modules = map[:modules] || [mod]

    children =
      for partition <- 0..(partitions - 1) do
        args = with_arguments.(args, partition)

        unless is_list(args) do
          raise "the call to the function in :with_arguments must return a list, got: #{inspect(args)}"
        end

        start = {__MODULE__, :start_child, [mod, fun, args, name, partition]}
        Map.merge(map, %{id: partition, start: start, modules: modules})
      end

    {init_opts, start_opts} = Keyword.split(opts, [:strategy, :max_seconds, :max_restarts])
    Supervisor.start_link(__MODULE__, {name, partitions, children, init_opts}, start_opts)
  end

  @doc false
  def start_child(mod, fun, args, name, partition) do
    case apply(mod, fun, args) do
      {:ok, pid} ->
        :ets.insert(name, {partition, pid})
        {:ok, pid}

      {:ok, pid, info} ->
        :ets.insert(name, {partition, pid})
        {:ok, pid, info}

      other ->
        other
    end
  end

  @impl true
  def init({name, partitions, children, init_opts}) do
    :ets.new(name, [:set, :named_table, :protected, read_concurrency: true])
    :ets.insert(name, {:partitions, partitions})
    Supervisor.init(children, Keyword.put_new(init_opts, :strategy, :one_for_one))
  end

  @doc """
  Returns the number of partitions for the partition supervisor.  
  """
  @doc since: "1.14.0"
  @spec partitions(name()) :: pos_integer()
  def partitions(supervisor) when is_atom(supervisor) do
    :ets.lookup_element(supervisor, :partitions, 2)
  end

  @doc """
  Returns a list with information about all children.

  This function returns a list of tuples containing:

    * `id` - the partition number

    * `child` - the PID of the corresponding child process or the
      atom `:restarting` if the process is about to be restarted

    * `type` - `:worker` or `:supervisor` as defined in the child
      specification

    * `modules` - as defined in the child specification

  """
  @doc since: "1.14.0"
  @spec which_children(name()) :: [
          # Inlining [module()] | :dynamic here because :supervisor.modules() is not exported
          {:undefined, pid | :restarting, :worker | :supervisor, [module()] | :dynamic}
        ]
  def which_children(supervisor) when is_atom(supervisor) do
    Supervisor.which_children(supervisor)
  end

  @doc """
  Returns a map containing count values for the supervisor.

  The map contains the following keys:

    * `:specs` - the number of partitions (children processes)

    * `:active` - the count of all actively running child processes managed by
      this supervisor

    * `:supervisors` - the count of all supervisors whether or not the child
      process is still alive

    * `:workers` - the count of all workers, whether or not the child process
      is still alive

  """
  @doc since: "1.14.0"
  @spec count_children(name()) :: %{
          specs: non_neg_integer,
          active: non_neg_integer,
          supervisors: non_neg_integer,
          workers: non_neg_integer
        }
  def count_children(supervisor) when is_atom(supervisor) do
    Supervisor.count_children(supervisor)
  end

  @doc """
  Synchronously stops the given partition supervisor with the given `reason`.

  It returns `:ok` if the supervisor terminates with the given
  reason. If it terminates with another reason, the call exits.

  This function keeps OTP semantics regarding error reporting.
  If the reason is any other than `:normal`, `:shutdown` or
  `{:shutdown, _}`, an error report is logged.
  """
  @doc since: "1.14.0"
  @spec stop(name(), reason :: term, timeout) :: :ok
  def stop(supervisor, reason \\ :normal, timeout \\ :infinity) when is_atom(supervisor) do
    Supervisor.stop(supervisor, reason, timeout)
  end

  ## Via callbacks

  @doc false
  def whereis_name({name, key}) when is_atom(name) do
    partitions = partitions(name)

    partition =
      if is_integer(key), do: rem(abs(key), partitions), else: :erlang.phash2(key, partitions)

    :ets.lookup_element(name, partition, 2)
  end

  @doc false
  def send(name_key, msg) do
    Kernel.send(whereis_name(name_key), msg)
  end

  @doc false
  def register_name(_, _) do
    raise "{:via, PartitionSupervisor, _} cannot be given on registration"
  end

  @doc false
  def unregister_name(_, _) do
    raise "{:via, PartitionSupervisor, _} cannot be given on unregistration"
  end
end
