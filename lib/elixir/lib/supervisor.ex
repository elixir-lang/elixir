defmodule Supervisor do
  @moduledoc ~S"""
  A behaviour module for implementing supervisors.

  A supervisor is a process which supervises other processes, which we
  refer to as *child processes*. Supervisors are used to build a hierarchical
  process structure called a *supervision tree*. Supervision trees provide
  fault-tolerance and encapsulate how our applications start and shutdown.

  A supervisor implemented using this module has a standard set
  of interface functions and includes functionality for tracing and error
  reporting.

  ## Examples

  In order to define a supervisor, we need to first define a child process
  that will be supervised. As an example, we will define a GenServer that
  represents a stack:

      defmodule Stack do
        use GenServer

        def start_link(state) do
          GenServer.start_link(__MODULE__, state, name: __MODULE__)
        end

        ## Callbacks

        def init(stack) do
          {:ok, stack}
        end

        def handle_call(:pop, _from, [h | t]) do
          {:reply, h, t}
        end

        def handle_cast({:push, h}, t) do
          {:noreply, [h | t]}
        end
      end

  The stack is a small wrapper around lists. It allows us to put
  an element on the top of the stack, by prepending to the list,
  and to get the top of the stack by pattern matching.

  We can now start a supervisor that will start and supervise our
  stack process as follows:

      # Start the supervisor with the stack as a single child.
      #
      # The first element of the tuple is the module containing
      # the child implementation, the second is the argument
      # given to start_link, in this case a stack with `:hello`.
      {:ok, pid} = Supervisor.start_link([
        {Stack, [:hello]}
      ], strategy: :one_for_one)

      # After started, we can query the supervisor for information
      Supervisor.count_children(pid)
      #=> %{active: 1, specs: 1, supervisors: 0, workers: 1}

  Notice that when starting the GenServer, we are registering it
  with name `Stack`, which allows us to call it directly and get
  what is on the stack:

      GenServer.call(Stack, :pop)
      #=> :hello

      GenServer.cast(Stack, {:push, :world})
      #=> :ok

      GenServer.call(Stack, :pop)
      #=> :world

  However, there is a bug in our stack server. If we call `:pop` and
  the stack is empty, it is going to crash because no clause matches:

      GenServer.call(Stack, :pop)
      ** (exit) exited in: GenServer.call(Stack, :pop, 5000)

  Luckily, since the server is being supervised by a supervisor, the
  supervisor will automatically start a new one, with the initial stack
  of `[:hello]`:

      GenServer.call(Stack, :pop)
      #=> :hello

  Supervisors support different strategies; in the example above, we
  have chosen `:one_for_one`. Furthermore, each supervisor can have many
  workers and supervisors as children, each of them with their specific
  configuration, shutdown values, and restart strategies.

  The rest of this document will cover how child processes are started,
  how they can be specified, different supervision strategies and more.

  ## The initialization process

  In the previous section, we have started a supervisor with one child:

      Supervisor.start_link([
        {Stack, [:hello]}
      ], strategy: :one_for_one)

  The first argument given to `start_link` is a list of children.
  In the example above, we have passed a tuple, where the child is
  implemented by the `Stack` module and receives an initial argument
  of `[:hello]` on `Stack.start_link/1`.

  Generally speaking, starting the child process happens in three steps:

    1. First the supervisor calls `Stack.child_spec([:hello])`. This
       function must return a **child specification** which describes
       how the `Stack` process is supervised. When you `use GenServer`,
       a `child_spec/1` is automatically defined for you but we will see
       when and how it can be configured. This function is called once
       when the supervisor starts (or in case of hot code upgrades).

    2. The **child specification** tells the supervisor which function
       to invoke to start the child process. By default, it is the
       `start_link/1` function, receiving the same argument and defined
       in the same module as the `child_spec/1` function. This function
       is called every time a new child process is necessary. For example,
       when we crashed a `Stack` in the previous session,
       `Stack.start_link([:hello])` was called once more to start a new stack.

    3. Finally, `Stack.start_link/1` starts a new process that runs
      `Stack.init/1`, which is responsible for setting a process that
       will react to messages.

  In summary, when the `Supervisor.start_link(children, opts)` is called,
  it traverses the list of children and retrieves their `child_spec/1`.
  Then each child specification describes how each child is started,
  typically via the `start_link/1` function. After the supervisor process
  starts, it starts each child process in the order they are defined. Each
  child process typically executes the `init` callback as its first step.
  The `init` callback is where we initialize and configure the child process.

  ## The shutdown process

  When a supervisor shuts down, it terminates all children in the opposite
  order they are listed. The termination happens by sending a shutdown exit
  signal to the child process and then awaiting for a time interval, which
  defaults to 5000 miliseconds, for the child process to terminate. If the
  child process does not terminate, it is abruptly terminated with reason
  `:brutal_kill`. The shutdown time can be configured in the child specification
  which is detailed in the next section.

  If the child process is not trapping exits, it will shutdown immediately
  when it receives the first exit signal. If the child process is trapping
  exits, then the `terminate` callback is invoked, and the child process
  must terminate in a reasonable time interval before being abruptly
  terminated by the supervisor.

  In other words, if it is important that a process cleans after itself
  when your application or the supervision tree is shutting down, then
  this process must trap exits and its child specification should specify
  the proper `:shutdown` value, ensuring it terminates within a reasonable
  interval.

  ## Child specification

  The child specification describes how the supervisor should start and
  supervise a child process. We have learned that, when we invoked
  `use GenServer`, a `Stack.child_spec/1` was automatically defined for
  us. Let's invoke it and see what it returns:

      Stack.child_spec([:hello])
      #=> %{
      #=>   id: Stack,
      #=>   start: {Stack, :start_link, [[:hello]]},
      #=>   restart: :permanent,
      #=>   shutdown: 5000,
      #=>   type: :worker
      #=> }

  The child specification contains 5 keys. The first two are required
  and the remaining ones are optional:

    * `:id` - a value used to identify the child specification
      internally by the supervisor; defaults to the given module.
      In case of conflicting `:id`, the supervisor will refuse
      to initialize and require explicit IDs. This key is required.

    * `:start` - a tuple with the module-function-args to be invoked
      to start the child process. This key is required.

    * `:restart` - an atom that defines when a terminated child process
       should be restarted (see the "Restart values" section below).
       This key is optional and defaults to `:permanent`.

    * `:shutdown` - an atom that defines how a child process should be
      terminated (see the "Shutdown values" section below). This key
      is optional and defaults to `5000` if the type is `:worker` or
      `:infinity` if the type is `:supervisor`.

    * `:type` - if the child process is a `:worker` or a `:supervisor`.
      This key is optional and defaults to `:worker`.

  There is a sixth key, called `:modules`, which is rarely changed and
  it is set automatically based on the value in `:start`.

  Most times, the behaviour module you are implementing will take care
  of setting up a proper `child_spec/1` for you. For example, `use Supervisor`
  will define a `child_spec/1` where the `:type` is set to `:supervisor`
  and the `:shutdown` is `:infinity`. Still, if you need to customize
  a certain behaviour, you can do so by defining your own `child_spec/1`
  function or by passing options on `use`. For example, to specify a
  `GenServer` with a shutdown limit of 10 seconds (10_000 miliseconds),
  one might do:

      use GenServer, shutdown: 10_000

  Let's understand what the `:shutdown` and `:restart` options control.

  ### Shutdown values (:shutdown)

  The following shutdown values are supported in the `:shutdown` option:

    * `:brutal_kill` - the child process is unconditionally terminated
      using `Process.exit(child, :kill)`.

    * any integer >= 0 - the amount of time in miliseconds that the
      supervisor will wait for children to terminate after emitting a
      `Process.exit(child, :shutdown)` signal. If the child process is
      not trapping exits, the initial `:shutdown` signal will terminate
      the child process immediately. If the child process is trapping
      exits, it has the given amount of time in miliseconds to terminate.
      If it doesn't terminate within the specified time, the child process
      is unconditionally terminated by the supervisor via
      `Process.exit(child, :kill)`.

    * `:infinity` - works as an integer except the supervisor will wait
      indefinitely for the child to terminate. If the child process is a
      supervisor, the recommended value is `:infinity` to give the supervisor
      and its children enough time to shutdown. This option can be used with
      regular workers but doing so is discouraged and requires extreme care.
      If not used carefully and the child process does not terminate, it means
      your application will never terminate as well.

  ### Restart values (:restart)

  The `:restart` option controls what the supervisor should consider to
  be a successful termination or not. If the termination is successful,
  the supervisor won't restart the child. If the child process crashed,
  the supervisor will start a new one.

  The following restart values are supported in the `:restart` option:

    * `:permanent` - the child process is always restarted.

    * `:temporary` - the child process is never restarted, regardless
      of the supervision strategy.

    * `:transient` - the child process is restarted only if it
      terminates abnormally, i.e., with an exit reason other than
      `:normal`, `:shutdown` or `{:shutdown, term}`.

  For a more complete understanding of the exit reasons and their
  impact, see the "Exit reasons" section next.

  ## Exit reasons and restarts

  A supervisor restarts a child process depending on its `:restart`
  configuration. For example, when `:restart` is set to `:transient`, the
  supervisor does not restart the child in case it exits with reason `:normal`,
  `:shutdown` or `{:shutdown, term}`.

  So one may ask: which exit reason should I choose when exiting? There are
  three options:

    * `:normal` - in such cases, the exit won't be logged, there is no restart
      in transient mode, and linked processes do not exit

    * `:shutdown` or `{:shutdown, term}` - in such cases, the exit won't be
      logged, there is no restart in transient mode, and linked processes exit
      with the same reason unless they're trapping exits

    * any other term - in such cases, the exit will be logged, there are
      restarts in transient mode, and linked processes exit with the same
      reason unless they're trapping exits

  Notice that supervisor that reached maximum restart intensity will exit with
  `:shutdown` reason. In this case the supervisor will only be restarted if its
  child specification was defined with the `:restart` option set to `:permanent`
  (the default).

  ## Module-based supervisors

  In the example above, a supervisor was started by passing the supervision
  structure to `start_link/2`. However, supervisors can also be created by
  explicitly defining a supervision module:

      defmodule MyApp.Supervisor do
        # Automatically defines child_spec/1
        use Supervisor

        def start_link(arg) do
          Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
        end

        def init(_arg) do
          Supervisor.init([
            {Stack, [:hello]}
          ], strategy: :one_for_one)
        end
      end

  The difference between the two approaches is that a module-based
  supervisor gives you more direct control over how the supervisor
  is initialized. Instead of calling `Supervisor.start_link/2` with
  a list of children that are automatically initialized, we have
  defined a supervisor alongside its `c:init/1` callback and manually
  initialized the children by calling `Supervisor.init/2`, passing
  the same arguments we would have given to `start_link/2`.

  You may want to use a module-based supervisor if:

    * You need to perform some particular action on supervisor
      initialization, like setting up an ETS table.

    * You want to perform partial hot-code swapping of the
      tree. For example, if you add or remove children,
      the module-based supervision will add and remove the
      new children directly, while dynamic supervision
      requires the whole tree to be restarted in order to
      perform such swaps.

  Note `use Supervisor` defines a `child_spec/1` function, allowing
  the defined module to be put under a supervision tree. The generated
  `child_spec/1` can be customized with the following options:

    * `:id` - the child specification id, defaults to the current module
    * `:start` - how to start the child process (defaults to calling `__MODULE__.start_link/1`)
    * `:restart` - when the supervisor should be restarted, defaults to `:permanent`

  ## `start_link/2`, `init/2`, and strategies

  So far we have started the supervisor passing a single child as a tuple
  as well as a strategy called `:one_for_one`:

      Supervisor.start_link([
        {Stack, [:hello]}
      ], strategy: :one_for_one)

  Or:

      Supervisor.init([
        {Stack, [:hello]}
      ], strategy: :one_for_one)

  However, children can be specified in three different formats and
  supervisors support different options. Let's formally define those.

  The first argument given to `start_link/2` is a list of children which may
  be either:

    * a module - such as `Stack`. In this case, it is equivalent to passing
      `{Stack, []}` (which means `Stack.child_spec/1` is invoked with an empty
      keywords list)
    * a tuple with a module as first element and the start argument as second -
      such as `{Stack, [:hello]}`. When such format is used, the supervisor
      will retrieve the child specification from the given module.
    * a map representing the child specification itself - such as the child
      specification map outlined in the previous section.

  The second argument is a keyword list of options:

    * `:strategy` - the restart strategy option. It can be either
      `:one_for_one`, `:rest_for_one`, `:one_for_all`, or
      `:simple_one_for_one`. See the "Strategies" section.

    * `:max_restarts` - the maximum amount of restarts allowed in
      a time frame. Defaults to `3`.

    * `:max_seconds` - the time frame in which `:max_restarts` applies.
      Defaults to `5`.

  The `:strategy` option is required and by default a maximum of 3 restarts
  is allowed within 5 seconds.

  ### Strategies

  Supervisors support different supervision strategies (through the
  `:strategy` option, as seen above):

    * `:one_for_one` - if a child process terminates, only that
      process is restarted.

    * `:one_for_all` - if a child process terminates, all other child
      processes are terminated and then all child processes (including
      the terminated one) are restarted.

    * `:rest_for_one` - if a child process terminates, the "rest" of
      the child processes, i.e., the child processes after the terminated
      one in start order, are terminated. Then the terminated child
      process and the rest of the child processes are restarted.

    * `:simple_one_for_one` - similar to `:one_for_one` but suits better
      when dynamically attaching children. This strategy requires the
      supervisor specification to contain only one child. Many functions
      in this module behave slightly differently when this strategy is
      used.

  ## Simple one for one

  The `:simple_one_for_one` supervisor is useful when you want to
  dynamically start and stop supervised children. As an example,
  let's start multiple agents dynamically to keep state.

  One important aspect in `:simple_one_for_one` supervisors is
  that we often want to pass the `:start` arguments later on,
  when starting the children dynamically, rather than when the
  child specification is defined. In such cases, we should not do

      Supervisor.start_link [
        {Agent, fn -> 0 end}
      ]

  as the example above would force all agents to have the same state.
  In such cases, we can use the `child_spec/2` function to build
  and override the fields in a child specification:

      # Override the :start field to have no args.
      # The second argument has no effect thanks to it.
      agent_spec =
        Supervisor.child_spec(Agent, start: {Agent, :start_link, []})

      # We start a supervisor with a simple one for one strategy.
      # The agent won't be started now but later on.
      {:ok, sup_pid} =
        Supervisor.start_link([agent_spec], strategy: :simple_one_for_one)

      # No child worker is active until start_child is called
      Supervisor.count_children(sup_pid)
      #=> %{active: 0, specs: 1, supervisors: 0, workers: 0}

  The simple one for one strategy can define only one child which works
  as a template for when we call `start_child/2`.

  With the supervisor started, let's dynamically start agents:

      {:ok, agent1} = Supervisor.start_child(sup_pid, [fn -> 0 end])
      Agent.update(agent1, & &1 + 1)
      Agent.get(agent1, & &1) #=> 1

      {:ok, agent2} = Supervisor.start_child(sup_pid, [fn -> %{} end])
      Agent.get(agent2, & &1) #=> %{}

      Supervisor.count_children(sup_pid)
      #=> %{active: 2, specs: 1, supervisors: 0, workers: 2}

  ## Name registration

  A supervisor is bound to the same name registration rules as a `GenServer`.
  Read more about these rules in the documentation for `GenServer`.
  """

  @doc false
  defmacro __using__(opts) do
    quote location: :keep, bind_quoted: [opts: opts] do
      @behaviour Supervisor
      import Supervisor.Spec

      spec = [
        id: opts[:id] || __MODULE__,
        start: Macro.escape(opts[:start]) || quote(do: {__MODULE__, :start_link, [arg]}),
        restart: opts[:restart] || :permanent,
        type: :supervisor
      ]

      @doc false
      def child_spec(arg) do
        %{unquote_splicing(spec)}
      end

      defoverridable child_spec: 1

      @doc false
      def init(arg)
    end
  end

  @doc """
  Callback invoked to start the supervisor and during hot code upgrades.
  """
  @callback init(args :: term) ::
    {:ok, {:supervisor.sup_flags, [:supervisor.child_spec]}} |
    :ignore

  @typedoc "Return values of `start_link` functions"
  @type on_start :: {:ok, pid} | :ignore |
                    {:error, {:already_started, pid} | {:shutdown, term} | term}

  @typedoc "Return values of `start_child` functions"
  @type on_start_child :: {:ok, child} | {:ok, child, info :: term} |
                          {:error, {:already_started, child} | :already_present | term}

  @type child :: pid | :undefined

  @typedoc "The Supervisor name"
  @type name :: atom | {:global, term} | {:via, module, term}

  @typedoc "Option values used by the `start*` functions"
  @type option :: {:name, name} | flag()

  @typedoc "Options used by the `start*` functions"
  @type options :: [option, ...]

  @typedoc "The supervisor reference"
  @type supervisor :: pid | name | {atom, node}

  @typedoc "Options given to `start_link/2` and `init/2`"
  @type flag :: {:strategy, strategy} |
                {:max_restarts, non_neg_integer} |
                {:max_seconds, pos_integer}

  @typedoc "Supported strategies"
  @type strategy :: :simple_one_for_one | :one_for_one | :one_for_all | :rest_for_one

  # Note we have inlined all types for readability
  @typedoc "The supervisor specification"
  @type child_spec :: %{
    required(:id) => term(),
    required(:start) => {module(), function(), [term()]},
    optional(:restart) => :permanent | :transient | :temporary,
    optional(:shutdown) => :brutal_kill | non_neg_integer() | :infinity,
    optional(:type) => :worker | :supervisor,
    optional(:modules) => [module()] | :dynamic
  }

  @doc """
  Starts a supervisor with the given children.

  The children is a list of modules, 2-element tuples with module and
  arguments or a map with the child specification. A strategy is required
  to be provided through the `:strategy` option. See
  "start_link/2, init/2 and strategies" for examples and other options.

  The options can also be used to register a supervisor name.
  The supported values are described under the "Name registration"
  section in the `GenServer` module docs.

  If the supervisor and its child processes are successfully spawned
  (if the start function of each child process returns `{:ok, child}`,
  `{:ok, child, info}`, or `:ignore`) this function returns
  `{:ok, pid}`, where `pid` is the PID of the supervisor. If the supervisor
  is given a name and a process with the specified name already exists,
  the function returns `{:error, {:already_started, pid}}`, where `pid`
  is the PID of that process.

  If the start function of any of the child processes fails or returns an error
  tuple or an erroneous value, the supervisor first terminates with reason
  `:shutdown` all the child processes that have already been started, and then
  terminates itself and returns `{:error, {:shutdown, reason}}`.

  Note that a supervisor started with this function is linked to the parent
  process and exits not only on crashes but also if the parent process exits
  with `:normal` reason.
  """
  @spec start_link([:supervisor.child_spec | {module, term} | module], options) :: on_start
  def start_link(children, options) when is_list(children) do
    {sup_opts, start_opts} = Keyword.split(options, [:strategy, :max_seconds, :max_restarts])
    start_link(Supervisor.Default, {children, sup_opts}, start_opts)
  end

  @doc """
  Receives a list of children to initialize and a set of options.

  This is typically invoked at the end of the `c:init/1` callback of
  module-based supervisors. See the sections "Module-based supervisors"
  and "start_link/2, init/2 and strategies" in the module
  documentation for more information.

  This function returns a tuple containing the supervisor
  flags and child specifications.

  ## Examples

      def init(_arg) do
        Supervisor.init([
          {Stack, [:hello]}
        ], strategy: :one_for_one)
      end

  ## Options

    * `:strategy` - the restart strategy option. It can be either
      `:one_for_one`, `:rest_for_one`, `:one_for_all`, or
      `:simple_one_for_one`. You can learn more about strategies
      in the `Supervisor` module docs.

    * `:max_restarts` - the maximum amount of restarts allowed in
      a time frame. Defaults to `3`.

    * `:max_seconds` - the time frame in which `:max_restarts` applies.
      Defaults to `5`.

  The `:strategy` option is required and by default a maximum of 3 restarts
  is allowed within 5 seconds. Check the `Supervisor` module for a detailed
  description of the available strategies.
  """
  @spec init([:supervisor.child_spec | {module, term} | module], flag) :: {:ok, tuple}
  def init(children, options) when is_list(children) and is_list(options) do
    unless strategy = options[:strategy] do
      raise ArgumentError, "expected :strategy option to be given"
    end
    intensity = Keyword.get(options, :max_restarts, 3)
    period = Keyword.get(options, :max_seconds, 5)
    flags = %{strategy: strategy, intensity: intensity, period: period}
    {:ok, {flags, Enum.map(children, &init_child/1)}}
  end

  defp init_child(module) when is_atom(module) do
    init_child({module, []})
  end
  defp init_child({module, arg}) when is_atom(module) do
    try do
      module.child_spec(arg)
    rescue
      e in UndefinedFunctionError ->
        case System.stacktrace do
          [{^module, :child_spec, [^arg], _} | _] ->
            raise ArgumentError, """
            The module #{inspect module} was given as a child to a supervisor
            but it does not implement child_spec/1.

            If you own the given module, please define a child_spec/1 function
            that receives an argument and returns a child specification as a map.
            For example:

                def child_spec(opts) do
                  %{
                    id: __MODULE__,
                    start: {__MODULE__, :start_link, [opts]},
                    type: :worker,
                    restart: :permanent,
                    shutdown: 500
                  }
                end

            Note that "use Agent", "use GenServer" and so on automatically define
            this function for you.

            However, if you don't own the given module and it doesn't implement
            child_spec/1, instead of passing the module name directly as a supervisor
            child, you will have to pass a child specification as a map:

                %{
                  id: #{inspect module},
                  start: {#{inspect module}, :start_link, [arg1, arg2]}
                }

            See the Supervisor documentation for more information.
            """
          stack ->
            reraise e, stack
        end
    end
  end
  defp init_child(map) when is_map(map) do
    map
  end
  defp init_child({_, _, _, _, _, _} = tuple) do
    tuple
  end
  defp init_child(other) do
    raise ArgumentError, """
    supervisors expect each child to be one of:

      * a module
      * a {module, arg} tuple
      * a child specification as a map with at least the :id and :start fields
      * or a tuple with 6 elements generated by Supervisor.Spec (deprecated)

    Got: #{inspect other}
    """
  end

  @doc """
  Builds and overrides a child specification.

  Similar to `start_link/2` and `init/2`, it expects a
  `module`, `{module, arg}` or a map as the child specification.
  If a module is given, the specification is retrieved by calling
  `module.child_spec(arg)`.

  After the child specification is retrieved, the fields on `config`
  are directly applied on the child spec. If `config` has keys that
  do not map to any child specification field, an error is raised.

  See the "Child specification" section in the module documentation
  for all of the available keys for overriding.

  ## Examples

  This function is often used to set an `:id` option when
  the same module needs to be started multiple times in the
  supervision tree:

      Supervisor.child_spec({Agent, fn -> :ok end}, id: {Agent, 1})
      #=> %{id: {Agent, 1},
      #=>   start: {Agent, :start_link, [fn -> :ok end]}}

  It may also be used when there is a need to change the number
  of arguments when starting a module under a `:simple_one_for_one`
  strategy, since most args may be given dynamically:

      Supervisor.child_spec(Agent, start: {Agent, :start_link, []})
      #=> %{id: Agent,
      #=>   start: {Agent, :start_link, []}}

  """
  @spec child_spec(child_spec() | {module, arg :: term} | module, keyword) :: child_spec()
  def child_spec(module_or_map, overrides)

  def child_spec({_, _, _, _, _, _} = tuple, _overrides) do
    raise ArgumentError, "old tuple-based child specification #{inspect tuple} " <>
                         "is not supported in Supervisor.child_spec/2"
  end

  def child_spec(module_or_map, overrides) do
    Enum.reduce overrides, init_child(module_or_map), fn
      {key, value}, acc when key in [:id, :start, :restart, :shutdown, :type, :modules] ->
        Map.put(acc, key, value)
      {key, _value}, _acc ->
        raise ArgumentError, "unknown key #{inspect key} in child specification override"
    end
  end

  @doc """
  Starts a supervisor process with the given `module` and `arg`.

  To start the supervisor, the `c:init/1` callback will be invoked in the given
  `module`, with `arg` as its argument. The `c:init/1` callback must return a
  supervisor specification which can be created with the help of the `init/2`
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
  @spec start_link(module, term) :: on_start
  @spec start_link(module, term, GenServer.options) :: on_start
  def start_link(module, arg, options \\ []) when is_list(options) do
    case Keyword.get(options, :name) do
      nil ->
        :supervisor.start_link(module, arg)
      atom when is_atom(atom) ->
        :supervisor.start_link({:local, atom}, module, arg)
      {:global, _term} = tuple ->
        :supervisor.start_link(tuple, module, arg)
      {:via, via_module, _term} = tuple when is_atom(via_module) ->
        :supervisor.start_link(tuple, module, arg)
      other ->
        raise ArgumentError, """
        expected :name option to be one of:

          * nil
          * atom
          * {:global, term}
          * {:via, module, term}

        Got: #{inspect(other)}
        """
    end
  end

  @doc """
  Dynamically adds a child specification to `supervisor` and starts that child.

  `child_spec` should be a valid child specification (unless the supervisor
  is a `:simple_one_for_one` supervisor, see below). The child process will
  be started as defined in the child specification.

  In the case of `:simple_one_for_one`, the child specification defined in
  the supervisor is used and instead of a `child_spec`, an arbitrary list
  of terms is expected. The child process will then be started by appending
  the given list to the existing function arguments in the child specification.

  If a child specification with the specified id already exists, `child_spec` is
  discarded and this function returns an error with `:already_started` or
  `:already_present` if the corresponding child process is running or not,
  respectively.

  If the child process start function returns `{:ok, child}` or `{:ok, child,
  info}`, then child specification and PID are added to the supervisor and
  this function returns the same value.

  If the child process start function returns `:ignore`, the child specification
  is added to the supervisor, the PID is set to `:undefined` and this function
  returns `{:ok, :undefined}`.

  If the child process start function returns an error tuple or an erroneous
  value, or if it fails, the child specification is discarded and this function
  returns `{:error, error}` where `error` is a term containing information about
  the error and child specification.
  """
  # TODO: Once we add DynamicSupervisor, we need to enforce receiving
  # a map here and deprecate the list and tuple formats.
  @spec start_child(supervisor, :supervisor.child_spec | [term]) :: on_start_child
  def start_child(supervisor, child_spec_or_args) do
    call(supervisor, {:start_child, child_spec_or_args})
  end

  @doc """
  Terminates the given children, identified by PID or child id.

  If the supervisor is not a `:simple_one_for_one`, the child id is expected
  and the process, if there's one, is terminated; the child specification is
  kept unless the child is temporary.

  In case of a `:simple_one_for_one` supervisor, a PID is expected. If the child
  specification identifier is given instead of a `pid`, this function returns
  `{:error, :simple_one_for_one}`.

  A non-temporary child process may later be restarted by the supervisor. The child
  process can also be restarted explicitly by calling `restart_child/2`. Use
  `delete_child/2` to remove the child specification.

  If successful, this function returns `:ok`. If there is no child specification
  for the given child id or there is no process with the given PID, this
  function returns `{:error, :not_found}`.
  """
  @spec terminate_child(supervisor, pid | term()) :: :ok | {:error, error}
        when error: :not_found | :simple_one_for_one
  def terminate_child(supervisor, pid_or_child_id) do
    call(supervisor, {:terminate_child, pid_or_child_id})
  end

  @doc """
  Deletes the child specification identified by `child_id`.

  The corresponding child process must not be running; use `terminate_child/2`
  to terminate it if it's running.

  If successful, this function returns `:ok`. This function may return an error
  with an appropriate error tuple if the `child_id` is not found, or if the
  current process is running or being restarted.

  This operation is not supported by `:simple_one_for_one` supervisors.
  """
  @spec delete_child(supervisor, term()) :: :ok | {:error, error}
        when error: :not_found | :simple_one_for_one | :running | :restarting
  def delete_child(supervisor, child_id) do
    call(supervisor, {:delete_child, child_id})
  end

  @doc """
  Restarts a child process identified by `child_id`.

  The child specification must exist and the corresponding child process must not
  be running.

  Note that for temporary children, the child specification is automatically deleted
  when the child terminates, and thus it is not possible to restart such children.

  If the child process start function returns `{:ok, child}` or `{:ok, child, info}`,
  the PID is added to the supervisor and this function returns the same value.

  If the child process start function returns `:ignore`, the PID remains set to
  `:undefined` and this function returns `{:ok, :undefined}`.

  This function may return an error with an appropriate error tuple if the
  `child_id` is not found, or if the current process is running or being
  restarted.

  If the child process start function returns an error tuple or an erroneous value,
  or if it fails, this function returns `{:error, error}`.

  This operation is not supported by `:simple_one_for_one` supervisors.
  """
  @spec restart_child(supervisor, term()) ::
        {:ok, child} | {:ok, child, term} | {:error, error}
        when error: :not_found | :simple_one_for_one | :running | :restarting | term
  def restart_child(supervisor, child_id) do
    call(supervisor, {:restart_child, child_id})
  end

  @doc """
  Returns a list with information about all children of the given supervisor.

  Note that calling this function when supervising a large number of children
  under low memory conditions can cause an out of memory exception.

  This function returns a list of `{id, child, type, modules}` tuples, where:

    * `id` - as defined in the child specification or `:undefined` in the case
      of a `simple_one_for_one` supervisor

    * `child` - the PID of the corresponding child process, `:restarting` if the
      process is about to be restarted, or `:undefined` if there is no such
      process

    * `type` - `:worker` or `:supervisor`, as specified by the child specification

    * `modules` - as specified by the child specification

  """
  @spec which_children(supervisor) ::
        [{term() | :undefined,
          child | :restarting,
          :worker | :supervisor,
          :supervisor.modules}]
  def which_children(supervisor) do
    call(supervisor, :which_children)
  end

  @doc """
  Returns a map containing count values for the given supervisor.

  The map contains the following keys:

    * `:specs` - the total count of children, dead or alive

    * `:active` - the count of all actively running child processes managed by
      this supervisor

    * `:supervisors` - the count of all supervisors whether or not these
      child supervisors are still alive

    * `:workers` - the count of all workers, whether or not these child workers
      are still alive

  """
  @spec count_children(supervisor) ::
        %{specs: non_neg_integer, active: non_neg_integer,
          supervisors: non_neg_integer, workers: non_neg_integer}
  def count_children(supervisor) do
    call(supervisor, :count_children) |> :maps.from_list
  end

  @doc """
  Synchronously stops the given supervisor with the given `reason`.

  It returns `:ok` if the supervisor terminates with the given
  reason. If it terminates with another reason, the call exits.

  This function keeps OTP semantics regarding error reporting.
  If the reason is any other than `:normal`, `:shutdown` or
  `{:shutdown, _}`, an error report is logged.
  """
  @spec stop(supervisor, reason :: term, timeout) :: :ok
  def stop(supervisor, reason \\ :normal, timeout \\ :infinity) do
    :gen.stop(supervisor, reason, timeout)
  end

  @compile {:inline, call: 2}

  defp call(supervisor, req) do
    GenServer.call(supervisor, req, :infinity)
  end
end
