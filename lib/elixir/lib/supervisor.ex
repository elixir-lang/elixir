defmodule Supervisor do
  @moduledoc ~S"""
  A behaviour module for implementing supervisors.

  A supervisor is a process which supervises other processes, which we
  refer to as *child processes*. Supervisors are used to build a hierarchical
  process structure called a *supervision tree*. Supervision trees provide
  fault-tolerance and encapsulate how our applications start and shutdown.

  A supervisor may be started directly with a list of children via
  `start_link/2` or you may define a module-based supervisor that implements
  the required callbacks. The sections below use `start_link/2` to start
  supervisors in most examples, but it also includes a specific section
  on module-based ones.

  ## Examples

  In order to start a supervisor, we need to first define a child process
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
  stack process. The first step is to define a list of **child
  specifications** that control how each child behaves. Each child
  specification is a map, as shown below:

      children = [
        # The Stack is a child started via Stack.start_link([:hello])
        %{
          id: Stack,
          start: {Stack, :start_link, [[:hello]]}
        }
      ]

      # Now we start the supervisor with the children and a strategy
      {:ok, pid} = Supervisor.start_link(children, strategy: :one_for_one)

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

  ## Start and shutdown

  When the supervisor starts, it traverses all children and retrieves
  each child specification. It is at this moment `{Stack, [:hello]}`
  becomes a child specification by calling `Stack.child_spec([:hello])`.

  Then the supervisor starts each child in the order they are defined.
  This is done by calling the function defined under the `:start` key
  in the child specification and typically defaults to `start_link/1`.

  The `start_link/1` (or a custom) is then called for each child process.
  The `start_link/1` function must return `{:ok, pid}` where `pid` is the
  process identifier of a new process that is linked to the supervisor.
  The child process usually starts its work by executing the `init/1`
  callback. Generally speaking, the `init` callback is where we initialize
  and configure the child process.

  The shutdown process happens in reverse order.

  When a supervisor shuts down, it terminates all children in the opposite
  order they are listed. The termination happens by sending a shutdown exit
  signal, via `Process.exit(child_pid, :shutdown)`, to the child process and
  then awaiting for a time interval for the child process to terminate. This
  interval defaults to 5000 milliseconds. If the child process does not
  terminate in this interval, the supervisor abruptly terminates the child
  with reason `:brutal_kill`. The shutdown time can be configured in the
  child specification which is fully detailed in the next section.

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

  Now that we understand the start and shutdown process, let's take a
  complete look at all of the options provided in the child specification.

  ## Child specification

  The child specification describes how the supervisor start, shutdown and
  restart child processes.

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

  Let's understand what the `:shutdown` and `:restart` options control.

  ### Shutdown values (:shutdown)

  The following shutdown values are supported in the `:shutdown` option:

    * `:brutal_kill` - the child process is unconditionally and immediately
      terminated using `Process.exit(child, :kill)`.

    * any integer >= 0 - the amount of time in milliseconds that the
      supervisor will wait for children to terminate after emitting a
      `Process.exit(child, :shutdown)` signal. If the child process is
      not trapping exits, the initial `:shutdown` signal will terminate
      the child process immediately. If the child process is trapping
      exits, it has the given amount of time in milliseconds to terminate.
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
  impact, see the "Exit reasons and restarts" section.

  ## child_spec/1

  When starting a supervisor, we pass a list of child specifications. Those
  specifications are maps that tell how the supervisor should start, stop and
  restart each of its children:

      %{
        id: Stack,
        start: {Stack, :start_link, [[:hello]]}
      }

  The map above defines a supervisor with `:id` of `Stack` that is started
  by calling `Stack.start_link([:hello])`.

  However, specifying the child specification for each child as a map can be
  quite error prone, as we may change the Stack implementation and forget to
  update its specification. That's why Elixir allows you to pass a tuple with
  the module name and the `start_link` argument instead of the specification:

      children = [
        {Stack, [:hello]}
      ]

  The supervisor will then invoke `Stack.child_spec([:hello])` to retrieve a
  child specification. Now the `Stack` module is responsible for building its
  own specification. By default, `use GenServer` defines a `Stack.child_spec/1`
  function which returns the same child specification we had before:

      %{
        id: Stack,
        start: {Stack, :start_link, [[:hello]]}
      }

  It is also possible to simply pass the `Stack` module as a child:

      children = [
        Stack
      ]

  When only the module name is given, it is equivalent to `{Stack, []}`. In this
  case, we will end-up with a child specification that looks like this:

      %{
        id: Stack,
        start: {Stack, :start_link, [[]]}
      }

  By replacing the map specification by `{Stack, [:hello]}` or `Stack`, we keep
  the child specification encapsulated in the Stack module, using the default
  implementation defined by `use GenServer`. We can now share our `Stack` worker
  with other developers and they can add it directly to their supervision tree
  without worrying about the low-level details of the worker.

  If you need to access or modify how a worker or a supervisor runs, you can use
  the `Supervisor.child_spec/2` function. For example, to run the stack with a
  different `:id` and a `:shutdown` value of 10 seconds (10_000 milliseconds):

      children = [
        Supervisor.child_spec({Stack, [:hello]}, id: MyStack, shutdown: 10_000)
      ]

  The call to `Supervisor.child_spec/2` above will return the following specification:

      %{
        id: MyStack,
        start: {Stack, :start_link, [[:hello]]},
        shutdown: 10_000
      }

  You may also configure the child specification in the Stack module itself to
  use a different `:id` or `:shutdown` value by passing options to `use GenServer`:

      defmodule Stack do
        use GenServer, id: MyStack, shutdown: 10_000

  The options above will customize the `Stack.child_spec/1` function defined
  by `use GenServer`. It accepts the same options as the `Supervisor.child_spec/2`
  function.

  You may also completely override the `child_spec/1` function in the Stack module
  and return your own child specification.

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
          children = [
            {Stack, [:hello]}
          ]

          Supervisor.init(children, strategy: :one_for_one)
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
      tree. The module-based approach allow you to add and remove
      children on a case-by-case basis.

  Note `use Supervisor` defines a `child_spec/1` function, allowing
  the defined module itself to be put under a supervision tree.
  The generated `child_spec/1` can be customized with the following
  options:

    * `:id` - the child specification id, defaults to the current module
    * `:start` - how to start the child process (defaults to calling `__MODULE__.start_link/1`)
    * `:restart` - when the supervisor should be restarted, defaults to `:permanent`

  ## `start_link/2`, `init/2`, and strategies

  So far we have started the supervisor passing a single child as a tuple
  as well as a strategy called `:one_for_one`:

      Supervisor.start_link([
        {Stack, [:hello]}
      ], strategy: :one_for_one)

  or from inside the `c:init/1` callback:

      Supervisor.init([
        {Stack, [:hello]}
      ], strategy: :one_for_one)

  Although we have mentioned that the supervisor automatically expands
  `{Stack, [:hello]}` to a child specification by calling
  `Stack.child_spec([:hello])`, we haven't formally defined all of the
  arguments accepted by `start_link/2` and `init/2`. Let's rectify that
  now.

  The first argument given to `start_link/2` is a list of children which may
  be either:

    * a map representing the child specification itself - as outlined in the
      "Child specification" section
    * a tuple with a module as first element and the start argument as second -
      such as `{Stack, [:hello]}`. In this case, `Stack.child_spec([:hello])`
      is called to retrieve the child specification
    * a module - such as `Stack`. In this case, `Stack.child_spec([])`
      is called to retrieve the child specification

  The second argument is a keyword list of options:

    * `:strategy` - the restart strategy option. It can be either
      `:one_for_one`, `:rest_for_one` or `:one_for_all`. See the
      "Strategies" section.

    * `:max_restarts` - the maximum number of restarts allowed in
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

  There is also a deprecated strategy called `:simple_one_for_one` which
  has been replaced by the `DynamicSupervisor`. The `:simple_one_for_one`
  supervisor was similar to `:one_for_one` but suits better when dynamically
  attaching children. Many functions in this module behaved slightly
  differently when this strategy is used.

  ## Name registration

  A supervisor is bound to the same name registration rules as a `GenServer`.
  Read more about these rules in the documentation for `GenServer`.
  """

  @doc false
  defmacro __using__(opts) do
    quote location: :keep do
      import Supervisor.Spec
      @behaviour Supervisor
      @opts unquote(opts)

      @doc false
      def child_spec(arg) do
        default = %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [arg]},
          type: :supervisor
        }

        Supervisor.child_spec(default, @opts)
      end

      defoverridable child_spec: 1

      @doc false
      def init(arg)
    end
  end

  @doc """
  Callback invoked to start the supervisor and during hot code upgrades.

  Developers typically invoke `Supervisor.init/2` at the end of their
  init callback to return the proper supervision flags.
  """
  @callback init(args :: term) ::
              {:ok, {:supervisor.sup_flags(), [:supervisor.child_spec()]}}
              | :ignore

  @typedoc "Return values of `start_link` functions"
  @type on_start ::
          {:ok, pid}
          | :ignore
          | {:error, {:already_started, pid} | {:shutdown, term} | term}

  @typedoc "Return values of `start_child` functions"
  @type on_start_child ::
          {:ok, child}
          | {:ok, child, info :: term}
          | {:error, {:already_started, child} | :already_present | term}

  @type child :: pid | :undefined

  @typedoc "The Supervisor name"
  @type name :: atom | {:global, term} | {:via, module, term}

  @typedoc "Option values used by the `start*` functions"
  @type option :: {:name, name} | init_option()

  @typedoc "Options used by the `start*` functions"
  @type options :: [option, ...]

  @typedoc "The supervisor reference"
  @type supervisor :: pid | name | {atom, node}

  @typedoc "Options given to `start_link/2` and `init/2`"
  @type init_option ::
          {:strategy, strategy}
          | {:max_restarts, non_neg_integer}
          | {:max_seconds, pos_integer}

  @typedoc "Supported strategies"
  @type strategy :: :one_for_one | :one_for_all | :rest_for_one

  # Note we have inlined all types for readability
  @typedoc "The supervisor specification"
  @type child_spec :: %{
          required(:id) => term(),
          required(:start) => {module(), atom(), [term()]},
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
  @spec start_link([:supervisor.child_spec() | {module, term} | module], options) :: on_start
  def start_link(children, options) when is_list(children) do
    {sup_opts, start_opts} = Keyword.split(options, [:strategy, :max_seconds, :max_restarts])
    start_link(Supervisor.Default, init(children, sup_opts), start_opts)
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
      `:one_for_one`, `:rest_for_one`, `:one_for_all`, or the deprecated
      `:simple_one_for_one`.

    * `:max_restarts` - the maximum number of restarts allowed in
      a time frame. Defaults to `3`.

    * `:max_seconds` - the time frame in which `:max_restarts` applies.
      Defaults to `5`.

  The `:strategy` option is required and by default a maximum of 3 restarts
  is allowed within 5 seconds. Check the `Supervisor` module for a detailed
  description of the available strategies.
  """
  # TODO: Warn if simple_one_for_one strategy is used on Elixir v1.8.
  @spec init([:supervisor.child_spec() | {module, term} | module], [init_option]) :: {:ok, tuple}
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
        case System.stacktrace() do
          [{^module, :child_spec, [^arg], _} | _] ->
            raise ArgumentError, child_spec_error(module)

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

    Got: #{inspect(other)}
    """
  end

  defp child_spec_error(module) do
    if Code.ensure_loaded?(module) do
      """
      The module #{inspect(module)} was given as a child to a supervisor
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
            id: #{inspect(module)},
            start: {#{inspect(module)}, :start_link, [arg1, arg2]}
          }

      See the Supervisor documentation for more information.
      """
    else
      "The module #{inspect(module)} was given as a child to a supervisor but it does not exist."
    end
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

  """
  @spec child_spec(child_spec() | {module, arg :: term} | module, keyword) :: child_spec()
  def child_spec(module_or_map, overrides)

  def child_spec({_, _, _, _, _, _} = tuple, _overrides) do
    raise ArgumentError,
          "old tuple-based child specification #{inspect(tuple)} " <>
            "is not supported in Supervisor.child_spec/2"
  end

  def child_spec(module_or_map, overrides) do
    Enum.reduce(overrides, init_child(module_or_map), fn
      {key, value}, acc when key in [:id, :start, :restart, :shutdown, :type, :modules] ->
        Map.put(acc, key, value)

      {key, _value}, _acc ->
        raise ArgumentError, "unknown key #{inspect(key)} in child specification override"
    end)
  end

  @doc """
  Starts a module-based supervisor process with the given `module` and `arg`.

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
  @spec start_link(module, term, GenServer.options()) :: on_start
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
  Adds a child specification to `supervisor` and starts that child.

  `child_spec` should be a valid child specification. The child process will
  be started as defined in the child specification.

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
  @spec start_child(supervisor, :supervisor.child_spec() | {module, term} | module | [term]) ::
          on_start_child
  def start_child(supervisor, {_, _, _, _, _, _} = child_spec) do
    call(supervisor, {:start_child, child_spec})
  end

  # TODO: Deprecate this on Elixir v1.8. Remove and update typespec on v2.0.
  def start_child(supervisor, args) when is_list(args) do
    call(supervisor, {:start_child, args})
  end

  def start_child(supervisor, child_spec) do
    call(supervisor, {:start_child, Supervisor.child_spec(child_spec, [])})
  end

  @doc """
  Terminates the given child identified by child id.

  The process is terminated, if there's one. The child specification is
  kept unless the child is temporary.

  A non-temporary child process may later be restarted by the supervisor.
  The child process can also be restarted explicitly by calling `restart_child/2`.
  Use `delete_child/2` to remove the child specification.

  If successful, this function returns `:ok`. If there is no child
  specification for the given child id, this function returns
  `{:error, :not_found}`.
  """
  @spec terminate_child(supervisor, term()) :: :ok | {:error, error}
        when error: :not_found | :simple_one_for_one
  # TODO: Deprecate this on Elixir v1.8
  def terminate_child(supervisor, child_id)

  def terminate_child(supervisor, pid) when is_pid(pid) do
    call(supervisor, {:terminate_child, pid})
  end

  def terminate_child(supervisor, child_id) do
    call(supervisor, {:terminate_child, child_id})
  end

  @doc """
  Deletes the child specification identified by `child_id`.

  The corresponding child process must not be running; use `terminate_child/2`
  to terminate it if it's running.

  If successful, this function returns `:ok`. This function may return an error
  with an appropriate error tuple if the `child_id` is not found, or if the
  current process is running or being restarted.
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
  """
  @spec restart_child(supervisor, term()) :: {:ok, child} | {:ok, child, term} | {:error, error}
        when error: :not_found | :simple_one_for_one | :running | :restarting | term
  def restart_child(supervisor, child_id) do
    call(supervisor, {:restart_child, child_id})
  end

  @doc """
  Returns a list with information about all children of the given supervisor.

  Note that calling this function when supervising a large number of children
  under low memory conditions can cause an out of memory exception.

  This function returns a list of `{id, child, type, modules}` tuples, where:

    * `id` - as defined in the child specification

    * `child` - the PID of the corresponding child process, `:restarting` if the
      process is about to be restarted, or `:undefined` if there is no such
      process

    * `type` - `:worker` or `:supervisor`, as specified by the child specification

    * `modules` - as specified by the child specification

  """
  @spec which_children(supervisor) :: [
          {term() | :undefined, child | :restarting, :worker | :supervisor, :supervisor.modules()}
        ]
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
  @spec count_children(supervisor) :: %{
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
  @spec stop(supervisor, reason :: term, timeout) :: :ok
  def stop(supervisor, reason \\ :normal, timeout \\ :infinity) do
    GenServer.stop(supervisor, reason, timeout)
  end

  @compile {:inline, call: 2}

  defp call(supervisor, req) do
    GenServer.call(supervisor, req, :infinity)
  end
end
