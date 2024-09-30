defmodule Supervisor do
  @moduledoc ~S"""
  A behaviour module for implementing supervisors.

  A supervisor is a process which supervises other processes, which we
  refer to as *child processes*. Supervisors are used to build a hierarchical
  process structure called a *supervision tree*. Supervision trees provide
  fault-tolerance and encapsulate how our applications start and shutdown.

  A supervisor may be started directly with a list of child specifications via
  `start_link/2` or you may define a module-based supervisor that implements
  the required callbacks. The sections below use `start_link/2` to start
  supervisors in most examples, but it also includes a specific section
  on module-based ones.

  ## Examples

  In order to start a supervisor, we need to first define a child process
  that will be supervised. As an example, we will define a `GenServer`,
  a generic server, that keeps a counter. Other processes can then send
  messages to this process to read the counter and bump its value.

  > #### Disclaimer {: .neutral}
  >
  > In practice you would not define a counter as a GenServer. Instead,
  > if you need a counter, you would pass it around as inputs and outputs to
  > the functions that need it. The reason we picked a counter in this example
  > is due to its simplicity, as it allows us to focus on how supervisors work.

      defmodule Counter do
        use GenServer

        def start_link(arg) when is_integer(arg) do
          GenServer.start_link(__MODULE__, arg, name: __MODULE__)
        end

        ## Callbacks

        @impl true
        def init(counter) do
          {:ok, counter}
        end

        @impl true
        def handle_call(:get, _from, counter) do
          {:reply, counter, counter}
        end

        def handle_call({:bump, value}, _from, counter) do
          {:reply, counter, counter + value}
        end
      end

  The `Counter` receives an argument on `start_link`. This argument
  is passed to the `init/1` callback which becomes the initial value
  of the counter. Our counter handles two operations (known as calls):
  `:get`, to get the current counter value, and `:bump`, that bumps
  the counter by the given `value` and returns the old counter.

  We can now start a supervisor that will start and supervise our
  counter process. The first step is to define a list of **child
  specifications** that control how each child behaves. Each child
  specification is a map, as shown below:

      children = [
        # The Counter is a child started via Counter.start_link(0)
        %{
          id: Counter,
          start: {Counter, :start_link, [0]}
        }
      ]

      # Now we start the supervisor with the children and a strategy
      {:ok, pid} = Supervisor.start_link(children, strategy: :one_for_one)

      # After started, we can query the supervisor for information
      Supervisor.count_children(pid)
      #=> %{active: 1, specs: 1, supervisors: 0, workers: 1}

  Note that when starting the GenServer, we are registering it
  with name `Counter` via the `name: __MODULE__` option. This allows
  us to call it directly and get its value:

      GenServer.call(Counter, :get)
      #=> 0

      GenServer.call(Counter, {:bump, 3})
      #=> 0

      GenServer.call(Counter, :get)
      #=> 3

  However, there is a bug in our counter server. If we call `:bump` with
  a non-numeric value, it is going to crash:

      GenServer.call(Counter, {:bump, "oops"})
      ** (exit) exited in: GenServer.call(Counter, {:bump, "oops"}, 5000)

  Luckily, since the server is being supervised by a supervisor, the
  supervisor will automatically start a new one, reset back to its initial
  value of `0`:

      GenServer.call(Counter, :get)
      #=> 0

  Supervisors support different strategies; in the example above, we
  have chosen `:one_for_one`. Furthermore, each supervisor can have many
  workers and/or supervisors as children, with each one having its own
  configuration (as outlined in the "Child specification" section).

  The rest of this document will cover how child processes are specified,
  how they can be started and stopped, different supervision strategies
  and more.

  ## Child specification

  The child specification describes how the supervisor starts, shuts down,
  and restarts child processes.

  The child specification is a map containing up to 6 elements. The first two keys
  in the following list are required, and the remaining ones are optional:

    * `:id` - any term used to identify the child specification internally by
      the supervisor; defaults to the given module. This key is required.
      For supervisors, in the case of conflicting `:id` values, the supervisor
      will refuse to initialize and require explicit IDs. This is not the case
      for [dynamic supervisors](`DynamicSupervisor`) though.

    * `:start` - a tuple with the module-function-args to be invoked
      to start the child process. This key is required.

    * `:restart` - an atom that defines when a terminated child process
       should be restarted (see the "Restart values" section below).
       This key is optional and defaults to `:permanent`.

    * `:shutdown` - an integer or atom that defines how a child process should
      be terminated (see the "Shutdown values" section below). This key
      is optional and defaults to `5_000` if the type is `:worker` or
      `:infinity` if the type is `:supervisor`.

    * `:type` - specifies that the child process is a `:worker` or a
      `:supervisor`. This key is optional and defaults to `:worker`.

    * `:modules` - a list of modules used by hot code upgrade mechanisms
      to determine which processes are using certain modules. It is typically
      set to the callback module of behaviours like `GenServer`, `Supervisor`,
      and such. It is set automatically based on the `:start` value and it is rarely
      changed in practice.

    * `:significant` - a boolean indicating if the child process should be
      considered significant with regard to automatic shutdown.  Only `:transient`
      and `:temporary` child processes can be marked as significant. This key is
      optional and defaults to `false`. See section "Automatic shutdown" below
      for more details.

  Let's understand what the `:shutdown` and `:restart` options control.

  ### Shutdown values (:shutdown)

  The following shutdown values are supported in the `:shutdown` option:

    * `:brutal_kill` - the child process is unconditionally and immediately
      terminated using `Process.exit(child, :kill)`.

    * any integer >= 0 - the amount of time in milliseconds that the
      supervisor will wait for its children to terminate after emitting a
      `Process.exit(child, :shutdown)` signal. If the child process is
      not trapping exits, the initial `:shutdown` signal will terminate
      the child process immediately. If the child process is trapping
      exits, it has the given amount of time to terminate.
      If it doesn't terminate within the specified time, the child process
      is unconditionally terminated by the supervisor via
      `Process.exit(child, :kill)`.

    * `:infinity` - works as an integer except the supervisor will wait
      indefinitely for the child to terminate. If the child process is a
      supervisor, the recommended value is `:infinity` to give the supervisor
      and its children enough time to shut down. This option can be used with
      regular workers but doing so is discouraged and requires extreme care.
      If not used carefully, the child process will never terminate,
      preventing your application from terminating as well.

  ### Restart values (:restart)

  The `:restart` option controls what the supervisor should consider to
  be a successful termination or not. If the termination is successful,
  the supervisor won't restart the child. If the child process crashed,
  the supervisor will start a new one.

  The following restart values are supported in the `:restart` option:

    * `:permanent` - the child process is always restarted.

    * `:temporary` - the child process is never restarted, regardless
      of the supervision strategy: any termination (even abnormal) is
      considered successful.

    * `:transient` - the child process is restarted only if it
      terminates abnormally, i.e., with an exit reason other than
      `:normal`, `:shutdown`, or `{:shutdown, term}`.

  For a more complete understanding of the exit reasons and their
  impact, see the "Exit reasons and restarts" section.

  ## `child_spec/1` function

  When starting a supervisor, we may pass a list of child specifications. Those
  specifications are maps that tell how the supervisor should start, stop and
  restart each of its children:

      %{
        id: Counter,
        start: {Counter, :start_link, [0]}
      }

  The map above defines a child with `:id` of `Counter` that is started
  by calling `Counter.start_link(0)`.

  However, defining the child specification for each child as a map can be
  quite error prone, as we may change the `Counter` implementation and forget
  to update its specification. That's why Elixir allows you to pass a tuple with
  the module name and the `start_link` argument instead of the specification:

      children = [
        {Counter, 0}
      ]

  The supervisor will then invoke `Counter.child_spec(0)` to retrieve a child
  specification. Now the `Counter` module is responsible for building its own
  specification, for example, we could write:

      def child_spec(arg) do
        %{
          id: Counter,
          start: {Counter, :start_link, [arg]}
        }
      end

  Then the supervisor will call `Counter.start_link(arg)` to start the child
  process. This flow is summarized in the diagram below. Caller is a process
  which spawns the Supervisor process. The Supervisor then proceeds to call
  your code (Module) to spawn its child process:

  ```mermaid
  sequenceDiagram
      participant C as Caller (Process)
      participant S as Supervisor (Process)
      participant M as Module (Code)

      note right of C: child is a {module, arg} specification
      C->>+S: Supervisor.start_link([child])
      S-->>+M: module.child_spec(arg)
      M-->>-S: %{id: term, start: {module, :start_link, [arg]}}
      S-->>+M: module.start_link(arg)
      M->>M: Spawns child process (child_pid)
      M-->>-S: {:ok, child_pid} | :ignore | {:error, reason}
      S->>-C: {:ok, supervisor_pid} | {:error, reason}
  ```

  Luckily for us, `use GenServer` already defines a `Counter.child_spec/1`
  exactly like above, so you don't need to write the definition above yourself.
  If you want to customize the automatically generated `child_spec/1` function,
  you can pass the options directly to `use GenServer`:

      use GenServer, restart: :transient

  Finally, note it is also possible to simply pass the `Counter` module as
  a child:

      children = [
        Counter
      ]

  When only the module name is given, it is equivalent to `{Counter, []}`,
  which in our case would be invalid, which is why we always pass the initial
  counter explicitly.

  By replacing the child specification with `{Counter, 0}`, we keep it
  encapsulated in the `Counter` module. We could now share our
  `Counter` implementation with other developers and they can add it directly
  to their supervision tree without worrying about the low-level details of
  the counter.

  Overall, a child specification can be one of the following:

    * a map representing the child specification itself - as outlined in the
      "Child specification" section

    * a tuple with a module as first element and the start argument as second -
      such as `{Counter, 0}`. In this case, `Counter.child_spec(0)` is called
      to retrieve the child specification

    * a module - such as `Counter`. In this case, `Counter.child_spec([])`
      would be called, which is invalid for the counter, but it is useful in
      many other cases, especially when you want to pass a list of options
      to the child process

  If you need to convert a `{module, arg}` tuple or a module child specification to a
  [child specification](`t:child_spec/0`) or modify a child specification itself,
  you can use the `Supervisor.child_spec/2` function.
  For example, to run the counter with a different `:id` and a `:shutdown` value of
  10 seconds (10_000 milliseconds):

      children = [
        Supervisor.child_spec({Counter, 0}, id: MyCounter, shutdown: 10_000)
      ]

  ## Supervisor strategies and options

  So far we have started the supervisor passing a single child as a tuple
  as well as a strategy called `:one_for_one`:

      children = [
        {Counter, 0}
      ]

      Supervisor.start_link(children, strategy: :one_for_one)

  The first argument given to `start_link/2` is a list of child
  specifications as defined in the "child_spec/1" section above.

  The second argument is a keyword list of options:

    * `:strategy` - the supervision strategy option. It can be either
      `:one_for_one`, `:rest_for_one` or `:one_for_all`. Required.
      See the "Strategies" section.

    * `:max_restarts` - the maximum number of restarts allowed in
      a time frame. Defaults to `3`.

    * `:max_seconds` - the time frame in which `:max_restarts` applies.
      Defaults to `5`.

    * `:auto_shutdown` - the automatic shutdown option. It can be
      `:never`, `:any_significant`, or `:all_significant`. Optional.
      See the "Automatic shutdown" section.

    * `:name` - a name to register the supervisor process. Supported values are
      explained in the "Name registration" section in the documentation for
      `GenServer`. Optional.

  ### Strategies

  Supervisors support different supervision strategies (through the
  `:strategy` option, as seen above):

    * `:one_for_one` - if a child process terminates, only that
      process is restarted.

    * `:one_for_all` - if a child process terminates, all other child
      processes are terminated and then all child processes (including
      the terminated one) are restarted.

    * `:rest_for_one` - if a child process terminates, the terminated child
      process and the rest of the children started after it, are terminated and
      restarted.

  In the above, process termination refers to unsuccessful termination, which
  is determined by the `:restart` option.

  To efficiently supervise children started dynamically, see `DynamicSupervisor`.

  ### Automatic shutdown

  Supervisors have the ability to automatically shut themselves down when child
  processes marked as `:significant` exit.

  Supervisors support different automatic shutdown options (through
  the `:auto_shutdown` option, as seen above):

    * `:never` - this is the default, automatic shutdown is disabled.

    * `:any_significant` - if any significant child process exits, the supervisor
    will automatically shut down its children, then itself.

    * `:all_significant` - when all significant child processes have exited,
    the supervisor will automatically shut down its children, then itself.

  Only `:transient` and `:temporary` child processes can be marked as significant,
  and this configuration affects the behavior. Significant `:transient` child
  processes must exit normally for automatic shutdown to be considered, where
  `:temporary` child processes may exit for any reason.

  ### Name registration

  A supervisor is bound to the same name registration rules as a `GenServer`.
  Read more about these rules in the documentation for `GenServer`.

  ## Module-based supervisors

  In the example so far, the supervisor was started by passing the supervision
  structure to `start_link/2`. However, supervisors can also be created by
  explicitly defining a supervision module:

      defmodule MyApp.Supervisor do
        # Automatically defines child_spec/1
        use Supervisor

        def start_link(init_arg) do
          Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
        end

        @impl true
        def init(_init_arg) do
          children = [
            {Counter, 0}
          ]

          Supervisor.init(children, strategy: :one_for_one)
        end
      end

  The difference between the two approaches is that a module-based
  supervisor gives you more direct control over how the supervisor
  is initialized. Instead of calling `Supervisor.start_link/2` with
  a list of child specifications that are implicitly initialized for us,
  we must explicitly initialize the children by calling `Supervisor.init/2`
  inside its `c:init/1` callback. `Supervisor.init/2` accepts the same
  `:strategy`, `:max_restarts`, and `:max_seconds` options as `start_link/2`.

  > #### `use Supervisor` {: .info}
  >
  > When you `use Supervisor`, the `Supervisor` module will
  > set `@behaviour Supervisor` and define a `child_spec/1`
  > function, so your module can be used as a child
  > in a supervision tree.

  `use Supervisor` also defines a `child_spec/1` function which allows
  us to run `MyApp.Supervisor` as a child of another supervisor or
  at the top of your supervision tree as:

      children = [
        MyApp.Supervisor
      ]

      Supervisor.start_link(children, strategy: :one_for_one)

  A general guideline is to use the supervisor without a callback
  module only at the top of your supervision tree, generally in the
  `c:Application.start/2` callback. We recommend using module-based
  supervisors for any other supervisor in your application, so they
  can run as a child of another supervisor in the tree. The `child_spec/1`
  generated automatically by `Supervisor` can be customized with the
  following options:

    * `:id` - the child specification identifier, defaults to the current module
    * `:restart` - when the supervisor should be restarted, defaults to `:permanent`

  The `@doc` annotation immediately preceding `use Supervisor` will be
  attached to the generated `child_spec/1` function.

  ## Start and shutdown

  When the supervisor starts, it traverses all child specifications and
  then starts each child in the order they are defined. This is done by
  calling the function defined under the `:start` key in the child
  specification and typically defaults to `start_link/1`.

  The `start_link/1` (or a custom) is then called for each child process.
  The `start_link/1` function must return `{:ok, pid}` where `pid` is the
  process identifier of a new process that is linked to the supervisor.
  The child process usually starts its work by executing the `c:init/1`
  callback. Generally speaking, the `init` callback is where we initialize
  and configure the child process.

  The shutdown process happens in reverse order.

  When a supervisor shuts down, it terminates all children in the opposite
  order they are listed. The termination happens by sending a shutdown exit
  signal, via `Process.exit(child_pid, :shutdown)`, to the child process and
  then awaiting for a time interval for the child process to terminate. This
  interval defaults to 5000 milliseconds. If the child process does not
  terminate in this interval, the supervisor abruptly terminates the child
  with reason `:kill`. The shutdown time can be configured in the child
  specification which is fully detailed in the next section.

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

  ## Exit reasons and restarts

  A supervisor restarts a child process depending on its `:restart` configuration.
  For example, when `:restart` is set to `:transient`, the supervisor does not
  restart the child in case it exits with reason `:normal`, `:shutdown` or
  `{:shutdown, term}`.

  Those exits also impact logging. By default, behaviours such as GenServers
  do not emit error logs when the exit reason is `:normal`, `:shutdown` or
  `{:shutdown, term}`.

  So one may ask: which exit reason should I choose? There are three options:

    * `:normal` - in such cases, the exit won't be logged, there is no restart
      in transient mode, and linked processes do not exit

    * `:shutdown` or `{:shutdown, term}` - in such cases, the exit won't be
      logged, there is no restart in transient mode, and linked processes exit
      with the same reason unless they're trapping exits

    * any other term - in such cases, the exit will be logged, there are
      restarts in transient mode, and linked processes exit with the same
      reason unless they're trapping exits

  Generally speaking, if you are exiting for expected reasons, you want to use
  `:shutdown` or `{:shutdown, term}`.

  Note that the supervisor that reaches maximum restart intensity will exit with
  `:shutdown` reason. In this case the supervisor will only be restarted if its
  child specification was defined with the `:restart` option set to `:permanent`
  (the default).
  """

  @doc false
  defmacro __using__(opts) do
    quote location: :keep, bind_quoted: [opts: opts] do
      import Supervisor.Spec
      @behaviour Supervisor

      if not Module.has_attribute?(__MODULE__, :doc) do
        @doc """
        Returns a specification to start this module under a supervisor.

        See `Supervisor`.
        """
      end

      def child_spec(init_arg) do
        default = %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [init_arg]},
          type: :supervisor
        }

        Supervisor.child_spec(default, unquote(Macro.escape(opts)))
      end

      defoverridable child_spec: 1
    end
  end

  @doc """
  Callback invoked to start the supervisor and during hot code upgrades.

  Developers typically invoke `Supervisor.init/2` at the end of their
  init callback to return the proper supervision flags.
  """
  @callback init(init_arg :: term) ::
              {:ok,
               {sup_flags(), [child_spec() | (old_erlang_child_spec :: :supervisor.child_spec())]}}
              | :ignore

  @typedoc "Return values of `start_link/2` and `start_link/3`."
  @type on_start ::
          {:ok, pid}
          | :ignore
          | {:error, {:already_started, pid} | {:shutdown, term} | term}

  @typedoc "Return values of `start_child/2`."
  @type on_start_child ::
          {:ok, child}
          | {:ok, child, info :: term}
          | {:error, {:already_started, child} | :already_present | term}

  @typedoc """
  A child process.

  It can be a PID when the child process was started, or `:undefined` when
  the child was created by a [dynamic supervisor](`DynamicSupervisor`).
  """
  @type child :: pid | :undefined

  @typedoc "The supervisor name."
  @type name :: atom | {:global, term} | {:via, module, term}

  @typedoc "Option values used by the `start_link/2` and `start_link/3` functions."
  @type option :: {:name, name}

  @typedoc "The supervisor flags returned on init."
  @type sup_flags() :: %{
          strategy: strategy(),
          intensity: non_neg_integer(),
          period: pos_integer(),
          auto_shutdown: auto_shutdown()
        }

  @typedoc "The supervisor reference."
  @type supervisor :: pid | name | {atom, node}

  @typedoc "Options given to `start_link/2` and `init/2`."
  @type init_option ::
          {:strategy, strategy}
          | {:max_restarts, non_neg_integer}
          | {:max_seconds, pos_integer}
          | {:auto_shutdown, auto_shutdown}

  @typedoc "Supported restart options."
  @type restart :: :permanent | :transient | :temporary

  @typedoc "Supported shutdown options."
  @type shutdown :: timeout() | :brutal_kill

  @typedoc "Supported strategies."
  @type strategy :: :one_for_one | :one_for_all | :rest_for_one

  @typedoc "Supported automatic shutdown options."
  @type auto_shutdown :: :never | :any_significant | :all_significant

  @typedoc """
  Type of a supervised child.

  Whether the supervised child is a worker or a supervisor.
  """
  @type type :: :worker | :supervisor

  # Note we have inlined all types for readability
  @typedoc """
  The supervisor child specification.

  It defines how the supervisor should start, stop and restart each of its children.
  """
  @type child_spec :: %{
          required(:id) => atom() | term(),
          required(:start) => {module(), function_name :: atom(), args :: [term()]},
          optional(:restart) => restart(),
          optional(:shutdown) => shutdown(),
          optional(:type) => type(),
          optional(:modules) => [module()] | :dynamic,
          optional(:significant) => boolean()
        }

  @typedoc """
  A module-based child spec.

  This is a form of child spec that you can pass to functions such as `child_spec/2`,
  `start_child/2`, and `start_link/2`, in addition to the normalized `t:child_spec/0`.

  A module-based child spec can be:

    * a **module** — the supervisor calls `module.child_spec([])` to retrieve the
      child specification

    * a **two-element tuple** in the shape of `{module, arg}` — the supervisor
      calls `module.child_spec(arg)` to retrieve the child specification

  """
  @typedoc since: "1.16.0"
  @type module_spec :: {module(), args :: term()} | module()

  @doc """
  Starts a supervisor with the given children.

  `children` is a list of the following forms:

    * a child specification (see `t:child_spec/0`)

    * a module, where the supervisor calls `module.child_spec([])`
      to retrieve the child specification (see `t:module_spec/0`)

    * a `{module, arg}` tuple, where the supervisor calls `module.child_spec(arg)`
      to retrieve the child specification (see `t:module_spec/0`)

    * a (old) Erlang-style child specification (see
      [`:supervisor.child_spec()`](`t::supervisor.child_spec/0`))

  A strategy is required to be provided through the `:strategy` option. See
  "Supervisor strategies and options" for examples and other options.

  The options can also be used to register a supervisor name.
  The supported values are described under the "Name registration"
  section in the `GenServer` module docs.

  If the supervisor and all child processes are successfully spawned
  (if the start function of each child process returns `{:ok, child}`,
  `{:ok, child, info}`, or `:ignore`), this function returns
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
  @spec start_link(
          [child_spec | module_spec | (old_erlang_child_spec :: :supervisor.child_spec())],
          [option | init_option]
        ) ::
          {:ok, pid} | {:error, {:already_started, pid} | {:shutdown, term} | term}
  def start_link(children, options) when is_list(children) do
    {sup_opts, start_opts} =
      Keyword.split(options, [:strategy, :max_seconds, :max_restarts, :auto_shutdown])

    start_link(Supervisor.Default, init(children, sup_opts), start_opts)
  end

  @doc """
  Receives a list of child specifications to initialize and a set of `options`.

  This is typically invoked at the end of the `c:init/1` callback of
  module-based supervisors. See the sections "Supervisor strategies and options" and
  "Module-based supervisors" in the module documentation for more information.

  This function returns a tuple containing the supervisor
  flags and child specifications.

  ## Examples

      def init(_init_arg) do
        children = [
          {Counter, 0}
        ]

        Supervisor.init(children, strategy: :one_for_one)
      end

  ## Options

    * `:strategy` - the supervision strategy option. It can be either
      `:one_for_one`, `:rest_for_one`, or `:one_for_all`

    * `:max_restarts` - the maximum number of restarts allowed in
      a time frame. Defaults to `3`.

    * `:max_seconds` - the time frame in seconds in which `:max_restarts`
      applies. Defaults to `5`.

    * `:auto_shutdown` - the automatic shutdown option. It can be either
      `:never`, `:any_significant`, or `:all_significant`

  The `:strategy` option is required and by default a maximum of 3 restarts
  is allowed within 5 seconds. Check the `Supervisor` module for a detailed
  description of the available strategies.
  """
  @doc since: "1.5.0"
  @spec init(
          [child_spec | module_spec | (old_erlang_child_spec :: :supervisor.child_spec())],
          [init_option]
        ) ::
          {:ok,
           {sup_flags(), [child_spec() | (old_erlang_child_spec :: :supervisor.child_spec())]}}
  def init(children, options) when is_list(children) and is_list(options) do
    strategy =
      case options[:strategy] do
        nil ->
          raise ArgumentError, "expected :strategy option to be given"

        :simple_one_for_one ->
          IO.warn(
            ":simple_one_for_one strategy is deprecated, please use DynamicSupervisor instead"
          )

          :simple_one_for_one

        other ->
          other
      end

    intensity = Keyword.get(options, :max_restarts, 3)
    period = Keyword.get(options, :max_seconds, 5)
    auto_shutdown = Keyword.get(options, :auto_shutdown, :never)

    flags = %{
      strategy: strategy,
      intensity: intensity,
      period: period,
      auto_shutdown: auto_shutdown
    }

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
        case __STACKTRACE__ do
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
    supervisors expect each child to be one of the following:

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

      See the Supervisor documentation for more information
      """
    else
      "The module #{inspect(module)} was given as a child to a supervisor but it does not exist"
    end
  end

  @doc """
  Builds and overrides a child specification.

  Similar to `start_link/2` and `init/2`, it expects a module, `{module, arg}`,
  or a [child specification](`t:child_spec/0`).

  If a two-element tuple in the shape of `{module, arg}` is given,
  the child specification is retrieved by calling `module.child_spec(arg)`.

  If a module is given, the child specification is retrieved by calling
  `module.child_spec([])`.

  After the child specification is retrieved, the fields on `overrides`
  are directly applied to the child spec. If `overrides` has keys that
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
  @spec child_spec(child_spec() | module_spec(), keyword()) :: child_spec()
  def child_spec(module_or_map, overrides)

  def child_spec({_, _, _, _, _, _} = tuple, _overrides) do
    raise ArgumentError,
          "old tuple-based child specification #{inspect(tuple)} " <>
            "is not supported in Supervisor.child_spec/2"
  end

  def child_spec(module_or_map, overrides) do
    Enum.reduce(overrides, init_child(module_or_map), fn
      {key, value}, acc
      when key in [:id, :start, :restart, :shutdown, :type, :modules, :significant] ->
        Map.put(acc, key, value)

      {key, _value}, _acc ->
        raise ArgumentError, "unknown key #{inspect(key)} in child specification override"
    end)
  end

  @doc """
  Starts a module-based supervisor process with the given `module` and `init_arg`.

  To start the supervisor, the `c:init/1` callback will be invoked in the given
  `module`, with `init_arg` as its argument. The `c:init/1` callback must return a
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

  # It is important to keep the two-arity spec because it is a catch-all
  # to start_link(children, options).
  @spec start_link(module, term) :: on_start
  @spec start_link(module, term, [option]) :: on_start
  def start_link(module, init_arg, options \\ []) when is_list(options) do
    case Keyword.get(options, :name) do
      nil ->
        :supervisor.start_link(module, init_arg)

      atom when is_atom(atom) ->
        :supervisor.start_link({:local, atom}, module, init_arg)

      {:global, _term} = tuple ->
        :supervisor.start_link(tuple, module, init_arg)

      {:via, via_module, _term} = tuple when is_atom(via_module) ->
        :supervisor.start_link(tuple, module, init_arg)

      other ->
        raise ArgumentError, """
        expected :name option to be one of the following:

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

  If a child specification with the specified ID already exists, `child_spec` is
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
  @spec start_child(
          supervisor,
          child_spec | module_spec | (old_erlang_child_spec :: :supervisor.child_spec())
        ) :: on_start_child
  def start_child(supervisor, {_, _, _, _, _, _} = child_spec) do
    call(supervisor, {:start_child, child_spec})
  end

  def start_child(supervisor, args) when is_list(args) do
    IO.warn_once(
      {__MODULE__, :start_child},
      fn ->
        "Supervisor.start_child/2 with a list of args is deprecated, please use DynamicSupervisor instead"
      end,
      _stacktrace_drop_levels = 2
    )

    call(supervisor, {:start_child, args})
  end

  def start_child(supervisor, child_spec) do
    call(supervisor, {:start_child, Supervisor.child_spec(child_spec, [])})
  end

  @doc """
  Terminates the given child identified by `child_id`.

  The process is terminated, if there's one. The child specification is
  kept unless the child is temporary.

  A non-temporary child process may later be restarted by the supervisor.
  The child process can also be restarted explicitly by calling `restart_child/2`.
  Use `delete_child/2` to remove the child specification.

  If successful, this function returns `:ok`. If there is no child
  specification for the given child ID, this function returns
  `{:error, :not_found}`.
  """
  @spec terminate_child(supervisor, term()) :: :ok | {:error, :not_found}
  def terminate_child(supervisor, child_id)

  def terminate_child(supervisor, pid) when is_pid(pid) do
    IO.warn(
      "Supervisor.terminate_child/2 with a PID is deprecated, please use DynamicSupervisor instead"
    )

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
        when error: :not_found | :running | :restarting
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
        when error: :not_found | :running | :restarting | term
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
          # inlining module() | :dynamic here because :supervisor.modules() is not exported
          {term() | :undefined, child | :restarting, :worker | :supervisor, [module()] | :dynamic}
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
