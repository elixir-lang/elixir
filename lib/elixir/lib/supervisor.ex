defmodule Supervisor do
  @moduledoc ~S"""
  A behaviour module for implementing supervision functionality.

  A supervisor is a process which supervises other processes, which we refer
  to as *child processes*. Supervisors are used to build a hierarchical process
  structure called a *supervision tree*. Supervision trees are a nice way to
  structure fault-tolerant applications.

  A supervisor implemented using this module has a standard set
  of interface functions and includes functionality for tracing and error
  reporting. It also fits into a supervision tree.

  ## Examples

  In order to define a supervisor, we need to first define a child process
  that is going to be supervised. In order to do so, we will define a GenServer
  that represents a stack:

      defmodule Stack do
        use GenServer

        def start_link(state, opts \\ []) do
          GenServer.start_link(__MODULE__, state, opts)
        end

        def handle_call(:pop, _from, [h | t]) do
          {:reply, h, t}
        end

        def handle_cast({:push, h}, t) do
          {:noreply, [h | t]}
        end
      end

  We can now define our supervisor and start it as follows:

      # Import helpers for defining supervisors
      import Supervisor.Spec

      # Supervise the Stack server which will be started with
      # two arguments. The initial stack, [:hello], and a
      # keyword list containing the GenServer options that
      # set the registered name of the server to MyStack.
      children = [
        worker(Stack, [[:hello], [name: MyStack]])
      ]

      # Start the supervisor with our child
      {:ok, pid} = Supervisor.start_link(children, strategy: :one_for_one)

      # There is one child worker started
      Supervisor.count_children(pid)
      #=> %{active: 1, specs: 1, supervisors: 0, workers: 1}

  Notice that when starting the GenServer, we are registering it
  with name `MyStack`, which allows us to call it directly and
  get what is on the stack:

      GenServer.call(MyStack, :pop)
      #=> :hello

      GenServer.cast(MyStack, {:push, :world})
      #=> :ok

      GenServer.call(MyStack, :pop)
      #=> :world

  However, there is a bug in our stack server. If we call `:pop` and
  the stack is empty, it is going to crash because no clause matches:

      GenServer.call(MyStack, :pop)
      ** (exit) exited in: GenServer.call(MyStack, :pop, 5000)

  Luckily, since the server is being supervised by a supervisor, the
  supervisor will automatically start a new one, with the initial stack
  of `[:hello]`:

      GenServer.call(MyStack, :pop)
      #=> :hello

  Supervisors support different strategies; in the example above, we
  have chosen `:one_for_one`. Furthermore, each supervisor can have many
  workers and supervisors as children, each of them with their specific
  configuration, shutdown values, and restart strategies.

  The rest of this documentation will cover supervision strategies; also read
  the documentation for the `Supervisor.Spec` module to learn about the
  specification for workers and supervisors.

  ## Module-based supervisors

  In the example above, a supervisor was started by passing the supervision
  structure to `start_link/2`. However, supervisors can also be created by
  explicitly defining a supervision module:

      defmodule MyApp.Supervisor do
        # Automatically imports Supervisor.Spec
        use Supervisor

        def start_link do
          Supervisor.start_link(__MODULE__, [])
        end

        def init([]) do
          children = [
            worker(Stack, [[:hello]])
          ]

          # supervise/2 is imported from Supervisor.Spec
          supervise(children, strategy: :one_for_one)
        end
      end

  You may want to use a module-based supervisor if:

    * You need to perform some particular action on supervisor
      initialization, like setting up an ETS table.

    * You want to perform partial hot-code swapping of the
      tree. For example, if you add or remove children,
      the module-based supervision will add and remove the
      new children directly, while dynamic supervision
      requires the whole tree to be restarted in order to
      perform such swaps.

  ## Strategies

  Supervisors support different supervision strategies (through the `:strategy`
  option, as seen above):

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

  The `:simple_one_for_one` supervisor is useful when you want to dynamically
  start and stop supervised children. For example, imagine you want to
  dynamically create multiple stacks. We can do so by defining a `:simple_one_for_one`
  supervisor:

      # Import helpers for defining supervisors
      import Supervisor.Spec

      # This time, we don't pass any argument because
      # the argument will be given when we start the child
      children = [
        worker(Stack, [], restart: :transient)
      ]

      # Start the supervisor with our one child as a template
      {:ok, sup_pid} = Supervisor.start_link(children, strategy: :simple_one_for_one)

      # No child worker is active yet until start_child is called
      Supervisor.count_children(sup_pid)
      #=> %{active: 0, specs: 1, supervisors: 0, workers: 0}

  There are a couple differences here:

    * the simple one for one specification can define only one child which
      works as a template for when we call `start_child/2`

    * we have defined the child to have a restart strategy of `:transient`. This
      means that, if the child process exits due to a `:normal`, `:shutdown`,
      or `{:shutdown, term}` reason, it won't be restarted. This is useful
      as it allows our workers to politely shutdown and be removed from the
      `:simple_one_for_one` supervisor, without being restarted. You can find
      more information about restart strategies in the documentation for the
      `Supervisor.Spec` module

  With the supervisor defined, let's dynamically start stacks:

      {:ok, pid} = Supervisor.start_child(sup_pid, [[:hello, :world], []])
      GenServer.call(pid, :pop) #=> :hello
      GenServer.call(pid, :pop) #=> :world

      {:ok, pid} = Supervisor.start_child(sup_pid, [[:something, :else], []])
      GenServer.call(pid, :pop) #=> :something
      GenServer.call(pid, :pop) #=> :else

      Supervisor.count_children(sup_pid)
      #=> %{active: 2, specs: 1, supervisors: 0, workers: 2}

  ## Exit reasons

  From the example above, you may have noticed that the `:transient` restart
  strategy for the worker does not restart the child in case it exits with
  reason `:normal`, `:shutdown` or `{:shutdown, term}`.

  So one may ask: which exit reason should I choose when exiting my worker?
  There are three options:

    * `:normal` - in such cases, the exit won't be logged, there is no restart
      in transient mode, and linked processes do not exit

    * `:shutdown` or `{:shutdown, term}` - in such cases, the exit won't be
      logged, there is no restart in transient mode, and linked processes exit
      with the same reason unless they're trapping exits

    * any other term - in such cases, the exit will be logged, there are
      restarts in transient mode, and linked processes exit with the same reason
      unless they're trapping exits

  ## Name registration

  A supervisor is bound to the same name registration rules as a `GenServer`.
  Read more about these rules in the documentation for `GenServer`.

  """

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @behaviour Supervisor
      import Supervisor.Spec

      @doc false
      def init(arg)
    end
  end

  @doc """
  Callback invoked to start the supervisor and during hot code upgrades.
  """
  # TODO: Support {:ok, [child_spec], Keyword.t}
  # TODO: Document options here and update Supervisor.Spec
  @callback init(args :: term) ::
    {:ok, {:supervisor.sup_flags, [Supervisor.Spec.spec]}} |
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

  @typedoc "Options used by the `start*` functions"
  @type options :: [name: name,
                    strategy: Supervisor.Spec.strategy,
                    max_restarts: non_neg_integer,
                    max_seconds: non_neg_integer]

  @typedoc "The supervisor reference"
  @type supervisor :: pid | name | {atom, node}

  @doc """
  Starts a supervisor with the given children.

  A strategy is required to be provided through the `:strategy` option.
  Furthermore, the `:max_restarts` and `:max_seconds` options can be
  configured as described in the documentation for `Supervisor.Spec.supervise/2`.

  The options can also be used to register a supervisor name.
  The supported values are described under the "Name registration"
  section in the `GenServer` module docs.

  If the supervisor and its child processes are successfully created
  (i.e., if the start function of each child process returns `{:ok, child}`,
  `{:ok, child, info}`, or `:ignore`) this function returns
  `{:ok, pid}`, where `pid` is the PID of the supervisor. If a process with the
   specified name already exists, the function returns `{:error,
   {:already_started, pid}}`, where `pid` is the PID of that process.

  If the start function of any of the child processes fails or returns an error
  tuple or an erroneous value, the supervisor first terminates with reason
  `:shutdown` all the child processes that have already been started, and then
  terminates itself and returns `{:error, {:shutdown, reason}}`.

  Note that a supervisor started with this function is linked to the parent
  process and exits not only on crashes but also if the parent process exits
  with `:normal` reason.
  """
  @spec start_link([Supervisor.Spec.spec], options) :: on_start
  def start_link(children, options) when is_list(children) do
    spec = Supervisor.Spec.supervise(children, options)
    start_link(Supervisor.Default, spec, options)
  end

  @doc """
  Starts a supervisor process with the given `module` and `arg`.

  To start the supervisor, the `c:init/1` callback will be invoked in the given
  `module`, with `arg` as its argument. The `c:init/1` callback must return a
  supervisor specification which can be created with the help of the functions
  in the `Supervisor.Spec` module (especially `Supervisor.Spec.supervise/2`).

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
  @spec start_link(module, term, options) :: on_start
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
  @spec start_child(supervisor, Supervisor.Spec.spec | [term]) :: on_start_child
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
  @spec terminate_child(supervisor, pid | Supervisor.Spec.child_id) :: :ok | {:error, error}
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
  @spec delete_child(supervisor, Supervisor.Spec.child_id) :: :ok | {:error, error}
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
  @spec restart_child(supervisor, Supervisor.Spec.child_id) ::
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
        [{Supervisor.Spec.child_id | :undefined,
           child | :restarting,
           Supervisor.Spec.worker,
           Supervisor.Spec.modules}]
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
  Stops the given supervisor with the given `reason`.

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
