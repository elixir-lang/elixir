defmodule Supervisor do
  @moduledoc """
  A behaviour module for implementing supevision functionality.

  A supervisor is a process which supervises other processes called
  child processes. Supervisors are used to build an hierarchical process
  structure called a supervision tree, a nice way to structure a fault
  tolerant applications.

  A supervisor implemented using this module will have a standard set
  of interface functions and include functionality for tracing and error
  reporting. It will also fit into an supervision tree.

  ## Example

  In order to define a supervisor, we need to first define a child process
  that is going to be supervised. In order to do so, we will define a GenServer
  that represents a stack:

      defmodule Stack do
        use GenServer

        def start_link(state) do
          GenServer.start_link(__MODULE__, state, [name: :sup_stack])
        end

        def handle_call(:pop, _from, [h|t]) do
          {:reply, h, t}
        end

        def handle_cast({:push, h}, _from, t) do
          {:noreply, [h|t]}
        end
      end

  We can now define our supervisor and start it as follows:

      # Import helpers for defining supervisors
      import Supervisor.Spec

      # We are going to supervise the Stack server which will
      # be started with a single argument [:hello]
      children = [
        worker(Stack, [[:hello]])
      ]

      # Start the supervisor with our one children
      {:ok, pid} = Supervisor.start_link(children, strategy: :one_for_one)

  Notice that when starting the GenServer, we have registered it
  with name `:sup_stack`, which allows us to call it directly and
  get what is on the stack:

      GenServer.call(:sup_stack, :pop)
      #=> :hello

      GenServer.cast(:sup_stack, {:push, :world})
      #=> :ok

      GenServer.call(:sup_stack, :pop)
      #=> :world

  However, there is a bug in our stack server. If we call `:pop` and
  the stack is empty, it is going to crash because no clause matches.
  Let's try it:

      GenServer.call(:sup_stack, :pop)
      =ERROR REPORT====

  Luckily, since the server is being supervised by a supervisor, the
  supervisor will automatically start a new one, with the default stack
  of `[:hello]` like before:

      GenServer.call(:sup_stack, :pop) == :hello

  Supervisors support different strategies, in the example above, we
  have chosen `:one_for_one`. Furthermore, each supervisor can have many
  workers and supervisors as children, each of them with their specific
  configuration, shutdown values and restart strategies.

  Continue reading this module to learn more about supervision strategies
  and then follow to the `Supervisor.Spec` module documentation to learn
  about the specification for workers and supervisors.

  ## Name registering

  A supervisor is bound to the same name registering rules as a `GenServer`.
  Read more about it in the `GenServer` docs.

  ## Strategies

  * `:one_for_one` - If a child process terminates, only that
    process is restarted;

  * `:one_for_all` - If a child process terminates, all other child
    processes are terminated and then all child processes, including
    the terminated one, are restarted;

  * `:rest_for_one` - If a child process terminates, the "rest" of
    the child processes, i.e. the child processes after the terminated
    process in start order, are terminated. Then the terminated child
    process and the rest of the child processes are restarted;

  * `:simple_one_for_one` - Similar to `:one_for_one` but suits better
    when dynamically attaching children. This strategy requires the
    supervisor specification to contain only one children. Many functions
    in this module behave slightly differently when this strategy is
    used;

  """

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @behaviour :supervisor
      import Supervisor.Spec
    end
  end

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

  A strategy is required to be given as an option. Furthermore,
  the `:max_restarts` and `:max_seconds` value can be configured
  as described in `Supervisor.Spec.supervise/2` docs.

  The options can also be used to register a supervisor name,
  the supported values are described under the `Name Registering`
  section in the `GenServer` module docs.

  If the supervisor and its child processes are successfully created
  (i.e. if all child process start functions return `{:ok, child}`,
  `{:ok, child, info}`, or `:ignore`) the function returns
  `{:ok, pid}`, where `pid` is the pid of the supervisor. If there
  already exists a process with the specified name the function returns
  `{:error, {:already_started, pid}}`, where pid is the pid of that
  process.

  If any child process start function fails or returns an error tuple or
  an erroneous value, the supervisor will first terminate all already
  started child processes with reason `:shutdown` and then terminate
  itself and return `{:error, {:shutdown, reason}}`.
  """
  @spec start_link([tuple], options) :: on_start
  def start_link(children, options) when is_list(children) do
    spec = Supervisor.Spec.supervise(children, options)
    start_link(Supervisor.Default, spec, options)
  end

  @doc """
  Starts a supervisor module with the given `arg`.

  To start the supervisor, the `init/1` callback will be invoked
  in the given module. The `init/1` callback must return a
  supervision specification which can be created with the help
  of `Supervisor.Spec` module.

  If the `init/1` callback returns `:ignore`, this function returns
  `:ignore` as well and the supervisor terminates with reason `:normal`.
  If it fails or returns an incorrect value, this function returns
  `{:error, term} where term is a term with information about the
  error, and the supervisor terminates with reason `term`.

  The `:name` option can also be given in order to register a supervisor
  name, the supported values are described under the `Name Registering`
  section in the `GenServer` module docs.

  Other failure conditions are specified in `start_link/2` docs.
  """
  @spec start_link(module, term, options) :: on_start
  def start_link(module, arg, options \\ []) do
    case Keyword.get(options, :name) do
      nil ->
        :supervisor.start_link(module, arg)
      atom when is_atom(atom) ->
        :supervisor.start_link({:local, atom}, module, arg)
      other when is_tuple(other) ->
        :supervisor.start_link(other, module, arg)
    end
  end

  @doc """
  Dynamically adds and starts a child specification to the supervisor.

  `child_spec` should be a valid child specification (unless the supervisor
  is a `:simple_one_for_one` supervisor, see below). The child process will
  be started as defined in the child specification.

  In the case of a `:simple_one_for_one`, the child specification defined in
  the supervisor will be used and instead of a `child_spec`, an arbitrary list
  of terms is expected. The child process will then be started by appending
  the given list to the existing function arguments in the child specification.

  If there already exists a child specification with the specified id,
  `child_spec` is discarded and the function returns an error with `:already_started`
  or `:already_present` if the corresponding child process is running or not.

  If the child process start function returns `{:ok, child}` or `{:ok, child, info}`,
  the child specification and pid is added to the supervisor and the function returns
  the same value.

  If the child process start function returns `:ignore, the child specification is
  added to the supervisor, the pid is set to undefined and the function returns
  `{:ok, :undefined}`.

  If the child process start function returns an error tuple or an erroneous value,
  or if it fails, the child specification is discarded and the function returns
  `{:error, error}` where error is a term containing information about the error
  and child specification.
  """
  @spec start_child(supervisor, Supervisor.Spec.spec | [term]) :: on_start_child
  defdelegate start_child(supervisor, child_spec_or_args), to: :supervisor

  @doc """
  Terminates the given pid or child id.

  If the supervisor is not a `simple_one_for_one`, the child id is expected
  and the process, if there is one, is terminated and, the child specification is
  kept unless the child is temporary.

  In case of a `simple_one_for_one` supervisor, a pid is expected. If the child
  specification identifier is given instead instead of a `pid`, the function will
  return `{:error, :simple_one_for_one}`.

  Non-temporary child process may later be restarted by the supervisor. The child
  process can also be restarted explicitly by calling `restart_child/2`. Use
  `delete_child/2` to remove the child specification.

  If successful, the function returns ok. If there is no child specification or
  pid, the function returns `{:error, :not_found}`.
  """
  @spec terminate_child(supervisor, pid | Supervisor.Spec.child_id) :: :ok | {:error, error}
        when error: :not_found | :simple_one_for_one
  defdelegate terminate_child(supervisor, pid_or_child_id), to: :supervisor

  @doc """
  Deletes the child specification identified by `child_id`.

  The corresponding child process must not be running, use `terminate_child/2`
  to terminate it.

  If successful, the function returns `:ok`. This function may error with an
  appropriate error tuple if the `child_id` is not found, or if the current
  process is running or being restarted.

  This operation is not supported by `simple_one_for_one` supervisors.
  """
  @spec delete_child(supervisor, Supervisor.Spec.child_id) :: :ok | {:error, error}
        when error: :not_found | :simple_one_for_one | :running | :restarting
  defdelegate delete_child(supervisor, child_id), to: :supervisor

  @doc """
  Restarts a child process identified by `child_id`.

  The child specification must exist and the corresponding child process must not
  be running.

  Note that for temporary children, the child specification is automatically deleted
  when the child terminates, and thus it is not possible to restart such children.

  If the child process start function returns `{:ok, child}` or
  `{:ok, child, info}`, the pid is added to the supervisor and the function returns
  the same value.

  If the child process start function returns `:ignore`, the pid remains set to
  `:undefined` and the function returns `{:ok, :undefined}`.

  This function may error with an appropriate error tuple if the `child_id` is not
  found, or if the current process is running or being restarted.

  If the child process start function returns an error tuple or an erroneous value,
  or if it fails, the function returns `{:error, error}`.

  This operation is not supported by `simple_one_for_one` supervisors.
  """
  @spec restart_child(supervisor, Supervisor.Spec.child_id) ::
        {:ok, child} | {:ok, child, term} | {:error, error}
        when error: :not_found | :simple_one_for_one | :running | :restarting | term
  defdelegate restart_child(supervisor, child_id), to: :supervisor

  @doc """
  Returns a list with information about all children.

  Note that calling this function when supervising a large number of children
  under low memory conditions can cause an out of memory exception.

  This function returns a list of tuples containing:

  * the `id` - as defined in the child specification or `:undefined` in the case
    of a `simple_one_for_one` supervisor;

  * the `child` - the pid of the corresponding child process, the atom `:restarting`
    if the process is about to be restarted or `:undefined` if there is no such process;

  * the `type` - `:worker` or `:supervisor` as defined in the child specification;

  * the `modules` as defined in the child specification;
  """
  @spec which_children(supervisor) ::
        [{Supervisor.Spec.child_id | :undefined,
           child | :restarting,
           Supervisor.Spec.worker,
           Supervisor.Spec.modules}]
  defdelegate which_children(supervisor), to: :supervisor

  @doc """
  Returns a map containing count value for the supervisor.

  The map contains the following keys:

  * `:specs` - the total count of children, dead or alive;

  * `:active` - the count of all actively running child processes managed by this supervisor;

  * `:supervisors` - the count of all supervisors whether or not the child process is still alive;

  * `:workers` - the count of all workers, whether or not the child process is still alive;

  """
  @spec count_children(supervisor) ::
        [specs: non_neg_integer, active: non_neg_integer,
         supervisors: non_neg_integer, workers: non_neg_integer]
  def count_children(supervisor) do
    :supervisor.count_children(supervisor) |> :maps.from_list
  end
end
