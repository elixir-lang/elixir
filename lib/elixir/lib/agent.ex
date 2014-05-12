defmodule Agent do
  @moduledoc """
  Agents are a simple abstraction around state.

  Often in Elixir there is a need to share or store state that
  must be accessed from different processes or by a same process
  in different points in time.

  The Agent module provides a basic server implementation that
  allows state to be retrieved and updated via a simple API.

  ## Examples

  For example, in the Mix tool that ships with Elixir, we need
  to keep a set of all tasks executed by a given project. Since
  this set is shared, we can implement it with an Agent:

      defmodule Mix.TasksServer do
        def start_link do
          Agent.start_link(fn -> HashSet.new end, name: __MODULE__)
        end

        @doc "Checks if the task has already executed"
        def executed?(task, project) do
          item = {task, project}
          Agent.get(__MODULE__, fn set ->
            item in set
          end)
        end

        @doc "Marks a task as executed"
        def put_task(task, project) do
          item = {task, project}
          Agent.update(__MODULE__, &Set.put(item, &1))
        end
      end

  Note that agents still provide a segregation in between the
  client and server APIs, as seen in GenServers. In particular,
  all code inside the function passed to the agent is executed
  by the agent. This distinction is important because you may
  want to avoid expensive operations inside the agent, as it will
  effectively block the agent until the request is fullfilled.

  Consider these two examples:

      # Compute in the agent/server
      def get_something(agent) do
        Agent.get(agent, fn state -> do_something_expensive(state) end)
      end

      # Compute in the agent/client
      def get_something(agent) do
        Agent.get(agent, &(&1)) |> do_something_expensive()
      end

  The first one blocks the agent while the second one moves copies
  all the state to the client and execute the operation in the client.
  The trade-off here is exactly if the data is small enough to be
  sent to the client cheaply or large enough to require processing on
  the server (or at least some initial processing).

  ## Name registering

  A GenEvent is bound to the same name registering rules as a `GenServer`.
  Read more about it in the `GenServer` docs.

  ## A word on distributed agents

  It is important to consider the limitation of distributed agents. Agents
  work by sending anonymous functions in between the caller and the agent.
  In a distributed setup with multiple nodes, agents only work if the caller
  (client) and the agent have the same version of a given module.

  This setup may exhibit issues when doing "rolling upgrades". By rolling
  upgrades we mean that you desire to deploy a new version your software
  by *shutting down* some of your nodes and replace them by nodes running
  a new version of the software. In this setup, part of your environment
  will have one version of a given module and other part another version
  of the same module, which may lead agents to crash. That said, if you
  plan to run on distributed environments, agents should likely be avoided.

  Note however agents work fine if you want to perform hot code swap, as
  hot code swapping keeps both the old and new versions of a given module.
  We detail how to do hot code swapping with agents in the next section.

  ## Hot code swapping

  An agent can have its code hot swapped live by simply passing a module,
  function and args tuple to the update instruction. For example, imagine
  you have an agent named `:sample` and you want to convert its inner state
  from some dict structure to a map. It can be done with the following
  instruction:

      {:update, :sample, {:advanced, {Enum, :into, [%{}]}}}

  The agent state will be added as first argument of the arguments list.
  """

  @typedoc "Return values of `start*` functions"
  @type on_start :: {:ok, pid} | {:error, {:already_started, pid} | term}

  @typedoc "The agent name"
  @type name :: atom | {:global, term} | {:via, module, term}

  @typedoc "The agent reference"
  @type agent :: pid | {atom, node} | name

  @typedoc "The agent state"
  @type state :: term

  @doc """
  Starts an agent linked to the current process.

  This is often used to start the agent as part of a supervision tree.

  Once the agent is spawned, the given function is invoked and its return
  value is used as the agent state. Note this function does not return
  until the given function has returned.

  ## Options

  The `:name` option is used for name registered as described in the module
  documentation. If the option `:timeout` option is present, the agent is
  allowed to spend the given milliseconds initializing or it will be terminated
  and the start function will return `{:error, :timeout}`.

  If the option `:debug` is present, the corresponding function in the
  [`:sys` module](http://www.erlang.org/doc/man/sys.html) will be invoked.

  If the option `:spawn_opt` is present, the given options will be passed
  to the underlying process as in `Process.spawn/3`.

  ## Return values

  If the server is successfully created and initialized the function returns
  `{:ok, pid}`, where pid is the pid of the server. If there already exists
  an agent with the specified name the function returns
  `{:error, {:already_started, pid}}` with the pid of that process.

  If the given function callback fails with reason, the function returns
  `{:error, reason}`.
  """
  @spec start_link((() -> term), GenServer.options) :: on_start
  def start_link(fun, options \\ []) when is_function(fun, 0) do
    GenServer.start_link(Agent.Server, fun, options)
  end

  @doc """
  Starts an agent process without links (outside of a supervision tree).

  See `start_link/2` for more information.
  """
  @spec start((() -> term), GenServer.options) :: on_start
  def start(fun, options \\ []) when is_function(fun, 0) do
    GenServer.start(Agent.Server, fun, options)
  end

  @doc """
  Gets the agent value and executes the given function.

  The function `fun` is sent to the `agent` which invokes the function
  passing the agent state. The result of the function invocation is
  returned.

  A timeout can also be specified (it has a default value of 5000).
  """
  @spec get(agent, (state -> a), timeout) :: a when a: var
  def get(agent, fun, timeout \\ 5000) when is_function(fun, 1) do
    GenServer.call(agent, {:get, fun}, timeout)
  end

  @doc """
  Gets and updates the agent state in one operation.

  The function `fun` is sent to the `agent` which invokes the function
  passing the agent state. The function must return a tuple with two
  elements, the first being the value to returned (i.e. the get value)
  and the second one is the new state.

  A timeout can also be specified (it has a default value of 5000).
  """
  @spec get_and_update(agent, (state -> {a, state}), timeout) :: a when a: var
  def get_and_update(agent, fun, timeout \\ 5000) when is_function(fun, 1) do
    GenServer.call(agent, {:get_and_update, fun}, timeout)
  end

  @doc """
  Updates the agent state.

  The function `fun` is sent to the `agent` which invokes the function
  passing the agent state. The function must return the new state.

  A timeout can also be specified (it has a default value of 5000).
  This function always return `:ok`.
  """
  @spec update(agent, (state -> state)) :: :ok
  def update(agent, fun, timeout \\ 5000) when is_function(fun, 1) do
    GenServer.call(agent, {:update, fun}, timeout)
  end

  @doc """
  Performs a cast (fire and forget) operation on the agent state.

  The function `fun` is sent to the `agent` which invokes the function
  passing the agent state. The function must return the new state.

  Note this function returns `:ok` immediately, ignoring if the
  destination node or agent does not exist.
  """
  @spec cast(agent, (state -> state)) :: :ok
  def cast(agent, fun) when is_function(fun, 1) do
    GenServer.cast(agent, fun)
  end

  @doc """
  Stops the agent.

  Returns `:ok` if the agent is stopped in the given `timeout`.
  """
  @spec stop(agent, timeout) :: :ok
  def stop(agent, timeout \\ 5000) do
    GenServer.call(agent, :stop, timeout)
  end
end
