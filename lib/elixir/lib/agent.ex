defmodule Agent do
  @moduledoc """
  Agents are a simple abstraction around state.

  Often in Elixir there is a need to share or store state that
  must be accessed from different processes or by the same process
  at different points in time.

  The Agent module provides a basic server implementation that
  allows state to be retrieved and updated via a simple API.

  ## Examples

  For example, in the Mix tool that ships with Elixir, we need
  to keep a set of all tasks executed by a given project. Since
  this set is shared, we can implement it with an Agent:

      defmodule Mix.TasksServer do
        def start_link do
          Agent.start_link(fn -> MapSet.new end, name: __MODULE__)
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
          Agent.update(__MODULE__, &MapSet.put(&1, item))
        end

        @doc "Resets the executed tasks and returns the previous list of tasks"
        def take_all() do
          Agent.get_and_update(__MODULE__, fn set ->
            {Enum.into(set, []), MapSet.new}
          end)
        end
      end

  Note that agents still provide a segregation between the
  client and server APIs, as seen in GenServers. In particular,
  all code inside the function passed to the agent is executed
  by the agent. This distinction is important because you may
  want to avoid expensive operations inside the agent, as it will
  effectively block the agent until the request is fulfilled.

  Consider these two examples:

      # Compute in the agent/server
      def get_something(agent) do
        Agent.get(agent, fn state -> do_something_expensive(state) end)
      end

      # Compute in the agent/client
      def get_something(agent) do
        Agent.get(agent, &(&1)) |> do_something_expensive()
      end

  The first function blocks the agent. The second function copies
  all the state to the client and then executes the operation in the
  client. The difference is whether the data is large enough to require
  processing in the server, at least initially, or small enough to be
  sent to the client cheaply.

  ## Name Registration

  An Agent is bound to the same name registration rules as GenServers.
  Read more about it in the `GenServer` docs.

  ## A word on distributed agents

  It is important to consider the limitations of distributed agents. Agents
  provide two APIs, one that works with anonymous functions and another
  that expects an explicit module, function, and arguments.

  In a distributed setup with multiple nodes, the API that accepts anonymous
  functions only works if the caller (client) and the agent have the same
  version of the caller module.

  Keep in mind this issue also shows up when performing "rolling upgrades"
  with agents. By rolling upgrades we mean the following situation: you wish
  to deploy a new version of your software by *shutting down* some of your
  nodes and replacing them with nodes running a new version of the software.
  In this setup, part of your environment will have one version of a given
  module and the other part another version (the newer one) of the same module.

  The best solution is to simply use the explicit module, function, and arguments
  APIs when working with distributed agents.

  ## Hot code swapping

  An agent can have its code hot swapped live by simply passing a module,
  function, and args tuple to the update instruction. For example, imagine
  you have an agent named `:sample` and you want to convert its inner state
  from some dict structure to a map. It can be done with the following
  instruction:

      {:update, :sample, {:advanced, {Enum, :into, [%{}]}}}

  The agent's state will be added to the given list as the first argument.
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
  Starts an agent linked to the current process with the given function.

  This is often used to start the agent as part of a supervision tree.

  Once the agent is spawned, the given function is invoked and its return
  value is used as the agent state. Note that `start_link` does not return
  until the given function has returned.

  ## Options

  The `:name` option is used for registration as described in the module
  documentation.

  If the `:timeout` option is present, the agent is allowed to spend at most
  the given number of milliseconds on initialization or it will be terminated
  and the start function will return `{:error, :timeout}`.

  If the `:debug` option is present, the corresponding function in the
  [`:sys` module](http://www.erlang.org/doc/man/sys.html) will be invoked.

  If the `:spawn_opt` option is present, its value will be passed as options
  to the underlying process as in `Process.spawn/4`.

  ## Return values

  If the server is successfully created and initialized, the function returns
  `{:ok, pid}`, where `pid` is the PID of the server. If an agent with the
  specified name already exists, the function returns
  `{:error, {:already_started, pid}}` with the PID of that process.

  If the given function callback fails with `reason`, the function returns
  `{:error, reason}`.
  """
  @spec start_link((() -> term), GenServer.options) :: on_start
  def start_link(fun, options \\ []) when is_function(fun, 0) do
    GenServer.start_link(Agent.Server, fun, options)
  end

  @doc """
  Starts an agent linked to the current process with the given module
  function and arguments.

  Same as `start_link/2` but a module, function and args are expected
  instead of an anonymous function.
  """
  @spec start_link(module, atom, [any], GenServer.options) :: on_start
  def start_link(module, fun, args, options \\ []) do
    GenServer.start_link(Agent.Server, {module, fun, args}, options)
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
  Starts an agent with the given module function and arguments.

  Similar to `start/2` but a module, function and args are expected
  instead of an anonymous function.
  """
  @spec start(module, atom, [any], GenServer.options) :: on_start
  def start(module, fun, args, options \\ []) do
    GenServer.start(Agent.Server, {module, fun, args}, options)
  end

  @doc """
  Gets an agent value via the given function.

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
  Gets an agent value via the given function.

  Same as `get/3` but a module, function and args are expected
  instead of an anonymous function. The state is added as first
  argument to the given list of args.
  """
  @spec get(agent, module, atom, [term], timeout) :: any
  def get(agent, module, fun, args, timeout \\ 5000) do
    GenServer.call(agent, {:get, {module, fun, args}}, timeout)
  end

  @doc """
  Gets and updates the agent state in one operation.

  The function `fun` is sent to the `agent` which invokes the function
  passing the agent state. The function must return a tuple with two
  elements, the first being the value to return (i.e. the `get` value)
  and the second one is the new state.

  A timeout can also be specified (it has a default value of 5000).
  """
  @spec get_and_update(agent, (state -> {a, state}), timeout) :: a when a: var
  def get_and_update(agent, fun, timeout \\ 5000) when is_function(fun, 1) do
    GenServer.call(agent, {:get_and_update, fun}, timeout)
  end

  @doc """
  Gets and updates the agent state in one operation.

  Same as `get_and_update/3` but a module, function and args are expected
  instead of an anonymous function. The state is added as first
  argument to the given list of args.
  """
  @spec get_and_update(agent, module, atom, [term], timeout) :: any
  def get_and_update(agent, module, fun, args, timeout \\ 5000) do
    GenServer.call(agent, {:get_and_update, {module, fun, args}}, timeout)
  end

  @doc """
  Updates the agent state.

  The function `fun` is sent to the `agent` which invokes the function
  passing the agent state. The function must return the new state.

  A timeout can also be specified (it has a default value of 5000).
  This function always returns `:ok`.
  """
  @spec update(agent, (state -> state), timeout) :: :ok
  def update(agent, fun, timeout \\ 5000) when is_function(fun, 1) do
    GenServer.call(agent, {:update, fun}, timeout)
  end

  @doc """
  Updates the agent state.

  Same as `update/3` but a module, function and args are expected
  instead of an anonymous function. The state is added as first
  argument to the given list of args.
  """
  @spec update(agent, module, atom, [term], timeout) :: :ok
  def update(agent, module, fun, args, timeout \\ 5000) do
    GenServer.call(agent, {:update, {module, fun, args}}, timeout)
  end

  @doc """
  Performs a cast (fire and forget) operation on the agent state.

  The function `fun` is sent to the `agent` which invokes the function
  passing the agent state. The function must return the new state.

  Note that `cast` returns `:ok` immediately, regardless of whether the
  destination node or agent exists.
  """
  @spec cast(agent, (state -> state)) :: :ok
  def cast(agent, fun) when is_function(fun, 1) do
    GenServer.cast(agent, {:cast, fun})
  end

  @doc """
  Performs a cast (fire and forget) operation on the agent state.

  Same as `cast/2` but a module, function and args are expected
  instead of an anonymous function. The state is added as first
  argument to the given list of args.
  """
  @spec cast(agent, module, atom, [term]) :: :ok
  def cast(agent, module, fun, args) do
    GenServer.cast(agent, {:cast, {module, fun, args}})
  end

  @doc """
  Stops the agent with the given `reason`.

  It returns `:ok` if the server terminates with the given
  reason, if it terminates with another reason, the call will
  exit.

  This function keeps OTP semantics regarding error reporting.
  If the reason is any other than `:normal`, `:shutdown` or
  `{:shutdown, _}`, an error report will be logged.
  """
  @spec stop(agent, reason :: term, timeout) :: :ok
  def stop(agent, reason \\ :normal, timeout \\ :infinity) do
    :gen.stop(agent, reason, timeout)
  end
end
