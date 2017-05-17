defmodule Agent do
  @moduledoc """
  Agents are a simple abstraction around state.

  Often in Elixir there is a need to share or store state that
  must be accessed from different processes or by the same process
  at different points in time.

  The `Agent` module provides a basic server implementation that
  allows state to be retrieved and updated via a simple API.

  ## Examples

  For example, in the Mix tool that ships with Elixir, we need
  to keep a set of all tasks executed by a given project. Since
  this set is shared, we can implement it with an agent:

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
  all code inside the function passed to `Agent` functions is executed
  by the agent. This distinction is important because you may
  want to avoid expensive operations inside the agent, as they will
  effectively block the agent until the request is fulfilled.

  Consider these two examples:

      # Compute in the agent/server
      def get_something(agent) do
        Agent.get(agent, fn state -> do_something_expensive(state) end)
      end

      # Compute in the agent/client
      def get_something(agent) do
        Agent.get(agent, &(&1)) |> do_something_expensive())
      end

  The first function blocks the agent. The second function copies all the state
  to the client and then executes the operation in the client. One difference is
  whether the data is large enough to require processing in the server, at least
  initially, or small enough to be sent to the client cheaply. Another
  difference is whether the data needs to be processed atomically: getting the
  state and calling `do_something_expensive(state)` outside of the agent means
  that the agent's state can be updated in the meantime, so putting the state
  back in the agent afterwards may override the updated that happened while
  processing.

  ## Name registration

  An agent is bound to the same name registration rules as GenServers.
  Read more about it in the `GenServer` documentation.

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
  function, and arguments tuple to the update instruction. For example, imagine
  you have an agent named `:sample` and you want to convert its inner state
  from a keyword list to a map. It can be done with the following
  instruction:

      {:update, :sample, {:advanced, {Enum, :into, [%{}]}}}

  The agent's state will be added to the given list of arguments (`[%{}]`) as
  the first argument.
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

  Once the agent is spawned, the given function `fun` is invoked and its return
  value is used as the agent state. Note that `start_link/2` does not return
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

  If the given function callback fails, the function returns `{:error, reason}`.

  ## Examples

      iex> {:ok, pid} = Agent.start_link(fn -> 42 end)
      iex> Agent.get(pid, fn state -> state end)
      42

      iex> {:error, {exception, _stacktrace}} = Agent.start(fn -> raise "oops" end)
      iex> exception
      %RuntimeError{message: "oops"}

  """
  @spec start_link((() -> term), GenServer.options) :: on_start
  def start_link(fun, options \\ []) when is_function(fun, 0) do
    GenServer.start_link(Agent.Server, fun, options)
  end

  @doc """
  Starts an agent linked to the current process.

  Same as `start_link/2` but a module, function, and arguments are expected
  instead of an anonymous function; `fun` in `module` will be called with the
  given arguments `args` to initialize the state.
  """
  @spec start_link(module, atom, [any], GenServer.options) :: on_start
  def start_link(module, fun, args, options \\ []) do
    GenServer.start_link(Agent.Server, {module, fun, args}, options)
  end

  @doc """
  Starts an agent process without links (outside of a supervision tree).

  See `start_link/2` for more information.

  ## Examples

      iex> {:ok, pid} = Agent.start(fn -> 42 end)
      iex> Agent.get(pid, fn(state) -> state end)
      42

  """
  @spec start((() -> term), GenServer.options) :: on_start
  def start(fun, options \\ []) when is_function(fun, 0) do
    GenServer.start(Agent.Server, fun, options)
  end

  @doc """
  Starts an agent without links with the given module, function, and arguments.

  See `start_link/4` for more information.
  """
  @spec start(module, atom, [any], GenServer.options) :: on_start
  def start(module, fun, args, options \\ []) do
    GenServer.start(Agent.Server, {module, fun, args}, options)
  end

  @doc """
  Gets an agent value via the given anonymous function.

  The function `fun` is sent to the `agent` which invokes the function
  passing the agent state. The result of the function invocation is
  returned from this function.

  `timeout` is an integer greater than zero which specifies how many
  milliseconds are allowed before the agent executes the function and returns
  the result value, or the atom `:infinity` to wait indefinitely. If no result
  is received within the specified time, the function call fails and the caller
  exits.

  ## Examples

      iex> {:ok, pid} = Agent.start_link(fn -> 42 end)
      iex> Agent.get(pid, fn state -> state end)
      42

  """
  @spec get(agent, (state -> a), timeout) :: a when a: var
  def get(agent, fun, timeout \\ 5000) when is_function(fun, 1) do
    GenServer.call(agent, {:get, fun}, timeout)
  end

  @doc """
  Gets an agent value via the given function.

  Same as `get/3` but a module, function, and arguments are expected
  instead of an anonymous function. The state is added as first
  argument to the given list of arguments.
  """
  @spec get(agent, module, atom, [term], timeout) :: any
  def get(agent, module, fun, args, timeout \\ 5000) do
    GenServer.call(agent, {:get, {module, fun, args}}, timeout)
  end

  @doc """
  Gets and updates the agent state in one operation via the given anonymous
  function.

  The function `fun` is sent to the `agent` which invokes the function
  passing the agent state. The function must return a tuple with two
  elements, the first being the value to return (that is, the "get" value)
  and the second one being the new state of the agent.

  `timeout` is an integer greater than zero which specifies how many
  milliseconds are allowed before the agent executes the function and returns
  the result value, or the atom `:infinity` to wait indefinitely. If no result
  is received within the specified time, the function call fails and the caller
  exits.

  ## Examples

      iex> {:ok, pid} = Agent.start_link(fn -> 42 end)
      iex> Agent.get_and_update(pid, fn state -> {state, state + 1} end)
      42
      iex> Agent.get(pid, fn state -> state end)
      43

  """
  @spec get_and_update(agent, (state -> {a, state}), timeout) :: a when a: var
  def get_and_update(agent, fun, timeout \\ 5000) when is_function(fun, 1) do
    GenServer.call(agent, {:get_and_update, fun}, timeout)
  end

  @doc """
  Gets and updates the agent state in one operation via the given function.

  Same as `get_and_update/3` but a module, function, and arguments are expected
  instead of an anonymous function. The state is added as first
  argument to the given list of arguments.
  """
  @spec get_and_update(agent, module, atom, [term], timeout) :: any
  def get_and_update(agent, module, fun, args, timeout \\ 5000) do
    GenServer.call(agent, {:get_and_update, {module, fun, args}}, timeout)
  end

  @doc """
  Updates the agent state via the given anonymous function.

  The function `fun` is sent to the `agent` which invokes the function
  passing the agent state. The return value of `fun` becomes the new
  state of the agent.

  This function always returns `:ok`.

  `timeout` is an integer greater than zero which specifies how many
  milliseconds are allowed before the agent executes the function and returns
  the result value, or the atom `:infinity` to wait indefinitely. If no result
  is received within the specified time, the function call fails and the caller
  exits.

  ## Examples

      iex> {:ok, pid} = Agent.start_link(fn -> 42 end)
      iex> Agent.update(pid, fn state -> state + 1 end)
      :ok
      iex> Agent.get(pid, fn state -> state end)
      43

  """
  @spec update(agent, (state -> state), timeout) :: :ok
  def update(agent, fun, timeout \\ 5000) when is_function(fun, 1) do
    GenServer.call(agent, {:update, fun}, timeout)
  end

  @doc """
  Updates the agent state via the given function.

  Same as `update/3` but a module, function, and arguments are expected
  instead of an anonymous function. The state is added as first
  argument to the given list of arguments.
  """
  @spec update(agent, module, atom, [term], timeout) :: :ok
  def update(agent, module, fun, args, timeout \\ 5000) do
    GenServer.call(agent, {:update, {module, fun, args}}, timeout)
  end

  @doc """
  Performs a cast (*fire and forget*) operation on the agent state.

  The function `fun` is sent to the `agent` which invokes the function
  passing the agent state. The return value of `fun` becomes the new
  state of the agent.

  Note that `cast` returns `:ok` immediately, regardless of whether `agent` (or
  the node it should live on) exists.
  """
  @spec cast(agent, (state -> state)) :: :ok
  def cast(agent, fun) when is_function(fun, 1) do
    GenServer.cast(agent, {:cast, fun})
  end

  @doc """
  Performs a cast (*fire and forget*) operation on the agent state.

  Same as `cast/2` but a module, function, and arguments are expected
  instead of an anonymous function. The state is added as first
  argument to the given list of arguments.
  """
  @spec cast(agent, module, atom, [term]) :: :ok
  def cast(agent, module, fun, args) do
    GenServer.cast(agent, {:cast, {module, fun, args}})
  end

  @doc """
  Synchronously stops the agent with the given `reason`.

  It returns `:ok` if the agent terminates with the given
  reason. If the agent terminates with another reason, the call will
  exit.

  This function keeps OTP semantics regarding error reporting.
  If the reason is any other than `:normal`, `:shutdown` or
  `{:shutdown, _}`, an error report will be logged.

  ## Examples

      iex> {:ok, pid} = Agent.start_link(fn -> 42 end)
      iex> Agent.stop(pid)
      :ok

  """
  @spec stop(agent, reason :: term, timeout) :: :ok
  def stop(agent, reason \\ :normal, timeout \\ :infinity) do
    GenServer.stop(agent, reason, timeout)
  end
end
