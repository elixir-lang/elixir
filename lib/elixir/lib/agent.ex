defmodule Agent do
  @moduledoc """
  Agents are a simple abstraction around state.

  Often in Elixir there is a need to share or store state that
  must be accessed from different processes or by the same process
  at different points in time.

  The `Agent` module provides a basic server implementation that
  allows state to be retrieved and updated via a simple API.

  ## Examples

  For example, the following agent implements a counter:

      defmodule Counter do
        use Agent

        def start_link(initial_value) do
          Agent.start_link(fn -> initial_value end, name: __MODULE__)
        end

        def value do
          Agent.get(__MODULE__, & &1)
        end

        def increment do
          Agent.update(__MODULE__, &(&1 + 1))
        end
      end

  Usage would be:

      Counter.start_link(0)
      #=> {:ok, #PID<0.123.0>}

      Counter.value()
      #=> 0

      Counter.increment()
      #=> :ok

      Counter.increment()
      #=> :ok

      Counter.value()
      #=> 2

  Thanks to the agent server process, the counter can be safely incremented
  concurrently.

  Agents provide a segregation between the client and server APIs (similar to
  `GenServer`s). In particular, the functions passed as arguments to the calls to
  `Agent` functions are invoked inside the agent (the server). This distinction
  is important because you may want to avoid expensive operations inside the
  agent, as they will effectively block the agent until the request is
  fulfilled.

  Consider these two examples:

      # Compute in the agent/server
      def get_something(agent) do
        Agent.get(agent, fn state -> do_something_expensive(state) end)
      end

      # Compute in the agent/client
      def get_something(agent) do
        Agent.get(agent, & &1) |> do_something_expensive()
      end

  The first function blocks the agent. The second function copies all the state
  to the client and then executes the operation in the client. One aspect to
  consider is whether the data is large enough to require processing in the server,
  at least initially, or small enough to be sent to the client cheaply. Another
  factor is whether the data needs to be processed atomically: getting the
  state and calling `do_something_expensive(state)` outside of the agent means
  that the agent's state can be updated in the meantime. This is specially
  important in case of updates as computing the new state in the client rather
  than in the server can lead to race conditions if multiple clients are trying
  to update the same state to different values.

  ## How to supervise

  An `Agent` is most commonly started under a supervision tree.
  When we invoke `use Agent`, it automatically defines a `child_spec/1`
  function that allows us to start the agent directly under a supervisor.
  To start an agent under a supervisor with an initial counter of 0,
  one may do:

      children = [
        {Counter, 0}
      ]

      Supervisor.start_link(children, strategy: :one_for_all)

  While one could also simply pass the `Counter` as a child to the supervisor,
  such as:

      children = [
        Counter # Same as {Counter, []}
      ]

      Supervisor.start_link(children, strategy: :one_for_all)

  The definition above wouldn't work for this particular example,
  as it would attempt to start the counter with an initial value
  of an empty list. However, this may be a viable option in your
  own agents. A common approach is to use a keyword list, as that
  would allow setting the initial value and giving a name to the
  counter process, for example:

      def start_link(opts) do
        {initial_value, opts} = Keyword.pop(opts, :initial_value, 0)
        Agent.start_link(fn -> initial_value end, opts)
      end

  and then you can use `Counter`, `{Counter, name: :my_counter}` or
  even `{Counter, initial_value: 0, name: :my_counter}` as a child
  specification.

  `use Agent` also accepts a list of options which configures the
  child specification and therefore how it runs under a supervisor.
  The generated `child_spec/1` can be customized with the following options:

    * `:id` - the child specification identifier, defaults to the current module
    * `:restart` - when the child should be restarted, defaults to `:permanent`
    * `:shutdown` - how to shut down the child, either immediately or by giving it time to shut down

  For example:

      use Agent, restart: :transient, shutdown: 10_000

  See the "Child specification" section in the `Supervisor` module for more
  detailed information. The `@doc` annotation immediately preceding
  `use Agent` will be attached to the generated `child_spec/1` function.

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
  Returns a specification to start an agent under a supervisor.

  See the "Child specification" section in the `Supervisor` module for more detailed information.
  """
  @doc since: "1.5.0"
  def child_spec(arg) do
    %{
      id: Agent,
      start: {Agent, :start_link, [arg]}
    }
  end

  @doc false
  defmacro __using__(opts) do
    quote location: :keep, bind_quoted: [opts: opts] do
      unless Module.has_attribute?(__MODULE__, :doc) do
        @doc """
        Returns a specification to start this module under a supervisor.

        See `Supervisor`.
        """
      end

      def child_spec(arg) do
        default = %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [arg]}
        }

        Supervisor.child_spec(default, unquote(Macro.escape(opts)))
      end

      defoverridable child_spec: 1
    end
  end

  @doc """
  Starts an agent linked to the current process with the given function.

  This is often used to start the agent as part of a supervision tree.

  Once the agent is spawned, the given function `fun` is invoked in the server
  process, and should return the initial agent state. Note that `start_link/2`
  does not return until the given function has returned.

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
  @spec start_link((() -> term), GenServer.options()) :: on_start
  def start_link(fun, options \\ []) when is_function(fun, 0) do
    GenServer.start_link(Agent.Server, fun, options)
  end

  @doc """
  Starts an agent linked to the current process.

  Same as `start_link/2` but a module, function, and arguments are expected
  instead of an anonymous function; `fun` in `module` will be called with the
  given arguments `args` to initialize the state.
  """
  @spec start_link(module, atom, [any], GenServer.options()) :: on_start
  def start_link(module, fun, args, options \\ []) do
    GenServer.start_link(Agent.Server, {module, fun, args}, options)
  end

  @doc """
  Starts an agent process without links (outside of a supervision tree).

  See `start_link/2` for more information.

  ## Examples

      iex> {:ok, pid} = Agent.start(fn -> 42 end)
      iex> Agent.get(pid, fn state -> state end)
      42

  """
  @spec start((() -> term), GenServer.options()) :: on_start
  def start(fun, options \\ []) when is_function(fun, 0) do
    GenServer.start(Agent.Server, fun, options)
  end

  @doc """
  Starts an agent without links with the given module, function, and arguments.

  See `start_link/4` for more information.
  """
  @spec start(module, atom, [any], GenServer.options()) :: on_start
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

  ## Examples

      iex> {:ok, pid} = Agent.start_link(fn -> 42 end)
      iex> Agent.update(pid, Kernel, :+, [12])
      :ok
      iex> Agent.get(pid, fn state -> state end)
      54

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

  ## Examples

      iex> {:ok, pid} = Agent.start_link(fn -> 42 end)
      iex> Agent.cast(pid, fn state -> state + 1 end)
      :ok
      iex> Agent.get(pid, fn state -> state end)
      43

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

  ## Examples

      iex> {:ok, pid} = Agent.start_link(fn -> 42 end)
      iex> Agent.cast(pid, Kernel, :+, [12])
      :ok
      iex> Agent.get(pid, fn state -> state end)
      54

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
