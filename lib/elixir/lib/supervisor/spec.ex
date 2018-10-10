defmodule Supervisor.Spec do
  @moduledoc """
  Outdated functions for building child specifications.

  The functions in this module are deprecated and they do not work
  with the module-based child specs introduced in Elixir v1.5.
  Please see the `Supervisor` documentation instead.

  Convenience functions for defining supervisor specifications.

  ## Example

  By using the functions in this module one can specify the children
  to be used under a supervisor, started with `Supervisor.start_link/2`:

      import Supervisor.Spec

      children = [
        worker(MyWorker, [arg1, arg2, arg3]),
        supervisor(MySupervisor, [arg1])
      ]

      Supervisor.start_link(children, strategy: :one_for_one)

  Sometimes, it may be handy to define supervisors backed
  by a module:

      defmodule MySupervisor do
        use Supervisor

        def start_link(arg) do
          Supervisor.start_link(__MODULE__, arg)
        end

        def init(arg) do
          children = [
            worker(MyWorker, [arg], restart: :temporary)
          ]

          supervise(children, strategy: :simple_one_for_one)
        end
      end

  Notice in this case we don't have to explicitly import
  `Supervisor.Spec` as `use Supervisor` automatically does so.
  Defining a module-based supervisor can be useful, for example,
  to perform initialization tasks in the `c:init/1` callback.

  ## Supervisor and worker options

  In the example above, we defined specs for workers and supervisors.
  These specs (both for workers as well as supervisors) accept the
  following options:

    * `:id` - a name used to identify the child specification
      internally by the supervisor; defaults to the given module
      name for the child worker/supervisor

    * `:function` - the function to invoke on the child to start it

    * `:restart` - an atom that defines when a terminated child process should
      be restarted (see the "Restart values" section below)

    * `:shutdown` - an atom that defines how a child process should be
      terminated (see the "Shutdown values" section below)

    * `:modules` - it should be a list with one element `[module]`,
      where module is the name of the callback module only if the
      child process is a `Supervisor` or `GenServer`; if the child
      process is a `GenEvent`, `:modules` should be `:dynamic`

  ### Restart values (:restart)

  The following restart values are supported in the `:restart` option:

    * `:permanent` - the child process is always restarted

    * `:temporary` - the child process is never restarted (not even
      when the supervisor's strategy is `:rest_for_one` or `:one_for_all`)

    * `:transient` - the child process is restarted only if it
      terminates abnormally, i.e., with an exit reason other than
      `:normal`, `:shutdown` or `{:shutdown, term}`

  Notice that supervisor that reached maximum restart intensity will exit with `:shutdown` reason.
  In this case the supervisor will only be restarted if its child specification was defined with
  the `:restart` option is set to `:permanent` (the default).

  ### Shutdown values (`:shutdown`)

  The following shutdown values are supported in the `:shutdown` option:

    * `:brutal_kill` - the child process is unconditionally terminated
      using `Process.exit(child, :kill)`

    * `:infinity` - if the child process is a supervisor, this is a mechanism
      to give the subtree enough time to shut down; it can also be used with
      workers with care

    * a non-negative integer - the amount of time in milliseconds
      that the supervisor tells the child process to terminate by calling
      `Process.exit(child, :shutdown)` and then waits for an exit signal back.
      If no exit signal is received within the specified time,
      the child process is unconditionally terminated
      using `Process.exit(child, :kill)`

  """

  @moduledoc deprecated:
               "Use the new child specifications outlined in the Supervisor module instead"

  # TODO: Deprecate all functions in this module on Elixir v1.9.
  # Also deprecate entry in Supervisor.Default.

  @typedoc "Supported strategies"
  @type strategy :: :simple_one_for_one | :one_for_one | :one_for_all | :rest_for_one

  @typedoc "Supported restart values"
  @type restart :: :permanent | :transient | :temporary

  @typedoc "Supported shutdown values"
  @type shutdown :: timeout | :brutal_kill

  @typedoc "Supported worker values"
  @type worker :: :worker | :supervisor

  @typedoc "Supported module values"
  @type modules :: :dynamic | [module]

  @typedoc "Supported id values"
  @type child_id :: term

  @typedoc "The supervisor specification"
  @type spec ::
          {child_id, start_fun :: {module, atom, [term]}, restart, shutdown, worker, modules}

  @doc """
  Receives a list of children (workers or supervisors) to
  supervise and a set of options.

  Returns a tuple containing the supervisor specification. This tuple can be
  used as the return value of the `c:init/1` callback when implementing a
  module-based supervisor.

  ## Examples

      supervise(children, strategy: :one_for_one)

  ## Options

    * `:strategy` - the restart strategy option. It can be either
      `:one_for_one`, `:rest_for_one`, `:one_for_all`, or
      `:simple_one_for_one`. You can learn more about strategies
      in the `Supervisor` module docs.

    * `:max_restarts` - the maximum number of restarts allowed in
      a time frame. Defaults to `3`.

    * `:max_seconds` - the time frame in which `:max_restarts` applies.
      Defaults to `5`.

  The `:strategy` option is required and by default a maximum of 3 restarts is
  allowed within 5 seconds. Check the `Supervisor` module for a detailed
  description of the available strategies.
  """
  @spec supervise(
          [spec],
          strategy: strategy,
          max_restarts: non_neg_integer,
          max_seconds: pos_integer
        ) :: {:ok, tuple}
  def supervise(children, options) do
    unless strategy = options[:strategy] do
      raise ArgumentError, "expected :strategy option to be given"
    end

    maxR = Keyword.get(options, :max_restarts, 3)
    maxS = Keyword.get(options, :max_seconds, 5)

    assert_unique_ids(Enum.map(children, &get_id/1))
    {:ok, {{strategy, maxR, maxS}, children}}
  end

  defp get_id({id, _, _, _, _, _}) do
    id
  end

  defp get_id(other) do
    raise ArgumentError,
          "invalid tuple specification given to supervise/2. If you are trying to use " <>
            "the map child specification that is part of the Elixir v1.5, use Supervisor.init/2 " <>
            "instead of Supervisor.Spec.supervise/2. See the Supervisor module for more information. " <>
            "Got: #{inspect(other)}"
  end

  defp assert_unique_ids([id | rest]) do
    if id in rest do
      raise ArgumentError,
            "duplicated id #{inspect(id)} found in the supervisor specification, " <>
              "please explicitly pass the :id option when defining this worker/supervisor"
    else
      assert_unique_ids(rest)
    end
  end

  defp assert_unique_ids([]) do
    :ok
  end

  @doc """
  Defines the given `module` as a worker which will be started
  with the given arguments.

      worker(ExUnit.Runner, [], restart: :permanent)

  By default, the function `start_link` is invoked on the given
  module. Overall, the default values for the options are:

      [
        id: module,
        function: :start_link,
        restart: :permanent,
        shutdown: 5000,
        modules: [module]
      ]

  See the "Supervisor and worker options" section in the `Supervisor.Spec` module for more
  information on the available options.
  """
  @spec worker(
          module,
          [term],
          restart: restart,
          shutdown: shutdown,
          id: term,
          function: atom,
          modules: modules
        ) :: spec
  def worker(module, args, options \\ []) do
    child(:worker, module, args, options)
  end

  @doc """
  Defines the given `module` as a supervisor which will be started
  with the given arguments.

      supervisor(module, [], restart: :permanent)

  By default, the function `start_link` is invoked on the given
  module. Overall, the default values for the options are:

      [
        id: module,
        function: :start_link,
        restart: :permanent,
        shutdown: :infinity,
        modules: [module]
      ]

  See the "Supervisor and worker options" section in the `Supervisor.Spec` module for more
  information on the available options.
  """
  @spec supervisor(
          module,
          [term],
          restart: restart,
          shutdown: shutdown,
          id: term,
          function: atom,
          modules: modules
        ) :: spec
  def supervisor(module, args, options \\ []) do
    options = Keyword.put_new(options, :shutdown, :infinity)
    child(:supervisor, module, args, options)
  end

  defp child(type, module, args, options) do
    id = Keyword.get(options, :id, module)
    modules = Keyword.get(options, :modules, modules(module))
    function = Keyword.get(options, :function, :start_link)
    restart = Keyword.get(options, :restart, :permanent)
    shutdown = Keyword.get(options, :shutdown, 5000)

    {id, {module, function, args}, restart, shutdown, type, modules}
  end

  defp modules(GenEvent), do: :dynamic
  defp modules(module), do: [module]
end
