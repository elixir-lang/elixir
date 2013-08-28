defmodule Supervisor.Behaviour do
  @moduledoc """
  This module is a convenience to define Supervisor
  callbacks in Elixir. By using this module, you get
  the module behaviour automatically tagged as
  `:supervisor` and some helper functions are imported
  to make defining supervisors easier.

  For more information on supervisors, please check the
  remaining functions defined in this module or refer to
  the following:

  http://www.erlang.org/doc/man/supervisor.html
  http://www.erlang.org/doc/design_principles/sup_princ.html
  http://learnyousomeerlang.com/supervisors

  ## Example

      defmodule ExUnit.Sup do
        use Supervisor.Behaviour

        def init(user_options) do
          tree = [ worker(ExUnit.Runner, [user_options]) ]
          supervise(tree, strategy: :one_for_one)
        end
      end

      { :ok, pid } = :supervisor.start_link(MyServer, [])

  """

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @behaviour :supervisor
      import unquote(__MODULE__)
    end
  end

  @doc """
  Receives a list of children (worker or supervisors) to
  supervise and a set of options. Returns a tuple containing
  the supervisor specification.

  ## Examples

      supervise children, strategy: :one_for_one

  ## Options

  * `:strategy` - the restart strategy option It can be either
    `:one_for_one`, `:rest_for_one`, `:one_for_all` and
    `:simple_one_for_one`;

  * `:max_restarts` - the maximum amount of restarts allowed in
    a time frame. Defaults to 5;

  * `:max_seconds` - the time frame in which max_restarts applies.
    Defaults to 5;

  The `:strategy` option is required and by default maximum 5 restarts
  are allowed in 5 seconds.

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
    when dynamically attaching children;

  """
  def supervise(children, options) do
    unless strategy = options[:strategy] do
      raise ArgumentError, message: "expected :strategy option to be given to supervise"
    end

    maxR = Keyword.get(options, :max_restarts, 5)
    maxS = Keyword.get(options, :max_seconds, 5)

    { :ok, { { strategy, maxR, maxS }, children } }
  end

  @child_doc """
  ## Options

  * `:id` - a name used to identify the child specification
    internally by the supervisor. Defaults to the module name;

  * `:function` - the function to invoke on the child to start it.
    Defaults to `:start_link`;

  * `:restart` - defines when the child process should restart.
    Defaults to `:permanent`;

  * `:shutdown` - defines how a child process should be terminated.
    Defaults to `5000` for a worker and `:infinity` for a supervisor;

  * `:modules` - it should be a list with one element `[module]`,
    where module is the name of the callback module only if the
    child process is a supervisor, `gen_server` or `gen_fsm`. If the
    child process is a gen_event, modules should be `:dynamic`.
    Defaults to a list with the given module;

  ## Restart values

  The following restart values are supported:

  * `:permanent` - the child process is always restarted;

  * `:temporary` - the child process is never restarted (not even
    when the supervisor's strategy is `:rest_for_one` or `:one_for_all`);

  * `:transient` - the child process is restarted only if it
    terminates abnormally, i.e. with another exit reason than
    `:normal`, `:shutdown` or `{ :shutdown, term }`;

  ## Shutdown values

  The following shutdown values are supported:

  * `:brutal_kill` - the child process is unconditionally terminated
    using `exit(child, :kill)`;

  * `:infinity` - if the child process is a supervisor, it is a mechanism
    to give the subtree enough time to shutdown. It can also be used with
    workers with care;

  * Finally, it can also be any integer meaning that the supervisor tells
    the child process to terminate by calling `exit(child, :shutdown)` and
    then waits for an exit signal back. If no exit signal is received within
    the specified time (in miliseconds), the child process is unconditionally
    terminated using `exit(child, :kill)`;
  """

  @doc """
  Defines the given `module` as a worker which will be started
  with the given arguments.

      worker ExUnit.Runner, [], restart: :permanent

  By default, the function `:start_link` is invoked on the given module.

  #{@child_doc}
  """
  def worker(module, args, options // []) do
    child(:worker, module, args, options)
  end

  @doc """
  Defines the given `module` as a supervisor which will be started
  with the given arguments.

      supervisor ExUnit.Runner, [], restart: :permanent

  By default, the function `:start_link` is invoked on the given module.

  #{@child_doc}
  """
  def supervisor(module, args, options // []) do
    options = Keyword.update(options, :shutdown, :infinity, fn(x) -> x end)
    child(:supervisor, module, args, options)
  end

  defp child(type, module, args, options) do
    id       = Keyword.get(options, :id, module)
    modules  = Keyword.get(options, :modules, [module])
    function = Keyword.get(options, :function, :start_link)
    restart  = Keyword.get(options, :restart, :permanent)
    shutdown = Keyword.get(options, :shutdown, 5000)

    { id, { module, function, args },
      restart, shutdown, type, modules }
  end
end
