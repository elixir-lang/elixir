defrecord IEx.Config, binding: nil, cache: '', counter: 1,
                      prefix: "iex", scope: nil, result: nil

defmodule IEx.Server do
  @moduledoc false

  alias IEx.Config

  @doc """
  Finds where the current IEx server is located.
  """
  @spec whereis :: pid | nil
  def whereis() do
    # Locate top group leader, always registered as user
    # can be implemented by group (normally) or user
    # (if oldshell or noshell)
    if user = Process.whereis(:user) do
      case :group.interfaces(user) do
        [] -> # Old or no shell
          case :user.interfaces(user) do
            [] -> nil
            [shell: shell] -> shell
          end
        [user_drv: user_drv] -> # Get current group from user_drv
          case :user_drv.interfaces(user_drv) do
            [] -> nil
            [current_group: group] -> :group.interfaces(group)[:shell]
          end
      end
    end
  end

  @doc """
  Requests to take over the given shell from the
  current process.
  """
  @spec take_over(binary, Keyword.t, pos_integer) ::
        :ok | { :error, :self } | { :error, :no_iex }
  def take_over(identifier, opts, timeout // 1000, server // whereis()) do
    cond do
      nil?(server) ->
        { :error, :no_iex }
      whereis_evaluator(server) == self ->
        { :error, :self }
      true ->
        ref = make_ref()
        server <- { :take?, self, ref }

        receive do
          ^ref ->
            opts = Keyword.put(opts, :evaluator, self)
            server <- { :take, identifier, opts }
            IEx.Evaluator.start(server)
        after
          timeout ->
            { :error, :no_iex }
        end
    end
  end

  defp whereis_evaluator(server) do
    case server && Process.info(server, :dictionary) do
      { :dictionary, dict } -> dict[:evaluator]
      _ -> nil
    end
  end

  @doc """
  Starts IEx by executing a given callback and spawning
  the server only after the callback is done.

  The server responsibilities include:

  * reading input
  * sending messages to the evaluator
  * handling take over process of the evaluator

  If there is any take over during the callback execution,
  we spawn a new server for it without waiting to its
  conclusion.
  """
  @spec start(list, fun) :: :ok
  def start(opts, callback) do
    { pid, ref } = Process.spawn_monitor(callback)
    start_loop(opts, pid, ref)
  end

  defp start_loop(opts, pid, ref) do
    receive do
      { :take?, other, ref } ->
        other <- ref
        start_loop(opts, pid, ref)

      { :take, identifier, opts } ->
        if allow_take?(identifier) do
          run(opts)
        else
          start_loop(opts, pid, ref)
        end

      { :DOWN, ^ref, :process, ^pid,  :normal } ->
        run(opts)

      { :DOWN, ^ref, :process, ^pid,  _reason } ->
        :ok
    end
  end

  # Run loop: this is where the work is really
  # done after the start loop.

  defp run(opts) when is_list(opts) do
    IO.puts "Interactive Elixir (#{System.version}) - press Ctrl+C to exit (type h() ENTER for help)"
    self_pid  = self
    evaluator = opts[:evaluator] || spawn(fn -> IEx.Evaluator.start(self_pid) end)
    Process.put(:evaluator, evaluator)
    loop(run_config(opts), evaluator, Process.monitor(evaluator))
  end

  defp reset_loop(opts, evaluator, evaluator_ref) do
    exit_loop(evaluator, evaluator_ref)
    IO.write [IO.ANSI.home, IO.ANSI.clear]
    run(opts)
  end

  defp exit_loop(evaluator, evaluator_ref) do
    Process.delete(:evaluator)
    Process.demonitor(evaluator_ref)
    evaluator <- { :done, self }
    :ok
  end

  defp loop(config, evaluator, evaluator_ref) do
    self_pid = self()
    counter  = config.counter
    prefix   = if config.cache != [], do: "...", else: config.prefix

    input = spawn(fn -> io_get(self_pid, prefix, counter) end)
    wait_input(config, evaluator, evaluator_ref, input)
  end

  defp wait_input(config, evaluator, evaluator_ref, input) do
    receive do
      # Input handling.
      # Message either go back to the main loop or exit.
      { :input, ^input, code } when is_binary(code) ->
        evaluator <- { :eval, self, code, config }
        receive do
          { :evaled, ^evaluator, config } ->
            loop(config, evaluator, evaluator_ref)
        end
      { :input, ^input, { :error, :interrupted } } ->
        io_error "** (EXIT) interrupted"
        loop(config.cache(''), evaluator, evaluator_ref)
      { :input, ^input, :eof } ->
        exit_loop(evaluator, evaluator_ref)
      { :input, ^input, { :error, :terminated } } ->
        exit_loop(evaluator, evaluator_ref)

      # Take process.
      # The take? message is received out of band, so we can
      # go back to wait for the same input. The take message
      # needs to take hold of the IO, so it kills the input,
      # re-runs the server OR goes back to the main loop.
      { :take?, other, ref } ->
        other <- ref
        wait_input(config, evaluator, evaluator_ref, input)
      { :take, identifier, opts } ->
        kill_input(input)

        if allow_take?(identifier) do
          reset_loop(opts, evaluator, evaluator_ref)
        else
          loop(config, evaluator, evaluator_ref)
        end

      # Evaluator handling.
      # We always kill the input to run a new one or to exit for real.
      { :respawn, ^evaluator } ->
        kill_input(input)
        IO.puts("")
        reset_loop([], evaluator, evaluator_ref)
      { :DOWN, ^evaluator_ref, :process, ^evaluator,  reason } ->
        io_error "** (EXIT from #{config.prefix} #{inspect evaluator}) #{inspect(reason)}"
        kill_input(input)
        exit_loop(evaluator, evaluator_ref)
    end
  end

  defp kill_input(input) do
    Process.exit(input, :kill)
  end

  defp allow_take?(identifier) do
    message = IEx.color(:eval_interrupt, "#{identifier}. Allow? [Yn] ")
    IO.gets(:stdio, message) =~ %r/^(Y(es)?)?$/i
  end

  ## Config

  defp run_config(opts) do
    locals = Keyword.get(opts, :delegate_locals_to, IEx.Helpers)

    scope =
      if env = opts[:env] do
        scope = :elixir_scope.to_erl_env(env)
        :elixir.scope_for_eval(scope, delegate_locals_to: locals)
      else
        :elixir.scope_for_eval(file: "iex", delegate_locals_to: locals)
      end

    { _, _, scope } = :elixir.eval('require IEx.Helpers', [], 0, scope)

    binding = Keyword.get(opts, :binding, [])
    prefix  = Keyword.get(opts, :prefix, "iex")
    config  = Config[binding: binding, scope: scope, prefix: prefix]

    case opts[:dot_iex_path] do
      ""   -> config
      path -> IEx.Evaluator.load_dot_iex(config, path)
    end
  end

  ## IO

  defp io_get(pid, prefix, counter) do
    prompt =
      if is_alive do
        "#{prefix || remote_prefix}(#{node})#{counter}> "
      else
        "#{prefix || "iex"}(#{counter})> "
      end

    pid <- { :input, self, IO.gets(:stdio, prompt) }
  end

  defp io_error(result) do
    IO.puts :stdio, IEx.color(:eval_error, result)
  end

  defp remote_prefix do
    if node == node(:erlang.group_leader), do: "iex", else: "rem"
  end
end
