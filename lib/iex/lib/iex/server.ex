defrecord IEx.Config, binding: nil, cache: '', counter: 1,
                      prefix: "iex", scope: nil, result: nil

defmodule IEx.Server do
  @moduledoc false

  alias IEx.Config

  @doc """
  Finds where the current IEx server is located.
  """
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
  def take_over(identifier, opts, timeout // 1000) do
    server = whereis()
    opts   = Keyword.put(opts, :evaluator, self)

    cond do
      nil?(server) ->
        { :error, :no_iex }
      server == self ->
        { :error, :self }
      true ->
        ref = make_ref()
        server <- { :take?, self, ref }

        receive do
          ^ref ->
            server <- { :take, identifier, opts }
            IEx.Evaluator.start(server)
        after
          timeout ->
            { :error, :no_iex }
        end
    end
  end

  @doc """
  Boots IEx by executing a given callback and starting
  the server only after the callback is done. If there
  is any take over during boot, we allow it.
  """
  def boot(opts, callback) do
    { pid, ref } = Process.spawn_monitor(callback)
    boot_loop(opts, pid, ref)
  end

  defp boot_loop(opts, pid, ref) do
    receive do
      { :take?, other, ref } ->
        other <- ref
        boot_loop(opts, pid, ref)

      { :take, identifier, opts } ->
        if allow_take?(identifier) do
          start(opts)
        else
          boot_loop(opts, pid, ref)
        end

      { :DOWN, ^ref, :process, ^pid,  :normal } ->
        start(opts)

      { :DOWN, ^ref, :process, ^pid,  _reason } ->
        :ok
    end
  end

  @doc """
  Server loop for an IEx session. Its responsibilities include:

  * setting up the evaluator
  * reading input
  * sending messages to the evaluator

  """
  def start(opts) when is_list(opts) do
    IO.puts "Interactive Elixir (#{System.version}) - press Ctrl+C to exit (type h() ENTER for help)"
    self_pid  = self
    evaluator = opts[:evaluator] || spawn(fn -> IEx.Evaluator.start(self_pid) end)
    loop(boot_config(opts), evaluator, start_loop(evaluator))
  end

  defp restart(evaluator, evaluator_ref, opts) do
    exit_loop(evaluator, evaluator_ref)
    IO.write [IO.ANSI.home, IO.ANSI.clear]
    start(opts)
  end

  defp start_loop(evaluator) do
    Process.monitor(evaluator)
  end

  defp exit_loop(evaluator, evaluator_ref) do
    evaluator <- { :done, self }
    Process.demonitor(evaluator_ref)
    :ok
  end

  ## Loop

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
      # needs to take hold of the IO, so it kills the input
      # starts a new evaluator OR goes back to the main loop.
      { :take?, other, ref } ->
        other <- ref
        wait_input(config, evaluator, evaluator_ref, input)
      { :take, identifier, opts } ->
        kill_input(input)

        if allow_take?(identifier) do
          restart(evaluator, evaluator_ref, opts)
        else
          loop(config, evaluator, evaluator_ref)
        end

      # Evaluator handling.
      # We always kill the input to start a new one or to exit for real.
      { :respawn, ^evaluator } ->
        kill_input(input)
        IO.puts("")
        restart(evaluator, evaluator_ref, [])
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

  ## Config and load dot iex helpers

  defp boot_config(opts) do
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

  ## Get input

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
