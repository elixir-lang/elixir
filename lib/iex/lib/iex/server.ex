defmodule IEx.Config do
  @moduledoc false
  defstruct binding: nil, cache: '', counter: 1, prefix: "iex", scope: nil, env: nil
end

defmodule IEx.Server do
  @moduledoc false

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
        :ok | {:error, :no_iex} | {:error, :refused}
  def take_over(identifier, opts, timeout \\ 1000, server \\ whereis()) do
    cond do
      nil?(server) ->
        {:error, :no_iex}
      true ->
        ref = make_ref()
        send server, {:take?, self, ref}

        receive do
          ^ref ->
            opts = [evaluator: self] ++ opts
            send server, {:take, self, identifier, ref, opts}

            receive do
              {^ref, nil} ->
                {:error, :refused}
              {^ref, leader} ->
                IEx.Evaluator.start(server, leader)
            end
        after
          timeout ->
            {:error, :no_iex}
        end
    end
  end

  @doc """
  Starts IEx by executing a given callback and spawning
  the server only after the callback is done.

  The server responsibilities include:

  * reading input
  * sending messages to the evaluator
  * handling takeover process of the evaluator

  If there is any takeover during the callback execution
  we spawn a new server for it without waiting for its
  conclusion.
  """
  @spec start(list, fun) :: :ok
  def start(opts, callback) do
    {pid, ref} = spawn_monitor(callback)
    start_loop(opts, pid, ref)
  end

  defp start_loop(opts, pid, ref) do
    receive do
      {:take?, other, ref} ->
        send(other, ref)
        start_loop(opts, pid, ref)

      {:take, other, identifier, ref, opts} ->
        if allow_take?(identifier) do
          send(other, {ref, Process.group_leader})
          run(opts)
        else
          send(other, {ref, nil})
          start_loop(opts, pid, ref)
        end

      {:DOWN, ^ref, :process, ^pid,  :normal} ->
        run(opts)

      {:DOWN, ^ref, :process, ^pid,  _reason} ->
        :ok
    end
  end

  # Run loop: this is where the work is really
  # done after the start loop.

  defp run(opts) when is_list(opts) do
    IO.puts "Interactive Elixir (#{System.version}) - press Ctrl+C to exit (type h() ENTER for help)"
    self_pid    = self
    self_leader = Process.group_leader
    evaluator   = opts[:evaluator] || spawn(fn -> IEx.Evaluator.start(self_pid, self_leader) end)
    Process.put(:evaluator, evaluator)
    loop(run_config(opts), evaluator, Process.monitor(evaluator))
  end

  defp reset_loop(opts, evaluator, evaluator_ref) do
    IO.puts("")
    # We only kill the evaluator if the new evaluator
    # is not the same as the current one. Otherwise
    # we end up killing the process that is taking over.
    exit_loop(evaluator, evaluator_ref, opts[:evaluator] != evaluator)
    run(opts)
  end

  defp exit_loop(evaluator, evaluator_ref, done? \\ true) do
    Process.delete(:evaluator)
    Process.demonitor(evaluator_ref)
    if done? do
      send(evaluator, {:done, self})
    end
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
      {:input, ^input, code} when is_binary(code) ->
        send evaluator, {:eval, self, code, config}
        wait_eval(evaluator, evaluator_ref)
      {:input, ^input, {:error, :interrupted}} ->
        io_error "** (EXIT) interrupted"
        loop(%{config | cache: ''}, evaluator, evaluator_ref)
      {:input, ^input, :eof} ->
        exit_loop(evaluator, evaluator_ref)
      {:input, ^input, {:error, :terminated}} ->
        exit_loop(evaluator, evaluator_ref)

      msg ->
        handle_take_over(msg, evaluator, evaluator_ref, input, fn ->
          wait_input(config, evaluator, evaluator_ref, input)
        end)
    end
  end

  defp wait_eval(evaluator, evaluator_ref) do
    receive do
      {:evaled, ^evaluator, config} ->
        loop(config, evaluator, evaluator_ref)
      msg ->
        handle_take_over(msg, evaluator, evaluator_ref, nil,
                         fn -> wait_eval(evaluator, evaluator_ref) end)
    end
  end

  # Take process.
  #
  # The take? message is received out of band, so we can
  # go back to wait for the same input. The take message
  # needs to take hold of the IO, so it kills the input,
  # re-runs the server OR goes back to the main loop.
  #
  # A take process may also happen if the evaluator dies,
  # then a new evaluator is created to tackle replace the dead
  # one.
  defp handle_take_over({:take?, other, ref}, _evaluator, _evaluator_ref, _input, callback) do
    send(other, ref)
    callback.()
  end

  defp handle_take_over({:take, other, identifier, ref, opts}, evaluator, evaluator_ref, input, callback) do
    kill_input(input)

    if allow_take?(identifier) do
      send other, {ref, Process.group_leader}
      reset_loop(opts, evaluator, evaluator_ref)
    else
      send other, {ref, nil}
      callback.()
    end
  end

  defp handle_take_over({:respawn, evaluator}, evaluator, evaluator_ref, input, _callback) do
    kill_input(input)
    reset_loop([], evaluator, evaluator_ref)
  end

  defp handle_take_over({:DOWN, evaluator_ref, :process, evaluator,  reason},
                        evaluator, evaluator_ref, input, _callback) do
    try do
      io_error Exception.format_banner({:EXIT, evaluator}, reason)
    catch
      type, detail ->
        io_error "** (IEx.Error) #{type} when printing EXIT message: #{inspect detail}"
    end
    kill_input(input)
    reset_loop([], evaluator, evaluator_ref)
  end

  defp handle_take_over(_, _evaluator, _evaluator_ref, _input, callback) do
    callback.()
  end

  defp kill_input(nil),   do: :ok
  defp kill_input(input), do: Process.exit(input, :kill)

  defp allow_take?(identifier) do
    message = IEx.color(:eval_interrupt, "#{identifier}. Allow? [Yn] ")
    IO.gets(:stdio, message) =~ ~r/^(Y(es)?)?$/i
  end

  ## Config

  defp run_config(opts) do
    locals = Keyword.get(opts, :delegate_locals_to, IEx.Helpers)

    env =
      if env = opts[:env] do
        :elixir.env_for_eval(env, delegate_locals_to: locals)
      else
        :elixir.env_for_eval(file: "iex", delegate_locals_to: locals)
      end

    {_, _, env, scope} = :elixir.eval('require IEx.Helpers', [], env)

    binding = Keyword.get(opts, :binding, [])
    prefix  = Keyword.get(opts, :prefix, "iex")
    config  = %IEx.Config{binding: binding, scope: scope, prefix: prefix, env: env}

    case opts[:dot_iex_path] do
      ""   -> config
      path -> IEx.Evaluator.load_dot_iex(config, path)
    end
  end

  ## IO

  defp io_get(pid, prefix, counter) do
    prompt = prompt(prefix, counter)
    send pid, {:input, self, IO.gets(:stdio, prompt)}
  end

  defp prompt(prefix, counter) do
    {mode, prefix} =
      if Node.alive? do
        {:alive, prefix || remote_prefix}
      else
        {:default, prefix || "iex"}
      end

    prompt = Application.get_env(:iex, :"#{mode}_prompt")
             |> String.replace("%counter", to_string(counter))
             |> String.replace("%prefix", to_string(prefix))
             |> String.replace("%node", to_string(node))

    prompt <> " "
  end

  defp io_error(result) do
    IO.puts :stdio, IEx.color(:eval_error, result)
  end

  defp remote_prefix do
    if node == node(Process.group_leader), do: "iex", else: "rem"
  end
end
