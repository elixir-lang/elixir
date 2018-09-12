defmodule IEx.State do
  @moduledoc false
  # This state is exchanged between IEx.Server and
  # IEx.Evaluator which is why it is a struct.
  defstruct cache: '', counter: 1, prefix: "iex"
  @type t :: %__MODULE__{}
end

defmodule IEx.Server do
  @moduledoc false

  @doc """
  Finds the IEx server, on this or another node.
  """
  @spec whereis :: pid | nil
  def whereis() do
    Enum.find_value([node() | Node.list()], fn node ->
      server = :rpc.call(node, IEx.Server, :local, [])
      if is_pid(server), do: server
    end)
  end

  @doc """
  Returns the PID of the IEx server on the local node if exists.
  """
  @spec local :: pid | nil
  def local() do
    # Locate top group leader, always registered as user
    # can be implemented by group (normally) or user
    # (if oldshell or noshell)
    if user = Process.whereis(:user) do
      case :group.interfaces(user) do
        # Old or no shell
        [] ->
          case :user.interfaces(user) do
            [] -> nil
            [shell: shell] -> shell
          end

        # Get current group from user_drv
        [user_drv: user_drv] ->
          case :user_drv.interfaces(user_drv) do
            [] -> nil
            [current_group: group] -> :group.interfaces(group)[:shell]
          end
      end
    end
  end

  @doc """
  Returns the PID of the IEx evaluator process if it exists.
  """
  @spec evaluator :: {evaluator :: pid, server :: pid} | nil
  def evaluator() do
    if pid = IEx.Server.local() do
      {:dictionary, dictionary} = Process.info(pid, :dictionary)
      {dictionary[:evaluator], pid}
    end
  end

  @doc """
  Requests to take over the given shell from the
  current process.
  """
  @spec take_over(binary, keyword) :: :ok | {:error, :no_iex} | {:error, :refused}
  def take_over(identifier, opts, server \\ whereis()) do
    if is_nil(server) do
      {:error, :no_iex}
    else
      ref = make_ref()
      opts = [evaluator: self()] ++ opts
      send(server, {:take, self(), identifier, ref, opts})

      receive do
        {^ref, nil} ->
          {:error, :refused}

        {^ref, leader} ->
          IEx.Evaluator.init(:no_ack, server, leader, opts)
      end
    end
  end

  @doc """
  `start/0` with a callback. The server is spawned only after
  the callback is done.

  If there is any takeover during the callback execution
  we spawn a new server for it without waiting for its
  conclusion.
  """
  @spec start(keyword, {module, atom, [any]}) :: :ok
  def start(opts, {m, f, a}) do
    Process.flag(:trap_exit, true)
    {pid, ref} = spawn_monitor(m, f, a)
    start_loop(opts, pid, ref)
  end

  defp start_loop(opts, pid, ref) do
    receive do
      {:take, take_pid, take_identifier, take_ref, take_opts} ->
        if allow_take?(take_identifier) do
          send(take_pid, {take_ref, Process.group_leader()})
          start(take_opts)
        else
          send(take_pid, {take_ref, nil})
          start_loop(opts, pid, ref)
        end

      {:DOWN, ^ref, :process, ^pid, :normal} ->
        start(opts)

      {:DOWN, ^ref, :process, ^pid, _reason} ->
        :ok
    end
  end

  @doc """
  Starts IEx without a callback.

  The server responsibilities include:

    * reading input
    * sending messages to the evaluator
    * handling takeover process of the evaluator

  """
  @spec start(keyword) :: :ok
  def start(opts) when is_list(opts) do
    Process.flag(:trap_exit, true)

    IO.puts(
      "Interactive Elixir (#{System.version()}) - press Ctrl+C to exit (type h() ENTER for help)"
    )

    evaluator = start_evaluator(opts)
    loop(iex_state(opts), evaluator, Process.monitor(evaluator))
  end

  defp restart(opts, evaluator, evaluator_ref, input) do
    kill_input(input)
    IO.puts("")
    stop_evaluator(evaluator, evaluator_ref)
    start(opts)
  end

  @doc """
  Starts an evaluator using the provided options.
  """
  @spec start_evaluator(keyword) :: pid
  def start_evaluator(opts) do
    evaluator =
      opts[:evaluator] ||
        :proc_lib.start(IEx.Evaluator, :init, [:ack, self(), Process.group_leader(), opts])

    Process.put(:evaluator, evaluator)
    evaluator
  end

  defp stop_evaluator(evaluator, evaluator_ref) do
    Process.delete(:evaluator)
    Process.demonitor(evaluator_ref, [:flush])
    send(evaluator, {:done, self()})
    :ok
  end

  defp loop(state, evaluator, evaluator_ref) do
    self_pid = self()
    counter = state.counter
    prefix = if state.cache != [], do: "...", else: state.prefix

    input = spawn(fn -> io_get(self_pid, prefix, counter) end)
    wait_input(state, evaluator, evaluator_ref, input)
  end

  defp wait_input(state, evaluator, evaluator_ref, input) do
    receive do
      {:input, ^input, code} when is_binary(code) ->
        send(evaluator, {:eval, self(), code, state})
        wait_eval(state, evaluator, evaluator_ref)

      {:input, ^input, {:error, :interrupted}} ->
        io_error("** (EXIT) interrupted")
        loop(%{state | cache: ''}, evaluator, evaluator_ref)

      {:input, ^input, :eof} ->
        stop_evaluator(evaluator, evaluator_ref)

      {:input, ^input, {:error, :terminated}} ->
        stop_evaluator(evaluator, evaluator_ref)

      msg ->
        handle_take_over(msg, state, evaluator, evaluator_ref, input, fn state ->
          wait_input(state, evaluator, evaluator_ref, input)
        end)
    end
  end

  defp wait_eval(state, evaluator, evaluator_ref) do
    receive do
      {:evaled, ^evaluator, new_state} ->
        loop(new_state, evaluator, evaluator_ref)

      msg ->
        handle_take_over(msg, state, evaluator, evaluator_ref, nil, fn state ->
          wait_eval(state, evaluator, evaluator_ref)
        end)
    end
  end

  defp wait_take_over(state, evaluator, evaluator_ref) do
    receive do
      msg ->
        handle_take_over(msg, state, evaluator, evaluator_ref, nil, fn state ->
          wait_take_over(state, evaluator, evaluator_ref)
        end)
    end
  end

  # Take process.
  #
  # A take process may also happen if the evaluator dies,
  # then a new evaluator is created to replace the dead one.
  defp handle_take_over(
         {:take, other, identifier, ref, opts},
         state,
         evaluator,
         evaluator_ref,
         input,
         callback
       ) do
    cond do
      evaluator == opts[:evaluator] ->
        send(other, {ref, Process.group_leader()})
        kill_input(input)
        loop(iex_state(opts), evaluator, evaluator_ref)

      allow_take?(identifier) ->
        send(other, {ref, Process.group_leader()})
        restart(opts, evaluator, evaluator_ref, input)

      true ->
        send(other, {ref, nil})
        callback.(state)
    end
  end

  # User did ^G while the evaluator was busy or stuck
  defp handle_take_over(
         {:EXIT, _pid, :interrupt},
         state,
         evaluator,
         evaluator_ref,
         input,
         _callback
       ) do
    kill_input(input)
    io_error("** (EXIT) interrupted")
    Process.delete(:evaluator)
    Process.exit(evaluator, :kill)
    Process.demonitor(evaluator_ref, [:flush])
    evaluator = start_evaluator([])
    loop(%{state | cache: ''}, evaluator, Process.monitor(evaluator))
  end

  defp handle_take_over({:respawn, evaluator}, _state, evaluator, evaluator_ref, input, _callback) do
    restart([], evaluator, evaluator_ref, input)
  end

  defp handle_take_over({:continue, evaluator}, state, evaluator, evaluator_ref, input, _callback) do
    kill_input(input)
    send(evaluator, {:done, self()})
    wait_take_over(state, evaluator, evaluator_ref)
  end

  defp handle_take_over(
         {:DOWN, evaluator_ref, :process, evaluator, :normal},
         _state,
         evaluator,
         evaluator_ref,
         input,
         _callback
       ) do
    restart([], evaluator, evaluator_ref, input)
  end

  defp handle_take_over(
         {:DOWN, evaluator_ref, :process, evaluator, reason},
         _state,
         evaluator,
         evaluator_ref,
         input,
         _callback
       ) do
    try do
      io_error(
        "** (EXIT from #{inspect(evaluator)}) shell process exited with reason: " <>
          Exception.format_exit(reason)
      )
    catch
      type, detail ->
        io_error("** (IEx.Error) #{type} when printing EXIT message: #{inspect(detail)}")
    end

    restart([], evaluator, evaluator_ref, input)
  end

  defp handle_take_over(_, state, _evaluator, _evaluator_ref, _input, callback) do
    callback.(state)
  end

  defp kill_input(nil), do: :ok
  defp kill_input(input), do: Process.exit(input, :kill)

  defp allow_take?(identifier) do
    message = IEx.color(:eval_interrupt, "#{identifier}\nAllow? [Yn] ")
    yes?(IO.gets(:stdio, message))
  end

  defp yes?(string) do
    is_binary(string) and String.trim(string) in ["", "y", "Y", "yes", "YES", "Yes"]
  end

  ## State

  defp iex_state(opts) do
    prefix = Keyword.get(opts, :prefix, "iex")
    %IEx.State{prefix: prefix}
  end

  ## IO

  defp io_get(pid, prefix, counter) do
    prompt = prompt(prefix, counter)
    send(pid, {:input, self(), IO.gets(:stdio, prompt)})
  end

  defp prompt(prefix, counter) do
    {mode, prefix} =
      if Node.alive?() do
        {:alive_prompt, prefix || remote_prefix()}
      else
        {:default_prompt, prefix || "iex"}
      end

    prompt =
      apply(IEx.Config, mode, [])
      |> String.replace("%counter", to_string(counter))
      |> String.replace("%prefix", to_string(prefix))
      |> String.replace("%node", to_string(node()))

    prompt <> " "
  end

  defp io_error(result) do
    IO.puts(:stdio, IEx.color(:eval_error, result))
  end

  defp remote_prefix do
    if node() == node(Process.group_leader()), do: "iex", else: "rem"
  end
end
