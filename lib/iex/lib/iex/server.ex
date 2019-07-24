defmodule IEx.State do
  @moduledoc false
  # This state is exchanged between IEx.Server and
  # IEx.Evaluator which is why it is a struct.
  defstruct cache: '', counter: 1, prefix: "iex", on_eof: :stop_evaluator
  @type t :: %__MODULE__{}
end

defmodule IEx.Server do
  @moduledoc """
  The IEx.Server.

  The server responsibilities include:

    * reading input from the group leader and writing to the group leader
    * sending messages to the evaluator
    * taking over the evaluator process when using `IEx.pry/0` or setting up breakpoints

  """

  @doc """
  Starts a new IEx server session.

  The accepted options are:

    * `:prefix` - the IEx prefix
    * `:env` - the `Macro.Env` used for the evaluator
    * `:binding` - an initial set of variables for the evaluator
    * `:on_eof` - if it should `:stop_evaluator` (default) or `:halt` the system

  """
  @doc since: "1.8.0"
  @spec run(keyword) :: :ok
  def run(opts) when is_list(opts) do
    IEx.Broker.register(self())
    run_without_registration(opts)
  end

  ## Private APIs

  # Starts IEx to run directly from the Erlang shell.
  #
  # The server is spawned only after the callback is done.
  #
  # If there is any takeover during the callback execution
  # we spawn a new server for it without waiting for its
  # conclusion.
  @doc false
  @spec run_from_shell(keyword, {module, atom, [any]}) :: :ok
  def run_from_shell(opts, {m, f, a}) do
    opts[:register] && IEx.Broker.register(self())
    Process.flag(:trap_exit, true)
    {pid, ref} = spawn_monitor(m, f, a)
    shell_loop(opts, pid, ref)
  end

  defp shell_loop(opts, pid, ref) do
    receive do
      {:take_over, take_pid, take_ref, take_identifier, take_opts} ->
        if take_over?(take_pid, take_ref, take_identifier) do
          run_without_registration(take_opts)
        else
          shell_loop(opts, pid, ref)
        end

      {:DOWN, ^ref, :process, ^pid, :normal} ->
        run_without_registration(opts)

      {:DOWN, ^ref, :process, ^pid, _reason} ->
        :ok
    end
  end

  # Since we want to register only once, this function is the
  # reentrant point for starting a new shell (instead of run/run_from_shell).
  defp run_without_registration(opts) do
    Process.flag(:trap_exit, true)
    Process.link(Process.group_leader())

    IO.puts(
      "Interactive Elixir (#{System.version()}) - press Ctrl+C to exit (type h() ENTER for help)"
    )

    evaluator = start_evaluator(opts)
    loop(iex_state(opts), evaluator, Process.monitor(evaluator))
  end

  # Starts an evaluator using the provided options.
  # Made public but undocumented for testing.
  @doc false
  @spec start_evaluator(keyword) :: pid
  def start_evaluator(opts) do
    evaluator =
      opts[:evaluator] ||
        :proc_lib.start(IEx.Evaluator, :init, [:ack, self(), Process.group_leader(), opts])

    Process.put(:evaluator, evaluator)
    evaluator
  end

  ## Helpers

  defp stop_evaluator(evaluator, evaluator_ref) do
    Process.delete(:evaluator)
    Process.demonitor(evaluator_ref, [:flush])
    send(evaluator, {:done, self()})
    :ok
  end

  defp rerun(opts, evaluator, evaluator_ref, input) do
    kill_input(input)
    IO.puts("")
    stop_evaluator(evaluator, evaluator_ref)
    run_without_registration(opts)
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

      {:input, ^input, :eof} ->
        case state.on_eof do
          :halt -> System.halt(0)
          :stop_evaluator -> stop_evaluator(evaluator, evaluator_ref)
        end

      # Triggered by pressing "i" as the job control switch
      {:input, ^input, {:error, :interrupted}} ->
        io_error("** (EXIT) interrupted")
        loop(%{state | cache: ''}, evaluator, evaluator_ref)

      # Triggered when IO dies while waiting for input
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
         {:take_over, take_pid, take_ref, take_identifier, opts},
         state,
         evaluator,
         evaluator_ref,
         input,
         callback
       ) do
    cond do
      evaluator == opts[:evaluator] ->
        if take_over?(take_pid, take_ref, true) do
          kill_input(input)
          loop(iex_state(opts), evaluator, evaluator_ref)
        else
          callback.(state)
        end

      take_over?(take_pid, take_ref, take_identifier) ->
        rerun(opts, evaluator, evaluator_ref, input)

      true ->
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

  defp handle_take_over(
         {:EXIT, pid, reason},
         state,
         _evaluator,
         _evaluator_ref,
         _input,
         callback
       ) do
    if pid == Process.group_leader() do
      exit(reason)
    else
      callback.(state)
    end
  end

  defp handle_take_over({:respawn, evaluator}, _state, evaluator, evaluator_ref, input, _callback) do
    rerun([], evaluator, evaluator_ref, input)
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
    rerun([], evaluator, evaluator_ref, input)
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

    rerun([], evaluator, evaluator_ref, input)
  end

  defp handle_take_over(_, state, _evaluator, _evaluator_ref, _input, callback) do
    callback.(state)
  end

  defp take_over?(take_pid, take_ref, take_identifier) when is_binary(take_identifier) do
    message = IEx.color(:eval_interrupt, "#{take_identifier}\nAllow? [Yn] ")
    take_over?(take_pid, take_ref, yes?(IO.gets(:stdio, message)))
  end

  defp take_over?(take_pid, take_ref, take_response) do
    case IEx.Broker.respond(take_pid, take_ref, take_response) do
      :ok ->
        true

      {:error, :refused} ->
        false

      {:error, :already_accepted} ->
        io_error("** session was already accepted elsewhere")
        false
    end
  end

  defp kill_input(nil), do: :ok
  defp kill_input(input), do: Process.exit(input, :kill)

  defp yes?(string) do
    is_binary(string) and String.trim(string) in ["", "y", "Y", "yes", "YES", "Yes"]
  end

  ## State

  defp iex_state(opts) do
    prefix = Keyword.get(opts, :prefix, "iex")
    on_eof = Keyword.get(opts, :on_eof, :stop_evaluator)
    %IEx.State{prefix: prefix, on_eof: on_eof}
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
