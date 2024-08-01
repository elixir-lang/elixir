defmodule IEx.Server do
  @moduledoc """
  The IEx.Server.

  The server responsibilities include:

    * reading input from the group leader and writing to the group leader
    * sending messages to the evaluator
    * taking over the evaluator process when using `IEx.pry/0` or setting up breakpoints

  """

  @doc false
  defstruct parser_state: "",
            counter: 1,
            prefix: "iex",
            on_eof: :stop_evaluator,
            evaluator_options: [],
            expand_fun: nil

  @doc """
  Starts a new IEx server session.

  The accepted options are:

    * `:prefix` - the IEx prefix
    * `:env` - the `Macro.Env` used for the evaluator
    * `:binding` - an initial set of variables for the evaluator
    * `:on_eof` - if it should `:stop_evaluator` (default) or `:halt` the system
    * `:register` - if this shell should be registered in the broker (default is `true`)

  """
  @doc since: "1.8.0"
  @spec run(keyword) :: :ok
  def run(opts) when is_list(opts) do
    if Keyword.get(opts, :register, true) do
      IEx.Broker.register(self())
    end

    run_without_registration(init_state(opts), opts, nil)
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
      {:take_over, take_pid, take_ref, take_location, take_whereami, take_opts} ->
        IO.puts(IEx.color(:eval_interrupt, "Break reached: #{take_location}#{take_whereami}"))

        if take_over?(take_pid, take_ref, 1, true) do
          run_without_registration(init_state(opts), take_opts, nil)
        else
          shell_loop(opts, pid, ref)
        end

      {:DOWN, ^ref, :process, ^pid, :normal} ->
        run_without_registration(init_state(opts), opts, nil)

      {:DOWN, ^ref, :process, ^pid, _reason} ->
        :ok
    end
  end

  # Since we want to register only once, this function is the
  # reentrant point for starting a new shell (instead of run/run_from_shell).
  defp run_without_registration(state, opts, input) do
    Process.flag(:trap_exit, true)
    Process.link(Process.group_leader())

    IO.puts(
      "Interactive Elixir (#{System.version()}) - press Ctrl+C to exit (type h() ENTER for help)"
    )

    evaluator = start_evaluator(state.counter, Keyword.merge(state.evaluator_options, opts))
    loop(state, :ok, evaluator, Process.monitor(evaluator), input)
  end

  # Starts an evaluator using the provided options.
  # Made public but undocumented for testing.
  @doc false
  def start_evaluator(counter, opts) do
    args = [:ack, self(), Process.group_leader(), counter, opts]
    evaluator = opts[:evaluator] || :proc_lib.start(IEx.Evaluator, :init, args)
    Process.put(:evaluator, evaluator)
    evaluator
  end

  ## Helpers

  defp stop_evaluator(evaluator, evaluator_ref) do
    Process.delete(:evaluator)
    Process.demonitor(evaluator_ref, [:flush])
    send(evaluator, {:done, self(), false})
    :ok
  end

  defp rerun(state, opts, evaluator, evaluator_ref, input) do
    IO.puts("")
    stop_evaluator(evaluator, evaluator_ref)
    state = reset_state(state)
    run_without_registration(state, opts, input)
  end

  defp loop(state, status, evaluator, evaluator_ref, input) do
    :io.setopts(expand_fun: state.expand_fun)
    input = input || io_get(prompt(status, state.prefix, state.counter))
    wait_input(state, evaluator, evaluator_ref, input)
  end

  defp wait_input(state, evaluator, evaluator_ref, input) do
    receive do
      {:io_reply, ^input, code} when is_binary(code) ->
        :io.setopts(expand_fun: fn _ -> {:yes, [], []} end)
        send(evaluator, {:eval, self(), code, state.counter, state.parser_state})
        wait_eval(state, evaluator, evaluator_ref)

      {:io_reply, ^input, :eof} ->
        case state.on_eof do
          :halt -> System.halt(0)
          :stop_evaluator -> stop_evaluator(evaluator, evaluator_ref)
        end

      # Triggered by pressing "i" as the job control switch
      {:io_reply, ^input, {:error, :interrupted}} ->
        io_error("** (EXIT) interrupted")
        loop(%{state | parser_state: ""}, :ok, evaluator, evaluator_ref, nil)

      # Unknown IO message
      {:io_reply, ^input, msg} ->
        io_error("** (EXIT) unknown IO message: #{inspect(msg)}")
        loop(%{state | parser_state: ""}, :ok, evaluator, evaluator_ref, nil)

      # Triggered when IO dies while waiting for input
      {:DOWN, ^input, _, _, _} ->
        stop_evaluator(evaluator, evaluator_ref)

      msg ->
        handle_common(msg, state, evaluator, evaluator_ref, input, fn state ->
          wait_input(state, evaluator, evaluator_ref, input)
        end)
    end
  end

  defp wait_eval(state, evaluator, evaluator_ref) do
    receive do
      {:evaled, ^evaluator, status, parser_state} ->
        counter = if(status == :ok, do: state.counter + 1, else: state.counter)
        state = %{state | counter: counter, parser_state: parser_state}
        loop(state, status, evaluator, evaluator_ref, nil)

      msg ->
        handle_common(msg, state, evaluator, evaluator_ref, nil, fn state ->
          wait_eval(state, evaluator, evaluator_ref)
        end)
    end
  end

  defp wait_common(state, evaluator, evaluator_ref, input) do
    receive do
      msg ->
        handle_common(msg, state, evaluator, evaluator_ref, input, fn state ->
          wait_common(state, evaluator, evaluator_ref, input)
        end)
    end
  end

  # Take process.
  #
  # A take process may also happen if the evaluator dies,
  # then a new evaluator is created to replace the dead one.
  defp handle_common(
         {:take_over, take_pid, take_ref, take_location, take_whereami, take_opts},
         state,
         evaluator,
         evaluator_ref,
         input,
         callback
       ) do
    cond do
      evaluator == take_opts[:evaluator] ->
        IO.puts(IEx.color(:eval_interrupt, "Break reached: #{take_location}#{take_whereami}"))

        if take_over?(take_pid, take_ref, state.counter + 1, true) do
          # Since we are in process, also bump the counter
          state = reset_state(bump_counter(state))
          loop(state, :ok, evaluator, evaluator_ref, input)
        else
          callback.(state)
        end

      take_over?(take_pid, take_ref, take_location, take_whereami, take_opts, state.counter) ->
        rerun(state, take_opts, evaluator, evaluator_ref, input)

      true ->
        callback.(state)
    end
  end

  # User did ^G while the evaluator was busy or stuck
  defp handle_common(
         {:EXIT, _pid, :interrupt},
         state,
         evaluator,
         evaluator_ref,
         input,
         _callback
       ) do
    io_error("** (EXIT) interrupted")
    Process.exit(evaluator, :kill)
    rerun(state, [], evaluator, evaluator_ref, input)
  end

  defp handle_common(
         {:EXIT, pid, reason},
         state,
         evaluator,
         evaluator_ref,
         _input,
         callback
       ) do
    if pid == Process.group_leader() do
      stop_evaluator(evaluator, evaluator_ref)
      exit(reason)
    else
      callback.(state)
    end
  end

  defp handle_common({:respawn, evaluator}, state, evaluator, evaluator_ref, input, _callback) do
    rerun(bump_counter(state), [], evaluator, evaluator_ref, input)
  end

  defp handle_common(
         {:continue, evaluator, next?},
         state,
         evaluator,
         evaluator_ref,
         input,
         _callback
       ) do
    send(evaluator, {:done, self(), next?})
    wait_common(state, evaluator, evaluator_ref, input)
  end

  defp handle_common(
         {:DOWN, evaluator_ref, :process, evaluator, reason},
         state,
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

    rerun(state, [], evaluator, evaluator_ref, input)
  end

  defp handle_common(_, state, _evaluator, _evaluator_ref, _input, callback) do
    callback.(state)
  end

  defp take_over?(take_pid, take_ref, take_location, take_whereami, take_opts, counter) do
    evaluator = take_opts[:evaluator] || self()
    message = "Request to pry #{inspect(evaluator)} at #{take_location}#{take_whereami}"
    interrupt = IEx.color(:eval_interrupt, "#{message}\nAllow? [Yn] ")
    answer = yes?(IO.gets(:stdio, interrupt))
    take_over?(take_pid, take_ref, counter, answer)
  end

  defp take_over?(take_pid, take_ref, counter, response) when is_boolean(response) do
    case IEx.Broker.respond(take_pid, take_ref, counter, response) do
      :ok ->
        true

      {:error, :refused} ->
        false

      {:error, :already_accepted} ->
        io_error("** session was already accepted elsewhere")
        false
    end
  end

  defp yes?(string) when is_binary(string),
    do: String.trim(string) in ["", "y", "Y", "yes", "YES", "Yes"]

  defp yes?(charlist) when is_list(charlist),
    do: yes?(List.to_string(charlist))

  defp yes?(_), do: false

  ## State

  defp init_state(opts) do
    prefix = Keyword.get(opts, :prefix, "iex")
    on_eof = Keyword.get(opts, :on_eof, :stop_evaluator)
    gl = Process.group_leader()

    expand_fun =
      if node(gl) != node() do
        IEx.Autocomplete.remsh(node())
      else
        &IEx.Autocomplete.expand/1
      end

    %IEx.Server{
      prefix: prefix,
      on_eof: on_eof,
      expand_fun: expand_fun,
      evaluator_options: Keyword.take(opts, [:dot_iex])
    }
  end

  # For the state, reset only reset the parser state.
  # The counter will continue going up as the input process is shared.
  # The opts can also set "dot_iex" and the "evaluator" itself,
  # but those are not stored: they are temporary to whatever is rerunning.
  # Once the rerunning session restarts, we keep the same evaluator_options
  # and rollback to a new evaluator.
  defp reset_state(state) do
    %{state | parser_state: ""}
  end

  defp bump_counter(state) do
    update_in(state.counter, &(&1 + 1))
  end

  ## IO

  defp io_get(prompt) do
    gl = Process.group_leader()
    ref = Process.monitor(gl)
    command = {:get_until, :unicode, prompt, __MODULE__, :__parse__, []}
    send(gl, {:io_request, self(), ref, command})
    ref
  end

  @doc false
  def __parse__([], :eof), do: {:done, :eof, []}
  def __parse__([], chars), do: {:done, List.to_string(chars), []}

  defp prompt(status, prefix, counter) do
    {mode, prefix} =
      if Node.alive?() do
        {prompt_mode(status, :alive), default_prefix(status, prefix)}
      else
        {prompt_mode(status, :default), default_prefix(status, prefix)}
      end

    prompt =
      apply(IEx.Config, mode, [])
      |> String.replace("%counter", to_string(counter))
      |> String.replace("%prefix", to_string(prefix))
      |> String.replace("%node", to_string(node()))

    [prompt, " "]
  end

  defp default_prefix(:incomplete, _prefix), do: "..."
  defp default_prefix(_ok_or_error, prefix), do: prefix

  defp prompt_mode(:incomplete, :default), do: :continuation_prompt
  defp prompt_mode(:incomplete, :alive), do: :alive_continuation_prompt
  defp prompt_mode(_ok_or_error, :default), do: :default_prompt
  defp prompt_mode(_ok_or_error, :alive), do: :alive_prompt

  defp io_error(result) do
    IO.puts(:stdio, IEx.color(:eval_error, result))
  end
end
