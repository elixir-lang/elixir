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
            expand_fun: nil,
            previous_state: nil

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
    run_without_registration(opts, nil)
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
        if take_over?(take_pid, take_ref, take_location, take_whereami, take_opts) do
          take_opts = Keyword.put(take_opts, :previous_state, iex_state(opts))
          run_without_registration(take_opts, nil)
        else
          shell_loop(opts, pid, ref)
        end

      {:DOWN, ^ref, :process, ^pid, :normal} ->
        run_without_registration(opts, nil)

      {:DOWN, ^ref, :process, ^pid, _reason} ->
        :ok
    end
  end

  # Since we want to register only once, this function is the
  # reentrant point for starting a new shell (instead of run/run_from_shell).
  defp run_without_registration(opts, input) do
    Process.flag(:trap_exit, true)
    Process.link(Process.group_leader())

    IO.puts(
      "Interactive Elixir (#{System.version()}) - press Ctrl+C to exit (type h() ENTER for help)"
    )

    evaluator = start_evaluator(opts)
    loop(iex_state(opts), :ok, evaluator, Process.monitor(evaluator), input)
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
    send(evaluator, {:done, self(), false})
    :ok
  end

  defp rerun(opts, evaluator, evaluator_ref, input) do
    IO.puts("")
    stop_evaluator(evaluator, evaluator_ref)
    run_without_registration(opts, input)
  end

  defp loop(state, prompt, evaluator, evaluator_ref, input) do
    counter = state.counter

    if prompt == :incomplete do
      write_prompt(:continuation_prompt, "...", counter)
    else
      write_prompt(:prompt, state.prefix, counter)
    end

    :io.setopts(expand_fun: state.expand_fun)
    input = input || io_get()
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
        loop(%{state | parser_state: ""}, :new, evaluator, evaluator_ref, nil)

      # Unknown IO message
      {:io_reply, ^input, msg} ->
        io_error("** (EXIT) unknown IO message: #{inspect(msg)}")
        loop(%{state | parser_state: ""}, :new, evaluator, evaluator_ref, nil)

      # Triggered when IO dies while waiting for input
      {:DOWN, ^input, _, _, _} ->
        stop_evaluator(evaluator, evaluator_ref)

      msg ->
        handle_take_over(msg, state, evaluator, evaluator_ref, input, fn state ->
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
        handle_take_over(msg, state, evaluator, evaluator_ref, nil, fn state ->
          wait_eval(state, evaluator, evaluator_ref)
        end)
    end
  end

  defp wait_take_over(state, evaluator, evaluator_ref, input) do
    receive do
      msg ->
        handle_take_over(msg, state, evaluator, evaluator_ref, input, fn state ->
          wait_take_over(state, evaluator, evaluator_ref, input)
        end)
    end
  end

  # Take process.
  #
  # A take process may also happen if the evaluator dies,
  # then a new evaluator is created to replace the dead one.
  defp handle_take_over(
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

        if take_over?(take_pid, take_ref, true) do
          take_opts = Keyword.put(take_opts, :previous_state, state)
          loop(iex_state(take_opts), :ok, evaluator, evaluator_ref, input)
        else
          callback.(state)
        end

      take_over?(take_pid, take_ref, take_location, take_whereami, take_opts) ->
        take_opts = Keyword.put(take_opts, :previous_state, state)
        rerun(take_opts, evaluator, evaluator_ref, input)

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
    io_error("** (EXIT) interrupted")
    Process.delete(:evaluator)
    Process.exit(evaluator, :kill)
    Process.demonitor(evaluator_ref, [:flush])
    evaluator = start_evaluator(state.evaluator_options)
    loop(%{state | parser_state: ""}, :ok, evaluator, Process.monitor(evaluator), input)
  end

  defp handle_take_over(
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

  defp handle_take_over({:respawn, evaluator}, state, evaluator, evaluator_ref, input, _callback) do
    opts =
      if state.previous_state,
        do: state.previous_state.evaluator_options,
        else: state.evaluator_options

    rerun(opts, evaluator, evaluator_ref, input)
  end

  defp handle_take_over(
         {:continue, evaluator, next?},
         state,
         evaluator,
         evaluator_ref,
         input,
         _callback
       ) do
    send(evaluator, {:done, self(), next?})
    wait_take_over(state, evaluator, evaluator_ref, input)
  end

  defp handle_take_over(
         {:DOWN, evaluator_ref, :process, evaluator, :normal},
         state,
         evaluator,
         evaluator_ref,
         input,
         _callback
       ) do
    opts =
      if state.previous_state,
        do: state.previous_state.evaluator_options,
        else: state.evaluator_options

    rerun(opts, evaluator, evaluator_ref, input)
  end

  defp handle_take_over(
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

    opts =
      if state.previous_state,
        do: state.previous_state.evaluator_options,
        else: state.evaluator_options

    rerun(opts, evaluator, evaluator_ref, input)
  end

  defp handle_take_over(_, state, _evaluator, _evaluator_ref, _input, callback) do
    callback.(state)
  end

  defp take_over?(take_pid, take_ref, take_location, take_whereami, take_opts) do
    evaluator = take_opts[:evaluator]
    message = "Request to pry #{inspect(evaluator)} at #{take_location}#{take_whereami}"
    interrupt = IEx.color(:eval_interrupt, "#{message}\nAllow? [Yn] ")
    take_over?(take_pid, take_ref, yes?(IO.gets(:stdio, interrupt)))
  end

  defp take_over?(take_pid, take_ref, take_response) when is_boolean(take_response) do
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

  defp yes?(string) do
    is_binary(string) and String.trim(string) in ["", "y", "Y", "yes", "YES", "Yes"]
  end

  ## State

  defp iex_state(opts) do
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
      previous_state: Keyword.get(opts, :previous_state),
      evaluator_options: Keyword.take(opts, [:dot_iex_path])
    }
  end

  ## IO

  defp io_get() do
    gl = Process.group_leader()
    ref = Process.monitor(gl)
    send(gl, {:io_request, self(), ref, {:get_line, :unicode, ""}})
    ref
  end

  defp write_prompt(prompt_type, prefix, counter) do
    {mode, prefix} =
      if Node.alive?() do
        {prompt_mode(prompt_type, :alive), prefix || remote_prefix()}
      else
        {prompt_mode(prompt_type, :default), prefix || "iex"}
      end

    prompt =
      apply(IEx.Config, mode, [])
      |> String.replace("%counter", to_string(counter))
      |> String.replace("%prefix", to_string(prefix))
      |> String.replace("%node", to_string(node()))

    IO.write([prompt, " "])
  end

  defp prompt_mode(:prompt, :default), do: :default_prompt
  defp prompt_mode(:prompt, :alive), do: :alive_prompt
  defp prompt_mode(:continuation_prompt, :default), do: :continuation_prompt
  defp prompt_mode(:continuation_prompt, :alive), do: :alive_continuation_prompt

  defp io_error(result) do
    IO.puts(:stdio, IEx.color(:eval_error, result))
  end

  defp remote_prefix do
    if node() == node(Process.group_leader()), do: "iex", else: "rem"
  end
end
