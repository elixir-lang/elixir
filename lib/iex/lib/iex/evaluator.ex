defmodule IEx.Evaluator do
  @moduledoc false

  @doc """
  Eval loop for an IEx session. Its responsibilities include:

    * loading of .iex files
    * evaluating code
    * trapping exceptions in the code being evaluated
    * keeping expression history

  """
  def init(command, server, leader, opts) do
    old_leader = Process.group_leader
    Process.group_leader(self(), leader)

    state = loop_state(opts)
    command == :ack && :proc_lib.init_ack(self())

    try do
      loop(server, IEx.History.init, state)
    after
      Process.group_leader(self(), old_leader)
    end
  end

  @doc """
  Gets a value out of the binding, using the provided
  variable name and map key path.
  """
  @spec value_from_binding(pid, atom, [atom]) :: {:ok, any} | :error
  def value_from_binding(evaluator, var_name, map_key_path) do
    ref = make_ref()
    send evaluator, {:value_from_binding, ref, self(), var_name, map_key_path}

    receive do
      {^ref, result} -> result
    after
      5000 -> :error
    end
  end

  @doc """
  Returns the current session environment if a session exists.
  """
  @spec value_from_env(pid, atom) :: {:ok, term} | :error
  def value_from_env(evaluator, key) do
    ref = make_ref()
    send evaluator, {:value_from_env, ref, self(), key}

    receive do
      {^ref, result} -> result
    after
      5000 -> :error
    end
  end

  defp loop(server, history, state) do
    receive do
      {:eval, ^server, code, iex_state} ->
        {result, history, state} = eval(code, iex_state, history, state)
        send server, {:evaled, self(), result}
        loop(server, history, state)
      {:value_from_env, ref, receiver, key} ->
        send receiver, {ref, Map.fetch(state.env, key)}
        loop(server, history, state)
      {:value_from_binding, ref, receiver, var_name, map_key_path} ->
        value = traverse_binding(state.binding, var_name, map_key_path)
        send receiver, {ref, value}
        loop(server, history, state)
      {:done, ^server} ->
        :ok
    end
  end

  defp traverse_binding(binding, var_name, map_key_path) do
    accumulator = Keyword.fetch(binding, var_name)

    Enum.reduce map_key_path, accumulator, fn
      key, {:ok, map} when is_map(map) -> Map.fetch(map, key)
      _key, _acc -> :error
    end
  end

  defp loop_state(opts) do
    env =
      if env = opts[:env] do
        :elixir.env_for_eval(env, [])
      else
        :elixir.env_for_eval(file: "iex")
      end

    {_, _, env, scope} = :elixir.eval('import IEx.Helpers', [], env)

    binding = Keyword.get(opts, :binding, [])
    state  = %{binding: binding, scope: scope, env: env}

    case opts[:dot_iex_path] do
      ""   -> state
      path -> load_dot_iex(state, path)
    end
  end

  defp load_dot_iex(state, path) do
    candidates = if path do
      [path]
    else
      Enum.map [".iex.exs", "~/.iex.exs"], &Path.expand/1
    end

    path = Enum.find candidates, &File.regular?/1

    if is_nil(path) do
      state
    else
      eval_dot_iex(state, path)
    end
  end

  defp eval_dot_iex(state, path) do
    try do
      code = File.read!(path)
      env  = :elixir.env_for_eval(state.env, file: path, line: 1)

      # Evaluate the contents in the same environment server_loop will run in
      {_result, binding, env, _scope} =
        :elixir.eval(String.to_charlist(code), state.binding, env)

      %{state | binding: binding, env: :elixir.env_for_eval(env, file: "iex", line: 1)}
    catch
      kind, error ->
        io_result "Error while evaluating: #{path}"
        print_error(kind, error, System.stacktrace)
        System.halt(1)
    end
  end

  # Instead of doing just :elixir.eval, we first parse the expression to see
  # if it's well formed. If parsing succeeds, we evaluate the AST as usual.
  #
  # If parsing fails, this might be a TokenMissingError which we treat in
  # a special way (to allow for continuation of an expression on the next
  # line in IEx). In case of any other error, we let :elixir_translator
  # to re-raise it.
  #
  # Returns updated state.
  #
  # The first two clauses provide support for the break-trigger allowing to
  # break out from a pending incomplete expression. See
  # https://github.com/elixir-lang/elixir/issues/1089 for discussion.
  @break_trigger '#iex:break\n'

  defp eval(code, iex_state, history, state) do
    try do
      do_eval(String.to_charlist(code), iex_state, history, state)
    catch
      kind, error ->
        print_error(kind, error, System.stacktrace)
        {%{iex_state | cache: ''}, history, state}
    end
  end

  defp do_eval(@break_trigger, %IEx.State{cache: ''} = iex_state, history, state) do
    {iex_state, history, state}
  end

  defp do_eval(@break_trigger, iex_state, _history, _state) do
    :elixir_errors.parse_error(iex_state.counter, "iex", "incomplete expression", "")
  end

  defp do_eval(latest_input, iex_state, history, state) do
    code = iex_state.cache ++ latest_input
    line = iex_state.counter
    Process.put(:iex_history, history)
    handle_eval(Code.string_to_quoted(code, [line: line, file: "iex"]), code, line, iex_state, history, state)
  after
    Process.delete(:iex_history)
  end

  defp handle_eval({:ok, forms}, code, line, iex_state, history, state) do
    {result, binding, env, scope} =
      :elixir.eval_forms(forms, state.binding, state.env, state.scope)
    unless result == IEx.dont_display_result, do: io_inspect(result)
    iex_state =
      %{iex_state | cache: '',
                    counter: iex_state.counter + 1}

    state =
      %{state | env: env,
                scope: scope,
                binding: binding}

    {iex_state, update_history(history, line, code, result), state}
  end

  defp handle_eval({:error, {_, _, ""}}, code, _line, iex_state, history, state) do
    # Update iex_state.cache so that IEx continues to add new input to
    # the unfinished expression in "code"
    {%{iex_state | cache: code}, history, state}
  end

  defp handle_eval({:error, {line, error, token}}, _code, _line, _iex_state, _, _state) do
    # Encountered malformed expression
    :elixir_errors.parse_error(line, "iex", error, token)
  end

  defp update_history(history, counter, cache, result) do
    IEx.History.append(history, {counter, cache, result}, IEx.Config.history_size)
  end

  defp io_inspect(result) do
    io_result inspect(result, IEx.inspect_opts)
  end

  defp io_result(result) do
    IO.puts :stdio, IEx.color(:eval_result, result)
  end

  defp io_error(result) do
    IO.puts :stdio, IEx.color(:eval_error, result)
  end

  ## Error handling

  defp print_error(kind, reason, stacktrace) do
    Exception.format_banner(kind, reason, stacktrace) |> io_error
    stacktrace |> prune_stacktrace |> format_stacktrace |> io_error
  end

  @elixir_internals [:elixir, :elixir_exp, :elixir_compiler, :elixir_module, :elixir_clauses,
                     :elixir_translator, :elixir_expand, :elixir_lexical, :elixir_exp_clauses,
                     :elixir_def, :elixir_map]

  defp prune_stacktrace(stacktrace) do
    # The order in which each drop_while is listed is important.
    # For example, the user my call Code.eval_string/2 in IEx
    # and if there is an error we should not remove erl_eval
    # and eval_bits information from the user stacktrace.
    stacktrace
    |> Enum.reverse()
    |> Enum.drop_while(&(elem(&1, 0) == :proc_lib))
    |> Enum.drop_while(&(elem(&1, 0) == __MODULE__))
    |> Enum.drop_while(&(elem(&1, 0) == :elixir))
    |> Enum.drop_while(&(elem(&1, 0) in [:erl_eval, :eval_bits]))
    |> Enum.reverse()
    |> Enum.reject(&(elem(&1, 0) in @elixir_internals))
  end

  @doc false
  def format_stacktrace(trace) do
    entries =
      for entry <- trace do
        split_entry(Exception.format_stacktrace_entry(entry))
      end

    width = Enum.reduce entries, 0, fn {app, _}, acc ->
      max(String.length(app), acc)
    end

    "    " <> Enum.map_join(entries, "\n    ", &format_entry(&1, width))
  end

  defp split_entry(entry) do
    case entry do
      "(" <> _ ->
        case :binary.split(entry, ") ") do
          [left, right] -> {left <> ") ", right}
          _ -> {"", entry}
        end
      _ ->
        {"", entry}
    end
  end

  defp format_entry({app, info}, width) do
    app = String.pad_leading(app, width)
    IEx.color(:stack_app, app) <> IEx.color(:stack_info, info)
  end
end
