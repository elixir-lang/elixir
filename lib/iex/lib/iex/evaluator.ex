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
    old_leader = Process.group_leader()
    Process.group_leader(self(), leader)

    old_server = Process.get(:iex_server)
    Process.put(:iex_server, server)

    evaluator = Process.get(:iex_evaluator)
    Process.put(:iex_evaluator, command)

    state = loop_state(server, IEx.History.init(), opts)
    command == :ack && :proc_lib.init_ack(self())

    try do
      loop(state)
    after
      Process.group_leader(self(), old_leader)

      if old_server do
        Process.put(:iex_server, old_server)
      else
        Process.delete(:iex_server)
      end

      cond do
        is_nil(evaluator) ->
          Process.delete(:iex_evaluator)

        evaluator != :ack ->
          # Ensure propagation to non-root level evaluators
          send(self(), {:done, server})

        true ->
          :ok
      end

      :ok
    end
  end

  @doc """
  Gets a value out of the binding, using the provided
  variable name and map key path.
  """
  @spec value_from_binding(pid, pid, atom, [atom]) :: {:ok, any} | :error
  def value_from_binding(evaluator, server, var_name, map_key_path) do
    ref = make_ref()
    send(evaluator, {:value_from_binding, server, ref, self(), var_name, map_key_path})

    receive do
      {^ref, result} -> result
    after
      5000 -> :error
    end
  end

  @doc """
  Gets a list of variables out of the binding that match the passed
  variable prefix.
  """
  @spec variables_from_binding(pid, pid, String.t()) :: [String.t()]
  def variables_from_binding(evaluator, server, variable_prefix) do
    ref = make_ref()
    send(evaluator, {:variables_from_binding, server, ref, self(), variable_prefix})

    receive do
      {^ref, result} -> result
    after
      5000 -> []
    end
  end

  @doc """
  Returns the named fields from the current session environment.
  """
  @spec fields_from_env(pid, pid, [atom]) :: %{optional(atom) => term}
  def fields_from_env(evaluator, server, fields) do
    ref = make_ref()
    send(evaluator, {:fields_from_env, server, ref, self(), fields})

    receive do
      {^ref, result} -> result
    after
      5000 -> %{}
    end
  end

  defp loop(%{server: server} = state) do
    receive do
      {:eval, ^server, code, iex_state} ->
        {result, state} = eval(code, iex_state, state)
        send(server, {:evaled, self(), result})
        loop(state)

      {:fields_from_env, ^server, ref, receiver, fields} ->
        send(receiver, {ref, Map.take(state.env, fields)})
        loop(state)

      {:value_from_binding, ^server, ref, receiver, var_name, map_key_path} ->
        value = traverse_binding(state.binding, var_name, map_key_path)
        send(receiver, {ref, value})
        loop(state)

      {:variables_from_binding, ^server, ref, receiver, var_prefix} ->
        value = find_matched_variables(state.binding, var_prefix)
        send(receiver, {ref, value})
        loop(state)

      {:done, ^server} ->
        :ok
    end
  end

  defp traverse_binding(binding, var_name, map_key_path) do
    accumulator = Keyword.fetch(binding, var_name)

    Enum.reduce(map_key_path, accumulator, fn
      key, {:ok, map} when is_map(map) -> Map.fetch(map, key)
      _key, _acc -> :error
    end)
  end

  defp find_matched_variables(binding, var_prefix) do
    for {var_name, _value} <- binding,
        is_atom(var_name),
        var_name = Atom.to_string(var_name),
        String.starts_with?(var_name, var_prefix),
        do: var_name
  end

  defp loop_state(server, history, opts) do
    env = opts[:env] || :elixir.env_for_eval(file: "iex")
    env = %{env | prematch_vars: :apply}
    {_, _, env, scope} = :elixir.eval('import IEx.Helpers', [], env)
    stacktrace = opts[:stacktrace]
    binding = opts[:binding] || []

    state = %{
      binding: binding,
      scope: scope,
      env: env,
      server: server,
      history: history,
      stacktrace: stacktrace
    }

    case opts[:dot_iex_path] do
      "" -> state
      path -> load_dot_iex(state, path)
    end
  end

  defp load_dot_iex(state, path) do
    candidates =
      if path do
        [path]
      else
        Enum.map([".iex.exs", "~/.iex.exs"], &Path.expand/1)
      end

    path = Enum.find(candidates, &File.regular?/1)

    if is_nil(path) do
      state
    else
      eval_dot_iex(state, path)
    end
  end

  defp eval_dot_iex(state, path) do
    try do
      code = File.read!(path)
      env = :elixir.env_for_eval(state.env, file: path, line: 1)

      # Evaluate the contents in the same environment server_loop will run in
      {_result, binding, env, _scope} = :elixir.eval(String.to_charlist(code), state.binding, env)

      %{state | binding: binding, env: :elixir.env_for_eval(env, file: "iex", line: 1)}
    catch
      kind, error ->
        io_result("Error while evaluating: #{path}")
        print_error(kind, error, __STACKTRACE__)
        System.halt(1)
    end
  end

  # Instead of doing just :elixir.eval, we first parse the expression to see
  # if it's well formed. If parsing succeeds, we evaluate the AST as usual.
  #
  # If parsing fails, this might be a TokenMissingError which we treat in
  # a special way (to allow for continuation of an expression on the next
  # line in IEx).
  #
  # Returns updated state.
  #
  # The first two clauses provide support for the break-trigger allowing to
  # break out from a pending incomplete expression. See
  # https://github.com/elixir-lang/elixir/issues/1089 for discussion.
  @break_trigger '#iex:break\n'

  defp eval(code, iex_state, state) do
    try do
      do_eval(String.to_charlist(code), iex_state, state)
    catch
      kind, error ->
        print_error(kind, error, __STACKTRACE__)
        {%{iex_state | cache: ''}, state}
    end
  end

  defp do_eval(@break_trigger, %IEx.State{cache: ''} = iex_state, state) do
    {iex_state, state}
  end

  defp do_eval(@break_trigger, iex_state, _state) do
    :elixir_errors.parse_error(iex_state.counter, "iex", "incomplete expression", "")
  end

  defp do_eval(latest_input, iex_state, state) do
    code = iex_state.cache ++ latest_input
    line = iex_state.counter
    put_history(state)
    put_whereami(state)
    quoted = Code.string_to_quoted(code, line: line, file: "iex")
    handle_eval(quoted, code, line, iex_state, state)
  after
    Process.delete(:iex_history)
    Process.delete(:iex_whereami)
  end

  defp put_history(%{history: history}) do
    Process.put(:iex_history, history)
  end

  defp put_whereami(%{env: %{file: "iex"}}) do
    :ok
  end

  defp put_whereami(%{env: %{file: file, line: line}, stacktrace: stacktrace}) do
    Process.put(:iex_whereami, {file, line, stacktrace})
  end

  defp handle_eval({:ok, forms}, code, line, iex_state, state) do
    {result, binding, env, scope} =
      :elixir.eval_forms(forms, state.binding, state.env, state.scope)

    unless result == IEx.dont_display_result() do
      io_inspect(result)
    end

    iex_state = %{iex_state | cache: '', counter: iex_state.counter + 1}
    state = %{state | env: env, scope: scope, binding: binding}
    {iex_state, update_history(state, line, code, result)}
  end

  defp handle_eval({:error, {_, _, ""}}, code, _line, iex_state, state) do
    # Update iex_state.cache so that IEx continues to add new input to
    # the unfinished expression in "code"
    {%{iex_state | cache: code}, state}
  end

  defp handle_eval({:error, {line, error, token}}, _code, _line, _iex_state, _state) do
    # Encountered malformed expression
    :elixir_errors.parse_error(line, "iex", error, token)
  end

  defp update_history(state, counter, _cache, result) do
    history_size = IEx.Config.history_size()
    update_in(state.history, &IEx.History.append(&1, {counter, result}, history_size))
  end

  defp io_inspect(result) do
    io_result(inspect(result, IEx.inspect_opts()))
  end

  defp io_result(result) do
    IO.puts(:stdio, IEx.color(:eval_result, result))
  end

  ## Error handling

  defp print_error(kind, reason, stacktrace) do
    {blamed, stacktrace} = Exception.blame(kind, reason, stacktrace)

    ansidata =
      case blamed do
        %FunctionClauseError{} ->
          {_, inspect_opts} = pop_in(IEx.inspect_opts()[:syntax_colors][:reset])
          banner = Exception.format_banner(kind, reason, stacktrace)
          blame = FunctionClauseError.blame(blamed, &inspect(&1, inspect_opts), &blame_match/2)
          [IEx.color(:eval_error, banner), pad(blame)]

        _ ->
          [IEx.color(:eval_error, Exception.format_banner(kind, blamed, stacktrace))]
      end

    stackdata = Exception.format_stacktrace(prune_stacktrace(stacktrace))
    IO.write(:stdio, [ansidata, ?\n, IEx.color(:stack_info, stackdata)])
  end

  defp pad(string) do
    "    " <> String.replace(string, "\n", "\n    ")
  end

  defp blame_match(%{match?: true, node: node}, _), do: Macro.to_string(node)
  defp blame_match(%{match?: false, node: node}, _), do: blame_ansi(:blame_diff, "-", node)
  defp blame_match(_, string), do: string

  defp blame_ansi(color, no_ansi, node) do
    case IEx.Config.color(color) do
      nil ->
        no_ansi <> Macro.to_string(node) <> no_ansi

      ansi ->
        [ansi | Macro.to_string(node)]
        |> IO.ANSI.format(true)
        |> IO.iodata_to_binary()
    end
  end

  @elixir_internals [:elixir, :elixir_expand, :elixir_compiler, :elixir_module] ++
                      [:elixir_clauses, :elixir_lexical, :elixir_def, :elixir_map] ++
                      [:elixir_erl, :elixir_erl_clauses, :elixir_erl_pass]

  defp prune_stacktrace(stacktrace) do
    # The order in which each drop_while is listed is important.
    # For example, the user may call Code.eval_string/2 in IEx
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
end
