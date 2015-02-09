defmodule IEx.Evaluator do
  @moduledoc false

  @doc """
  Eval loop for an IEx session. Its responsibilities include:

    * loading of .iex files
    * evaluating code
    * trapping exceptions in the code being evaluated
    * keeping expression history

  """
  def start(server, leader) do
    IEx.History.init
    old_leader = Process.group_leader
    Process.group_leader(self, leader)

    try do
      loop(server)
    after
      IEx.History.reset
      Process.group_leader(self, old_leader)
    end
  end

  defp loop(server) do
    receive do
      {:eval, ^server, code, state} ->
        send server, {:evaled, self, eval(code, state)}
        loop(server)
      {:done, ^server} ->
        IEx.History.reset
        :ok
    end
  end

  @doc """
  Locates and loads an .iex.exs file from one of predefined locations.
  Returns the new state.
  """
  def load_dot_iex(state, path \\ nil) do
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
        :elixir.eval(String.to_char_list(code), state.binding, env)

      %{state | binding: binding, env: :elixir.env_for_eval(env, file: "iex", line: 1)}
    catch
      kind, error ->
        io_result "Error while evaluating: #{path}"
        print_error(kind, error, System.stacktrace)
        System.halt(1)
    end
  end

  # Instead of doing just `:elixir.eval`, we first parse the expression to see
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

  defp eval(code, state) do
    try do
      do_eval(String.to_char_list(code), state)
    catch
      kind, error ->
        print_error(kind, error, System.stacktrace)
        %{state | cache: ''}
    end
  end

  defp do_eval(@break_trigger, state=%IEx.State{cache: ''}) do
    state
  end

  defp do_eval(@break_trigger, state) do
    :elixir_errors.parse_error(state.counter, "iex", "incomplete expression", "")
  end

  defp do_eval(latest_input, state) do
    code = state.cache ++ latest_input
    line = state.counter
    handle_eval(Code.string_to_quoted(code, [line: line, file: "iex"]), code, line, state)
  end
  
  defp handle_eval({:ok, forms}, code, line, state) do
    {result, new_binding, env, scope} =
      :elixir.eval_forms(forms, state.binding, state.env, state.scope)
    unless result == IEx.dont_display_result, do: io_inspect(result)
    update_history(line, code, result)
    %{state | env: env,
              cache: '',
              scope: scope,
              binding: new_binding,
              counter: state.counter + 1}
  end

  defp handle_eval({:error, {_, _, ""}}, code, _line, state) do
    # Update state.cache so that IEx continues to add new input to
    # the unfinished expression in `code`
    %{state | cache: code}
  end

  defp handle_eval({:error, {line, error, token}}, _code, _line, _state) do
    # Encountered malformed expression
    :elixir_errors.parse_error(line, "iex", error, token)
  end

  defp update_history(counter, cache, result) do
    IEx.History.append({counter, cache, result}, counter, IEx.Config.history_size)
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
    message = Exception.format_banner(kind, reason, stacktrace)
    io_error message
    io_error (stacktrace |> prune_stacktrace |> format_stacktrace)
  end

  @elixir_internals [:elixir_exp, :elixir_compiler, :elixir_module, :elixir_exp_clauses,
                     :elixir_translator, :elixir_expand, :elixir_lexical]

  defp prune_stacktrace(stacktrace) do
    # The order in which each drop_while is listed is important.
    # For example, the user my call Code.eval_string/2 in IEx
    # and if there is an error we should not remove erl_eval
    # and eval_bits information from the user stacktrace.
    stacktrace
    |> Enum.reverse()
    |> Enum.drop_while(&(elem(&1, 0) == __MODULE__))
    |> Enum.drop_while(&(elem(&1, 0) == :elixir))
    |> Enum.drop_while(&(elem(&1, 0) in [:erl_eval, :eval_bits]))
    |> Enum.reverse()
    |> Enum.reject(fn {mod, _, _, _} when mod in @elixir_internals -> true
                      _ -> false end)
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
    app = String.rjust(app, width)
    "#{IEx.color(:stack_app, app)}#{IEx.color(:stack_info, info)}"
  end
end
