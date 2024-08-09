defmodule IEx.Evaluator do
  @moduledoc false

  alias IEx.Config

  @doc """
  Eval loop for an IEx session. Its responsibilities include:

    * loading of .iex files
    * evaluating code
    * trapping exceptions in the code being evaluated
    * keeping expression history

  """
  def init(command, server, leader, start, opts) do
    ref = make_ref()
    old_leader = Process.group_leader()
    Process.group_leader(self(), leader)

    old_server = Process.get(:iex_server)
    Process.put(:iex_server, server)

    old_evaluator = Process.get(:iex_evaluator)
    Process.put(:iex_evaluator, ref)

    if old_evaluator do
      send(self(), {:done, old_evaluator, false})
    end

    state = loop_state(ref, server, IEx.History.init(start), opts)
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

      if old_evaluator do
        Process.put(:iex_evaluator, old_evaluator)
      else
        Process.delete(:iex_evaluator)
      end

      :ok
    end
  end

  # If parsing fails, this might be a TokenMissingError which we treat in
  # a special way (to allow for continuation of an expression on the next
  # line in IEx).
  #
  # The first two clauses provide support for the break-trigger allowing to
  # break out from a pending incomplete expression. See
  # https://github.com/elixir-lang/elixir/issues/1089 for discussion.
  @break_trigger "#iex:break\n"

  @op_tokens [:or_op, :and_op, :comp_op, :rel_op, :arrow_op, :in_op] ++
               [:three_op, :concat_op, :mult_op]

  @doc false
  def parse(input, opts, parser_state)

  def parse(input, opts, ""), do: parse(input, opts, {"", :other})

  def parse(@break_trigger, _opts, {"", _} = parser_state) do
    {:incomplete, parser_state}
  end

  def parse(@break_trigger, opts, _parser_state) do
    :elixir_errors.parse_error(
      [line: opts[:line]],
      opts[:file],
      "incomplete expression",
      "",
      {~c"", Keyword.get(opts, :line, 1), Keyword.get(opts, :column, 1)}
    )
  end

  def parse(input, opts, {buffer, last_op}) do
    input = buffer <> input
    file = Keyword.get(opts, :file, "nofile")
    line = Keyword.get(opts, :line, 1)
    column = Keyword.get(opts, :column, 1)
    charlist = String.to_charlist(input)

    result =
      with {:ok, tokens} <- :elixir.string_to_tokens(charlist, line, column, file, opts),
           {:ok, adjusted_tokens} <- adjust_operator(tokens, line, column, file, opts, last_op),
           {:ok, forms} <- :elixir.tokens_to_quoted(adjusted_tokens, file, opts) do
        last_op =
          case forms do
            {:=, _, [_, _]} -> :match
            _ -> :other
          end

        {:ok, forms, last_op}
      end

    case result do
      {:ok, forms, last_op} ->
        {:ok, forms, {"", last_op}}

      {:error, {_, _, ""}} ->
        {:incomplete, {input, last_op}}

      {:error, {location, error, token}} ->
        :elixir_errors.parse_error(
          location,
          file,
          error,
          token,
          {charlist, line, column}
        )
    end
  end

  defp adjust_operator([{op_type, _, token} | _] = _tokens, line, column, _file, _opts, :match)
       when op_type in @op_tokens,
       do:
         {:error,
          {[line: line, column: column],
           "pipe shorthand is not allowed immediately after a match expression in IEx. To make it work, surround the whole pipeline with parentheses ",
           "'#{token}'"}}

  defp adjust_operator([{op_type, _, _} | _] = tokens, line, column, file, opts, _last_op)
       when op_type in @op_tokens do
    {:ok, prefix} = :elixir.string_to_tokens(~c"v(-1)", line, column, file, opts)
    {:ok, prefix ++ tokens}
  end

  defp adjust_operator(tokens, _line, _column, _file, _opts, _last_op), do: {:ok, tokens}

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

  defp loop(%{server: server, ref: ref} = state) do
    receive do
      {:eval, ^server, code, counter, parser_state} ->
        {status, parser_state, state} = parse_eval_inspect(code, counter, parser_state, state)
        send(server, {:evaled, self(), status, parser_state})
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

      {:done, ^server, next?} ->
        {:ok, next?}

      {:done, ^ref, next?} ->
        {:ok, next?}
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

  defp loop_state(ref, server, history, opts) do
    env = opts[:env] || Code.env_for_eval(file: "iex")
    {_, _, env} = Code.eval_quoted_with_env(quote(do: import(IEx.Helpers)), [], env)
    stacktrace = opts[:stacktrace]
    binding = opts[:binding] || []

    state = %{
      binding: binding,
      env: env,
      server: server,
      history: history,
      stacktrace: stacktrace,
      ref: ref
    }

    dot_iex = opts[:dot_iex] || Config.dot_iex()

    case dot_iex do
      "" -> state
      path -> load_dot_iex(state, path)
    end
  end

  defp load_dot_iex(state, path) do
    candidates =
      if path do
        [path]
      else
        # Do not assume there is a $HOME
        for dir <- [".", System.get_env("IEX_HOME") || System.user_home()],
            dir != nil,
            do: dir |> Path.join(".iex.exs") |> Path.expand()
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
      quoted = :elixir.string_to_quoted!(String.to_charlist(code), 1, 1, path, [])
      Process.put(:iex_imported_paths, MapSet.new([path]))

      # Evaluate the contents in the same environment server_loop will run in
      env = %{state.env | file: path, line: 1}
      {_result, binding, env} = eval_expr_by_expr(quoted, state.binding, env)
      %{state | binding: binding, env: %{env | file: "iex", line: 1}}
    catch
      kind, error ->
        io_result("Error while evaluating: #{path}")
        print_error(kind, error, __STACKTRACE__)
        state
    after
      Process.delete(:iex_imported_paths)
    end
  end

  defp parse_eval_inspect(code, counter, parser_state, state) do
    try do
      {parser_module, parser_fun, args} = IEx.Config.parser()
      args = [code, [line: counter, file: "iex"], parser_state | args]
      eval_and_inspect_parsed(apply(parser_module, parser_fun, args), counter, state)
    catch
      kind, error ->
        print_error(kind, error, __STACKTRACE__)
        {:error, "", state}
    end
  end

  defp eval_and_inspect_parsed({:ok, forms, parser_state}, counter, state) do
    put_history(state)
    put_whereami(state)
    state = eval_and_inspect(forms, counter, state)
    {:ok, parser_state, state}
  after
    Process.delete(:iex_history)
    Process.delete(:iex_whereami)
  end

  defp eval_and_inspect_parsed({:incomplete, parser_state}, _counter, state) do
    {:incomplete, parser_state, state}
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

  defp eval_and_inspect(forms, line, state) do
    %{env: env, binding: binding} = state
    forms = add_if_undefined_apply_to_vars(forms, env)
    {result, binding, env} = eval_expr_by_expr(forms, binding, env)

    if result != IEx.dont_display_result() do
      io_inspect(result)
    end

    history = IEx.History.append(state.history, {line, result}, IEx.Config.history_size())
    %{state | env: env, binding: binding, history: history}
  end

  defp add_if_undefined_apply_to_vars(forms, env) do
    Macro.prewalk(forms, fn
      {var, meta, context} when is_atom(var) and is_atom(context) ->
        if Macro.Env.lookup_import(env, {var, 0}) != [] do
          {var, Keyword.put_new(meta, :if_undefined, :apply), context}
        else
          {var, meta, context}
        end

      other ->
        other
    end)
  end

  defp eval_expr_by_expr(expr, binding, env) do
    case maybe_expand(expr, env) do
      {:__block__, _, exprs} ->
        Enum.reduce(exprs, {nil, binding, env}, fn expr, {_result, binding, env} ->
          eval_expr_by_expr(expr, binding, env)
        end)

      expr ->
        Code.eval_quoted_with_env(expr, binding, env)
    end
  end

  defp maybe_expand({import_file, _, [_ | _]} = expr, env)
       when import_file in [:import_file, :import_file_if_available],
       do: Macro.expand(expr, env)

  defp maybe_expand(expr, _env),
    do: expr

  defp io_inspect(result) do
    io_result(inspect(result, IEx.inspect_opts()))
  end

  defp io_result(result) do
    IO.puts(:stdio, IEx.color(:eval_result, result))
  end

  ## Error handling

  defp print_error(kind, reason, stacktrace) do
    {blamed, stacktrace} =
      case reason do
        %CompileError{description: "cannot compile file (errors have been logged)" <> _, line: 0} ->
          {%CompileError{description: "cannot compile code (errors have been logged)"}, []}

        _ ->
          Exception.blame(kind, reason, stacktrace)
      end

    ansidata =
      case blamed do
        %FunctionClauseError{} ->
          {_, inspect_opts} = pop_in(IEx.inspect_opts()[:syntax_colors][:reset])
          banner = Exception.format_banner(kind, reason, stacktrace)
          blame = FunctionClauseError.blame(blamed, &inspect(&1, inspect_opts), &blame_match/1)
          [IEx.color(:eval_error, banner), pad(blame)]

        _ ->
          banner = Exception.format_banner(kind, blamed, stacktrace)

          if String.contains?(banner, IO.ANSI.reset()) do
            [banner]
          else
            [IEx.color(:eval_error, banner)]
          end
      end

    stackdata = Exception.format_stacktrace(prune_stacktrace(stacktrace))
    IO.write(:stdio, [ansidata, ?\n, IEx.color(:stack_info, stackdata)])
  end

  defp pad(string) do
    "    " <> String.replace(string, "\n", "\n    ")
  end

  defp blame_match(%{match?: true, node: node}), do: Macro.to_string(node)
  defp blame_match(%{match?: false, node: node}), do: blame_ansi(:blame_diff, "-", node)

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

  defp prune_stacktrace(stack) do
    stack
    |> Enum.reverse()
    |> Enum.drop_while(&(elem(&1, 0) != :elixir_eval))
    |> Enum.reverse()
    |> case do
      [] -> stack
      stack -> stack
    end
  end
end
