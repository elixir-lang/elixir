defmodule IEx.Server do
  @moduledoc false

  @doc """
  Eval loop for an IEx session. Its responsibilities include:

  * loading of .iex files
  * reading input
  * trapping exceptions in the code being evaluated
  * keeping expression history

  """
  def start(config) do
    IEx.History.init

    { _, _, scope } = :elixir.eval('require IEx.Helpers', [], 0, config.scope)
    config = config.scope(scope)

    config = case config.dot_iex_path do
      ""   -> config                     # don't load anything
      nil  -> load_dot_iex(config)       # load .iex from predefined locations
      path -> load_dot_iex(config, path) # load from `path`
    end

    IO.puts "Interactive Elixir (#{System.version}) - press Ctrl+C to exit (type h() ENTER for help)"

    old_flag = Process.flag(:trap_exit, true)
    self_pid = self

    # We have one loop for receiving input and
    # another loop for evaluating contents.
    pid = spawn_link(fn -> input_loop(self_pid) end)

    try do
      eval_loop(config.input_pid(pid))
    after
      Process.exit(pid, :normal)
      Process.flag(:trap_exit, old_flag)
    end
  end

  ## Eval loop

  defp eval_loop(config) do
    prefix = config.cache != []
    config.input_pid <- { :do_input, self, prefix, config.counter }
    wait_input(config)
  end

  defp wait_input(config) do
    pid = config.input_pid

    receive do
      { :input, ^pid, data } when is_binary(data) ->
        new_config =
          try do
            line    = String.to_char_list!(data)
            counter = config.counter
            code    = config.cache
            eval(code, line, counter, config)
          catch
            kind, error ->
              print_error(kind, error, System.stacktrace)
              config.cache('')
          end

        eval_loop(new_config)
      { :input, ^pid, :eof } ->
        :ok
      { :input, ^pid, { :error, :interrupted } } ->
        io_error "** (EXIT) interrupted"
        eval_loop(config.cache(''))
      { :input, ^pid, { :error, :terminated } } ->
        :ok
      { :EXIT, _pid, :normal } ->
        wait_input(config)
      { :EXIT, pid, reason } ->
        print_exit(pid, reason)
        wait_input(config)
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
  # Returns updated config.
  #
  # The first two clauses provide support for the break-trigger allowing to
  # break out from a pending incomplete expression. See
  # https://github.com/elixir-lang/elixir/issues/1089 for discussion.
  #
  @break_trigger '#iex:break\n'

  defp eval(_, @break_trigger, _, config=IEx.Config[cache: '']) do
    config
  end

  defp eval(_, @break_trigger, line_no, _) do
    :elixir_errors.parse_error(line_no, "iex", 'incomplete expression', [])
  end

  defp eval(code_so_far, latest_input, line_no, config) do
    code = code_so_far ++ latest_input

    case :elixir_translator.forms(code, line_no, "iex", []) do
      { :ok, forms } ->
        { result, new_binding, scope } =
          :elixir.eval_forms(forms, config.binding, config.scope)

        unless result == IEx.dont_display_result, do: io_put result

        config = config.cache(code).scope(nil).result(result)
        update_history(config)
        config.update_counter(&(&1+1)).cache('').binding(new_binding).scope(scope).result(nil)

      { :error, { line_no, error, token } } ->
        if token == [] do
          # Update config.cache so that IEx continues to add new input to
          # the unfinished expression in `code`
          config.cache(code)
        else
          # Encountered malformed expression
          :elixir_errors.parse_error(line_no, "iex", error, token)
        end
    end
  end

  defp update_history(config) do
    IEx.History.append(config, config.counter)
  end

  defp io_put(result) do
    IO.puts :stdio, IEx.color(:eval_result, inspect(result, inspect_opts))
  end

  defp io_error(result) do
    IO.puts :stdio, IEx.color(:eval_error, result)
  end

  defp inspect_opts do
    opts = IEx.Options.get(:inspect)
    case :io.columns(:standard_input) do
      { :ok, width } -> Keyword.put(opts, :width, min(width, 80))
      { :error, _ }  -> opts
    end
  end

  ## Load dot iex helpers

  # Locates and loads an .iex file from one of predefined locations. Returns
  # new config.
  defp load_dot_iex(config, path // nil) do
    candidates = if path do
      [path]
    else
      Enum.map [".iex", "~/.iex"], &Path.expand/1
    end

    path = Enum.find candidates, &File.regular?/1

    if nil?(path) do
      config
    else
      eval_dot_iex(config, path)
    end
  end

  defp eval_dot_iex(config, path) do
    try do
      code  = File.read!(path)
      scope = :elixir.scope_for_eval(config.scope, file: path)

      # Evaluate the contents in the same environment eval_loop will run in
      { _result, binding, scope } =
        :elixir.eval(String.to_char_list!(code),
                     config.binding,
                     0,
                     scope)

      scope = :elixir.scope_for_eval(scope, file: "iex")
      config.binding(binding).scope(scope)
    catch
      kind, error ->
        print_error(kind, error, System.stacktrace)
        System.halt(1)
    end
  end

  ## Input loop

  defp input_loop(pid) do
    receive do
      { :do_input, ^pid, prefix, counter } ->
        pid <- { :input, self, io_get(prefix, counter) }
    end
    input_loop(pid)
  end

  defp io_get(prefix, counter) do
    prefix = if prefix, do: "..."

    prompt =
      if is_alive do
        "#{prefix || remote_prefix}(#{node})#{counter}> "
      else
        "#{prefix || "iex"}(#{counter})> "
      end

    IO.gets(:stdio, prompt)
  end

  defp remote_prefix do
    if node == node(:erlang.group_leader), do: "iex", else: "rem"
  end

  ## Error handling

  defp print_error(:error, exception, stacktrace) do
    { exception, stacktrace } = normalize_exception(exception, stacktrace)
    print_stacktrace stacktrace, fn ->
      "** (#{inspect exception.__record__(:name)}) #{exception.message}"
    end
  end

  defp print_error(kind, reason, stacktrace) do
    print_stacktrace stacktrace, fn ->
      "** (#{kind}) #{inspect(reason)}"
    end
  end

  defp print_exit(pid, reason) do
    io_error "** (EXIT from #{inspect pid}) #{inspect(reason)}"
  end

  defp normalize_exception(:undef, [{ IEx.Helpers, fun, arity, _ }|t]) do
    { UndefinedFunctionError[function: fun, arity: arity], t }
  end

  defp normalize_exception(exception, stacktrace) do
    { Exception.normalize(:error, exception), stacktrace }
  end

  defp print_stacktrace(trace, callback) do
    try do
      io_error callback.()
      case prune_stacktrace(trace) do
        []    -> :ok
        other -> io_error Exception.format_stacktrace(other)
      end
    catch
      _, _ ->
        io_error "** (IEx.Error) error when printing exception message and stacktrace"
    end
  end

  defp prune_stacktrace([{ :erl_eval, _, _, _ }|_]),  do: []
  defp prune_stacktrace([{ IEx.Server, _, _, _ }|_]), do: []
  defp prune_stacktrace([h|t]), do: [h|prune_stacktrace(t)]
  defp prune_stacktrace([]), do: []
end
