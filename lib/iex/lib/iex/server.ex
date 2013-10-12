defmodule IEx.Server do
  @moduledoc false

  defrecord Config, binding: nil, cache: '', counter: 1, prefix: "iex",
                    scope: nil, result: nil, evaluator: nil

  @doc """
  Finds where the current IEx server is located.
  """
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
        :ok | { :error, :self } | { :error, :no_iex }
  def take_over(identifier, opts, timeout // 1000) do
    server = whereis()
    opts   = Keyword.put(opts, :evaluator, self)

    if server == self do
      { :error, :self }
    else
      ref = make_ref()
      server <- { :take?, self, ref }

      receive do
        ^ref ->
          server <- { :take, identifier, opts }
          IEx.History.init
          eval_loop(server)
      after
        timeout ->
          { :error, :no_iex }
      end
    end
  end

  defp eval_loop(server) do
    receive do
      { :eval, ^server, code, config } ->
        server <- { :evaled, self, eval(code, config) }
        eval_loop(server)
      { :done, ^server } ->
        IEx.History.reset
        :ok
    end
  end

  @doc """
  Eval loop for an IEx session. Its responsibilities include:

  * loading of .iex files
  * reading input
  * trapping exceptions in the code being evaluated
  * keeping expression history

  """
  def start(opts) when is_list(opts) do
    IO.puts "Interactive Elixir (#{System.version}) - press Ctrl+C to exit (type h() ENTER for help)"
    IEx.History.init

    config   = boot_config(opts)
    old_flag = Process.flag(:trap_exit, true)

    try do
      server_loop(config)
    after
      if evaluator = config.evaluator do
        evaluator <- { :done, self }
      end

      Process.flag(:trap_exit, old_flag)
    end
  end

  ## Server loop

  defp server_loop(config) do
    self_pid = self()
    counter  = config.counter
    prefix   = if config.cache != [], do: "...", else: config.prefix

    pid = spawn(fn -> io_get(self_pid, prefix, counter) end)
    wait_input(config, pid)
  end

  defp wait_input(config, pid) do
    receive do
      { :input, ^pid, code } when is_binary(code) ->
        server_loop(
          try do
            code = String.to_char_list!(code)
            server_eval(code, config)
          catch
            kind, error ->
              print_error(kind, error, System.stacktrace)
              config.cache('')
          end
        )
      { :input, ^pid, :eof } ->
        :ok
      { :input, ^pid, { :error, :interrupted } } ->
        io_error "** (EXIT) interrupted"
        server_loop(config.cache(''))
      { :input, ^pid, { :error, :terminated } } ->
        :ok

      { :take?, other, ref } ->
        other <- ref
        wait_input(config, pid)
      { :take, identifier, opts } ->
        Process.exit(pid, :kill)
        answer = IO.gets(:stdio, "\n#{identifier}. Allow? [Yn] ")

        if answer =~ %r/^(Y(es)?)?$/i do
          IEx.History.reset
          start(opts)
        else
          IO.puts("")
          server_loop(config)
        end

      { :EXIT, _other, :normal } ->
        wait_input(config, pid)
      { :EXIT, other, reason } ->
        print_exit(other, reason)
        wait_input(config, pid)
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

  defp server_eval(code, config) do
    if evaluator = config.evaluator do
      evaluator <- { :eval, self, code, config }
      receive do: ({ :evaled, ^evaluator, config } -> config)
    else
      eval(code, config)
    end
  end

  defp eval(code, config) do
    try do
      do_eval(code, config)
    catch
      kind, error ->
        print_error(kind, error, System.stacktrace)
        config.cache('')
    end
  end

  defp do_eval(@break_trigger, config=Config[cache: '']) do
    config
  end

  defp do_eval(@break_trigger, config) do
    :elixir_errors.parse_error(config.counter, "iex", 'incomplete expression', [])
  end

  defp do_eval(latest_input, config) do
    code = config.cache ++ latest_input
    line = config.counter

    case :elixir_translator.forms(code, line, "iex", []) do
      { :ok, forms } ->
        { result, new_binding, scope } =
          :elixir.eval_forms(forms, config.binding, config.scope)

        unless result == IEx.dont_display_result, do: io_put result

        config = config.cache(code).scope(nil).result(result)
        update_history(config)
        config.update_counter(&(&1+1)).cache('').binding(new_binding).scope(scope).result(nil)

      { :error, { line, error, token } } ->
        if token == [] do
          # Update config.cache so that IEx continues to add new input to
          # the unfinished expression in `code`
          config.cache(code)
        else
          # Encountered malformed expression
          :elixir_errors.parse_error(line, "iex", error, token)
        end
    end
  end

  defp update_history(Config[result: result, counter: counter, cache: cache]) do
    IEx.History.append({ counter, cache, result }, counter, IEx.Options.get(:history_size))
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

  ## Config and load dot iex helpers

  defp boot_config(opts) do
    locals = Keyword.get(opts, :delegate_locals_to, IEx.Helpers)

    scope =
      if env = opts[:env] do
        scope = :elixir_scope.to_erl_env(env)
        :elixir.scope_for_eval(scope, delegate_locals_to: locals)
      else
        :elixir.scope_for_eval(file: "iex", delegate_locals_to: locals)
      end

    { _, _, scope } = :elixir.eval('require IEx.Helpers', [], 0, scope)

    binding = Keyword.get(opts, :binding, [])
    prefix  = Keyword.get(opts, :prefix, "iex")
    config  = Config[binding: binding, scope: scope,
                     prefix: prefix, evaluator: opts[:evaluator]]

    case opts[:dot_iex_path] do
      ""   -> config                     # don't load anything
      nil  -> load_dot_iex(config)       # load .iex from predefined locations
      path -> load_dot_iex(config, path) # load from `path`
    end
  end

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

      # Evaluate the contents in the same environment server_loop will run in
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

  ## Get input

  defp io_get(pid, prefix, counter) do
    prompt =
      if is_alive do
        "#{prefix || remote_prefix}(#{node})#{counter}> "
      else
        "#{prefix || "iex"}(#{counter})> "
      end

    pid <- { :input, self, IO.gets(:stdio, prompt) }
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
