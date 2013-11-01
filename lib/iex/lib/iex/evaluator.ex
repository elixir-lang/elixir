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
  def start(server, leader) do
    IEx.History.init
    old_leader = Process.group_leader
    old_flag   = Process.flag(:trap_exit, true)
    Process.group_leader(self, leader)

    try do
      loop(server)
    after
      IEx.History.reset
      Process.group_leader(self, old_leader)
      Process.flag(:trap_exit, old_flag)
    end
  end

  defp loop(server) do
    receive do
      { :eval, ^server, code, config } ->
        server <- { :evaled, self, eval(code, config) }
        loop(server)
      { :done, ^server } ->
        IEx.History.reset
        :ok

      { :EXIT, _other, :normal } ->
        loop(server)
      { :EXIT, other, reason } ->
        print_exit(other, reason)
        loop(server)
    end
  end

  @doc """
  Locates and loads an .iex file from one of predefined locations.
  Returns the new config.
  """
  def load_dot_iex(config, path // nil) do
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

  defp eval(code, config) do
    try do
      do_eval(String.to_char_list!(code), config)
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
        update_history(line, code, result)
        config.update_counter(&(&1+1)).cache('').binding(new_binding).scope(scope)

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

  defp update_history(counter, cache, result) do
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
    { RuntimeError[message: "undefined function: #{format_function(fun, arity)}"], t }
  end

  defp normalize_exception(exception, stacktrace) do
    { Exception.normalize(:error, exception), stacktrace }
  end

  defp format_function(fun, arity) do
    cond do
      is_list(arity) ->
        "#{fun}/#{Enum.count(arity)}"
      true ->
        "#{fun}/#{arity}"
    end
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
  defp prune_stacktrace([{ __MODULE__, _, _, _ }|_]), do: []
  defp prune_stacktrace([h|t]), do: [h|prune_stacktrace(t)]
  defp prune_stacktrace([]), do: []
end