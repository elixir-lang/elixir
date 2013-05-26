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
    do_loop(config)
  end

  defp do_loop(config) do
    counter = config.counter
    code    = config.cache
    line    = io_get(config)

    unless line == :eof do
      new_config =
        try do
          eval(code, line, counter, config)
        rescue
          exception ->
            print_exception(exception, System.stacktrace)
            config.cache('')
        catch
          kind, error ->
            print_error(kind, error, System.stacktrace)
            config.cache('')
        end

      do_loop(new_config)
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
    # do nothing
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

        io_put result

        config = config.result(result)
        update_history(config.cache(code).scope(nil))
        config.update_counter(&1+1).cache('').binding(new_binding).scope(scope)

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

  # Locates and loads an .iex file from one of predefined locations. Returns
  # new config.
  defp load_dot_iex(config, path // nil) do
    candidates = if path do
      [path]
    else
      Enum.map [".iex", "~/.iex"], Path.expand(&1)
    end
    path = Enum.find candidates, fn path -> File.regular?(path) end
    if nil?(path) do
      config
    else
      try do
        code = File.read!(path)
        scope = :elixir.scope_for_eval(config.scope, file: path)

        # Evaluate the contents in the same environment do_loop will run in
        { _result, binding, scope } =
          :elixir.eval(:unicode.characters_to_list(code),
                       config.binding,
                       0,
                       scope)

        scope = :elixir.scope_for_eval(scope, file: "iex")
        config.binding(binding).scope(scope)
      rescue
        exception ->
          print_exception(exception, System.stacktrace)
          System.halt(1)
      catch
        kind, error ->
          print_error(kind, error, System.stacktrace)
          System.halt(1)
      end
    end
  end

  defp update_history(config) do
    IEx.History.append(config)
  end

  defp io_get(config) do
    prefix = if config.cache != [], do: "..."

    prompt =
      if is_alive do
        "#{prefix || remote_prefix}(#{node})#{config.counter}> "
      else
        "#{prefix || "iex"}(#{config.counter})> "
      end

    case IO.gets(:stdio, prompt) do
      :eof -> :eof
      { :error, _ } -> ''
      data -> :unicode.characters_to_list(data)
    end
  end

  defp io_put(result) do
    IO.puts :stdio, IEx.color(:eval_result, inspect(result, IEx.Options.get(:inspect)))
  end

  defp io_error(result) do
    IO.puts :stdio, IEx.color(:error, result)
  end

  defp remote_prefix do
    if node == node(:erlang.group_leader), do: "iex", else: "rem"
  end

  defp print_exception(exception, stacktrace) do
    print_stacktrace stacktrace, fn ->
      "** (#{inspect exception.__record__(:name)}) #{exception.message}"
    end
  end

  defp print_error(kind, reason, stacktrace) do
    print_stacktrace stacktrace, fn ->
      "** (#{kind}) #{inspect(reason)}"
    end
  end

  defp print_stacktrace(trace, callback) do
    try do
      io_error callback.()
      io_error Exception.format_stacktrace(trace)
    catch
      _, _ ->
        io_error "** (IEx.Error) error when printing exception message and stacktrace"
    end
  end
end
