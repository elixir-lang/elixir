defmodule IEx.Server do
  @moduledoc false

  def start(config) do
    IO.puts "Interactive Elixir (#{System.version}) - press Ctrl+C to exit (type h() ENTER for help)"
    Process.put :iex_history, []
    { _, _, scope } = :elixir.eval('require IEx.Helpers', [], 0, config.scope)
    do_loop(config.scope(scope))
  end

  defp do_loop(config) do
    counter = config.counter
    cache   = config.cache
    code    = cache ++ io_get(config)
    file    = "iex"

    new_config =
      try do
        # Instead of doing just `eval`, we first parse the expression to see if
        # it's well formed. If parsing succeeds, we evaluate the AST as usual.
        #
        # If parsing fails, this might be a TokenMissingError which we treat in
        # a special way (to allow for continuation of an expression on the next
        # line in IEx). In case of any other error, we let :elixir_translator
        # to re-raise it.
        case :elixir_translator.forms(code, counter, file, []) do
          { :ok, forms } ->
            { result, new_binding, scope } =
              :elixir.eval_forms(forms, config.binding, config.scope)

            io_put result

            config = config.result(result)
            update_history(config.cache(code).scope(nil))
            config.update_counter(&1+1).cache('').binding(new_binding).scope(scope)

          { :error, { line, error, token } } ->
            if token == [] do
              # Update config.cache so that IEx continues to add new input to
              # the unfinished expression in `code`
              config.cache(code)
            else
              # Encountered malformed expression
              :elixir_translator.parse_error(line, file, error, token)
            end
        end
      rescue
        exception ->
          print_stacktrace System.stacktrace, fn ->
            "** (#{inspect exception.__record__(:name)}) #{exception.message}"
          end
          config.cache('')
      catch
        kind, error ->
          print_stacktrace System.stacktrace, fn ->
            "** (#{kind}) #{inspect(error)}"
          end
          config.cache('')
      end

    do_loop(new_config)
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

  defp update_history(config) do
    current = Process.get :iex_history
    Process.put :iex_history, [config|current]
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
      { :error, _ } -> ''
      data -> :unicode.characters_to_list(data)
    end
  end

  defp io_put(result) do
    IO.puts :stdio, IO.ANSI.escape("%{yellow}#{inspect(result, IEx.inspect_opts)}")
  end

  defp io_error(result) do
    IO.puts :stdio, result
  end

  defp remote_prefix do
    if node == node(:erlang.group_leader), do: "iex", else: "rem"
  end
end
