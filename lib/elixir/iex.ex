defmodule Elixir::IEx do
  import Exception, only: [format_stacktrace: 1]

  def start do
    IO.puts "Interactive Elixir (#{Code.version}) - press Ctrl+C to exit"
    function = fn(do: do_loop([], ''))
    Erlang.user_drv.start([:"tty_sl -c -e", {:erlang, :spawn, [function]}])
  end

  def do_loop(binding, code_cache) do
    prompt = case code_cache do
    match: []
      "iex> "
    match: _
      "...> "
    end

    code = code_cache ++ Erlang.io.get_line(prompt)

    { binding_to_return, code_cache_to_return } = try do
      { result, new_binding } = Erlang.elixir.eval(code, binding)
      IO.puts inspect(result)
      { new_binding, '' }
    rescue: TokenMissingError
      { binding, code }
    rescue: exception
      stacktrace = Code.stacktrace
      IO.puts :standard_error, "** (#{exception.__record__(:name)}) #{exception.message}"
      print_stacktrace stacktrace
      { binding, '' }
    catch: kind, error
      stacktrace = Code.stacktrace
      IO.puts :standard_error, "** (#{kind}) #{inspect(error)}"
      print_stacktrace stacktrace
      { binding, '' }
    end

    do_loop(binding_to_return, code_cache_to_return)
  end

  defp print_stacktrace(stacktrace) do
    Enum.each stacktrace, fn(s, do: IO.puts :standard_error, "    #{format_stacktrace(s)}")
  end
end