module Elixir::IEx do
  def start do
    IO.puts "Interactive Elixir (#{Code.version}) - press Ctrl+C to exit"
    function = fn { do_loop([], '') }
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

    {binding_to_return, code_cache_to_return} = try do
      {result, new_binding} = Erlang.elixir.eval(code, binding)
      # TODO: Remove this for IO.puts inspect() once we have inspect protocol
      Erlang.io.format("~p~n", [result])
      {new_binding, ''}
    catch: { :error, {:badsyntax, {_, _, _, []}}, _}
      {binding, code}
    catch: { kind, error, _ }
      IO.puts :standard_error, "** #{atom_to_binary(kind, :utf8)} #{Elixir::Formatter.format_catch(kind, error)}"
      List.each Code.stacktrace, fn(s) { IO.puts :standard_error, "    #{Elixir::Formatter.format_stacktrace(s)}" }
      { binding, '' }
    end

    do_loop(binding_to_return, code_cache_to_return)
  end
end