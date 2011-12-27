module Elixir::IEx

def start do
  Erlang.io.format("Interactive Elixir (#{Elixir::Code.version})\nRunning on ")
  function = fn { loop([], '') }
  Erlang.user_drv.start([:"tty_sl -c -e", {:erlang, :spawn, [function]}])
end

def loop(binding, code_cache) do
  prompt = case code_cache do
  match: []
    "iex> "
  match: _
    "...> "
  end

  code = code_cache ++ Erlang.io.get_line(prompt)

  {binding_to_return, code_cache_to_return} = try do
    {result, new_binding} = Erlang.elixir.eval(code, binding)
    Erlang.io.format("~p~n", [result])
    {new_binding, ''}
  catch: { :error, {:badsyntax, {_, _, _, []}}, _};
    {binding, code}
  end

  loop(binding_to_return, code_cache_to_return)
end
