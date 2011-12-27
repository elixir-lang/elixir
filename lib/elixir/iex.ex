module Elixir::IEx

def start do
  Erlang.io.format("Interactive Elixir Running on")
  function = fn { loop([], "") }
  Erlang.user_drv.start([:"tty_sl -c -e", {:erlang, :spawn, [function]}])
end

def loop(binding, code_cache) do
  code = Erlang.io.get_line("iex> ")
  {result, new_binding} = Erlang.elixir.eval(code, binding)
  Erlang.io.format("~p~n", [result])
  loop(new_binding, code_cache)
end
