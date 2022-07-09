defmodule Foo do
  dbg(Atom.to_string(:inside_module))

  def foo do
    dbg(Atom.to_string(:inside_fun))
  end
end

Foo.foo()

dbg(:crypto.strong_rand_bytes(10))

"http://google.com"
|> URI.parse()
|> URI.merge("/search")
|> dbg()

result = dbg(
  [1, 2, 3]
  |> tl()
  |> hd
  |> Kernel.+(10), base: :binary)

[:pipe1, :pipe2]
|> tl()
|> hd()
|> dbg()

Application.put_env(:elixir, :ansi_enabled, false)
[:pipe1, :pipe2]
|> tl()
|> hd()
|> dbg()
