dbg(:crypto.strong_rand_bytes(10))
dbg(var = :foo)
dbg(var)

"http://google.com"
|> URI.parse()
|> URI.merge("/search")
|> dbg()


result = dbg(
  [1, 2, 3]
  |> tl()
  |> hd
  |> Kernel.+(10), base: :binary)

IO.inspect(result, label: "Result")


[:pipe1, :pipe2]
|> tl()
|> hd()
|> dbg()
