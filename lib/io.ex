defmodule IO do
  def print(device // :standard_io, item) do
    Erlang.io.put_chars device, to_binary(item)
  end

  def puts(device // :standard_io, item) do
    Erlang.io.put_chars device, to_binary(item)
    Erlang.io.nl(device)
  end

  def inspect(device // :standard_io, item) do
    puts device, Elixir.Builtin.inspect(item)
  end
end