defmodule IO do
  def print(device // :standard_io, item) do
    Erlang.io.format device, to_binary(item), []
  end

  def puts(device // :standard_io, item) do
    print(device, item)
    Erlang.io.format("~n")
  end

  def inspect(device // :standard_io, item) do
    puts device, Elixir.Builtin.inspect(item)
  end
end