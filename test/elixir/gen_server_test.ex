Code.require "fixtures/bookshelf"

object GenServerTest
  proto ExUnit::Case

  def gen_server_with_module_test
    { 'ok, pid } = Bookshelf.start! ["Crafting Rails Apps", "Programming Erlang"]
    'ok = Bookshelf.take(pid, "Programming Erlang")
    ["Crafting Rails Apps"] = Bookshelf.see(pid)
    'ok = Bookshelf.put(pid, "Programming Elixir")
    ["Programming Elixir", "Crafting Rails Apps"] = Bookshelf.see(pid)
    Bookshelf.terminate(pid)
  end
end