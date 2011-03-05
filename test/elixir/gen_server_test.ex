Code.require File.expand_path("../test_helper", __FILE__)
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

  def gen_server_with_object_test
    bookshelf = BestBookshelf.new ["Crafting Rails Apps", "Programming Erlang"]
    'ok = bookshelf.take "Programming Erlang"
    ["Crafting Rails Apps"] = bookshelf.see
    'ok = bookshelf.put "Programming Elixir"
    ["Programming Elixir", "Crafting Rails Apps"] = bookshelf.see
    bookshelf.terminate
  end
end