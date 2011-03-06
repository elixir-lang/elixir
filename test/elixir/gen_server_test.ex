Code.require File.expand_path("../test_helper", __FILE__)
Code.require "fixtures/bookshelf"

object GenServerTest
  proto ExUnit::Case

  def gen_server_test
    bookshelf = Bookshelf.new ["Crafting Rails Apps", "Programming Erlang"]
    'ok = bookshelf.take "Programming Erlang"
    ["Crafting Rails Apps"] = bookshelf.see
    'ok = bookshelf.put "Programming Elixir"
    ["Programming Elixir", "Crafting Rails Apps"] = bookshelf.see
    bookshelf.terminate
  end
end