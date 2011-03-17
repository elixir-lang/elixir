Code.require File.expand_path("../test_helper", __FILE__)
Code.require "ets.ex"

object ETSTest
  proto ExUnit::Case

  def ets_test
    table = ETS.create()
    name = String.new "Andrew"
    age = 21
    prof = 'student
    table.insert({"i", {name, age, prof}})
    [{"i", _}] = table.lookup("i")
    [{_, {_, 21, _}}] = table.lookup("i")
    [{_, {"Andrew", _, 'student}}] = table.lookup("i")
    table.delete("i")
    [] = table.lookup("i")
    table.delete
  end
end