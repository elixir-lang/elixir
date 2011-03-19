Code.require File.expand_path("../test_helper", __FILE__)
Code.require "ets.ex"

object ETSTest
  proto ExUnit::Case

  def ets_set_test
    t = ETS.create('set, 1)
    t.insert({String.new("Andrew"), 21, 'boy, 'minsk})
    t.insert({String.new("Mary"), 19, 'girl, 'moscow})
    t.insert({String.new("Jack"), 21, 'boy, 'paris})
    t.insert({String.new("Mary"), 20, 'girl, 'london})

    1 = t.lookup("Mary").size

    true = t.update_element "Mary", 4, 'oslo
    'badarg = t.update_element "Mary", 1, "Suzzy"
    false = t.update_element "John", 2, 18

    t.delete
  end

  def ets_ord_test
    t = ETS.create('ordered_set, 2)
    t.insert({String.new("Andrew"), 21, 'boy, 'minsk})
    t.insert({String.new("Mary"), 19, 'girl, 'moscow})
    t.insert({String.new("Jane"), 20, 'girl, 'london})
    t.insert({String.new("Jack"), 21, 'boy, 'paris})

    21 = t.last
    19 = t.first

    [{"Jack", _, _, _}] = t.lookup 21

    t.delete
  end

  def ets_bag_test
    t = ETS.create('bag, 1)
    t.insert({String.new("Andrew"), 21, 'boy, 'minsk})
    t.insert({String.new("Mary"), 19, 'girl, 'moscow})
    t.insert({String.new("Jane"), 20, 'girl, 'london})
    t.insert({String.new("Jack"), 21, 'boy, 'paris})

    [{_, year, sex, _}] = t.lookup "Jane"
    false = t.insert_new({"Jane", year, sex, 'rome})
    [{_, _, _, 'london}] = t.lookup "Jane"

    ['boy] = t.lookup_element "Andrew", 3
    'badarg = t.lookup_element "Peter", 3

    'badarg = t.update_element "Mary", 2, 21

    t.insert({String.new("Jane"), 20, 'girl, 'london})
    t.insert({String.new("Jane"), 20, 'girl, 'berlin})
    [20, 20] = t.lookup_element "Jane", 2
    2 = t.lookup("Jane").size

    t.delete
  end

  def ets_dup_test
    t = ETS.create('duplicate_bag, 1)
    t.insert({String.new("Andrew"), 21, 'boy, 'minsk})
    t.insert({String.new("Mary"), 19, 'girl, 'moscow})
    t.insert({String.new("Jane"), 20, 'girl, 'london})
    t.insert({String.new("Jack"), 21, 'boy, 'paris})
    t.insert({String.new("Jane"), 20, 'girl, 'london})

    t.insert({String.new("Jane"), 22, 'girl, 'berlin})
    3 = t.lookup("Jane").size

    t.delete
  end
end