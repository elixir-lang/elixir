Code.require "ex_unit"
Code.require "list_test"
Code.require "tuple_test"
Code.require "ordered_dict_test"

ExUnit.run([ListTest, TupleTest, OrderedDictTest])
