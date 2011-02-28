% Dependencies
Code.require "ex_unit"

% Tests
Code.require "atom_test"
Code.require "bit_string_test"
Code.require "exceptions_test"
Code.require "gen_server_test"
Code.require "list_test"
Code.require "object_test"
Code.require "ordered_dict_test"
Code.require "process_test"
Code.require "regexp_test"
Code.require "string_test"
Code.require "tuple_test"

ExUnit.run([AtomTest, BitStringTest, ExceptionsTest, GenServerTest, ListTest, ObjectTest,
  OrderedDictTest, ProcessTest, RegexpTest, StringTest, TupleTest])
