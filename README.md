# Elixir

Nothing interesting here yet. Try again later.

# Roadmap

* Add true, false, if/else, case/when
* Check what happens with several method definitions
* Add alias_method
* Add interpolation to regexp and atoms
* Add missing types (regexp)
* Add $"foo" as shortcut to create a list of integers
* Allow classes to be reopened
* Add generators
* Add variables to object declarations and copy them on inheritance
* Add tail call optimization
* Add metaprogramming
* Add partial application, pipeline f1 + f2, and 1#add and Integer##add 
* Add _.foo
* Improve STDLIB
* Add load paths
* Add JIT on module compilation
* Allow methods to be private
* Extending builtin types (like inheriting from Integer)
* Add method cache table
* Improve constant lookup

# Examples

Imagine that we have a dictionary and we want to represent it as a string in the following format:

    { k1: v1, k2: v2, ... }
    
In Elixir, the implementation would be as follow:

    object Dict
      def to_s
        transformer = -> (key, value, acc) ["#{key.inspect}: #{value.inspect}"|acc]
        "{" + fold([], transformer).join(", ") + "}"
      end
    end

In Erlang, we would need the following:

    -module(dict).

    to_s(Dict) ->
      Transformer = fun(Key, Value, Acc) -> [string_format(Key) ++ ": " ++ string_format(Value)|Acc] end,
      List = dict:fold(Transformer, [], Dict),
      "{" ++ string:join(List, ", ") ++ "}".

    string_format(Thing) ->
      lists:flatten(io_lib:format("~p", [Thing])).

The Object Oriented aspect of Elixir brings several benefits. For example, we don't need to explicitly call *dict:fold()* because the *to_s* method in Elixir is already in the Dictionary scope, so we just call the *fold* method directly. The same applies to calling the function *join*. In Elixir, *join* is a method implemented in the List object, which is returned as result of the *fold* call. So we can simply call *join(", ")* in the List object instead of calling *string:join(List, ", ")* passing the List as argument.